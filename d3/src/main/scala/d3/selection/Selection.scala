package d3.selection

import org.scalajs.dom
import scalajs.js
import cats.syntax.all._

import Selection._
import cats.effect.kernel.Async

sealed abstract class Selection[+F[_], +N, +D, +PN, +PD] {

  def select[N0](selector: String): Selection[F, N0, D, PN, PD] =
    Continue(this, Select[F, N, D, PN, PD](selector))

  def select[N0, F1[x] >: F[x]](
      selector: (N, D, Int, List[N]) => F1[N0]
  ): Selection[F1, N0, D, PN, PD] =
    Continue(this, SelectFn[F1, N, D, PN, PD, N0](selector))

  def each[F1[x] >: F[x]](
      fn: (N, D, Int, List[N]) => F1[Unit]
  ): Selection[F1, N, D, PN, PD] =
    Continue(this, Each[F1, N, D, PN, PD](fn))

  def text(value: String): Selection[F, N, D, PN, PD] =
    Continue(this, Text(value))

  def selectAll[N0, D0](selector: String): Selection[F, N0, D0, N, D] =
    Continue(this, SelectAll[F, N, D, PN, PD](selector))

  def attr(name: String, value: String): Selection[F, N, D, PN, PD] =
    Continue(this, SetAttr(name, value))

  def append[N0](tpe: String): Selection[F, N0, D, PN, PD] =
    Continue(this, Append(tpe))

  // def data(value: List[D0], key: D0 => String): Selection[F, D0] = ???

}

object Selection {

  implicit class SelectionOps[F[_], N, D, PN, PD](
      private val sel: Selection[F, N, D, PN, PD]
  ) extends AnyVal {

    def compile(implicit F: Async[F]) = Selection.compile(sel)

  }

  def compile[F[_], N, D, PN, PD](
      selection: Selection[F, N, D, PN, PD]
  )(implicit F: Async[F]): F[Unit] = {

    def go[N0, D0, PN0, PD0](
        s: Selection[F, N0, D0, PN0, PD0]
    ): F[Terminal[F, N0, D0, PN0, PD0]] = {
      s match {
        case Select(selector) =>
          F.delay(dom.document.querySelector(selector)).map { elm =>
            val groups = List(List(elm.asInstanceOf[N0]))
            val parents = List(dom.document.documentElement.asInstanceOf[PN0])
            Terminal(groups, parents)
          }
        case SelectAll(selector) =>
          F.delay(dom.document.querySelectorAll(selector)).map { nodes =>
            val groups = List(nodes.toList.asInstanceOf[List[N0]])
            val parents = List(dom.document.documentElement.asInstanceOf[PN0])
            Terminal(groups, parents)
          }
        case Continue(t @ Terminal(groups, parents), step) =>
          println(
            s"groups: ${groups.map(group => group.mkString(", ")).mkString("\n")}"
          )
          step match {
            case Text(value) =>
              go(
                Continue(
                  t,
                  Each { (n: N, _: D, _: Int, _: List[N]) =>
                    F.delay(
                      n.asInstanceOf[dom.HTMLElement].textContent = value
                    )
                  }
                )
              )
            case Append(tpe) =>
              go(
                Continue(
                  t,
                  SelectFn { (n: N, _: D, _: Int, _: List[N]) =>
                    F.delay {
                      n.asInstanceOf[dom.HTMLElement]
                        .appendChild(
                          dom.document.createElement(tpe)
                        )
                    }
                  }
                )
              )

            case SetAttr(name, value) => {
              groups.traverse_ {
                _.traverse_ { elm =>
                  F.delay(
                    elm.asInstanceOf[dom.Element].setAttribute(name, value)
                  )
                }
              } *> F.pure(
                Terminal(
                  groups.asInstanceOf[List[List[N0]]],
                  parents.asInstanceOf[List[PN0]]
                )
              )

            }

            case Select(selector) => {
              val newGroups = groups.map { group =>
                group.map(
                  _.asInstanceOf[dom.Element]
                    .querySelector(selector)
                    .asInstanceOf[N0]
                )
              }
              F.pure(Terminal(newGroups, parents.asInstanceOf[List[PN0]]))
            }

            case Each(fn) => {
              groups
                .traverse { group =>
                  group.zipWithIndex.traverse { case (node, i) =>
                    val data = node.asInstanceOf[js.Dynamic].`__data__`
                    val fn0 =
                      fn.asInstanceOf[(Any, Any, Int, List[Any]) => F[N0]]
                    fn0(node, data, i, group)
                  }
                }
                .map(_ =>
                  Terminal(
                    groups.asInstanceOf[List[List[N0]]],
                    parents.asInstanceOf[List[PN0]]
                  )
                )
            }

            case SelectFn(selector) => {
              val newGroups = groups.traverse { group =>
                group.zipWithIndex.traverse { case (node, i) =>
                  val data = node.asInstanceOf[js.Dynamic].`__data__`
                  val sel0 =
                    selector.asInstanceOf[(Any, Any, Int, List[Any]) => F[N0]]
                  val newNode = sel0(node, data, i, group)
                  newNode.flatMap { n0 =>
                    F.delay(newNode.asInstanceOf[js.Dynamic].`__data__` =
                      data
                    ) *> F.pure(n0)
                  }
                }
              }
              newGroups.map { newGroups =>
                Terminal(
                  newGroups,
                  parents.asInstanceOf[List[PN0]]
                )
              }
            }

            case SelectAll(_) => throw new NotImplementedError("boom")

            case SelectAllFn(_) => throw new NotImplementedError("boom")

          }
        case Continue(current, step) =>
          go(current).flatMap { s =>
            go(Continue(s, step))
          }
        case _ => throw new IllegalStateException("boom")
      }
    }

    go(selection).void

  }

  def select[F[_], N, D](
      selector: String
  ): Selection[F, N, D, dom.HTMLElement, Nothing] =
    Select[F, N, D, dom.HTMLElement, Nothing](selector)

  private sealed abstract class Action[+F[_], +N, +D, +PN, +PD]
      extends Selection[F, N, D, PN, PD]

  private case class SetAttr[+F[_], N, D, PN, PD](key: String, value: String)
      extends Action[F, N, D, PN, PD]

  private case class Append[+F[_], N, D, PN, PD](tpe: String)
      extends Action[F, N, D, PN, PD]

  private case class Text[+F[_], N, D, PN, PD](value: String)
      extends Action[F, N, D, PN, PD]

  private case class Each[F[_], N, D, PN, PD](
      fn: (N, D, Int, List[N]) => F[Unit]
  ) extends Action[F, N, D, PN, PD]

  private case class Select[+F[_], N, D, PN, PD](selector: String)
      extends Action[F, N, D, PN, PD]

  private case class SelectFn[F[_], N, D, PN, PD, N0](
      selector: (N, D, Int, List[N]) => F[N0]
  ) extends Action[F, N, D, PN, PD]

  private case class SelectAll[+F[_], N, D, PN, PD](selector: String)
      extends Action[F, N, D, PN, PD]

  private case class SelectAllFn[+F[_], N, D, PN, PD, N0](
      selector: (N, D, Int, List[N]) => N0
  ) extends Action[F, N0, D, PN, PD]

  private case class Terminal[+F[_], N, D, PN, PD](
      groups: List[List[N]],
      parents: List[PN]
  ) extends Selection[F, N, D, PN, PD]

  private case class Continue[+F[_], N, D, PN, PD, N0, D0, PN0, PD0](
      current: Selection[F, N, D, PN, PD],
      step: Action[F, N, D, PN, PD]
  ) extends Selection[F, N0, D0, PN0, PD0]

}
