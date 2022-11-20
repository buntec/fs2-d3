package d3.selection

import org.scalajs.dom
import scalajs.js
import cats.syntax.all._

import Selection._
import cats.effect.kernel.Async

sealed abstract class Selection[+N, +D, +PN, +PD] {

  def select[N0](selector: String): Selection[N0, D, PN, PD] =
    Continue(this, Select[N, D, PN, PD](selector))

  def select[N0](
      selector: (N, D, Int, List[N]) => N0
  ): Selection[N0, D, PN, PD] =
    Continue(this, SelectFn[N, D, PN, PD, N0](selector))

  def selectAll[N0, D0](selector: String): Selection[N0, D0, N, D] =
    Continue(this, SelectAll[N, D, PN, PD](selector))

  def attr(name: String, value: String): Selection[N, D, PN, PD] =
    Continue(this, SetAttr(name, value))

  // def append(tpe: String): Selection[F, D] = ???

  // def data(value: List[D0], key: D0 => String): Selection[F, D0] = ???

}

object Selection {

  def compile[F[_], N, D, PN, PD](
      selection: Selection[N, D, PN, PD]
  )(implicit F: Async[F]) = {

    def go[N0, D0, PN0, PD0](
        s: Selection[N0, D0, PN0, PD0]
    ): F[Terminal[N0, D0, PN0, PD0]] = {
      s match {
        case Select(selector) =>
          F.delay(dom.document.querySelector(selector)).map { elm =>
            val groups = List(List(elm.asInstanceOf[N0]))
            val parents = List(dom.document.documentElement.asInstanceOf[PN0])
            Terminal(groups, parents)
          }
        case Continue(Terminal(groups, parents), step) =>
          step match {
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
            case SelectFn(selector) => {
              val newGroups = groups.map { group =>
                group.zipWithIndex.map { case (node, i) =>
                  val data = node.asInstanceOf[js.Dynamic].`__data__`
                  val newNode = selector(node, data, i, group)
                  newNode.asInstanceOf[js.Dynamic].`__data__` = data
                  newNode
                }
              }
              F.pure(
                Terminal(
                  newGroups.asInstanceOf[List[List[N0]]],
                  parents.asInstanceOf[List[PN0]]
                )
              )
            }
            case SelectAll(selector)   => ???
            case SelectAllFn(selector) => ???
          }
        case Continue(current, step) =>
          go(current).flatMap { s =>
            go(Continue(s, step))
          }
      }
    }

    go(selection).void

  }

  def select[N, D](
      selector: String
  ): Selection[N, D, dom.HTMLElement, Nothing] =
    Select[N, D, dom.HTMLElement, Nothing](selector)

  private sealed abstract class Action[N, D, PN, PD]
      extends Selection[N, D, PN, PD]

  private case class SetAttr[N, D, PN, PD](key: String, value: String)
      extends Action[N, D, PN, PD]

  private case class Select[N, D, PN, PD](selector: String)
      extends Action[N, D, PN, PD]

  private case class SelectFn[N, D, PN, PD, N0](
      selector: (N, D, Int, List[N]) => N0
  ) extends Action[N, D, PN, PD]

  private case class SelectAll[N, D, PN, PD](selector: String)
      extends Action[N, D, PN, PD]

  private case class SelectAllFn[N, D, PN, PD, N0](
      selector: (N, D, Int, List[N]) => N0
  ) extends Action[N0, D, PN, PD]

  private case class Terminal[N, D, PN, PD](
      groups: List[List[N]],
      parents: List[PN]
  ) extends Selection[N, D, PN, PD]

  private case class Continue[N, D, PN, PD, N0, D0, PN0, PD0](
      current: Selection[N, D, PN, PD],
      step: Action[N, D, PN, PD]
  ) extends Selection[N0, D0, PN0, PD0]

}
