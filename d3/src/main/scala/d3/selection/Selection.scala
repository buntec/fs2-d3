package d3.selection

import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.syntax.all._
import d3.transition.TransitionManager
import org.scalajs.dom

import scala.concurrent.duration.FiniteDuration

import scalajs.js
import Selection._

sealed abstract class Selection[+F[_], +N, +D, +PN, +PD] {

  def append[N0](name: String): Selection[F, N0, D, PN, PD] =
    Continue(this, Append(name))

  def append[F1[x] >: F[x], N0](
      fn: (N, D, Int, List[N]) => F1[N0]
  ): Selection[F, N0, D, PN, PD] =
    Continue(this, AppendFn(fn))

  def attr(name: String, value: String): Selection[F, N, D, PN, PD] =
    Continue(this, AttrFn(name, (_: N, _: D, _: Int, _: List[N]) => value))

  def attr(
      name: String,
      value: (N, D, Int, List[N]) => String
  ): Selection[F, N, D, PN, PD] =
    Continue(this, AttrFn(name, value))

  def call[F1[x] >: F[x]](
      fn: Selection[F, N, D, PN, PD] => F1[Unit]
  ): Selection[F1, N, D, PN, PD] =
    Continue(this, Call(fn))

  def classed(names: String, value: Boolean): Selection[F, N, D, PN, PD] =
    Continue(this, ClassedFn(names, (_: N, _: D, _: Int, _: List[N]) => value))

  def classed(
      names: String,
      value: (N, D, Int, List[N]) => Boolean
  ): Selection[F, N, D, PN, PD] =
    Continue(this, ClassedFn(names, value))

  def data[D0](data: List[D0]): Selection[F, N, D0, PN, PD] =
    Continue(this, Data(data, None))

  def datum[D0](value: Option[D0]): Selection[F, N, D0, PN, PD] =
    Continue(
      this,
      Property("__data__", (_: N, _: D, _: Int, _: List[N]) => value)
    )

  def datum[D0](
      value: (N, D, Int, List[N]) => Some[D0]
  ): Selection[F, N, D0, PN, PD] =
    Continue(this, Property("__data__", value))

  def dispatch(
      tpe: String,
      params: CustomEventParams
  ): Selection[F, N, D, PN, PD] =
    Continue(this, Dispatch(tpe, (_: N, _: D, _: Int, _: List[N]) => params))

  def dispatch(
      tpe: String,
      params: (N, D, Int, List[N]) => CustomEventParams
  ): Selection[F, N, D, PN, PD] =
    Continue(this, Dispatch(tpe, params))

  def keyedData[D0](
      data: List[D0]
  )( // curry for better type inference
      nodeKey: (N, D, Int, List[N]) => String,
      datumKey: (PN, D0, Int, List[D0]) => String
  ): Selection[F, N, D0, PN, PD] =
    Continue(this, Data(data, Some((nodeKey, datumKey))))

  def each[F1[x] >: F[x]](
      fn: (N, D, Int, List[N]) => F1[Unit]
  ): Selection[F1, N, D, PN, PD] =
    Continue(this, Each(fn))

  def join[F1[x] >: F[x], N0, N1 >: N, D1 >: D, PN1 >: PN, PD1 >: PD](
      onEnter: Enter[F, N, D, PN, PD] => Selection[F1, N0, D1, PN1, PD1],
      onUpdate: Selection[F, N, D, PN, PD] => Selection[F1, N1, D1, PN1, PD1] =
        (sel: Selection[F, N, D, PN, PD]) => sel,
      onExit: Selection[F, N, D, PN, PD] => Selection[F1, N1, D1, PN1, PD1] =
        (sel: Selection[F, N, D, PN, PD]) => sel.remove
  ): Selection[F, N, D, PN, PD] =
    Continue(this, Join(onEnter, onUpdate, onExit))

  def join(name: String): Selection[F, N, D, PN, PD] =
    join(enter => enter.append(name))

  def order: Selection[F, N, D, PN, PD] =
    Continue(this, Order())

  def property(
      name: String,
      value: Option[Any]
  ): Selection[F, N, D, PN, PD] =
    Continue(this, Property(name, (_: N, _: D, _: Int, _: List[N]) => value))

  def property(
      name: String,
      value: (N, D, Int, List[N]) => Option[Any]
  ): Selection[F, N, D, PN, PD] =
    Continue(this, Property(name, value))

  def remove: Selection[F, N, D, PN, PD] =
    Continue(this, Remove())

  def select[N0](selector: String): Selection[F, N0, D, PN, PD] =
    Continue(this, Select(selector))

  def select[N0, F1[x] >: F[x]](
      selector: (N, D, Int, List[N]) => F1[N0]
  ): Selection[F1, N0, D, PN, PD] =
    Continue(this, SelectFn(selector))

  def selectAll[N0, D0](selector: String): Selection[F, N0, D0, N, D] =
    Continue(this, SelectAll(selector))

  def text(value: String): Selection[F, N, D, PN, PD] =
    Continue(this, TextFn((_: N, _: D, _: Int, _: List[N]) => value))

  def text(value: (N, D, Int, List[N]) => String): Selection[F, N, D, PN, PD] =
    Continue(this, TextFn(value))

  def transition: Transition[F, N, D, PN, PD] =
    new Transition(Continue(this, NewTransition()))

}

object Selection {

  def select[F[_], N, D](
      selector: String
  ): Selection[F, N, D, dom.HTMLElement, Unit] =
    Select[F, N, D, dom.HTMLElement, Unit](selector)

  def selectAll[F[_], N, D](
      selector: String
  ): Selection[F, N, D, dom.HTMLElement, Unit] =
    SelectAll[F, N, D, dom.HTMLElement, Unit](selector)

  implicit class SelectionOps[F[_], N, D, PN, PD](
      private val sel: Selection[F, N, D, PN, PD]
  ) extends AnyVal {

    def compile(implicit F: Async[F]): F[Unit] = Selection.compile(sel)

  }

  sealed class Transition[+F[_], +N, +D, +PN, +PD](
      private val sel: Selection[F, N, D, PN, PD]
  ) {

    def selection: Selection[F, N, D, PN, PD] = sel

    def transition: Transition[F, N, D, PN, PD] =
      new Transition(Continue(sel, NewTransition()))

    def remove: Transition[F, N, D, PN, PD] =
      new Transition(Continue(sel, RemoveAfterTransition()))

    def attr(
        name: String,
        value: String,
        duration: FiniteDuration,
        delay: FiniteDuration
    ): Transition[F, N, D, PN, PD] =
      new Transition(
        Continue(
          sel,
          AttrTransitionFn(
            name,
            (_: N, _: D, _: Int, _: List[N]) => value,
            duration,
            delay
          )
        )
      )

    def attr(
        name: String,
        value: (N, D, Int, List[N]) => String,
        duration: FiniteDuration,
        delay: FiniteDuration
    ): Transition[F, N, D, PN, PD] =
      new Transition(
        Continue(
          sel,
          AttrTransitionFn(
            name,
            value,
            duration,
            delay
          )
        )
      )

  }

  object Transition {

    implicit def toSelection[F[_], N, D, PN, PD](
        transition: Transition[F, N, D, PN, PD]
    ): Selection[F, N, D, PN, PD] = transition.selection

    implicit class TransitionOps[F[_], N, D, PN, PD](
        private val t: Transition[F, N, D, PN, PD]
    ) extends AnyVal {

      def compile(implicit F: Async[F]): F[Unit] =
        Selection.compile(t.selection)

    }

  }

  sealed abstract class Enter[+F[_], +N, +D, +PN, +PD] {
    def append[N0](name: String): Selection[F, N0, D, PN, PD] =
      EnterAppend(this, name)
  }

  object Enter {

    private[selection] case class Nodes[F[_], N, D, PN, PD](
        nodes: List[List[EnterNode[D, PN]]]
    ) extends Enter[F, N, D, PN, PD]

    def nodes[F[_], N, D, PN, PD](
        nodes: List[List[EnterNode[D, PN]]]
    ): Enter[F, N, D, PN, PD] = Nodes(nodes)

  }

  def compile[F[_], N, D, PN, PD](
      selection: Selection[F, N, D, PN, PD]
  )(implicit F: Async[F]): F[Unit] = {

    def log(msg: String): F[Unit] =
      F.whenA(d3.Configuration.logging)(F.delay(println(msg)))

    def run[N0, D0, PN0, PD0](
        selection: Selection[F, N0, D0, PN0, PD0]
    ): F[Terminal[F, N0, D0, PN0, PD0]] = {

      TransitionManager[F].use { tm =>
        tm.next.flatMap(F.ref(_)).flatMap { transRef =>
          def go[N0, D0, PN0, PD0](
              s: Selection[F, N0, D0, PN0, PD0]
          ): F[Terminal[F, N0, D0, PN0, PD0]] = {
            s match {
              case Select(selector) =>
                log("Select") *>
                  F.delay(dom.document.querySelector(selector)).map { elm =>
                    val groups = List(List(elm.asInstanceOf[N0]))
                    val parents =
                      List(dom.document.documentElement.asInstanceOf[PN0])
                    Terminal(groups, parents)
                  }
              case SelectAll(selector) =>
                log("SelectAll") *>
                  F.delay(dom.document.querySelectorAll(selector)).map {
                    nodes =>
                      val groups = List(nodes.toList.asInstanceOf[List[N0]])
                      val parents =
                        List(dom.document.documentElement.asInstanceOf[PN0])
                      Terminal(groups, parents)
                  }
              case Continue(t @ Terminal(groups, parents, enter, exit), step) =>
                log(
                  s"""Continue=(Terminal, step)
            |groups(${groups.length}/${groups.map(_.length)}}): ${groups
                    .map(group => group.mkString(", "))
                    .mkString("\n")}
            |parents(${parents.length}): ${parents.mkString(
                    ", "
                  )}""".stripMargin
                ) *> {
                  step match {
                    case RemoveAfterTransition() =>
                      log("Step=RemoveAfterTransition") *>
                        transRef.get.flatMap { trans =>
                          F.defer(
                            go(
                              Continue(
                                t,
                                Each { (n: N, _: D, _: Int, _: List[N]) =>
                                  val removeNode = F.delay {
                                    val node = n.asInstanceOf[dom.Node]
                                    val parent = node.parentNode
                                    if (parent != null) {
                                      parent.removeChild(node)
                                    }
                                    ()
                                  }
                                  trans.onComplete(removeNode)
                                }
                              )
                            )
                          )
                        }
                    case AttrTransitionFn(name, value, duration, delay) =>
                      val fn =
                        value.asInstanceOf[(Any, Any, Any, Any) => String]
                      log("Step=AttrTransitionFn") *>
                        transRef.get.flatMap { trans =>
                          F.defer(
                            go(
                              Continue(
                                t,
                                Each { (n: N, d: D, i: Int, group: List[N]) =>
                                  trans.attr(
                                    n.asInstanceOf[dom.Element],
                                    name,
                                    fn(n, d, i, group),
                                    duration,
                                    delay
                                  )
                                }
                              )
                            )
                          )
                        }
                    case NewTransition() =>
                      log("Step=NewTransition") *>
                        tm.next.flatMap { newTrans =>
                          transRef.set(newTrans)
                        } *>
                        F.pure(t.asInstanceOf[Terminal[F, N0, D0, PN0, PD0]])
                    case Remove() =>
                      log("Step=Remove") *>
                        F.defer(
                          go(
                            Continue(
                              t,
                              Each { (n: N, _: D, _: Int, _: List[N]) =>
                                F.delay {
                                  val node = n.asInstanceOf[dom.Node]
                                  val parent = node.parentNode
                                  if (parent != null) {
                                    parent.removeChild(node)
                                  }
                                  ()
                                }
                              }
                            )
                          )
                        )

                    case Order() => {
                      log("Step=Order") *>
                        F.delay {
                          groups.foreach {
                            case group @ (_ :: tail) =>
                              group.zip(tail).reverse.foreach {
                                case (node, next) =>
                                  if (
                                    (next != null) && (node != null) && ((node
                                      .asInstanceOf[dom.Node]
                                      .compareDocumentPosition(
                                        next.asInstanceOf[dom.Node]
                                      ) ^ dom.Node.DOCUMENT_POSITION_FOLLOWING) != 0)
                                  ) {
                                    next
                                      .asInstanceOf[dom.Node]
                                      .parentNode
                                      .insertBefore(
                                        node.asInstanceOf[dom.Node],
                                        next.asInstanceOf[dom.Node]
                                      )
                                  }
                              }
                            case _ => ()
                          }
                        } *> F
                          .pure(t.asInstanceOf[Terminal[F, N0, D0, PN0, PD0]])
                    }

                    case Join(onEnter, onUpdate, onExit) =>
                      log("Step=Join") *> {
                        val enterSection = Enter.nodes(enter.get)

                        val s1 =
                          onEnter
                            .asInstanceOf[
                              Any => Selection[F, N0, D0, PN0, PD0]
                            ](
                              enterSection.asInstanceOf[Any]
                            )

                        val s2 =
                          onUpdate
                            .asInstanceOf[
                              Any => Selection[F, N0, D0, PN0, PD0]
                            ](
                              t.asInstanceOf[Any]
                            )

                        val doExit = onExit
                          .asInstanceOf[Any => Selection[F, N0, D0, PN0, PD0]](
                            Terminal(exit.get, parents).asInstanceOf[Any]
                          )
                          .compile

                        F.defer((run(s1), run(s2), doExit).parTupled).flatMap {
                          case (
                                Terminal(groups0, parents, _, _),
                                Terminal(groups1, _, _, _),
                                _
                              ) =>
                            F.delay {
                              val groups0Arr = groups0.toArray
                              val groups1Arr = groups1.toArray
                              val m0 = groups0Arr.length
                              val m1 = groups1Arr.length
                              val m = math.min(m0, m1)
                              val merges = new Array[Array[Any]](m0)
                              var j = 0
                              while (j < m) {
                                val group0 = groups0Arr(j)
                                val group1 = groups1Arr(j)
                                val n = group0.length
                                merges(j) = new Array[Any](n)
                                val merge = merges(j)
                                var i = 0
                                while (i < n) {
                                  if (group0(i) != null) {
                                    merge(i) = group0(i)
                                  } else {
                                    merge(i) = group1(i)
                                  }
                                  i += 1
                                }
                                j += 1
                              }
                              while (j < m0) {
                                merges(j) = groups0Arr(j).toArray
                                j += 1
                              }
                              Terminal(
                                merges
                                  .map(_.toList)
                                  .toList
                                  .asInstanceOf[List[List[N0]]],
                                parents
                              )
                                .asInstanceOf[Terminal[F, N0, D0, PN0, PD0]]
                            }.flatMap { terminal =>
                              // go(terminal)
                              go(Continue(terminal, Order()))
                            }
                        }

                      }

                    case Data(data, keyOpt) =>
                      val update = Array.fill(groups.length)(Array.empty[Any])
                      val enter =
                        Array
                          .fill(groups.length)(Array.empty[EnterNode[Any, Any]])
                      val exit = Array.fill(groups.length)(Array.empty[Any])
                      log("Step=Data") *>
                        F.delay {
                          (groups.zip(parents).zipWithIndex).map {
                            case ((group, parent), j) =>
                              val groupLength = group.length
                              val dataLength = data.length
                              val enterGroup =
                                new Array[EnterNode[Any, Any]](dataLength)
                              val updateGroup = new Array[Any](dataLength)
                              val exitGroup = new Array[Any](groupLength)

                              val nodes = group.toArray
                              val dataArr = data.toArray

                              keyOpt match {
                                case None =>
                                  var i = 0
                                  while (i < dataLength) {
                                    if (i < groupLength && nodes(i) != null) {
                                      nodes(i).asInstanceOf[js.Dictionary[Any]](
                                        "__data__"
                                      ) = dataArr(i)
                                      updateGroup(i) = nodes(i)
                                    } else {
                                      enterGroup(i) =
                                        new EnterNode(parent, data(i), null)
                                    }
                                    i += 1
                                  }

                                  while (i < groupLength) {
                                    if (nodes(i) != null) {
                                      exitGroup(i) = nodes(i)
                                    }
                                    i += 1
                                  }
                                case Some((nodeKey, datumKey)) =>
                                  val nKey =
                                    nodeKey
                                      .asInstanceOf[
                                        (Any, Any, Any, Any) => String
                                      ]
                                  val dKey =
                                    datumKey
                                      .asInstanceOf[
                                        (Any, Any, Any, Any) => String
                                      ]

                                  val nodebyKeyValue =
                                    collection.mutable.Map.empty[String, Any]

                                  val keyValues = new Array[String](groupLength)

                                  var i = 0
                                  while (i < groupLength) {
                                    if (i < groupLength && nodes(i) != null) {
                                      val node = nodes(i)
                                      val keyValue = nKey(
                                        node,
                                        node
                                          .asInstanceOf[js.Dictionary[Any]]
                                          .get("__data__")
                                          .getOrElse(null),
                                        i,
                                        group
                                      )
                                      keyValues(i) = keyValue
                                      if (nodebyKeyValue.contains(keyValue)) {
                                        exitGroup(i) = node
                                      } else {
                                        nodebyKeyValue.addOne(keyValue -> node)
                                      }
                                    }
                                    i += 1
                                  }

                                  i = 0
                                  while (i < dataLength) {
                                    val keyValue =
                                      dKey(parent, data(i), i, data)
                                    if (nodebyKeyValue.contains(keyValue)) {
                                      val node = nodebyKeyValue(keyValue)
                                      updateGroup(i) = node
                                      node.asInstanceOf[js.Dictionary[Any]](
                                        "__data__"
                                      ) = dataArr(i)
                                      nodebyKeyValue.remove(keyValue)
                                    } else {
                                      enterGroup(i) =
                                        new EnterNode(parent, data(i), null)
                                    }
                                    i += 1
                                  }

                                  i = 0
                                  while (i < groupLength) {
                                    if (
                                      (nodes(i) != null) && nodebyKeyValue
                                        .contains(
                                          keyValues(i)
                                        ) && (nodebyKeyValue(
                                        keyValues(i)
                                      ) == nodes(i))
                                    ) {
                                      exitGroup(i) = nodes(i)
                                    }
                                    i += 1
                                  }

                              }

                              var i0 = 0
                              var i1 = 0
                              while (i0 < dataLength) {
                                if (enterGroup(i0) != null) {
                                  if (i0 >= i1) i1 = i0 + 1
                                  while (
                                    i1 < dataLength && updateGroup(
                                      i1
                                    ) == null && i1 + 1 < dataLength
                                  ) {
                                    i1 += 1
                                  }
                                  enterGroup(i0)._next =
                                    if (i1 <= dataLength) updateGroup(i1 - 1)
                                    else null
                                }
                                i0 += 1
                              }

                              update(j) = updateGroup
                              enter(j) = enterGroup
                              exit(j) = exitGroup
                          }
                        } *> log(s"update=${update}") *> F.delay {
                          Terminal(
                            update
                              .map(_.toList)
                              .toList
                              .asInstanceOf[List[List[N0]]],
                            parents.asInstanceOf[List[PN0]],
                            enter = Some(
                              enter
                                .map(_.toList)
                                .toList
                                .asInstanceOf[List[List[EnterNode[D0, PN0]]]]
                            ),
                            exit = Some(
                              exit
                                .map(_.toList)
                                .toList
                                .asInstanceOf[List[List[N0]]]
                            )
                          )
                        }

                    case TextFn(value) => {
                      val fn =
                        value.asInstanceOf[(Any, Any, Any, Any) => String]
                      log("Step=TextFn") *>
                        F.defer(
                          go(
                            Continue(
                              t,
                              Each { (n: N, d: D, i: Int, group: List[N]) =>
                                F.delay(
                                  n.asInstanceOf[dom.Node].textContent =
                                    fn(n, d, i, group)
                                )
                              }
                            )
                          )
                        )
                    }

                    case AttrFn(name, value) => {
                      val fn =
                        value.asInstanceOf[(Any, Any, Any, Any) => String]
                      log("Step=AttrFn") *>
                        F.defer(
                          go(
                            Continue(
                              t,
                              Each { (n: N, d: D, i: Int, group: List[N]) =>
                                F.delay {
                                  val elm = n.asInstanceOf[dom.Element]
                                  elm.setAttribute(name, fn(n, d, i, group))
                                }
                              }
                            )
                          )
                        )
                    }

                    case Dispatch(tpe, paramsFn) => {
                      log("Step=Dispatch") *>
                        F.defer(
                          go(
                            Continue(
                              t,
                              Each { (n: N, d: D, i: Int, group: List[N]) =>
                                F.delay {
                                  val params = paramsFn.asInstanceOf[
                                    (Any, Any, Any, Any) => CustomEventParams
                                  ](n, d, i, group)
                                  val init = new dom.CustomEventInit {}
                                  init.bubbles = params.bubbles
                                  init.cancelable = params.cancelable
                                  params.detail.fold(()) { detail =>
                                    init.detail = detail
                                  }
                                  val event = new dom.CustomEvent(tpe, init)
                                  val node = n.asInstanceOf[dom.Node]
                                  node.dispatchEvent(event)
                                  ()
                                }
                              }
                            )
                          )
                        )
                    }

                    case Property(name, valueFn) => {
                      log("Step=AttrFn") *>
                        F.defer(
                          go(
                            Continue(
                              t,
                              Each { (n: N, d: D, i: Int, group: List[N]) =>
                                F.delay {
                                  val props =
                                    n.asInstanceOf[js.Dictionary[js.Any]]
                                  val value = valueFn.asInstanceOf[
                                    (Any, Any, Any, Any) => Option[Any]
                                  ](n, d, i, group)
                                  value.fold(props -= name)(a =>
                                    props += (name -> a.asInstanceOf[js.Any])
                                  )
                                  ()
                                }
                              }
                            )
                          )
                        )
                    }

                    case ClassedFn(names, value) => {
                      val fn =
                        value.asInstanceOf[(Any, Any, Any, Any) => Boolean]
                      log("Step=ClassedFn") *>
                        F.defer(
                          go(
                            Continue(
                              t,
                              Each { (n: N, d: D, i: Int, group: List[N]) =>
                                F.delay {
                                  val elm = n.asInstanceOf[dom.Element]
                                  val classes = names.trim().split("""^|\s+""")
                                  val value = fn(n, d, i, group)
                                  if (value) {
                                    classes.foreach(elm.classList.add)
                                  } else {
                                    classes.foreach(elm.classList.remove)
                                  }
                                }
                              }
                            )
                          )
                        )
                    }

                    case Append(name) =>
                      log("Step=Append") *>
                        F.defer(
                          go(
                            Continue(
                              t,
                              SelectFn { (n: N, _: D, _: Int, _: List[N]) =>
                                F.delay {
                                  val parent = n.asInstanceOf[dom.Node]
                                  parent.appendChild(creator(name, parent))
                                }
                              }
                            )
                          )
                        )

                    case AppendFn(fn) =>
                      log("Step=AppendFn") *>
                        F.defer(
                          go(
                            Continue(
                              t,
                              SelectFn { (n: N, d: D, i: Int, group: List[N]) =>
                                fn.asInstanceOf[(Any, Any, Any, Any) => F[
                                  dom.Node
                                ]](n, d, i, group)
                                  .flatMap { node =>
                                    F.delay {
                                      val parent = n.asInstanceOf[dom.Node]
                                      parent.appendChild(node)
                                    }
                                  }
                              }
                            )
                          )
                        )

                    case Select(selector) =>
                      log("Step=Select") *> F.delay {
                        val newGroups = groups.map { group =>
                          group.map(
                            _.asInstanceOf[dom.Element]
                              .querySelector(selector)
                              .asInstanceOf[N0]
                          )
                        }
                        Terminal(newGroups, parents.asInstanceOf[List[PN0]])
                      }

                    case Call(f) => {
                      val f0 = f.asInstanceOf[Any => F[Unit]]
                      val t0 = t.asInstanceOf[Terminal[F, N0, D0, PN0, PD0]]
                      f0(t0).as(t0)
                    }

                    case Each(fn) => {
                      log("Step=Each") *>
                        groups
                          .traverse_ { group =>
                            group.zipWithIndex.traverse_ { case (node, i) =>
                              F.whenA(node != null) {
                                val data =
                                  node
                                    .asInstanceOf[js.Dictionary[Any]]
                                    .get(
                                      "__data__"
                                    )
                                    .getOrElse(null)
                                val fn0 =
                                  fn.asInstanceOf[
                                    (Any, Any, Int, List[Any]) => F[
                                      Unit
                                    ]
                                  ]
                                fn0(node, data, i, group)
                              }
                            }
                          } *> F
                          .pure(t.asInstanceOf[Terminal[F, N0, D0, PN0, PD0]])
                    }

                    case SelectFn(selector) => {
                      log("Step=SelectFn") *>
                        groups
                          .traverse { group =>
                            group.zipWithIndex.traverse { case (node, i) =>
                              if (node != null) {
                                val data =
                                  node
                                    .asInstanceOf[js.Dictionary[Any]]
                                    .get("__data__")
                                    .getOrElse(null)
                                val sel0 =
                                  selector
                                    .asInstanceOf[
                                      (Any, Any, Int, List[Any]) => F[
                                        N0
                                      ]
                                    ]
                                val newNode = sel0(node, data, i, group)
                                newNode.flatMap { n0 =>
                                  F.delay {
                                    n0.asInstanceOf[js.Dictionary[Any]](
                                      "__data__"
                                    ) = data
                                    n0
                                  }
                                }
                              } else {
                                F.pure(null.asInstanceOf[N0])
                              }
                            }
                          }
                          .map { newGroups =>
                            Terminal(
                              newGroups,
                              parents.asInstanceOf[List[PN0]]
                            )
                          }
                    }

                    case SelectAll(sel) => {
                      log("selectAll") *>
                        F.delay {
                          val (newGroups, newParents) =
                            groups.foldLeft(
                              (List.empty[List[Any]], List.empty[Any])
                            ) { case ((sg, ps), group) =>
                              val (a, b) =
                                group.foldLeft(
                                  (List.empty[List[Any]], List.empty[Any])
                                ) { case ((sg, ps), node) =>
                                  if (node != null) {
                                    (
                                      node
                                        .asInstanceOf[dom.Element]
                                        .querySelectorAll(sel)
                                        .toList
                                        .asInstanceOf[List[Any]] :: sg,
                                      node :: ps
                                    )
                                  } else {
                                    (sg, ps)
                                  }
                                }

                              (a ++ sg.reverse, b ++ ps.reverse)
                            }
                          Terminal(
                            newGroups.asInstanceOf[List[List[N0]]],
                            newParents.asInstanceOf[List[PN0]]
                          )
                        }

                    }

                    case SelectAllFn(_) => throw new NotImplementedError("boom")

                  }
                }
              case Continue(current, step) =>
                log("Continue") *>
                  F.defer(go(current)).flatMap { s =>
                    go(Continue(s, step))
                  }
              case EnterAppend(Enter.Nodes(nodes), name) => {
                log(
                  s"EnterAppend: nodes = ${nodes.map(_.mkString(", ")).mkString("\n")}"
                ) *>
                  nodes
                    .traverse { nodes =>
                      nodes.traverse { eNode =>
                        F.delay {
                          if (eNode != null) {
                            val parent = eNode.parent.asInstanceOf[dom.Node]
                            val child = creator(name, parent)
                            parent
                              .insertBefore(
                                child,
                                eNode._next.asInstanceOf[dom.Node]
                              )
                            child.asInstanceOf[js.Dictionary[Any]]("__data__") =
                              eNode.data
                            child
                          } else {
                            null
                          }
                        }
                      }
                    }
                    .map { groups =>
                      val parents = nodes
                        .map(nodes =>
                          nodes.find(_ != null).map(_.parent).getOrElse(null)
                        )
                        .asInstanceOf[List[PN0]]
                      Terminal(groups.asInstanceOf[List[List[N0]]], parents)
                    }
              }
              case t @ Terminal(_, _, _, _) =>
                log("Terminal") *> F
                  .pure(t.asInstanceOf[Terminal[F, N0, D0, PN0, PD0]])
              case sel =>
                throw new IllegalStateException(s"Unexpected type: $sel")
            }

          }

          go(selection) <* tm.run
        }

      }

    }

    run(selection).void

  }

  private sealed abstract class Action[+F[_], +N, +D, +PN, +PD]
      extends Selection[F, N, D, PN, PD]

  private case class AttrFn[F[_], N, D, PN, PD](
      name: String,
      value: (N, D, Int, List[N]) => String
  ) extends Action[F, N, D, PN, PD]

  private case class Property[F[_], N, D, PN, PD](
      name: String,
      value: (N, D, Int, List[N]) => Option[Any]
  ) extends Action[F, N, D, PN, PD]

  private case class Dispatch[F[_], N, D, PN, PD](
      tpe: String,
      params: (N, D, Int, List[N]) => CustomEventParams
  ) extends Action[F, N, D, PN, PD]

  private case class AttrTransitionFn[F[_], N, D, PN, PD](
      name: String,
      value: (N, D, Int, List[N]) => String,
      duration: FiniteDuration,
      delay: FiniteDuration
  ) extends Action[F, N, D, PN, PD]

  private case class Append[F[_], N, D, PN, PD](name: String)
      extends Action[F, N, D, PN, PD]

  private case class AppendFn[F[_], F1[x] >: F[x], N, N1, D, PN, PD](
      fn: (N, D, Int, List[N]) => F1[N1]
  ) extends Action[F, N, D, PN, PD]

  private case class TextFn[F[_], N, D, PN, PD](
      fn: (N, D, Int, List[N]) => String
  ) extends Action[F, N, D, PN, PD]

  private case class ClassedFn[F[_], N, D, PN, PD](
      names: String,
      value: (N, D, Int, List[N]) => Boolean
  ) extends Action[F, N, D, PN, PD]

  private case class Each[F[_], N, D, PN, PD](
      fn: (N, D, Int, List[N]) => F[Unit]
  ) extends Action[F, N, D, PN, PD]

  private case class Call[F0[_], F[_], N, D, PN, PD](
      fn: Selection[F0, N, D, PN, PD] => F[Unit]
  ) extends Action[F, N, D, PN, PD]

  private case class Data[F[_], N, D0, D, PN, PD](
      data: List[D],
      keys: Option[
        ((N, D0, Int, List[N]) => String, (PN, D, Int, List[D]) => String)
      ]
  ) extends Action[F, N, D0, PN, PD]

  private case class Select[F[_], N, D, PN, PD](selector: String)
      extends Action[F, N, D, PN, PD]

  private case class SelectFn[F[_], N, D, PN, PD, N0](
      selector: (N, D, Int, List[N]) => F[N0]
  ) extends Action[F, N, D, PN, PD]

  private case class Join[F[_], F1[x] >: F[
    x
  ], N, D, PN, PD, N0, N1 >: N, D1 >: D, PN1 >: PN, PD1 >: PD](
      onEnter: Enter[F, N, D, PN, PD] => Selection[F1, N0, D1, PN1, PD1],
      onUpdate: Selection[F, N, D, PN, PD] => Selection[F1, N1, D1, PN1, PD1],
      onExit: Selection[F, N, D, PN, PD] => Selection[F1, N1, D1, PN1, PD1]
  ) extends Action[F, N, D, PN, PD]

  private case class Order[F[_], N, D, PN, PD]() extends Action[F, N, D, PN, PD]

  private case class Remove[F[_], N, D, PN, PD]()
      extends Action[F, N, D, PN, PD]

  private case class RemoveAfterTransition[F[_], N, D, PN, PD]()
      extends Action[F, N, D, PN, PD]

  private case class NewTransition[F[_], N, D, PN, PD]()
      extends Action[F, N, D, PN, PD]

  private case class SelectAll[F[_], N, D, PN, PD](selector: String)
      extends Action[F, N, D, PN, PD]

  private case class SelectAllFn[F[_], N, D, PN, PD, N0](
      selector: (N, D, Int, List[N]) => N0
  ) extends Action[F, N0, D, PN, PD]

  private case class Terminal[F[_], N, D, PN, PD](
      groups: List[List[N]],
      parents: List[PN],
      enter: Option[List[List[EnterNode[D, PN]]]] = None,
      exit: Option[List[List[N]]] = None
  ) extends Selection[F, N, D, PN, PD]

  private case class Continue[F[_], N, D, PN, PD, N0, D0, PN0, PD0](
      current: Selection[F, N, D, PN, PD],
      step: Action[F, N, D, PN, PD]
  ) extends Selection[F, N0, D0, PN0, PD0]

  private case class EnterAppend[F[_], N, D, PN, PD, N0, D0, PN0, PD0](
      enter: Enter[F, N, D, PN, PD],
      name: String
  ) extends Selection[F, N0, D0, PN0, PD0]

  private[selection] class EnterNode[D, PN](
      val parent: PN,
      val data: D,
      var _next: Any
  )

}
