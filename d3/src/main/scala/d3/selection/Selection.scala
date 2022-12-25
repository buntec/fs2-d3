/*
 * Copyright 2022 buntec
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * Copyright 2010-2021 Mike Bostock
 *
 * Permission to use, copy, modify, and/or distribute this software for any purpose
 * with or without fee is hereby granted, provided that the above copyright notice
 * and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
 * FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
 * OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
 * THIS SOFTWARE.
 */

package d3.selection

import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.effect.std.Dispatcher
import cats.syntax.all._
import d3.transition.TransitionManager
import org.scalajs.dom

import scala.concurrent.duration.FiniteDuration

import scalajs.js
import Selection._

/*
 *
 * F = effect type
 * N = node type
 * D = node datum type
 * PN = parent node type
 * PD = parent node datum type
 *
 */
sealed abstract class Selection[+F[_], +N, +D, +PN, +PD] {

  def append[N0](name: String): Selection[F, N0, D, PN, PD] =
    Continue(this, Append(name))

  def append[F1[x] >: F[x], N0](
      fn: (N, D, Int, List[N]) => F1[N0]
  ): Selection[F, N0, D, PN, PD] =
    Continue(this, AppendFn(fn))

  def attr(name: String, value: Option[String]): Selection[F, N, D, PN, PD] =
    Continue(this, AttrFn(name, (_: N, _: D, _: Int, _: List[N]) => value))

  def attr(
      name: String,
      value: (N, D, Int, List[N]) => Option[String]
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

  def compile: CompileOps[F, N, D, PN, PD] =
    new CompileOps(this)

  def data[D0](data: List[D0]): DataOps[F, N, D0, PN, PD] =
    new DataOps(
      Continue(this, DataFn((_: PN, _: PD, _: Int, _: List[PN]) => data, None))
    )

  def data[D0](
      data: (PN, PD, Int, List[PN]) => List[D0]
  ): DataOps[F, N, D0, PN, PD] =
    new DataOps(Continue(this, DataFn(data, None)))

  def dataKeyed[D0](
      data: List[D0]
  )( // curry for better type inference
      nodeKey: (N, D, Int, List[N]) => String,
      datumKey: (PN, D0, Int, List[D0]) => String
  ): DataOps[F, N, D0, PN, PD] =
    new DataOps(
      Continue(
        this,
        DataFn(
          (_: PN, _: PD, _: Int, _: List[PN]) => data,
          Some((nodeKey, datumKey))
        )
      )
    )

  def dataKeyed[D0](
      data: (PN, PD, Int, List[PN]) => List[D0]
  )( // curry for better type inference
      nodeKey: (N, D, Int, List[N]) => String,
      datumKey: (PN, D0, Int, List[D0]) => String
  ): DataOps[F, N, D0, PN, PD] =
    new DataOps(Continue(this, DataFn(data, Some((nodeKey, datumKey)))))

  def datum[D0](value: Option[D0]): Selection[F, N, D0, PN, PD] =
    Continue(
      this,
      Property(DATA_KEY, (_: N, _: D, _: Int, _: List[N]) => value)
    )

  def datum[D0](
      value: (N, D, Int, List[N]) => Some[D0]
  ): Selection[F, N, D0, PN, PD] =
    Continue(this, Property(DATA_KEY, value))

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

  def each[F1[x] >: F[x]](
      fn: (N, D, Int, List[N]) => F1[Unit]
  ): Selection[F1, N, D, PN, PD] =
    Continue(this, Each(fn))

  def filter(selector: String): Selection[F, N, D, PN, PD] =
    Continue(this, Filter(selector))

  def filter(
      pred: (N, D, Int, List[N]) => Boolean
  ): Selection[F, N, D, PN, PD] =
    Continue(this, FilterFn(pred))

  def on[F1[x] >: F[x]](
      typenames: String,
      listener: Option[(N, dom.Event, D) => F1[Unit]],
      options: Option[dom.EventListenerOptions],
      dispatcher: Dispatcher[F1]
  ): Selection[F1, N, D, PN, PD] =
    Continue(this, On(typenames, listener, options, dispatcher))

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

  def sort[D1 >: D](implicit ord: Ordering[D1]): Selection[F, N, D1, PN, PD] =
    Continue(this, Sort(ord.lt))

  def sort(lt: (D, D) => Boolean): Selection[F, N, D, PN, PD] =
    Continue(this, Sort(lt))

  def style(
      name: String,
      value: Option[String],
      priority: Boolean = false
  ): Selection[F, N, D, PN, PD] =
    Continue(
      this,
      StyleFn(name, (_: N, _: D, _: Int, _: List[N]) => value, priority)
    )

  def style(
      name: String,
      value: (N, D, Int, List[N]) => Option[String],
      priority: Boolean
  ): Selection[F, N, D, PN, PD] =
    Continue(this, StyleFn(name, value, priority))

  def text(value: String): Selection[F, N, D, PN, PD] =
    Continue(this, TextFn((_: N, _: D, _: Int, _: List[N]) => value))

  def text(value: (N, D, Int, List[N]) => String): Selection[F, N, D, PN, PD] =
    Continue(this, TextFn(value))

  def transition: TransitionOps[F, N, D, PN, PD] =
    new TransitionOps(Continue(this, NewTransition()))

}

object Selection {

  def select[F[_], N, D](
      selector: String
  ): Selection[F, N, D, dom.HTMLElement, Unit] =
    Select[F, N, D, dom.HTMLElement, Unit](selector)

  def select[F[_], N, D](node: N): Selection[F, N, D, dom.HTMLElement, Unit] =
    Terminal(List(List(node)), List(null))

  def selectAll[F[_], N, D](
      selector: String
  ): Selection[F, N, D, dom.HTMLElement, Unit] =
    SelectAll[F, N, D, dom.HTMLElement, Unit](selector)

  def selectAll[F[_], N, D](
      nodes: List[N]
  ): Selection[F, N, D, dom.HTMLElement, Unit] =
    Terminal(List(nodes), List(null))

  final class CompileOps[+F[_], +N, +D, +PN, +PD] private[selection] (
      private val sel: Selection[F, N, D, PN, PD]
  ) {

    def attr[F2[x] >: F[x]](name: String)(implicit
        F: Async[F2]
    ): F2[Option[String]] =
      node[F2, N].map(_.map(_.asInstanceOf[dom.Element].getAttribute(name)))

    def datum[F2[x] >: F[x], D2 >: D](implicit F: Async[F2]): F2[Option[D2]] =
      node[F2, N].map(
        _.flatMap(_.asInstanceOf[js.Dictionary[D2]].get(DATA_KEY))
      )

    def drain[F2[x] >: F[x]](implicit F: Async[F2]): F2[Unit] =
      Selection.run[F2, N, D, PN, PD](sel).void

    def empty[F2[x] >: F[x]](implicit F: Async[F2]): F2[Boolean] =
      Selection
        .run[F2, N, D, PN, PD](sel)
        .map(_.groups.flatten.filter(_ != null).isEmpty)

    def html[F2[x] >: F[x]](implicit
        F: Async[F2]
    ): F2[Option[String]] =
      node[F2, N].map(_.map(_.asInstanceOf[dom.Element].innerHTML))

    def node[F2[x] >: F[x], N2 >: N](implicit F: Async[F2]): F2[Option[N2]] =
      Selection
        .run[F2, N, D, PN, PD](sel)
        .map(_.groups.flatten.find(_ != null))

    def nodes[F2[x] >: F[x], N2 >: N](implicit F: Async[F2]): F2[List[N2]] =
      Selection
        .run[F2, N, D, PN, PD](sel)
        .map(_.groups.flatten.filter(_ != null))

    def size[F2[x] >: F[x]](implicit F: Async[F2]): F2[Int] =
      nodes[F2, N].map(_.length)

    def style[F2[x] >: F[x]](name: String)(implicit
        F: Async[F2]
    ): F2[Option[String]] = node[F2, N].map {
      _.map { n =>
        val s = n.asInstanceOf[dom.HTMLElement].style.getPropertyValue(name)
        if (s.nonEmpty) {
          s
        } else {
          window
            .defaultView(n.asInstanceOf[dom.Node])
            .asInstanceOf[js.Dynamic]
            .getComputedStyle(n.asInstanceOf[js.Any], null)
            .getPropertyValue(name)
            .asInstanceOf[String]
        }
      }
    }

    def text[F2[x] >: F[x]](implicit
        F: Async[F2]
    ): F2[Option[String]] =
      node[F2, N].map(_.map(_.asInstanceOf[dom.Node].textContent))

  }

  final class DataOps[+F[_], +N, +D, +PN, +PD] private[selection] (
      private val sel: Selection[F, N, D, PN, PD]
  ) {

    def update: Selection[F, N, D, PN, PD] = sel

    def join(name: String): Selection[F, N, D, PN, PD] =
      join[F, N, D, PN, PD](_.append(name))

    def join[F1[x] >: F[x], N1 >: N, D1 >: D, PN1 >: PN, PD1 >: PD](
        onEnter: Enter[F1, N1, D1, PN1, PD1] => Selection[F1, N1, D1, PN1, PD1],
        onUpdate: Selection[F1, N1, D1, PN1, PD1] => Selection[
          F1,
          N1,
          D1,
          PN1,
          PD1
        ] = (sel: Selection[F1, N1, D1, PN1, PD1]) => sel,
        onExit: Selection[F1, N1, D1, PN1, PD1] => Selection[
          F1,
          N1,
          D1,
          PN1,
          PD1
        ] = (sel: Selection[F1, N1, D1, PN1, PD1]) => sel.remove
    ): Selection[F1, N1, D1, PN1, PD1] =
      Continue(sel, Join(onEnter, onUpdate, onExit))

  }

  final class TransitionOps[+F[_], +N, +D, +PN, +PD] private[selection] (
      private val sel: Selection[F, N, D, PN, PD]
  ) {

    def attr(
        name: String,
        value: String
    ): TransitionOps[F, N, D, PN, PD] =
      new TransitionOps(
        Continue(
          sel,
          AttrTransitionFn(
            name,
            (_: N, _: D, _: Int, _: List[N]) => value
          )
        )
      )

    def attr(
        name: String,
        value: (N, D, Int, List[N]) => String
    ): TransitionOps[F, N, D, PN, PD] =
      new TransitionOps(
        Continue(
          sel,
          AttrTransitionFn(
            name,
            value
          )
        )
      )

    def compile: CompileOps[F, N, D, PN, PD] = sel.compile

    def delay(
        delay: FiniteDuration
    ): TransitionOps[F, N, D, PN, PD] =
      new TransitionOps(
        Continue(
          sel,
          TransitionDelayFn((_: N, _: D, _: Int, _: List[N]) => delay)
        )
      )

    def delay(
        delay: (N, D, Int, List[N]) => FiniteDuration
    ): TransitionOps[F, N, D, PN, PD] =
      new TransitionOps(
        Continue(sel, TransitionDelayFn(delay))
      )

    def duration(
        duration: FiniteDuration
    ): TransitionOps[F, N, D, PN, PD] =
      new TransitionOps(
        Continue(
          sel,
          TransitionDurationFn((_: N, _: D, _: Int, _: List[N]) => duration)
        )
      )

    def ease(
        ease: Double => Double
    ): TransitionOps[F, N, D, PN, PD] =
      new TransitionOps(
        Continue(
          sel,
          TransitionEaseFn((_: N, _: D, _: Int, _: List[N]) => ease)
        )
      )

    def ease(
        ease: (N, D, Int, List[N]) => (Double => Double)
    ): TransitionOps[F, N, D, PN, PD] =
      new TransitionOps(
        Continue(sel, TransitionEaseFn(ease))
      )

    def duration(
        duration: (N, D, Int, List[N]) => FiniteDuration
    ): TransitionOps[F, N, D, PN, PD] =
      new TransitionOps(
        Continue(sel, TransitionDurationFn(duration))
      )

    def remove: TransitionOps[F, N, D, PN, PD] =
      new TransitionOps(Continue(sel, RemoveAfterTransition()))

    def selection: Selection[F, N, D, PN, PD] = sel

    def transition: TransitionOps[F, N, D, PN, PD] =
      new TransitionOps(Continue(sel, NewTransition()))

  }

  object TransitionOps {

    implicit def toSelection[F[_], N, D, PN, PD](
        transition: TransitionOps[F, N, D, PN, PD]
    ): Selection[F, N, D, PN, PD] = transition.selection

  }

  sealed abstract class Enter[+F[_], +N, +D, +PN, +PD] {
    def append[N0](name: String): Selection[F, N0, D, PN, PD] =
      EnterAppend(this, name)
  }

  object Enter {

    private[selection] case class Nodes[F[_], N, D, PN, PD](
        nodes: List[List[EnterNode[D, PN]]]
    ) extends Enter[F, N, D, PN, PD]

    private[selection] def nodes[F[_], N, D, PN, PD](
        nodes: List[List[EnterNode[D, PN]]]
    ): Enter[F, N, D, PN, PD] = Nodes(nodes)

  }

  private def run[F[_], N, D, PN, PD](
      selection: Selection[F, N, D, PN, PD]
  )(implicit F: Async[F]): F[Terminal[F, N, D, PN, PD]] = {

    def log(msg: String): F[Unit] =
      F.whenA(d3.Configuration.logging)(F.delay(println(msg)))

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
                F.delay(dom.document.querySelectorAll(selector)).map { nodes =>
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
                  case Filter(selector) =>
                    log("Step=Filter") *> F.delay {
                      val subgroups = groups.map { group =>
                        group.filter { node =>
                          (node != null) && node
                            .asInstanceOf[dom.Element]
                            .matches(selector)
                        }
                      }
                      Terminal(subgroups, parents)
                        .asInstanceOf[Terminal[F, N0, D0, PN0, PD0]]
                    }
                  case FilterFn(pred) =>
                    log("Step=FilterFn") *> F.delay {
                      val subgroups = groups.map { group =>
                        group.zipWithIndex.filter { case (node, i) =>
                          if (node != null) {
                            false
                          } else {
                            val data = node
                              .asInstanceOf[js.Dictionary[Any]]
                              .get(DATA_KEY)
                              .getOrElse(null)
                            pred
                              .asInstanceOf[(Any, Any, Any, Any) => Boolean](
                                node,
                                data,
                                i,
                                group
                              )
                          }
                        }
                      }
                      Terminal(subgroups, parents)
                        .asInstanceOf[Terminal[F, N0, D0, PN0, PD0]]
                    }
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

                  case AttrTransitionFn(name, value) =>
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
                                  fn(n, d, i, group)
                                )
                              }
                            )
                          )
                        )
                      }

                  case TransitionDurationFn(duration) =>
                    val fn = duration
                      .asInstanceOf[(Any, Any, Any, Any) => FiniteDuration]
                    log("Step=TransitionDurationFn") *>
                      transRef.get.flatMap { trans =>
                        F.defer(
                          go(
                            Continue(
                              t,
                              Each { (n: N, d: D, i: Int, group: List[N]) =>
                                trans.duration(
                                  n.asInstanceOf[dom.Element],
                                  fn(n, d, i, group)
                                )
                              }
                            )
                          )
                        )
                      }

                  case TransitionEaseFn(ease) =>
                    val fn = ease
                      .asInstanceOf[(Any, Any, Any, Any) => (Double => Double)]
                    log("Step=TransitionEaseFn") *>
                      transRef.get.flatMap { trans =>
                        F.defer(
                          go(
                            Continue(
                              t,
                              Each { (n: N, d: D, i: Int, group: List[N]) =>
                                trans.ease(
                                  n.asInstanceOf[dom.Element],
                                  fn(n, d, i, group)
                                )
                              }
                            )
                          )
                        )
                      }

                  case TransitionDelayFn(delay) =>
                    val fn = delay
                      .asInstanceOf[(Any, Any, Any, Any) => FiniteDuration]
                    log("Step=TransitionDelayFn") *>
                      transRef.get.flatMap { trans =>
                        F.defer(
                          go(
                            Continue(
                              t,
                              Each { (n: N, d: D, i: Int, group: List[N]) =>
                                trans.delay(
                                  n.asInstanceOf[dom.Element],
                                  fn(n, d, i, group)
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

                  case On(typenames0, listener0, options0, dispatcher) =>
                    log("Step=On") *> F.defer {
                      val typenames = parseListenerTypenames(typenames0)
                      val options = options0.getOrElse(null)
                      go(
                        Continue(
                          t,
                          Each { (n: N, d: D, _: Int, _: List[N]) =>
                            typenames.traverse_ { typename =>
                              F.delay {
                                listener0 match {
                                  case None =>
                                    val dict =
                                      n.asInstanceOf[js.Dictionary[List[
                                        WrappedListener
                                      ]]]

                                    val on = dict
                                      .get(LISTENER_KEY)
                                      .getOrElse(Nil)

                                    on.filter(l =>
                                      l.tpe == typename.tpe && l.name == typename.name
                                    ).foreach { l =>
                                      n.asInstanceOf[dom.Node]
                                        .removeEventListener(
                                          l.tpe,
                                          l.listener,
                                          l.options
                                        )
                                    }

                                    dict(LISTENER_KEY) = on.filterNot(l =>
                                      l.tpe == typename.tpe && l.name == typename.name
                                    )

                                  case Some(listener1) => {
                                    val listener
                                        : js.Function1[dom.Event, Unit] =
                                      (ev: dom.Event) =>
                                        dispatcher
                                          .asInstanceOf[Dispatcher[F]]
                                          .unsafeRunAndForget(
                                            listener1.asInstanceOf[
                                              (Any, Any, Any) => F[Unit]
                                            ](n, ev, d)
                                          )

                                    val dict =
                                      n.asInstanceOf[js.Dictionary[List[
                                        WrappedListener
                                      ]]]

                                    val on = dict
                                      .get(LISTENER_KEY)
                                      .getOrElse(Nil)

                                    val on2 = on.map { l =>
                                      if (
                                        l.tpe == typename.tpe && l.name == typename.name
                                      ) {
                                        n.asInstanceOf[dom.Node]
                                          .removeEventListener(
                                            l.tpe,
                                            l.listener,
                                            l.options
                                          )
                                        n.asInstanceOf[dom.Node]
                                          .addEventListener(
                                            l.tpe,
                                            listener,
                                            options
                                          )
                                        l.copy(
                                          listener = listener,
                                          options = options,
                                          value = listener0
                                        )
                                      } else {
                                        l
                                      }
                                    }

                                    val wl = WrappedListener(
                                      typename.tpe,
                                      typename.name,
                                      listener0,
                                      listener,
                                      options
                                    )

                                    n.asInstanceOf[dom.Node]
                                      .addEventListener(
                                        wl.tpe,
                                        wl.listener,
                                        wl.options
                                      )

                                    dict(LISTENER_KEY) = wl :: on2

                                  }
                                }
                              }
                            }
                          }
                        )
                      )
                    }

                  case Order() =>
                    log("Step=Order") *>
                      F.delay {
                        groups
                          .map(
                            _.filter(_ != null).asInstanceOf[List[dom.Node]]
                          )
                          .foreach {
                            case group @ (_ :: tail) =>
                              group.zip(tail).reverse.foreach {
                                case (node, next) =>
                                  if (
                                    (node
                                      .compareDocumentPosition(
                                        next
                                      ) ^ dom.Node.DOCUMENT_POSITION_FOLLOWING) != 0
                                  ) {
                                    next.parentNode.insertBefore(node, next)
                                  }
                              }
                            case _ => ()
                          }
                      }.as(t.asInstanceOf[Terminal[F, N0, D0, PN0, PD0]])

                  case Sort(lt) =>
                    log("Step=Sort") *> F
                      .delay {
                        val sortedGroups =
                          groups.map { group =>
                            group
                              .filter(_ != null)
                              .map { node =>
                                val data = node
                                  .asInstanceOf[js.Dictionary[Any]]
                                  .get(DATA_KEY)
                                  .getOrElse(null)
                                (node, data)
                              }
                              .filter(_._2 != null)
                              .sortWith { case ((_, d1), (_, d2)) =>
                                lt.asInstanceOf[(Any, Any) => Boolean](d1, d2)
                              }
                              .map(_._1)
                          }
                        Terminal(sortedGroups, parents)
                          .asInstanceOf[Terminal[F, N0, D0, PN0, PD0]]
                      }
                      .flatMap(t => F.defer(go(Continue(t, Order()))))

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
                        .drain

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
                            go(Continue(terminal, Order()))
                          }
                      }

                    }

                  case DataFn(dataFn, keyOpt) =>
                    val update = Array.fill(groups.length)(Array.empty[Any])
                    val enter =
                      Array
                        .fill(groups.length)(Array.empty[EnterNode[Any, Any]])
                    val exit = Array.fill(groups.length)(Array.empty[Any])
                    log("Step=Data") *>
                      F.delay {
                        (groups.zip(parents).zipWithIndex).map {
                          case ((group, parent), j) =>
                            val parentData = if (parent != null) {
                              parent
                                .asInstanceOf[js.Dictionary[Any]]
                                .get(DATA_KEY)
                                .getOrElse(null)
                            } else {
                              null
                            }
                            val data = dataFn
                              .asInstanceOf[(Any, Any, Any, Any) => List[
                                Any
                              ]](
                                parent,
                                parentData,
                                j,
                                parents
                              )
                            val groupLength = group.length
                            val dataLength = data.length
                            val enterGroup =
                              new Array[EnterNode[Any, Any]](dataLength)
                            val updateGroup = new Array[Any](dataLength)
                            val exitGroup = new Array[Any](groupLength)

                            val nodes = group.asInstanceOf[List[Any]].toArray
                            val dataArr = data.toArray

                            keyOpt match {
                              case None =>
                                var i = 0
                                while (i < dataLength) {
                                  if (i < groupLength && nodes(i) != null) {
                                    nodes(i).asInstanceOf[js.Dictionary[Any]](
                                      DATA_KEY
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
                                        .get(DATA_KEY)
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
                                      DATA_KEY
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

                  case AttrFn(name, valueFn) =>
                    log("Step=AttrFn") *>
                      F.defer(
                        go(
                          Continue(
                            t,
                            Each { (n: N, d: D, i: Int, group: List[N]) =>
                              F.delay {
                                val fn = valueFn
                                  .asInstanceOf[(Any, Any, Any, Any) => Option[
                                    String
                                  ]]
                                val elm = n.asInstanceOf[dom.Element]
                                namespace(name) match {
                                  case namespace.Name.Simple(name0) =>
                                    fn(n, d, i, group) match {
                                      case Some(value) =>
                                        elm.setAttribute(name0, value)
                                      case None => elm.removeAttribute(name0)
                                    }
                                  case namespace.Name
                                        .Namespaced(space, local) =>
                                    fn(n, d, i, group) match {
                                      case Some(value) =>
                                        elm.setAttributeNS(
                                          space,
                                          local,
                                          value
                                        )
                                      case None =>
                                        elm
                                          .removeAttributeNS(space, local)
                                    }
                                }
                              }
                            }
                          )
                        )
                      )

                  case StyleFn(name, valueFn, priority) => {
                    log("Step=StyleFn") *>
                      F.defer(
                        go(
                          Continue(
                            t,
                            Each { (n: N, d: D, i: Int, group: List[N]) =>
                              F.delay {
                                val elm = n.asInstanceOf[dom.HTMLElement]
                                valueFn.asInstanceOf[
                                  (Any, Any, Any, Any) => Option[String]
                                ](n, d, i, group) match {
                                  case None => elm.style.removeProperty(name)
                                  case Some(value) => {
                                    if (priority) {
                                      elm.style
                                        .setProperty(name, value, "important")
                                    } else {
                                      elm.style.setProperty(name, value)
                                    }
                                  }
                                }
                              }.void
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
                                  .get(DATA_KEY)
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

                  case Select(selector) =>
                    log("Step=Select") *> F.defer {
                      go(
                        Continue(
                          t,
                          SelectFn { (n: N0, _: D0, _: Int, _: List[N0]) =>
                            F.delay {
                              n.asInstanceOf[dom.Element]
                                .querySelector(selector)
                                .asInstanceOf[N0]
                            }
                          }
                        )
                      )
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
                                  .get(DATA_KEY)
                                  .getOrElse(null)
                              val newNode =
                                selector
                                  .asInstanceOf[
                                    (Any, Any, Int, List[Any]) => F[N0]
                                  ](node, data, i, group)
                              newNode.flatMap { n0 =>
                                F.delay {
                                  if (n0 != null) {
                                    n0.asInstanceOf[js.Dictionary[Any]](
                                      DATA_KEY
                                    ) = data
                                  }
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

                  case SelectAll(sel) =>
                    log("Step=SelectAll") *> F.defer {
                      go(
                        Continue(
                          t,
                          SelectAllFn((n: N0, _: D0, _: Int, _: List[N0]) =>
                            F.delay {
                              n.asInstanceOf[dom.Element]
                                .querySelectorAll(sel)
                                .toList
                            }
                          )
                        )
                      )
                    }

                  case SelectAllFn(sel) =>
                    log("Step=SelectAllFn") *> groups
                      .traverse { group =>
                        group.filter(_ != null).zipWithIndex.traverse {
                          case (node, i) =>
                            val data =
                              node
                                .asInstanceOf[js.Dictionary[Any]]
                                .get(DATA_KEY)
                                .getOrElse(null)
                            sel.asInstanceOf[(Any, Any, Any, Any) => F[
                              List[N0]
                            ]](node, data, i, group)
                        }
                      }
                      .map(_.flatten)
                      .map { newGroups =>
                        val newParents = groups.map { group =>
                          group.filter(_ != null)
                        }.flatten
                        Terminal(
                          newGroups.asInstanceOf[List[List[N0]]],
                          newParents.asInstanceOf[List[PN0]]
                        )
                      }

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
                          child.asInstanceOf[js.Dictionary[Any]](DATA_KEY) =
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

  private sealed abstract class Action[+F[_], +N, +D, +PN, +PD]
      extends Selection[F, N, D, PN, PD]

  private case class AttrFn[F[_], N, D, PN, PD](
      name: String,
      value: (N, D, Int, List[N]) => Option[String]
  ) extends Action[F, N, D, PN, PD]

  private case class AttrTransitionFn[F[_], N, D, PN, PD](
      name: String,
      value: (N, D, Int, List[N]) => String
  ) extends Action[F, N, D, PN, PD]

  private case class TransitionDurationFn[F[_], N, D, PN, PD](
      duration: (N, D, Int, List[N]) => FiniteDuration
  ) extends Action[F, N, D, PN, PD]

  private case class TransitionEaseFn[F[_], N, D, PN, PD](
      ease: (N, D, Int, List[N]) => (Double => Double)
  ) extends Action[F, N, D, PN, PD]

  private case class TransitionDelayFn[F[_], N, D, PN, PD](
      delay: (N, D, Int, List[N]) => FiniteDuration
  ) extends Action[F, N, D, PN, PD]

  private case class Append[F[_], N, D, PN, PD](name: String)
      extends Action[F, N, D, PN, PD]

  private case class AppendFn[F[_], F1[x] >: F[x], N, N1, D, PN, PD](
      fn: (N, D, Int, List[N]) => F1[N1]
  ) extends Action[F, N, D, PN, PD]

  private case class Call[F0[_], F[_], N, D, PN, PD](
      fn: Selection[F0, N, D, PN, PD] => F[Unit]
  ) extends Action[F, N, D, PN, PD]

  private case class ClassedFn[F[_], N, D, PN, PD](
      names: String,
      value: (N, D, Int, List[N]) => Boolean
  ) extends Action[F, N, D, PN, PD]

  private case class Continue[F[_], N, D, PN, PD, N0, D0, PN0, PD0](
      current: Selection[F, N, D, PN, PD],
      step: Action[F, N, D, PN, PD]
  ) extends Selection[F, N0, D0, PN0, PD0]

  private case class DataFn[F[_], N, D0, D, PN, PD](
      data: (PN, PD, Int, List[PN]) => List[D],
      keys: Option[
        ((N, D0, Int, List[N]) => String, (PN, D, Int, List[D]) => String)
      ]
  ) extends Action[F, N, D0, PN, PD]

  private case class Dispatch[F[_], N, D, PN, PD](
      tpe: String,
      params: (N, D, Int, List[N]) => CustomEventParams
  ) extends Action[F, N, D, PN, PD]

  private case class Each[F[_], N, D, PN, PD](
      fn: (N, D, Int, List[N]) => F[Unit]
  ) extends Action[F, N, D, PN, PD]

  private case class EnterAppend[F[_], N, D, PN, PD, N0, D0, PN0, PD0](
      enter: Enter[F, N, D, PN, PD],
      name: String
  ) extends Selection[F, N0, D0, PN0, PD0]

  private case class Filter[F[_], N, D, PN, PD](
      selector: String
  ) extends Action[F, N, D, PN, PD]

  private case class FilterFn[F[_], N, D, PN, PD](
      fn: (N, D, Int, List[N]) => Boolean
  ) extends Action[F, N, D, PN, PD]

  private case class Join[F[_], N, D, PN, PD, N0](
      onEnter: Enter[F, N, D, PN, PD] => Selection[F, N0, D, PN, PD],
      onUpdate: Selection[F, N, D, PN, PD] => Selection[F, N, D, PN, PD],
      onExit: Selection[F, N, D, PN, PD] => Selection[F, N, D, PN, PD]
  ) extends Action[F, N, D, PN, PD]

  private case class NewTransition[F[_], N, D, PN, PD]()
      extends Action[F, N, D, PN, PD]

  private case class On[F[_], N, D, PN, PD](
      typenames: String,
      listener: Option[(N, dom.Event, D) => F[Unit]],
      options: Option[dom.EventListenerOptions],
      dispatcher: Dispatcher[F]
  ) extends Action[F, N, D, PN, PD]

  private case class Order[F[_], N, D, PN, PD]() extends Action[F, N, D, PN, PD]

  private case class Property[F[_], N, D, PN, PD](
      name: String,
      value: (N, D, Int, List[N]) => Option[Any]
  ) extends Action[F, N, D, PN, PD]

  private case class Remove[F[_], N, D, PN, PD]()
      extends Action[F, N, D, PN, PD]

  private case class RemoveAfterTransition[F[_], N, D, PN, PD]()
      extends Action[F, N, D, PN, PD]

  private case class Select[F[_], N, D, PN, PD](selector: String)
      extends Action[F, N, D, PN, PD]

  private case class SelectFn[F[_], N, D, PN, PD, N0](
      selector: (N, D, Int, List[N]) => F[N0]
  ) extends Action[F, N, D, PN, PD]

  private case class SelectAll[F[_], N, D, PN, PD](selector: String)
      extends Action[F, N, D, PN, PD]

  private case class SelectAllFn[F[_], N, N0, D, PN, PD](
      selector: (N, D, Int, List[N]) => F[List[N0]]
  ) extends Action[F, N, D, PN, PD]

  private case class Sort[F[_], N, D, PN, PD](lt: (D, D) => Boolean)
      extends Action[F, N, D, PN, PD]

  private case class StyleFn[F[_], N, D, PN, PD](
      name: String,
      fn: (N, D, Int, List[N]) => Option[String],
      priority: Boolean
  ) extends Action[F, N, D, PN, PD]

  private[selection] case class Terminal[F[_], N, D, PN, PD](
      groups: List[List[N]],
      parents: List[PN],
      enter: Option[List[List[EnterNode[D, PN]]]] = None,
      exit: Option[List[List[N]]] = None
  ) extends Selection[F, N, D, PN, PD]

  private case class TextFn[F[_], N, D, PN, PD](
      fn: (N, D, Int, List[N]) => String
  ) extends Action[F, N, D, PN, PD]

  private[selection] class EnterNode[D, PN](
      val parent: PN,
      val data: D,
      var _next: Any
  )

  private def parseListenerTypenames(
      typenames: String
  ): List[ListenerTypeAndName] = {
    typenames.trim().split("""^|\s+""").toList.map { t =>
      val i = t.indexOf(".")
      val (t0, name) = if (i >= 0) {
        (t.take(i), t.drop(i + 1))
      } else {
        (t, "")
      }
      ListenerTypeAndName(t0, name)
    }
  }

  private case class ListenerTypeAndName(tpe: String, name: String)

  private case class WrappedListener(
      tpe: String,
      name: String,
      value: Any,
      listener: js.Function1[dom.Event, Unit],
      options: dom.EventListenerOptions
  )

}
