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

package examples

import cats.effect.kernel.Async
import org.scalajs.dom

class Example3[F[_]](implicit F: Async[F]) {

  val data = List("foo", "bar", "baz")

  def run: F[Unit] =
    d3.select[F, dom.Element, Nothing]("#app")
      .append("span")
      .selectAll[dom.Element, String]("span")
      .data(data)
      .join[F, dom.Element, String, dom.Element, Nothing](
        _.append("p")
      )
      .append[F, dom.Element] { (_, d, _, _) =>
        F.delay {
          val node = dom.document.createElement("div")
          node.textContent = d
          node
        }
      }
      .property("fooProp", (_, d, _, _) => Some(d))
      .compile
      .drain

}
