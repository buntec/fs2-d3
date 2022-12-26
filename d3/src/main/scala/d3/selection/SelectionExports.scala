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

package d3.selection

import org.scalajs.dom

private[d3] trait SelectionExports {

  def select[F[_], N, D](
      selector: String
  ): Selection[F, N, D, dom.HTMLElement, Unit] = Selection.select(selector)

  def select[F[_], N, D](node: N): Selection[F, N, D, dom.HTMLElement, Unit] =
    Selection.Terminal(List(List(node)), List(null))

  def selectAll[F[_], N, D](
      selector: String
  ): Selection[F, N, D, dom.HTMLElement, Unit] =
    Selection.selectAll(selector)

  def selectAll[F[_], N, D](
      nodes: List[N]
  ): Selection[F, N, D, dom.HTMLElement, Unit] =
    Selection.Terminal(List(nodes), List(null))

}
