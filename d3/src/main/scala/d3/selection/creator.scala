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

import org.scalajs.dom

private[selection] object creator {

  def apply(name: String, parent: dom.Node): dom.Element = {
    namespace(name) match {
      case namespace.Name.Simple(name0) =>
        creatorInherited(name0, parent)
      case ns @ namespace.Name.Namespaced(_, _) =>
        creatorFixed(ns, parent)
    }
  }

  private def creatorFixed(
      ns: namespace.Name.Namespaced,
      parent: dom.Node
  ): dom.Element = {
    parent.ownerDocument.createElementNS(ns.space, ns.local)
  }

  private def creatorInherited(name: String, parent: dom.Node): dom.Element = {
    val document = parent.ownerDocument
    val uri = parent.namespaceURI
    if (
      uri == namespaces.xhtml && document.documentElement.namespaceURI == namespaces.xhtml
    ) {
      document.createElement(name)
    } else {
      document.createElementNS(uri, name)
    }
  }

}
