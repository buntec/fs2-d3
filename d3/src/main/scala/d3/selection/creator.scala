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

object creator {

  def apply(name: String, parent: dom.Node) = {
    namespace(name) match {
      case Left(name0) =>
        creatorInherited(name0, parent)
      case Right(ns) =>
        creatorFixed(ns, parent)
    }
  }

  private def creatorFixed(ns: namespace.Namespaced, parent: dom.Node) = {
    parent.ownerDocument.createElementNS(ns.space, ns.local)
  }

  private def creatorInherited(name: String, parent: dom.Node) = {
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
