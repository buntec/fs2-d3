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

object namespace {

  case class Namespaced(space: String, local: String)

  def apply(name: String): Either[String, Namespaced] = {
    val i = name.indexOf(":")
    val (prefix, name0) = if (i >= 0) {
      val prefix = name.take(i)
      (prefix, if (prefix != "xmlns") name.drop(i) else name)
    } else {
      (name, name)
    }
    namespaces.map.get(prefix) match {
      case None     => Left(name0)
      case Some(ns) => Right(Namespaced(ns, name0))
    }
  }

}
