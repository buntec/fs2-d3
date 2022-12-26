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

package d3.transition

object interpolate {

  def apply(a: Option[String], b: Option[String]): Double => Option[String] = {
    (a, b) match {
      case (Some(a), Some(b)) =>
        val interp = apply(a, b)
        (t: Double) => Some(interp(t))
      case (None, Some(b)) =>
        (t: Double) => if (t < 1) None else Some(b)
      case (Some(a), None) =>
        (t: Double) => if (t < 1) Some(a) else None
      case (None, None) => (_: Double) => None
    }
  }

  def apply(a: String, b: String): Double => String = {

    val fallback = (t: Double) => if (t < 1) a else b

    b.toDoubleOption match {
      case Some(_) =>
        d3.interpolate.number(a, b).getOrElse(fallback)
      case None => {
        d3.color.fromString(b) match {
          case Some(_) => d3.interpolate.rgb(a, b).getOrElse(fallback)
          case None    => d3.interpolate.string(a, b)
        }
      }
    }

  }

}
