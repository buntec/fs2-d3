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

package d3.interpolate

object string {

  private val reA = """[-+]?(?:\d+.?\d*|.?\d+)(?:[eE][-+]?\d+)?""".r

  def apply(a: String, b: String): Double => String = {

    val ips = reA
      .findAllMatchIn(a)
      .zip(reA.findAllMatchIn(b))
      .map { case (ma, mb) =>
        mb.matched -> number.apply(ma.matched.toDouble, mb.matched.toDouble)
      }
      .toMap

    (t: Double) =>
      if (t <= 0) a
      else if (t >= 1) b
      else
        reA.replaceAllIn(
          b,
          m => ips.get(m.matched).fold(m.matched)(_(t).toString)
        )

  }

}
