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

import cats.syntax.all._

object number {

  def apply(a: Double, b: Double): Double => Double = { (t: Double) =>
    (1 - t) * a + t * b
  }

  def apply(a: String, b: String): Option[Double => String] = {
    (a.toDoubleOption, b.toDoubleOption).tupled
      .map { case (a0, b0) =>
        val interp = apply(a0, b0)
        (t: Double) => interp(t).toString
      }
  }

}
