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

package d3.ease

object quad {

  def quadIn(t: Double): Double = t * t

  def quadOut(t: Double): Double = t * (2 - t)

  def quadInOut(t: Double): Double = {
    val t2 = 2.0 * t
    if (t2 <= 1) (t2 * t2) / 2.0
    else ((t2 - 1) * (3 - t2) + 1) / 2.0
  }

}
