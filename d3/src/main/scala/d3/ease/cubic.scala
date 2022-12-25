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

object cubic {

  def cubicIn(t: Double): Double = t * t * t

  def cubicOut(t: Double): Double =
    (t - 1) * (t - 1) * (t - 1) + 1

  def cubicInOut(t: Double): Double = {
    if (t < 0.5) {
      4 * t * t * t
    } else {
      1 - math.pow(-2 * t + 2, 3) / 2
    }
  }

}