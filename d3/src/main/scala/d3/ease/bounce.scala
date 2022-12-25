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

package d3.ease

object bounce {

  private val b1 = 4.0 / 11
  private val b2 = 6.0 / 11
  private val b3 = 8.0 / 11
  private val b4 = 3.0 / 4
  private val b5 = 9.0 / 11
  private val b6 = 10.0 / 11
  private val b7 = 15.0 / 16
  private val b8 = 21.0 / 22
  private val b9 = 63.0 / 64
  private val b0 = 1.0 / b1 / b1

  def bounceOut(t: Double): Double = {
    if (t < b1) b0 * t * t
    else if (t < b3) b0 * (t - b2) * (t - b2) + b4
    else if (t < b6) b0 * (t - b5) * (t - b5) + b7
    else b0 * (t - b8) * (t - b8) + b9
  }

  def bounceIn(t: Double): Double = {
    1 - bounceOut(1 - t)
  }

  def bounceInOut(t: Double): Double = {
    val t2 = 2 * t
    if (t2 <= 1) (1 - bounceOut(1 - t2)) / 2
    else (bounceOut(t2 - 1) + 1) / 2
  }

}
