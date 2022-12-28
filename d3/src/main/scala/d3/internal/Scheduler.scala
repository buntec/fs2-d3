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

package d3.internal

import cats.effect.kernel.Async
import cats.syntax.all._
import fs2.Stream
import org.scalajs.dom

import scala.concurrent.duration.Duration

private[d3] object Scheduler {

  def awakeEveryAnimationFrame[F[_]](implicit
      F: Async[F]
  ): Stream[F, Duration] = {
    val ticks = Stream.repeatEval {
      F.async[Unit] { cb =>
        F.delay {
          val id = dom.window.requestAnimationFrame { _ => cb(Right(())) }
          Some(F.delay(dom.window.cancelAnimationFrame(id)))
        }
      }
    }
    Stream.eval(F.monotonic).flatMap { start =>
      ticks >> Stream.eval(F.monotonic.map(_ - start))
    }
  }

}
