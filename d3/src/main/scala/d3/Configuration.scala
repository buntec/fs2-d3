package d3

import scala.util.Try

import scalajs.js

private[d3] object Configuration {

  val logging: Boolean = Try(
    js.Dynamic.global.d3.logging.asInstanceOf[Boolean]
  ).toOption.getOrElse(false)

}
