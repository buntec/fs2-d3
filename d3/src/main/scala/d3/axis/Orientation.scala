package d3.axis

sealed trait Orientation

object Orientation {

  case class Top() extends Orientation
  case class Right() extends Orientation
  case class Bottom() extends Orientation
  case class Left() extends Orientation

}
