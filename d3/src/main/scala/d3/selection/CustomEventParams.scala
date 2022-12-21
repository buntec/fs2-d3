package d3.selection

case class CustomEventParams(
    bubbles: Boolean,
    cancelable: Boolean,
    detail: Option[Any]
)
