package net.adamcin.sling.commons.uri


trait URITemplate {

  sealed trait Value
  case class SimpleValue(value: String) extends Value
  case class ListValue(values: List[String])
  case class MapValue(values: Map[String, String])

  def matches(uri: String): Boolean

  def parse(uri: String): Map[String, Value]

  def expand(data: Map[String, Any]): String
}

class URITemplateException(val input: String, val reason: String, val index: Int)
  extends Exception(reason) {
  override def toString: String = super.toString + (" [pos: %s, input: %s]" format (index, input))
  override def equals(obj: Any): Boolean = {
    obj match {
      case ex: URITemplateException => this.eq(ex) ||
        List(input, reason, index) == List(ex.input, ex.reason, ex.index)
      case _ => false
    }
  }
}
