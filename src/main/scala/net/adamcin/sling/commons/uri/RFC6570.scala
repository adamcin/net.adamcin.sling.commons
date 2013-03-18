package net.adamcin.sling.commons.uri

import org.slf4j.LoggerFactory
import net.adamcin.sling.commons.uri.RFC6570.Expression


class RFC6570 private (expressions: List[Expression]) extends URITemplate {

  override def toString: String = expressions.toString

  def matches(uri: String): Boolean = ???

  def parse(uri: String): Map[String, Value] = ???

  def expand(data: Map[String, Any]): String = ???
}

object RFC6570 {
  final val LOGGER = LoggerFactory.getLogger(classOf[RFC6570])
  final val BEGIN_EXP = '{'
  final val END_EXP = '}'

  final val OP_RESERVED = '+'
  final val OP_FRAGMENT = '#'
  final val OP_LABEL = '.'
  final val OP_SLASH = '/'
  final val OP_SEMICOLON = ';'
  final val OP_QUERY = '?'
  final val OP_AMPERSAND = '&'

  final val OPERATORS = Set(OP_RESERVED, OP_FRAGMENT, OP_LABEL, OP_SLASH, OP_SEMICOLON, OP_QUERY, OP_AMPERSAND)

  final val MOD_PREFIX = ':'
  final val MOD_EXPLODE = '*'

  @throws(classOf[URITemplateException])
  def apply(template: String) = construct(template) match {
    case Right(expressions) => new RFC6570(expressions)
    case Left(ex) => throw ex
  }

  sealed trait Variable
  case class Single(name: String) extends Variable
  case class Explode(name: String) extends Variable
  case class Prefix(name: String, length: Int) extends Variable

  sealed trait Expression
  case class Literal(value: String) extends Expression
  case class Simple(vars: List[Variable]) extends Expression
  case class Reserved(vars: List[Variable]) extends Expression
  case class Fragment(vars: List[Variable]) extends Expression
  case class Label(vars: List[Variable]) extends Expression
  case class Slash(vars: List[Variable]) extends Expression
  case class Semicolon(vars: List[Variable]) extends Expression
  case class Query(vars: List[Variable]) extends Expression
  case class Ampersand(vars: List[Variable]) extends Expression

  protected[uri] def construct(t: String): Either[URITemplateException, List[Expression]] = {
    def _construct(t: String, startIndex: Int): Either[URITemplateException, List[Expression]] = {
      if (t == null || t.length <= startIndex) {
        Right(Nil)
      } else if (t.charAt(startIndex) != BEGIN_EXP) {
        val literalEnd = t.indexOf(BEGIN_EXP, startIndex)
        val toIndex = if (literalEnd < 0) {
          t.length
        } else {
          literalEnd
        }

        _construct(t, toIndex) match {
          case Right(vars) => Right(Literal(t.substring(startIndex, toIndex)) :: vars)
          case Left(ex) => Left(ex)
        }
      } else {
        val endExp = t.indexOf(END_EXP, startIndex)

        val expression =
          if (endExp < 0) {
            Left(new URITemplateException(t, "Unclosed expression", startIndex))
          } else {
            val first = t.charAt(startIndex + 1)
            if (OPERATORS contains first) {
              parseVars(t, startIndex + 2, endExp) match {
                case Right(Nil) =>
                  Left(new URITemplateException(t, "No variables specified for expression", startIndex + 2))
                case Right(vars) => {
                  Right(first match {
                    case OP_RESERVED => Reserved(vars)
                    case OP_FRAGMENT => Fragment(vars)
                    case OP_LABEL => Label(vars)
                    case OP_SLASH => Slash(vars)
                    case OP_SEMICOLON => Semicolon(vars)
                    case OP_QUERY => Query(vars)
                    case OP_AMPERSAND => Ampersand(vars)
                  })
                }
                case Left(ex) => Left(ex)
              }
            } else {
              parseVars(t, startIndex + 1, endExp) match {
                case Right(Nil) =>
                  Left(new URITemplateException(t, "No variables specified for expression", startIndex + 1))
                case Right(vars) => Right(Simple(vars))
                case Left(ex) => Left(ex)
              }
            }
          }

        expression match {
          case Right(exp) => {
            _construct(t, endExp + 1) match {
              case Right(vars) => Right(exp :: vars)
              case Left(ex) => Left(ex)
            }
          }
          case Left(ex) => {
            Left(ex)
          }
        }
      }
    }

    _construct(t, 0)
  }

  protected[uri] def parseVars(t: String, startIndex: Int, stopIndex: Int): Either[URITemplateException, List[Variable]] = {
    if (stopIndex <= startIndex) {
      Right(Nil)
    } else {
      val comma = t.indexOf(',', startIndex)
      val varStop = if (comma < 0) {
        stopIndex
      } else {
        comma min stopIndex
      }
      val rawVar = t.substring(startIndex, varStop)

      val variable =
        if (t.charAt(varStop - 1) == MOD_EXPLODE) {
          Explode(t.substring(startIndex, varStop - 1))
        } else {
          val opPrefix = rawVar.indexOf(MOD_PREFIX)
          if (opPrefix < 0) {
            Single(rawVar)
          } else {
            val len = rawVar.substring(opPrefix + 1).toInt
            Prefix(rawVar.substring(0, opPrefix), len)
          }
        }

      parseVars(t, varStop + 1, stopIndex) match {
        case Right(vars) => Right(variable :: vars)
        case Left(ex) => Left(ex)
      }
    }
  }
}
