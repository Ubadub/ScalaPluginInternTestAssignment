package boolexps

import scala.languageFeature.postfixOps

import net.liftweb.json._

sealed trait BooleanExpression {
  def toJSON: String = {
    val s: String = {
      this match {
        case True => "true"
        case False => "false"
        case Not(e) => s""""NOT", ${e toJSON}"""
        case Or(e1, e2) => s""" "OR", ${e1 toJSON}, ${e2 toJSON} """
        case And(e1, e2) => s""" "AND", ${e1 toJSON}, ${e2 toJSON} """
      }
    }
    '[' + s + ']'
  }
}

case object True extends BooleanExpression

case object False extends BooleanExpression

case class Variable(symbol: String) extends BooleanExpression {
  override def toJSON: String = s""""$symbol"""" // we don't need or want solitary variables to be in a JSON array
}

case class Not(e: BooleanExpression) extends BooleanExpression

case class Or(e1: BooleanExpression, e2: BooleanExpression) extends BooleanExpression

case class And(e1: BooleanExpression, e2: BooleanExpression) extends BooleanExpression

object BooleanExpression {

  /**
    * Deserialize a list of JValues, which will normally correspond to a JSON array.
    * @param l the list of JValues
    * @return A `Some` containing the corresponding deserialized BooleanExpression, or a `None` if the JSON was
    *         malformed.
    */
  def deserialize(l: List[JValue]): Option[BooleanExpression] = {
    l match {
      // if it's a single value, it must be a variable or boolean literal, so just evaluate it
      case (js: JString) :: Nil => deserialize(js)
      case (jb: JBool) :: Nil => deserialize(jb)

      // Not takes one operand
      case JString(s) :: t :: Nil if s.toUpperCase == "NOT" => deserialize(t).map(Not)

      // Or & And take two operands each
      case JString(s) :: t1 :: t2 :: Nil if s.toUpperCase == "OR" =>
        for (e1 <- deserialize(t1); e2 <- deserialize(t2)) yield Or(e1, e2)

      case JString(s) :: t1 :: t2 :: Nil if s.toUpperCase == "AND" =>
        for (e1 <- deserialize(t1); e2 <- deserialize(t2)) yield And(e1, e2)

      // if we got this far, the JSON was malformed
      case _ => None
    }
  }

  /**
    * Deserialize a JValue (representing some JSON segment).
    * @param jv the JValue to serialize
    * @return A `Some` containing the deserialized BooleanExpression, or a `None` if the JSON was malformed
    */
  def deserialize(jv: JValue): Option[BooleanExpression] = {
    jv match {
      case JsonAST.JString(s) => {
        // check that the String isn't a reserved keyword, otherwise make it a variable
        val sUpper = s.toUpperCase
        if (sUpper == "NOT" || sUpper == "OR" || sUpper == "AND") None
        else Option(Variable(s))
      }

      case JsonAST.JBool(value) =>
        if (value) Option(True)
        else Option(False)

      case JsonAST.JArray(arr) => deserialize(arr)

      // if we got this far, the JSON was malformed
      case _ => None
    }
  }

  def deserialize(s: String): Option[BooleanExpression] = deserialize(parse(s))

  def main(args: Array[String]): Unit = {

  }

}