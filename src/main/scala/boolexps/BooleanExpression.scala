package boolexps

import scala.languageFeature.postfixOps
import net.liftweb.json._

sealed trait BooleanExpression {
  import BooleanExpression._

  /**
    * The set of all variables in this expression. Variable names are case sensitive.
    */
  def allVars: Set[String] = Set.empty // operator and variable case classes override this.

  def isDNF: Boolean = true

  /**
    * Checks if this BooleanExpression is logically equivalent to another one, i.e. their truth tables are identical.
    * Note that, as a consequence of the Cook-Levin theorem, this is an NP-complete problem. Thus this function should
    * not be called on BooleanExpressions containing more than a small handful of variables.
    *
    * @param otherExpr the other expression to be compared against
    * @return `true` iff these two expressions are equivalent, false otherwise
    */
  def isEquivalentTo(otherExpr: BooleanExpression): Boolean = {
    // first we simplify
    val thisSimp = simplify
    val otherSimp = otherExpr.simplify
    val thisVars = thisSimp.allVars
    val otherVars = otherSimp.allVars

    // no variables in either expression, meaning both are True/False literals
    if (thisVars.isEmpty && otherVars.isEmpty) thisSimp == otherSimp
    else if (thisVars.nonEmpty && otherVars.nonEmpty && thisVars.intersect(otherVars).isEmpty) false
    else {
      val interps = makeInterpretations(thisVars ++ otherVars)
      interps.forall(i => thisSimp.evaluateForInterpretation(i) == otherSimp.evaluateForInterpretation(i))
    }
  }

  def evaluateForInterpretation(interp: Map[String, Boolean]): Option[Boolean]

  /**
    * `true` iff this is a literal (an atom or the negation of an atom), `false` otherwise.
    */
  def isLiteral: Boolean = false // literal case classes override this

  def isTF: Boolean = false

  /**
    * Simplifies this expression according to a specific subset of the laws of boolean algebra. This function makes no
    * guarantees regarding how or to what extent the expression will be simplified, other than that the resultant
    * expression will be logically equivalent to the current object, and that literals will not be modified.
    *
    * @return a `BooleanExpression` equivalent to the current object, which may be simplified to some degree.
    */
  def simplify: BooleanExpression = this // operator case classes override this.

  /**
    * Convert this expression to disjunctive normal form.
    * @return the disjunctive normal form equivalent of this BooleanExpression.
    */
  def toDNF: BooleanExpression = this // operator case classes override this.

  /**
    * Returns the JSON-serialized equivalent of this expression, compliant with
    * <a href="https://tools.ietf.org/html/rfc8259#section-2">RFC 8259</a>. The structure of the JSON is inspired by
    * LISP S-Expressions. The Extended Bachus-Naur form describing the grammar of all valid JSON strings is given below,
    * but in essence it is composed of boolean literals ("true", "false"), string literals (variable names), and/or JSON
    * arrays containing, in order, a boolean operand (as a string) and one or two valid operands, depending on the
    * valency of the operator. Operators are case insensitive strings, variables are case sensitive strings. Embedding
    * any expression inside an array as the sole member of that array does not change meaning, and thus redundant
    * brackets are acceptable, but discouraged for readability.
    *
    * <br><br>
    * EBNF (whitespace not accounted for here, follows usual JSON specifications):
    * <br><br>
    *
    * JSON-bool-expr = literal | not-expr | or-expr | and-expr | "[", JSON-bool-expr, "]" ;
    *
    * <br><br>
    * literal = boolean-literal | variable-literal
    *
    * <br><br>
    * boolean-literal = true | false
    *
    * <br><br>
    * true = "t"|"T", "r"|"R", "u"|"U", "e"|"E"
    *
    * <br><br>
    * false = "f"|"F", "a"|"A", "l"|"L", "s"|"S", "e"|"E"
    *
    * <br><br>
    * variable-literal = ? any valid JSON string without whitespace besides not-str, or-str, and-str ?
    *
    * <br><br>
    * not-expr = "[", not-str, JSON-bool-expr, "]"
    *
    * <br><br>
    * or-expr = "[", or-str, JSON-bool-expr, ",", JSON-bool-expr, "]"
    *
    * <br><br>
    * and-expr = "[", and-str, JSON-bool-expr, ",", JSON-bool-expr, "]"
    *
    * not-str = '"', "n"|"N", "o"|"O", "t"|"T", '",'
    *
    * or-str = "'"', o"|"O", "r"|"R", '",'
    *
    * and-str = '"', "a"|"A", "n"|"N", "d"|"D", '",'
    *
    * @return the JSON-serialized equivalent of this expression (i.e. "true").
    */
  def toJSON: String = {
    val s: String = {
      this match {
        case Not(e) => s""""NOT", ${e toJSON}"""
        case Or(e1, e2) => s""""OR", ${e1 toJSON}, ${e2 toJSON} """
        case And(e1, e2) => s""""AND", ${e1 toJSON}, ${e2 toJSON} """
      }
    }
    '[' + s + ']'
  }

  /**
    * Convert this expression to negation normal form.
    * @return the negation normal form equivalent of this BooleanExpression.
    */
  def toNNF: BooleanExpression = this

  /**
    * Negate this expression.
    * @return the negation of this expression.
    */
  def negate: BooleanExpression
}

case object True extends BooleanExpression {
  override def evaluateForInterpretation(interp: Map[String, Boolean]): Option[Boolean] = Option(true)

  /**
    * Returns the JSON-serialized equivalent of this expression. Per
    * <a href="https://tools.ietf.org/html/rfc8259#section-2">RFC 8259</a>, boolean literals are valid JSON (i.e. they
    * do not need to be contained in a larger structure)
    * @return the JSON-serialized equivalent of this expression (i.e. "true").
    */
  override def toJSON: String = "true"

  override def isTF: Boolean = true

  override def isLiteral: Boolean = true

  override def negate: BooleanExpression = False
}

case object False extends BooleanExpression {
  override def evaluateForInterpretation(interp: Map[String, Boolean]): Option[Boolean] = Option(false)

  override def isTF: Boolean = true

  /**
    * Returns the JSON-serialized equivalent of this expression. Per
    * <a href="https://tools.ietf.org/html/rfc8259#section-2">RFC 8259</a>, boolean literals are valid JSON (i.e. they
    * do not need to be contained in a larger structure)
    * @return the JSON-serialized equivalent of this expression (i.e. "false").
    */
  override lazy val toJSON: String = "false"

  override lazy val isLiteral: Boolean = true

  override lazy val negate: BooleanExpression = True
}

case class Variable(symbol: String) extends BooleanExpression {
  override lazy val allVars: Set[String] = Set(symbol)

  override def evaluateForInterpretation(interp: Map[String, Boolean]): Option[Boolean] = interp.get(symbol)

  /**
    * Returns the JSON-serialized equivalent of this expression. Per
    * <a href="https://tools.ietf.org/html/rfc8259#section-2">RFC 8259</a>, string literals are valid JSON (i.e. they
    * do not need to be contained in a larger structure)
    * @return the JSON-serialized equivalent of this expression (i.e. the variable name as a JSON string).
    */
  override lazy val toJSON: String = s""""$symbol""""

  override lazy val isLiteral: Boolean = true

  override lazy val negate: BooleanExpression = Not(this)
}

case class Not(e: BooleanExpression) extends BooleanExpression {
  override lazy val allVars: Set[String] = e.allVars

  override def evaluateForInterpretation(interp: Map[String, Boolean]): Option[Boolean] =
    e.evaluateForInterpretation(interp).map(!_)

  override lazy val isDNF: Boolean = e.isLiteral

  override lazy val isLiteral: Boolean = e.isLiteral

  override lazy val simplify: BooleanExpression = e.simplify match {
    //case Not(expr) => expr // double negation
    case expr => expr.negate
  }

  override lazy val toDNF: BooleanExpression = e.negate.toDNF

  override lazy val toNNF: BooleanExpression = {
    e match {
      case True => False
      case False => True
      case Variable(symbol) => this
      case Not(e) => e.toNNF
      case Or(e1, e2) => And(Not(e1).toNNF, Not(e2).toNNF)
      case And(e1, e2) => Or(Not(e1).toNNF, Not(e2).toNNF)
    }
  }

  override lazy val negate: BooleanExpression = e
}

case class Or(e1: BooleanExpression, e2: BooleanExpression) extends BooleanExpression {
  override lazy val allVars: Set[String] = e1.allVars ++ e2.allVars

  override def evaluateForInterpretation(interp: Map[String, Boolean]): Option[Boolean] = {
    e1.evaluateForInterpretation(interp).flatMap {
      e1Result => e2.evaluateForInterpretation(interp).map(e2Result => e1Result || e2Result)
    }
  }

  /**
    * `true` iff this expression is in disjunctive normal form, `false` otherwise. For a disjunction, that can only be
    * the case if its operands are themselves in disjunctive normal form.
    */
  override lazy val isDNF: Boolean = e1.isDNF && e2.isDNF

  override lazy val simplify: BooleanExpression = (e1.simplify, e2.simplify) match {
    // identity laws
    case (expr, False) => expr
    case (False, expr) => expr

    // annihilator laws
    case (expr, True) => True
    case (True, expr) => True

    // idempotence
    case (expr1, expr2) if expr1.isEquivalentTo(expr2) => expr1
    case (expr1, expr2) => Or(expr1, expr2)
  }

  override lazy val toNNF: BooleanExpression = Or(e1.toNNF, e2.toNNF)

  override lazy val toDNF: BooleanExpression = {
    (e1, e2) match {
      case (x, y) if x.isLiteral && y.isLiteral => this
      case (andExpr: And, y) if y.isLiteral => Or(andExpr.toDNF, y)
    }
  }

  override def negate: BooleanExpression = And(e1.negate, e2.negate)
}

case class And(e1: BooleanExpression, e2: BooleanExpression) extends BooleanExpression {
  override lazy val allVars: Set[String] = e1.allVars ++ e2.allVars

  override def evaluateForInterpretation(interp: Map[String, Boolean]): Option[Boolean] = {
    e1.evaluateForInterpretation(interp).flatMap {
      e1Result => e2.evaluateForInterpretation(interp).map(e2Result => e1Result && e2Result)
    }
  }

  /**
    * `true` iff this expression is in disjunctive normal form, `false` otherwise. For a conjunction, that can only be
    * the case if its operands are literals.
    */
  override lazy val isDNF: Boolean = e1.isLiteral && e2.isLiteral

  override lazy val toNNF: BooleanExpression = And(e1.toNNF, e2.toNNF)

  override lazy val toDNF: BooleanExpression = {
    (e1, e2) match {
      case (x, y) if x.isLiteral && y.isLiteral => this
      case (And(x, y), z) if z.isLiteral => And(e1.toDNF, z)
      case (x, And(y, z)) if x.isLiteral => And(x, e2.toDNF)
    }
  }

  override lazy val simplify: BooleanExpression = (e1.simplify, e2.simplify) match {
    // identity laws
    case (expr, True) => expr
    case (True, expr) => expr

    // annihilator laws
    case (expr, False) => False
    case (False, expr) => False

    // idempotence
    case (expr1, expr2) if expr1.isEquivalentTo(expr2) => expr1
    case (expr1, expr2) => And(expr1, expr2)
  }

  override lazy val negate: BooleanExpression = Or(e1.negate, e2.negate)
}

object BooleanExpression {

  /**
    * Deserialize a list of JValues, which will normally correspond to a JSON array.
    * @param l the list of JValues
    * @return A `Some` containing the corresponding deserialized BooleanExpression, or a `None` if the JSON was
    *         malformed.
    */
  private def deserialize(l: List[JValue]): Option[BooleanExpression] = {
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
  private def deserialize(jv: JValue): Option[BooleanExpression] = {
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

  def deserialize(s: String): Option[BooleanExpression] = {
    try {
      deserialize(parse(s))
    } catch {
      case parseException: JsonParser.ParseException => None
    }
  }

  /**
    * Given a `Set` of variable names, generates all possible logical interpretations (an assignment of `true` or
    * `false` to each variable) for the given variables.
    *
    * @param variables a `Set` of `String`s corresponding to variable names
    * @return a `List` of `Map`s whose keys are variable names and whose values are `true`/`false` assignments.
    */
  private def makeInterpretations(variables: Set[String]): Set[Map[String, Boolean]] = {
    variables.foldLeft(Set[Map[String, Boolean]]()) {
      (acc: Set[Map[String, Boolean]], variable: String) => {
        if (acc.isEmpty) Set(Map(variable -> true), Map(variable -> false))
        else acc.map(_ + (variable -> true)) ++ acc.map(_ + (variable -> false))
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val w = Variable("w")
    val x = Variable("x")
    val y = Variable("y")
    val z = Variable("z")
  }
}