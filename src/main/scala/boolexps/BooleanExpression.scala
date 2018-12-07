package boolexps

import scala.languageFeature.postfixOps
import net.liftweb.json._

/** Represents a boolean statement that can encode conjunctions, disjunctions, negations, atoms (named variables), and
  * any combination of those.
  * <br><br>
  * Can be serialized to JSON and deserialized from JSON (the latter uses a function in the companion object). The
  * generated JSON is compliant with [[https://tools.ietf.org/html/rfc8259#section-2 RFC 8259]]
  * <br><br>
  * The structure of the JSON is inspired by LISP S-Expressions. The Extended Bachus-Naur form describing the grammar of
  * all valid JSON strings is given below, but in essence it is composed of boolean literals ("true", "false"), string
  * literals (variable names), and/or JSON arrays containing, in order, a boolean operand (as a string) and one or two
  * valid operands, depending on the valency of the operator. Operators are case insensitive strings, variables are
  * case sensitive strings. Embedding any expression inside an array as the sole member of that array does not change
  * meaning, and thus redundant brackets are acceptable, but discouraged for readability.
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
  */
sealed trait BooleanExpression {
  import BooleanExpression._

  /**
    * The set of all variables in this expression. Variable names are case sensitive.
    */
  def allVars: Set[String] = Set.empty // operator and variable case classes override this.

  /** Given a logical interpretation (a mapping of variables to their truth values), calculates the truth value of this
    * expression using that interpretation.
    *
    * @param interp a logical interpretation as represented by a map whose keys are variable names and whose values are
    *               the true/false values of those variables
    * @return a [[scala.Some Some]] containing the result of the evaluation, or a [[scala.None None]] if given an
    *         invalid interpretation (i.e. one lacking some of the variables in this expression)
    */
  def evaluateForInterpretation(interp: Map[String, Boolean]): Option[Boolean]

  lazy val interpretations: Set[Map[String, Boolean]] = makeInterpretations(allVars)

  /**
    * Determines if this object is in disjunctive normal form.
    * @return `true` if this object is in disjunctive normal form, or `false` otherwise.
    */
  def isDNF: Boolean = true // operator case classes override this.

  /** Checks if this BooleanExpression is logically equivalent to another one, i.e. their truth tables are identical.
    * Note that, as a consequence of the Cook-Levin theorem, this is an NP-complete problem. Thus this function should
    * not be called on `BooleanExpression`s containing more than a small number of variables.
    *
    * @param otherExpr the other expression to be compared against
    * @return `true` iff these two expressions are equivalent, `false` otherwise
    */
  def isEquivalentTo(otherExpr: BooleanExpression): Boolean = {
    // first we simplify
    val thisSimp = simplify
    val otherSimp = otherExpr.simplify
    val thisVars = thisSimp.allVars
    val otherVars = otherSimp.allVars

    // no variables in either expression, meaning both are True/False literals
    if (thisVars.isEmpty && otherVars.isEmpty) thisSimp == otherSimp
    // no variables in common
    else if (thisVars.nonEmpty && otherVars.nonEmpty && thisVars.intersect(otherVars).isEmpty) false
    else { // compare truth tables
      val interps = makeInterpretations(thisVars ++ otherVars)
      interps.forall(i => thisSimp.evaluateForInterpretation(i) == otherSimp.evaluateForInterpretation(i))
    }
  }

  /** Determines if this expression is a literal (an atom or the negation of an atom)
    * @return `true` iff this is a literal, `false` otherwise.
    */
  def isLiteral: Boolean = false // literal case classes override this

  /** Determines if this expression is a [[https://en.wikipedia.org/wiki/Clause_(logic) logical clause]].
    * <br>
    * A BooleanExpression is a logical clause iff it consists only of [[boolexps.Variable variables]],
    * [[boolexps.Not negations]] of variables, and a single binary operator: either [[boolexps.Or Or]] or
    * [[boolexps.And]], but not both. In other words, a logical clause is either a disjunction or a conjunction of a
    * finite number of literals
    * <br>
    * @return `true` if this clause is a logical clause, `false` otherwise
    */
  def isClause: Boolean

  /** Negate this expression. The result is logically, but not necessarily structurally, equivalent to wrapping the
    * expression in [[boolexps.Not Not]].
    * @return the negation of this expression.
    */
  def negate: BooleanExpression

  /** Distributes all conjunctions over disjunctions such that no disjunctions are nested inside any conjunctions, which
    * has the effect of producing a `BooleanExpression` in disjunctive normal form when called on a `BooleanExpression`
    * in negation normal form.
    *
    * @return a `BooleanExpression` which contains no disjunctions nested inside any conjunction.
    */
  def NNFtoDNF: BooleanExpression

  /** Simplifies this expression according to a specific subset of the laws of boolean algebra.
    *
    * This function makes no guarantees regarding how or to what extent the expression will be simplified, other than
    * that the resultant expression will be logically equivalent to the current one, and that literals will not be
    * modified.
    *
    * @return a `BooleanExpression` equivalent to the current object, which may be simplified to some degree.
    */
  def simplify: BooleanExpression = this // operator case classes override this.

  /** Convert this expression to disjunctive normal form.
    * @return the disjunctive normal form equivalent of this BooleanExpression.
    */
  final lazy val toDNF: BooleanExpression = toNNF.simplify.NNFtoDNF

  /** Generates the JSON-serialized equivalent of this expression.
    *
    * For a description of the grammar of the
    * JSON, see this class' documentation.
    */
  def toJSON: String = {
    val s: String = {
      this match {
        case Not(e) => s""""NOT", ${e toJSON}"""
        case Or(e1, e2) => s""""OR", ${e1 toJSON}, ${e2 toJSON}"""
        case And(e1, e2) => s""""AND", ${e1 toJSON}, ${e2 toJSON}"""
      }
    }
    '[' + s + ']'
  }

  /** Convert this expression to negation normal form.
    * @return the negation normal form equivalent of this BooleanExpression.
    */
  def toNNF: BooleanExpression = this
}

case object True extends BooleanExpression {
  override def evaluateForInterpretation(interp: Map[String, Boolean]): Option[Boolean] = Option(true)

  override lazy val isLiteral: Boolean = true

  override lazy val isClause: Boolean = true

  override lazy val toJSON: String = "true"

  override lazy val negate: BooleanExpression = False

  override lazy val NNFtoDNF: BooleanExpression = this
}

case object False extends BooleanExpression {
  override def evaluateForInterpretation(interp: Map[String, Boolean]): Option[Boolean] = Option(false)

  override lazy val isLiteral: Boolean = true

  override lazy val isClause: Boolean = true

  override lazy val negate: BooleanExpression = True

  override lazy val NNFtoDNF: BooleanExpression = this

  override lazy val toJSON: String = "false"
}

case class Variable(symbol: String) extends BooleanExpression {
  override lazy val allVars: Set[String] = Set(symbol)

  override def evaluateForInterpretation(interp: Map[String, Boolean]): Option[Boolean] = interp.get(symbol)

  override lazy val isLiteral: Boolean = true

  override lazy val isClause: Boolean = true

  override lazy val negate: BooleanExpression = Not(this)

  override lazy val NNFtoDNF: BooleanExpression = this

  override lazy val toJSON: String = s""""$symbol""""
}

case class Not(e: BooleanExpression) extends BooleanExpression {
  override lazy val allVars: Set[String] = e.allVars

  override def evaluateForInterpretation(interp: Map[String, Boolean]): Option[Boolean] =
    e.evaluateForInterpretation(interp).map(!_)

  override lazy val isDNF: Boolean = isLiteral

  override lazy val isLiteral: Boolean = e match {
      case Variable(_) => true
      case _ => false
  }

  override lazy val isClause: Boolean = isLiteral

  override lazy val negate: BooleanExpression = e

  override lazy val NNFtoDNF: BooleanExpression = e match {
    case _: Variable => this
    case Not(ee) => ee.NNFtoDNF
    case _ => e.NNFtoDNF.negate
  }

  override lazy val simplify: BooleanExpression = e.simplify.negate

  override lazy val toNNF: BooleanExpression = {
    e match {
      case True => False
      case False => True
      case Variable(_) => this
      case Not(e) => e.toNNF
      case Or(e1, e2) => And(Not(e1).toNNF, Not(e2).toNNF)
      case And(e1, e2) => Or(Not(e1).toNNF, Not(e2).toNNF)
    }
  }
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

  override lazy val isClause: Boolean = {
    (e1, e2) match {
      case (x: Or, y: Or) => x.isClause && y.isClause
      case (x, y) => x.isLiteral && y.isLiteral
    }
  }

  override lazy val negate: BooleanExpression = And(e1.negate, e2.negate)

  override lazy val NNFtoDNF: BooleanExpression = {
    (e1, e2) match {
      case (x, y) if x.isLiteral && y.isLiteral => this
      case _ => Or(e1.NNFtoDNF, e2.NNFtoDNF)
      //case (andExpr: And, y) if y.isLiteral => Or(andExpr.toDNF, y)
      //case (x, andExpr: And) if x.isLiteral => Or(x, andExpr.toDNF)
    }
  }

  override lazy val simplify: BooleanExpression = (e1.simplify, e2.simplify) match {
    // identity laws
    case (expr, False) => expr
    case (False, expr) => expr

    // annihilator laws
    case (_, True) => True
    case (True, _) => True

    // idempotence
    case (expr1, expr2) if expr1.isEquivalentTo(expr2) => expr1

    case (expr1, expr2) if expr1.negate.isEquivalentTo(expr2) => True
    case (expr1, expr2) => Or(expr1, expr2)
  }

  override lazy val toNNF: BooleanExpression = Or(e1.toNNF, e2.toNNF)
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
    * the case if its operands are literals or if they are conjunctions of literals.
    */
  override lazy val isDNF: Boolean = {
    if (e1.isLiteral && e2.isLiteral) true
    else {
      (e1, e2) match {
        case (_: Or, _) | (_, _: Or) => false
        case (andExpr1: And, andExpr2: And) => andExpr1.isClause && andExpr2.isClause
        case (andExpr: And, expr: BooleanExpression) if expr.isLiteral => andExpr.isClause
        case (expr: BooleanExpression, andExpr: And) if expr.isLiteral => andExpr.isClause
      }
    }
  }

  override lazy val isClause: Boolean = {
    (e1, e2) match {
      case (x: And, y: And) => x.isClause && y.isClause
      case (x: And, y) => x.isClause && y.isLiteral
      case (x, y: And) => y.isClause && x.isLiteral
      case (x, y) => x.isLiteral && y.isLiteral
    }
  }

  override lazy val negate: BooleanExpression = Or(e1.negate, e2.negate)

  override lazy val NNFtoDNF: BooleanExpression = {
    if (this.isClause) this
    else {
      (e1, e2) match {
        case (Or(x, y), z) => Or(And(x, z).NNFtoDNF, And(y, z).NNFtoDNF)
        case (x, Or(y, z)) => Or(And(x, y).NNFtoDNF, And(x, z).NNFtoDNF)
        case (And(_, _), y) if y.isLiteral => And(e1.NNFtoDNF, y).NNFtoDNF
        case (x, And(_, _)) if x.isLiteral => And(x, e2.NNFtoDNF).NNFtoDNF
        case (andExpr1: And, andExpr2: And) if andExpr1.isClause && andExpr2.isClause => this
        case (andExpr1: And, andExpr2: And) =>
          And(andExpr1.NNFtoDNF, andExpr2.NNFtoDNF).NNFtoDNF
        case _ => And(e1.NNFtoDNF, e2.NNFtoDNF).NNFtoDNF
      }
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
    case (expr1, expr2) if expr1.negate.isEquivalentTo(expr2) => False

    case (expr1, expr2) => And(expr1, expr2)
  }

  override lazy val toNNF: BooleanExpression = And(e1.toNNF, e2.toNNF)
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
    * Deserialize a `JValue` (representing some JSON segment).
    * @param jv the `JValue` to serialize
    * @return A [[scala.Some Some]] containing the deserialized [[boolexps.BooleanExpression BooleanExpression]], or a
    *         [[scala.None None]] if the JSON was malformed.
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

  /**
    * Deserialize a JSON [[java.lang.String String]] to its [[boolexps.BooleanExpression BooleanExpression]]
    * representation.
    * @param s
    * @return
    */
  def deserialize(s: String): Option[BooleanExpression] = {
    try {
      deserialize(parse(s))
    } catch {
      case _: JsonParser.ParseException => None
    }
  }

  /**
    * Given a [[scala.collection.immutable.Set Set]] of variable names, generates all possible logical interpretations (an assignment of `true` or
    * `false` to each variable) for the given variables.
    *
    * @param variables a [[scala.collection.immutable.Set Set]] of [[java.lang.String Strings]] corresponding to
    *                  variable names
    * @return a comprehensive [[scala.collection.immutable.List List]] of all possible interpretations, each represented
    *         as a [[scala.collection.immutable.Map Map]] whose keys are variable names and whose values are
    *         the corresponding `true`/`false` assignments.
    */
  private def makeInterpretations(variables: Set[String]): Set[Map[String, Boolean]] = {
    variables.foldLeft(Set[Map[String, Boolean]]()) {
      (acc: Set[Map[String, Boolean]], variable: String) => {
        if (acc.isEmpty) Set(Map(variable -> true), Map(variable -> false))
        else acc.map(_ + (variable -> true)) ++ acc.map(_ + (variable -> false))
      }
    }
  }
}