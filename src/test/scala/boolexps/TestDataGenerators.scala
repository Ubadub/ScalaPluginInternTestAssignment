package boolexps

import org.scalacheck.Gen

import boolexps.TestUtils._

/**
  * A collection of generators that produce BooleanExpressions or JSON representations of them (including malformed
  * ones), as well as relevant utility functions. Useful for testing.
  *
  * Contains two general classes of generators: JSON generators (variable names end in `JSON`) and `BooleanExpression`
  * generators (variable names end in `Expr`.
  */
object TestDataGenerators {

  lazy val genTorFJSON: Gen[String] = Gen.oneOf(Gen.const("true"), Gen.const("false"))
  lazy val genTorFExpr: Gen[BooleanExpression] = Gen.oneOf(Gen.const(True), Gen.const(False))

  // unfortunately the various String generating functions in Gen produce too many discards
  lazy val genVarName: Gen[String] = Gen.nonEmptyListOf(Gen.alphaChar).map(_.mkString).suchThat { s =>
    val sUpper = s.toUpperCase
    sUpper != "NOT" && sUpper != "OR" && sUpper != "AND"
  }
  lazy val genVarJSON: Gen[String] = genVarName.map(addQuotes)
  lazy val genVarExpr: Gen[Variable] = genVarName.map(Variable)
  def genVarList(n: Int): Gen[Seq[Variable]] = Gen.listOfN(n, Gen.alphaLowerChar).map(_.map(c => Variable(c.toString)))

  lazy val genNotJSON: Gen[String] = genBoolJSON.map(expr => s"${addQuotes("NOT")}, $expr")
  lazy val genNotExpr: Gen[Not] = genBoolExpr.map(Not)
  def genNotExpr(vars: Gen[Variable]): Gen[Not] = genBoolExpr(vars).map(Not)

  lazy val genOrJSON: Gen[String] = for (e1 <- genBoolJSON; e2 <- genBoolJSON) yield s"${addQuotes("OR")}, $e1, $e2"

  lazy val genOrExpr: Gen[Or] =
    for (e1 <- genBoolExpr; e2 <- genBoolExpr)
      yield Or(e1, e2)

  def genOrExpr(vars: Gen[Variable]): Gen[Or] =
    for (e1 <- genBoolExpr(vars); e2 <- genBoolExpr(vars))
      yield Or(e1, e2)

  lazy val genAndJSON: Gen[String] = for (e1 <- genBoolJSON; e2 <- genBoolJSON) yield s"${addQuotes("AND")}, $e1, $e2"

  lazy val genAndExpr: Gen[And] = for (e1 <- genBoolExpr; e2 <- genBoolExpr) yield And(e1, e2)

  def genAndExpr(vars: Gen[Variable]): Gen[And] =
    for (e1 <- genBoolExpr(vars); e2 <- genBoolExpr(vars))
      yield And(e1, e2)

  lazy val genOperatorJSON: Gen[String] = Gen.oneOf(genNotJSON, genOrJSON, genAndJSON).map(s => s"[$s]")
  lazy val genOperatorExpr: Gen[BooleanExpression] = Gen.oneOf(genNotExpr, genOrExpr, genAndExpr)

  def genOperatorExpr(vars: Gen[Variable]): Gen[BooleanExpression] =
    Gen.oneOf(genNotExpr(vars), genOrExpr(vars), genAndExpr(vars))

  // The next three generators use Gen.sized to prevent the binary expression tree of the BooleanExpression from being
  // unreasonably deep (which will lead to StackOverflowErrors)
  lazy val genBoolJSON: Gen[String] = Gen.sized { size =>
    if (size <= 0) Gen.oneOf(genVarJSON, genTorFJSON)
    else Gen.resize(size/2,
      Gen.frequency(
        75 -> genOperatorJSON,
        20 -> genVarJSON,
        5 -> genTorFJSON)
    )
  }

  lazy val genBoolExpr: Gen[BooleanExpression] = Gen.sized { size =>
    if (size <= 0) Gen.oneOf(genVarExpr, genTorFExpr)
    else Gen.resize(size/2,
      Gen.frequency(
        75 -> genOperatorExpr,
        20 -> genVarExpr,
        5 -> genTorFExpr)
    )
  }

  def genBoolExpr(n: Int): Gen[BooleanExpression] = genBoolExpr(genVarFromListOfSize(n))

  def genVarFromListOfSize(n: Int): Gen[Variable] = genVarList(n).flatMap(Gen.oneOf(_))

  def genBoolExpr(vars: Gen[Variable]): Gen[BooleanExpression] =
    Gen.sized { size =>
      if (size <= 0) Gen.oneOf(vars, genTorFExpr)
      else Gen.resize(size / 2,
        Gen.frequency(
          75 -> genOperatorExpr(vars),
          20 -> vars,
          5 -> genTorFExpr)
      )
    }

  /**
    * Generate a boolean expression that uses at most three unique variables. Used for testing algorithms which are
    * NP-complete and thus take too long on `BooleanExpression`s with a large number of variables.
    *
    * @return a `Gen` which generates `BooleanExpression`s that use at most three variables
    */
  def genThreeVarBoolExpr: Gen[BooleanExpression] = genBoolExpr(Gen.oneOf(genVarList(3).sample.get))

  lazy val genUnbalancedBracesJSON: Gen[String] = {
    for (boolJSON: String <- genBoolJSON;
         brace: Char <- Gen.oneOf(Gen.const('{'), Gen.const('}'))
    ) yield {
      // boolJSON.substring(0, index) + brace + boolJSON.substring(index + 1)
      brace + boolJSON
    }
  }

  lazy val genUnbalancedBracketsJSON: Gen[String] = {
    for (boolJSON: String <- genBoolJSON;
         bracket: Char <- Gen.oneOf(Gen.const('['), Gen.const(']'))
    ) yield {
      bracket + boolJSON
    }
  }

  /**
    * Finds all indices of a `Char` in a `String`
    * @param c the `Char` to search for
    * @param s the `String` to search
    * @return a List of integers corresponding to the indices of the `Char` in the `String`
    */
  def allIndicesOf(c: Char, s: String): List[Int] = s.foldLeft((List[Int](), 0)) {
    (acc, cc: Char) => {
      acc match {
        case (indices: List[Int], idx: Int) => {
          if (c == cc) (idx :: indices, idx + 1)
          else (indices, idx + 1)
        }
      }
    }
  }._1
}
