package boolexps

import org.scalacheck.Gen
import boolexps.TestUtils._

import scala.collection.immutable.NumericRange

/**
  * A collection of generators that produce BooleanExpressions or JSON representations of them (including malformed
  * ones), as well as relevant utility functions. Useful for testing.
  */
object TestDataGenerators {
  val lowercaseChars: NumericRange.Inclusive[Char] = 'a' to 'z'

  lazy val genTorFStr: Gen[String] = Gen.oneOf(Gen.const("true"), Gen.const("false"))
  lazy val genTorFExpr: Gen[BooleanExpression] = Gen.oneOf(Gen.const(True), Gen.const(False))

  def genFiniteVarList(n: Int): Gen[Seq[Variable]] = Gen.pick(n, lowercaseChars).map(_.map(c => Variable(c.toString)))

  // unfortunately the various String generating functions in Gen produce too many discards
  lazy val genVarName: Gen[String] = Gen.nonEmptyListOf(Gen.alphaChar).map(_.mkString).suchThat { s =>
    val sUpper = s.toUpperCase
    sUpper != "NOT" && sUpper != "OR" && sUpper != "AND"
  }

  lazy val genVarStr: Gen[String] = genVarName.map(addQuotes)
  lazy val genVarExpr: Gen[Variable] = genVarName.map(Variable)
  def genVarExprFrom(s: Seq[Variable]): Gen[Variable] = Gen.oneOf(s)

  lazy val genNotStr: Gen[String] = genBoolStr.map(expr => s"${addQuotes("NOT")}, $expr")
  lazy val genNotExpr: Gen[Not] = genBoolExpr.map(Not)

  lazy val genOrStr: Gen[String] = for (e1 <- genBoolStr; e2 <- genBoolStr) yield s"${addQuotes("OR")}, $e1, $e2"
  lazy val genOrExpr: Gen[Or] =
    for (e1 <- genBoolExpr; e2 <- genBoolExpr)
      yield Or(e1, e2)

  lazy val genAndStr: Gen[String] = {
    for (e1 <- genBoolStr; e2 <- genBoolStr) yield s"${addQuotes("AND")}, $e1, $e2"
  }
  lazy val genAndExpr: Gen[And] =
    for (e1 <- genBoolExpr; e2 <- genBoolExpr)
      yield And(e1, e2)

  lazy val genOperatorStr: Gen[String] = Gen.oneOf(genNotStr, genOrStr, genAndStr).map(s => s"[$s]")
  lazy val genOperatorExpr: Gen[BooleanExpression] = Gen.oneOf(genNotExpr, genOrExpr, genAndExpr)

  // The next three generators use Gen.sized to prevent the abstract syntax tree of the BooleanExpression from being
  // unreasonably deep (which will lead to StackOverflowErrors)
  lazy val genBoolStr: Gen[String] = Gen.sized { size =>
    if (size <= 0) Gen.oneOf(genVarStr, genTorFStr)
    else Gen.resize(size/2,
      Gen.frequency(
        75 -> genOperatorStr,
        20 -> genVarStr,
        5 -> genTorFStr)
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

  def genBoolExprWithFiniteVariableList(n: Int): Gen[BooleanExpression] = genFiniteVarList(n).flatMap {
    vars: Seq[Variable] =>
      Gen.sized { size =>
        if (size <= 0) Gen.oneOf(genVarExprFrom(vars), genTorFExpr)
        else Gen.resize(size / 2,
          Gen.frequency(
            75 -> genOperatorExpr,
            20 -> genVarExprFrom(vars),
            5 -> genTorFExpr)
        )
      }
  }

  lazy val genSimpleBoolExpr: Gen[BooleanExpression] = genBoolExprWithFiniteVariableList(4)

  lazy val genUnbalancedBracesJSON: Gen[String] = ???

  lazy val genUnbalancedBracketsJSON: Gen[String] = ???

  lazy val genUnclosedStringLiteralJSON: Gen[String] = ???

  lazy val genMalformedJSON: Gen[String] =
    Gen.oneOf(genUnbalancedBracesJSON, genUnbalancedBracketsJSON, genUnclosedStringLiteralJSON)

  def main(args: Array[String]): Unit = {
    for (i <- 1 to 1000) println(genSimpleBoolExpr.sample)
  }
}
