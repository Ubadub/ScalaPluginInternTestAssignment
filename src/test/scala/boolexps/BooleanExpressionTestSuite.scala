package boolexps

import org.scalacheck.{Gen, Shrink}
import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.Matchers._

import TestUtils._

class BooleanExpressionTestSuite extends FlatSpec with GeneratorDrivenPropertyChecks {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(minSize = 1, minSuccessful = 10000, maxDiscarded = 50000000)

  def makeOperatorExpr(operator: String)(operands: List[Gen[String]]): Gen[String] = ???

  lazy val genTorFStr: Gen[String] = Gen.oneOf(Gen.const("true"), Gen.const("false"))
  lazy val genTorFExpr: Gen[BooleanExpression] = Gen.oneOf(Gen.const(True), Gen.const(False))

  // unfortunately the various String generating functions in Gen produce too many discards
  lazy val genVarName: Gen[String] = Gen.nonEmptyListOf(Gen.alphaChar).map(_.mkString).suchThat { s =>
    val sUpper = s.toUpperCase
    sUpper != "NOT" && sUpper != "OR" && sUpper != "AND"
  }

  lazy val genVarStr: Gen[String] = genVarName.map(addQuotes)
  lazy val genVarExpr: Gen[Variable] = genVarName.map(Variable)

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

  val genBoolStr: Gen[String] = Gen.sized { size =>
    if (size <= 0) Gen.oneOf(genVarStr, genTorFStr)
    else Gen.resize(size/2,
      Gen.frequency(
        75 -> genOperatorStr,
        20 -> genVarStr,
        5 -> genTorFStr)
    )
  }

  val genBoolExpr: Gen[BooleanExpression] = Gen.sized { size =>
    if (size <= 0) Gen.oneOf(genVarExpr, genTorFExpr)
    else Gen.resize(size/2,
      Gen.frequency(
        75 -> genOperatorExpr,
        20 -> genVarExpr,
        5 -> genTorFExpr)
    )
  }

  "True" should "serialize to true" in {
    assertResult("true")(True.toJSON)
  }

  "False" should "serialize to false" in {
    assertResult("false")(False.toJSON)
  }

  "Serializing a BooleanExpression and then deserializing it" should "produce the original BooleanExpression" in {
    forAll (genBoolExpr) {
      b: BooleanExpression => {
        val serialized: String = b.toJSON
        val deserialized: Option[BooleanExpression] = BooleanExpression.deserialize(serialized)
        deserialized should not be empty
        deserialized.head shouldBe b
      }
    }
  }

  "Deserializing a JSON String and then serializing it" should "produce the original JSON String" in {
    // suppress the shrinking of Strings because it obscures the actual test which failed
    // obviously, this is only ever relevant if the test was gonna fail anyways
    implicit def suppressShrink[String](implicit s1: Shrink[String]): Shrink[String] = Shrink((s: String) => Stream.empty)

    forAll(genBoolStr) {
      boolJSON: String => {
        whenever(boolJSON.nonEmpty) {
          val deserialized: Option[BooleanExpression] = BooleanExpression.deserialize(boolJSON)
          deserialized should not be empty
          val reserialized = deserialized.head.toJSON
          strip(reserialized) shouldBe strip(boolJSON)
        }
      }
    }
  }

  "Malformed JSON" should "cause the serializer to fail gracefully" in {

  }

  "Not Not" should "cancel out" in {

  }

  "Reordering arguments of And" should "yield an equivalent BooleanExpression" in {

  }

  "Reordering arguments of Or" should "yield an equivalent BooleanExpression" in {

  }
}