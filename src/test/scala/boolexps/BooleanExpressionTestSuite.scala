package boolexps

import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.Matchers._

class BooleanExpressionTestSuite extends FlatSpec with GeneratorDrivenPropertyChecks {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfig(minSize = 1, minSuccessful = 10000, maxDiscarded = 5000)

  lazy val genBool: Gen[BooleanExpression] = Gen.oneOf(Gen.const(True), Gen.const(False))

  lazy val genVar: Gen[Variable] = Gen.nonEmptyListOf(Gen.alphaChar).map(_.mkString).suchThat { s =>
    val sUpper = s.toUpperCase
    sUpper.nonEmpty && sUpper != "NOT" && sUpper != "OR" && sUpper != "AND"
  }.map(Variable)

  lazy val genNot: Gen[Not] = boolExprGen.map(Not)

  lazy val genOr: Gen[Or] =
    for (e1 <- boolExprGen; e2 <- boolExprGen)
      yield Or(e1, e2)

  lazy val genAnd: Gen[And] =
    for (e1 <- boolExprGen; e2 <- boolExprGen)
        yield And(e1, e2)

  lazy val operatorGen: Gen[BooleanExpression] = Gen.oneOf(genNot, genOr, genAnd)

  def boolExprGen: Gen[BooleanExpression] = Gen.sized { size =>
    if (size <= 0) Gen.oneOf(genVar, genBool)
    else Gen.resize(size/2,
      Gen.frequency(
        75 -> operatorGen,
        20 -> genVar,
        5 -> genBool)
    )
  }

  "True" should "serialize to [true]" in {
    assertResult("[true]")(True.toJSON)
  }

  "False" should "serialize to [false]" in {
    assertResult("[false]")(False.toJSON)
  }

  "Serializing a BooleanExpression and then deserializing it" should "produce the original BooleanExpression" in {
    forAll (boolExprGen) {
      b: BooleanExpression => assertResult(b)(BooleanExpression.deserialize(b.toJSON).head)
    }
  }

  "Deserializing a JSON String and then serializing it" should "produce the original JSON String" in {
  }

  "Malformed JSON" should "cause the serializer to fail gracefully" in {

  }

  "Not Not" should "cancel out" in {

  }

  "Reordering arguments of And" should "yield an equivalent BooleanExpression" in {

  }

  "Reordering arguments of Or" should "yield an equivalent BooleanExpression" in {

  }

  def strip(s: String): String = s.replaceAll("""\s+""", "")
}