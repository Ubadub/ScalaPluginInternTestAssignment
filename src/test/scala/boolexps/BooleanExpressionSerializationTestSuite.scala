package boolexps

import org.scalacheck.Shrink
import org.scalatest.FlatSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.Matchers._
import TestDataGenerators._
import TestUtils._

/**
  * A testing suite for the functionality of the serializer/deserializer.
  */
class BooleanExpressionSerializationTestSuite extends FlatSpec with GeneratorDrivenPropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 10000, maxDiscardedFactor = 20.0, minSize = 1)

  // suppress the shrinking of Strings because it obscures the actual test which failed
  // obviously, this is only ever relevant if the test was gonna fail anyways
  implicit def suppressShrink[String](implicit s1: Shrink[String]): Shrink[String] =
    Shrink((s: String) => Stream.empty)

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
        deserialized.get shouldBe b
      }
    }
  }

  "Deserializing a JSON String and then serializing it" should "produce the original JSON String" in {
    forAll(genBoolJSON) {
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

  "JSON with unbalanced braces" should "cause the deserializer to fail gracefully" in {
    forAll(genUnbalancedBracesJSON) {
      boolJSON: String => {
        whenever(boolJSON.nonEmpty) {
          val deserialized: Option[BooleanExpression] = BooleanExpression.deserialize(boolJSON)
          deserialized shouldBe empty
        }
      }
    }
  }

  "JSON with unbalanced brackets" should "cause the deserializer to fail gracefully" in {
    forAll(genUnbalancedBracketsJSON) {
      boolJSON: String => {
        whenever(boolJSON.nonEmpty) {
          val deserialized: Option[BooleanExpression] = BooleanExpression.deserialize(boolJSON)
          deserialized shouldBe empty
        }
      }
    }
  }

  "JSON with unbalanced quote marks" should "cause the deserializer to fail gracefully" in {
    forAll(genUnbalancedQuotesJSON) {
      boolJSON: String => {
        whenever(boolJSON.nonEmpty) {
          val deserialized: Option[BooleanExpression] = BooleanExpression.deserialize(boolJSON)
          deserialized shouldBe empty
        }
      }
    }
  }
}