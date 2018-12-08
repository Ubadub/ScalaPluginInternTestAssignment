package boolexps

import org.json4s._
import org.json4s.jackson.JsonMethods._

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Shrink

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
    assertResult(JBool(true))(True.toJson)
  }

  "False" should "serialize to false" in {
    assertResult(JBool(false))(False.toJson)
  }

  "Serializing a BooleanExpression and then deserializing it" should "produce the original BooleanExpression" in {
    forAll (genBoolExpr) {
      b: BooleanExpression => {
        val serialized: JValue = b.toJson
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
          // workaround for a quirk in the json4s parser that treats string literals by themselves as invalid JSON
          val deserialized: Option[BooleanExpression] = BooleanExpression.deserialize(boolJSON)
          deserialized should not be empty
          val reserialized = deserialized.head.toJson
          compact(render(reserialized)) shouldBe strip(boolJSON)
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

  // Jackson treats unbalanced quote marks in an idiosyncratic/error-tolerant way, so this test is commented out for now
  /*
  "JSON with unbalanced quote marks" should "cause the deserializer to fail gracefully" in {
    forAll(genUnbalancedQuotesJSON) {
      boolJSON: String => {
        whenever(boolJSON.nonEmpty) {
          val deserialized: Option[BooleanExpression] = BooleanExpression.deserialize(boolJSON)
          deserialized shouldBe empty
        }
      }
    }
  }*/
}