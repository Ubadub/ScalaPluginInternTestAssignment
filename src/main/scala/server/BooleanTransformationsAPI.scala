package server

import org.json4s._
import org.scalatra.ScalatraServlet
import org.scalatra.json._

import boolexps._
import BooleanExpression._

class BooleanTransformationsAPI extends ScalatraServlet with JacksonJsonSupport {
  protected implicit lazy val jsonFormats: Formats = DefaultFormats

  /** Takes a JSON representation of a BooleanExpression, applies the given transformation to it, and returns the result
    * as JSON.
    *
    * @param input JSON representation of a BooleanExpression to process
    * @param transform the transformation function to apply
    * @return a JSON representation of the result of the transformation if the JSON was well-formed, or a JSON string
    *         indicating an error if the JSON was malformed
    */
  private def transformAndSerialize(input: JValue, transform: BooleanExpression => BooleanExpression): JValue = {
    deserialize(input).map(transform(_).toJson).getOrElse(JString("Malformed JSON"))
  }

  before() {
    contentType = formats("json")
  }

  // disjunctive normal form
  post("/DNF") {
    transformAndSerialize(parsedBody, _.toDNF)
  }

  // negation normal form
  post("/NNF") {
    transformAndSerialize(parsedBody, _.toNNF)
  }

  // generic simplification
  post("/simplify") {
    transformAndSerialize(parsedBody, _.simplify)
  }

}
