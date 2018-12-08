package server

import org.json4s.JsonAST.JString
import org.json4s.{DefaultFormats, Formats}
import org.scalatra.ScalatraServlet
import org.scalatra.json._

import boolexps._

class BooleanAlgebraAPI extends ScalatraServlet with JacksonJsonSupport {
  protected implicit lazy val jsonFormats: Formats = DefaultFormats

  before() {
    contentType = formats("json")
  }

  // TODO: have this return a list of endpoints
  get("/") {
  }

  post("/DNF") {
    BooleanExpression.deserialize(parsedBody).map(_.toDNF.toJson).getOrElse(JString("Malformed JSON"))
  }

  post("/NNF") {
    BooleanExpression.deserialize(parsedBody).map(_.toNNF.toJson).getOrElse(JString("Malformed JSON"))
  }

  post("/simplify") {
    BooleanExpression.deserialize(parsedBody).map(_.simplify.toJson).getOrElse(JString("Malformed JSON"))
  }

}
