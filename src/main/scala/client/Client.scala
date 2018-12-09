package client

import java.io.FileNotFoundException
import java.net.ConnectException

import scala.io._
import com.softwaremill.sttp._

case class TransformationRequest(json: String, endpoint: String) {
  implicit private val backend: SttpBackend[Id, Nothing] = HttpURLConnectionBackend()
  private val SERVER_URI = "http://localhost:8080/"

  private lazy val request = sttp
    .post(uri"$SERVER_URI$endpoint")
    .body(s"$json")
    .header("Content-Type", "application/json")

  private lazy val response = request.send()

  lazy val result: Either[String, String] = response.body
}

object Client extends App {

  lazy val endpointsForChoice = Map("D" -> "DNF", "N" -> "NNF", "S" -> "simplify")

  def readJSONFromFile(filePath: String): Either[String, String] = {
    try {
      val bufferedSource = Source.fromFile(filePath)
      val s = bufferedSource.mkString
      bufferedSource.close
      Right(s)
    } catch {
      case _: FileNotFoundException => Left("File not found!")
      case _: Exception => Left("Something went wrong reading that file.")
    }
  }

  lazy val getJSONInput: () => Either[String, String] = () => {
    val filePath: String = StdIn.readLine("Enter the path of a file containing your JSON input (relative path from " +
      "the current directory or an absolute path): ")
    readJSONFromFile(filePath)
  }

  lazy val getTransformationChoice: () => Either[String, String] = {
    () => {
      println("Here are the transformations you can choose from: ")
      println("\t\t[N]egation Normal Form")
      println("\t\t[D]isjunctive Normal Form")
      println("\t\t[S]implify")
      val choice: String = StdIn.readLine("Make a choice (enter the bracketed letter, case-insensitive): ")

      // check if choice is amongst possible choices
      if (endpointsForChoice.keys.forall(!choice.equalsIgnoreCase(_))) Left("Invalid choice")
      else Right(choice)
    }
  }

  lazy val getEndpointChoice: () => Either[String, String] = {
    () => getTransformationChoice() map { s => endpointsForChoice(s.toUpperCase) }
  }

  lazy val askToContinue: () => Boolean = {
    () => {
      val continue = StdIn.readLine("Would you like to continue ([y]es/[n]o)?").toLowerCase
      if (continue.startsWith("y")) true // any string starting with y or Y is treated as yes
      else if (continue.startsWith("n")) false // any string starting with n or N is treated as no
      else {
        println("Invalid choice!")
        askToContinue()
      }
    }
  }

  /** A function which takes a boolean function to determine whether or not to continue, which then executes the main
    * loop of the program, calling itself continually until the boolean function returns false.
    */
  lazy val inputLoopBody: (() => Boolean) => (() => Any) = {
    continueFunc: (() => Boolean) => {
      if (continueFunc()) {
        getJSONInput() match {
          case Left(errMsg) => () => {
            println(errMsg)
            inputLoopBody(askToContinue)()
          }
          case Right(json) => () => {
            getEndpointChoice() match {
              case Left(errMsg) => () => {
                println(errMsg)
                inputLoopBody(askToContinue)()
              }
              case Right(endpoint) => {
                println(TransformationRequest(json, endpoint).result.merge)
                inputLoopBody(askToContinue)()
              }
            }
          }
        }
      }
      else () => println("Thank you for using our service!")
    }
  }

  try {
    println("Welcome to the Boolean Expressions Transformation Service. Here you can submit boolean expressions " +
      "formatted as JSON for transformation to normal forms or for simplification.")
    inputLoopBody(() => true)()
  }
  catch {
    case _: ConnectException => println("Sorry, could not connect to the server. Please make sure the server is " +
      "running at the right port. Quitting program.")
    case _: Throwable => {
      println("Sorry, something went wrong. Quitting program.")
    }
  }
}
