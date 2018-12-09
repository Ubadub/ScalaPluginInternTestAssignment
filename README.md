# Scala Plugin Intern Test Assignment
Abhinav Patil

Submission for the test assignment for summer 2019 Scala plugin internship candidates.

## Table of Contents
[Project Choice](#project-choice)

[Warmup](#warmup)

[BooleanExpression Project](#booleanexpression-project)

[Algebraic Transformation Server](#algebraic-transformation-server)

[Algebraic Transformation Client](#algebraic-transformation-client)

## Project Choice

Of the suggested projects, two are especially interesting to me: giter8 templates integration and Build Server Protocol implementations.

### Giter8

I have a good working knowledge of git and I have a fair amount of experience writing REST APIs. I've used giter8 before (for this very project, in fact) so I'm a fan of how useful it is, especially for beginners to a language, tool, or framework, who would want to avoid the hassle and overhead of lots of boilerplate. I also generally enjoy working with REST services. The stretch goal seems particularly intriguing because of how much more seamless it would be to use giter8 with Intellij if such a tool existed. I would love to bring it to fruition.

### Build Server Protocol

The concept of a unified protocol that all build tools and language editors could use to communicate is an idea that I find highly compelling. I am familiar with how successful and useful the Language Server Protocol (BSP's inspiration) was, and I could see the BSP enjoying similar success in the Scala community. Technically, it seems like a project I would have a great deal of fun with: it has a concrete goal (the protocol itself) but permits a fair amount of latitude to the developer when it comes to questions of "how." I've worked primarily with the Gradle and SBT build tools in the past but I have more experience with the latter would want to work on a BSP implementation for SBT.

## Warmup

The code for the warmup exercise is located in `/src/main/scala/warmup/Warmup.scala`.

The first function I wrote, `f(x: Int): Int` has big-O complexity of O(x), as at each call of the function, x is only decremented by one, thus for all non-negative integers x, the function is guaranteed to run x+1 times, which corresponds to an asymptotic time complexity of O(x).

If we want to mantain some level of structural fidelity to the form of the given recurrence relation, we cannot improve the asymptotic runtime of this function. However, we can produce an equivalent function by noticing that the recurrence relation is equivalent to the closed-form expression f(x)=2^x for non-negative x. Several integer exponentiation algorithms can do much better than O(x). For example, exponentiation by squaring would have O(log(x)) runtime complexity. I implemented this as the function `ff(x: Int): Int`.

We can do even better. The third function I wrote, `fff(x: Int): Int`, takes advantage of the fact that calculating powers of two is easy with bitshift operations: `1 << x`, where x is our exponent as before. This is effectively a O(1) operation.

## `BooleanExpression` Project

The source code for this is located in `/src/main/scala/boolexps/`. Tests for serialization and deserialization are located in `/src/test/scala/boolexps/BooleanExpressionSerializationTestSuite.scala`, and tests for the functionality of the `BooleanExpression` class itself, including the algebraic transformation functions, are in `/src/test/scala/boolexps/BooleanExpressionSpec.scala`.

I had a great deal of fun with this project, so thank you for assigning it. My implementation meets the basic requirements of being able to serialize/deserialize `BooleanExpressions` to and from JSON, and it also provides functionality for algebraic transformations: converting a `BooleanExpression` to negation normal form, to disjunctive normal form, or just simplifying per the laws of boolean algebra. Finally, I also created a server that offers the transformations as a service and a client that a user can use to communicate with the server (details of how to run the server and use the client are in the next sections). The serialization/deserialization and algebraic transformation functionality is extensively tested.

The grammar of the JSON is explained in the Scaladoc comment for the `BooleanExpression` trait in `BooleanExpression.scala`.

## Algebraic Transformation Server

The code for the server is primarily in `/src/main/scala/server/`. The easiest way to run the server is to enter a SBT shell in the root directory, type `jetty:start` and hit enter. The server listens at port 8080. It has the following API endpoints, all of which only accept POST requests whose payload is JSON representing a boolean expression:

`/DNF` - returns the Boolean expression converted to disjunctive normal form

`/NNF` - returns the Boolean expression converted to negation normal form

`/simplify` - returns a simplified version of the Boolean expression

Sending malformed JSON to any endpoint will cause the program to fail gracefully: the server will simply respond with an error message and keep listening at the same port.

## Algebraic Transformation Client
The code for the client is in `/src/main/scala/client/Client.scala`. The client is a command line program that elicits the location of a JSON file and the desired transformation operation from the user, queries the server, and returns the result.

The easiest way to run the client is from inside Intellij: just run the main method of the Client.scala file. Equivalently, you can execute `sbt compile run` at the terminal. Alternatively, you can use the plugin sbt-assembly to compile and package a standalone JAR file (as explained [here](https://alvinalexander.com/scala/sbt-how-build-single-executable-jar-file-assembly)), which can then be executed. Obviously, the server must be listening for the client to work; otherwise, it will display an error message and quit.
