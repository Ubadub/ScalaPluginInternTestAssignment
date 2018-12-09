# Scala Plugin Intern Test Assignment
Abhinav Patil

Submission for the test assignment for summer 2019 Scala plugin internship candidates.

## Table of Contents
[Project Choice](#project-choice)

[Warmup](#warmup)

[BooleanExpression Project](#booleanexpression-project)

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

I had a great deal of fun with this project, so thank you for assigning it. My implementation meets the basic requirements of being able to serialize/deserialize `BooleanExpressions` to and from JSON, and it also provides functionality for algebraic transformations: converting a `BooleanExpression` to negation normal form, to disjunctive normal form, or just simplifying per the laws of boolean algebra. Finally, I also created a server that offers the transformations as a service and a client that a user can use to communicate with the server (details of how to run the server and use the client are in the next section). All of this functionality is extensively tested.

## Algebraic Transformation Server
