package boolexps

import org.scalatest.{FunSpec, GivenWhenThen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import TestDataGenerators._

/**
  * This class specifically tests that the `BooleanExpression` class is correctly governed by the rules of boolean
  * algebra and that it can be correctly converted to simplified or normal forms.
  */
class BooleanExpressionSpec extends FunSpec with GeneratorDrivenPropertyChecks with GivenWhenThen {

  describe("OR") {
    it("should be associative") {
      forAll(genBoolExpr, genBoolExpr, genBoolExpr) {
        Given("three BooleanExpressions")
        (expr1, expr2, expr3) => {
          Then("the order in which these expressions are OR'ed together should not matter provided their " +
              "linear sequence is preserved")
          assert(Or(expr1, Or(expr2, expr3)).isEquivalentTo(Or(Or(expr1, expr2), expr3)))
        }
      }
    }

    it("should be commutative"){
      Given("two BooleanExpressions")
      forAll(genBoolExpr, genBoolExpr) {
        (expr1, expr2) => {
          Then("the order in which these expressions are OR'ed together should not matter")
          assert(Or(expr1, expr2).isEquivalentTo(Or(expr2, expr1)))
        }
      }
    }

    it("should have false as its identity element") {
      forAll(genBoolExpr) {
        Given("a BooleanExpression expr")
        expr => {
          Then("expr OR false should be equivalent to expr")
          val orExpr1: Or = Or(expr, False)
          assert(orExpr1.isEquivalentTo(expr))

          And("vice versa")
          assert(expr.isEquivalentTo(orExpr1))

          And("false OR expr should be equivalent to expr")
          val orExpr2: Or = Or(False, expr)
          assert(orExpr2.isEquivalentTo(expr))

          And("vice versa")
          assert(expr.isEquivalentTo(orExpr2))
        }
      }
    }

    it("should have true as its annihilator") {
      forAll(genBoolExpr) {
        Given("a BooleanExpression E")
        expr => {
          Then("E OR true should be equivalent to true")
          val orExpr1: Or = Or(expr, True)
          assert(orExpr1.isEquivalentTo(True))

          And("vice versa")
          assert(True.isEquivalentTo(orExpr1))

          And("true OR E should be equivalent to true")
          val orExpr2: Or = Or(True, expr)
          assert(orExpr2.isEquivalentTo(True))

          And("vice versa")
          assert(True.isEquivalentTo(orExpr2))
        }
      }
    }

    it("should be idempotent") {
      forAll(genBoolExpr) {
        Given("a BooleanExpression E")
        expr => {
          Then("E OR E should be equivalent to E")
          assert(Or(expr, expr).isEquivalentTo(expr))

          And("vice versa")
          assert(expr.isEquivalentTo(Or(expr, expr)))
        }
      }
    }

    it("should absorb AND") {
      Given("a BooleanExpression E and a BooleanExpression F")
      forAll(genBoolExpr, genBoolExpr) {
        (expr1, expr2) => {
          Then("E OR (E AND F) should be equivalent to E")
          val orExpr1: Or = Or(expr1,  boolexps.And(expr1, expr2))
          assert(orExpr1.isEquivalentTo(expr1))

          And("vice versa")
          assert(expr1.isEquivalentTo(orExpr1))

          And("(E AND F) OR E should be equivalent to E")
          val orExpr2: Or = Or(boolexps.And(expr1, expr2), expr1)
          assert(orExpr2.isEquivalentTo(expr1))

          And("vice versa")
          assert(expr1.isEquivalentTo(orExpr2))
        }
      }
    }

    it("should distribute over AND") {
      Given("three BooleanExpressions E, F, G")
      forAll(genBoolExpr, genBoolExpr, genBoolExpr) {
        (expr1, expr2, expr3) => {
          Then("E OR (F AND G) should be equivalent to ((E OR F) AND (E OR G))")
          val orExpr1: Or = Or(expr1,  boolexps.And(expr2, expr3))
          val orExpr2: Or = Or(expr1,  expr2)
          val orExpr3: Or = Or(expr1,  expr3)
          val andExpr1: boolexps.And = boolexps.And(orExpr2, orExpr3)
          assert(orExpr1.isEquivalentTo(andExpr1))

          And("vice versa")
          assert(andExpr1.isEquivalentTo(orExpr1))

          And("(F AND G) OR E should be equivalent to ((E OR F) AND (E OR G))")
          val orExpr4: Or = Or(boolexps.And(expr2, expr3), expr1)
          assert(orExpr4.isEquivalentTo(andExpr1))

          And("vice versa")
          assert(andExpr1.isEquivalentTo(orExpr4))
        }
      }
    }

    it("should be true when its operands are inverses of each other") {
      Given("a BooleanExpression E")
      forAll(genBoolExpr) {
        e => {
          Then("E OR (NOT E) should always evaluate to true")
          assert(Or(e, Not(e)).isEquivalentTo(True))
        }
      }
    }

    it("should obey DeMorgan's Law") {
      Given("two BooleanExpressions P and Q")
      forAll(genBoolExpr, genBoolExpr) {
        (expr1, expr2) => {
          Then("NOT(P OR Q) should be equivalent to NOT(P) AND NOT(Q)")
          val notExpr = Not(Or(expr1, expr2))
          val andExpr = boolexps.And(Not(expr1), Not(expr2))
          assert(notExpr.isEquivalentTo(andExpr))

          And("vice versa")
          assert(andExpr.isEquivalentTo(notExpr))
        }
      }
    }
  }

  describe("AND") {
    it("should be associative") {
      forAll(genBoolExpr, genBoolExpr, genBoolExpr) {
        Given("three BooleanExpressions")
        (expr1, expr2, expr3) => {
          Then("the order in which these expressions are AND'ed together should not matter provided their " +
            "linear sequence is preserved")
          assert(boolexps.And(expr1, boolexps.And(expr2, expr3)).isEquivalentTo(
            boolexps.And(boolexps.And(expr1, expr2), expr3)))
        }
      }
    }

    it("should be commutative")(pending)

    it("should have true as its identity element")(pending)

    it("should have false as its annihilator")(pending)

    it("should be idempotent")(pending)

    it("should absorb OR")(pending)

    it("should distribute over OR")(pending)

    it("should be false when its operands are inverses of each other")(pending)

    it("should obey DeMorgan's Law")(pending)
  }

  describe("NOT") {
    it("should be its own inverse"){
      forAll(genBoolExpr) {
        Given("A BooleanExpression")
        b: BooleanExpression => {
          When("The BooleanExpression is negated twice")
          val doubleNegatedExpr = Not(Not(b))

          Then("The new BooleanExpression should be equivalent to the original one")
            assert(doubleNegatedExpr.isEquivalentTo(b))

          And("vice versa")
          assert(b.isEquivalentTo(doubleNegatedExpr))
        }
      }
    }
  }

  describe("BooleanExpression") {
    it("should always be equivalent to itself") {
      forAll(genBoolExpr) {
        b: BooleanExpression => b.isEquivalentTo(b)
      }
    }

    it("should always be equivalent to the simplified form of itself") {
      forAll(genBoolExpr) {
        b: BooleanExpression => b.isEquivalentTo(b.simplify)
      }
    }
  }
}
