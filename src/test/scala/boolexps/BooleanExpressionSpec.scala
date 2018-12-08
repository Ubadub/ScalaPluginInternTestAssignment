package boolexps

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import TestDataGenerators._

/**
  * This class specifically tests that the `BooleanExpression` class is correctly governed by the rules of boolean
  * algebra and that it can be correctly converted to simplified or normal forms.
  */
class BooleanExpressionSpec extends FlatSpec with GeneratorDrivenPropertyChecks {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 10000, maxDiscardedFactor = 20.0, minSize = 1)

  // p OR (q OR r) == (p OR q) OR r
  "The OR operator" should "be associative" in {
    forAll(genThreeVarBoolExpr, genThreeVarBoolExpr, genThreeVarBoolExpr) {
      (expr1, expr2, expr3) => {
        assert(Or(expr1, Or(expr2, expr3)).isEquivalentTo(Or(Or(expr1, expr2), expr3)))
      }
    }
  }

  // p OR q == q OR p
  it should "be commutative" in {
    forAll(genThreeVarBoolExpr, genThreeVarBoolExpr) {
      (expr1, expr2) => {
        assert(Or(expr1, expr2).isEquivalentTo(Or(expr2, expr1)))
      }
    }
  }

  // p OR False == p
  it should "have false as its identity element" in {
    forAll(genThreeVarBoolExpr) {
      expr => {
        val orExpr1: Or = Or(expr, False)
        assert(orExpr1.isEquivalentTo(expr))
        assert(expr.isEquivalentTo(orExpr1))

        val orExpr2: Or = Or(False, expr)
        assert(orExpr2.isEquivalentTo(expr))
        assert(expr.isEquivalentTo(orExpr2))
      }
    }
  }

  // p OR True == True
  it should "have true as its annihilator" in {
    forAll(genThreeVarBoolExpr) {
      expr => {
        val orExpr1: Or = Or(expr, True)
        assert(orExpr1.isEquivalentTo(True))
        assert(True.isEquivalentTo(orExpr1))

        val orExpr2: Or = Or(True, expr)
        assert(orExpr2.isEquivalentTo(True))
        assert(True.isEquivalentTo(orExpr2))
      }
    }
  }

  // p OR p == p
  it should "be idempotent" in {
    forAll(genThreeVarBoolExpr) {
      expr => {
        assert(Or(expr, expr).isEquivalentTo(expr))
        assert(expr.isEquivalentTo(Or(expr, expr)))
      }
    }
  }

  // p OR (p AND q) == p
  it should "absorb AND" in {
    forAll(genThreeVarBoolExpr, genThreeVarBoolExpr) {
      (expr1, expr2) => {
        val orExpr1: Or = Or(expr1, boolexps.And(expr1, expr2))
        assert(orExpr1.isEquivalentTo(expr1))
        assert(expr1.isEquivalentTo(orExpr1))

        val orExpr2: Or = Or(boolexps.And(expr1, expr2), expr1)
        assert(orExpr2.isEquivalentTo(expr1))
        assert(expr1.isEquivalentTo(orExpr2))
      }
    }
  }

  // p OR (q AND r) == (p OR q) AND (p OR r)
  it should "distribute over AND" in {
    forAll(genThreeVarBoolExpr, genThreeVarBoolExpr, genThreeVarBoolExpr) {
      (expr1, expr2, expr3) => {
        val orExpr1: Or = Or(expr1, boolexps.And(expr2, expr3))
        val orExpr2: Or = Or(expr1, expr2)
        val orExpr3: Or = Or(expr1, expr3)
        val andExpr1: boolexps.And = boolexps.And(orExpr2, orExpr3)
        assert(orExpr1.isEquivalentTo(andExpr1))
        assert(andExpr1.isEquivalentTo(orExpr1))

        val orExpr4: Or = Or(boolexps.And(expr2, expr3), expr1)
        assert(orExpr4.isEquivalentTo(andExpr1))
        assert(andExpr1.isEquivalentTo(orExpr4))
      }
    }
  }

  // p OR ~p == True
  it should "be true when its operands are complements of each other" in {
    forAll(genThreeVarBoolExpr) {
      e => {
        assert(Or(e, Not(e)).isEquivalentTo(True))
      }
    }
  }

  // ~(p OR q) == ~p AND ~q")
  it should "obey DeMorgan's Law" in {
    forAll(genThreeVarBoolExpr, genThreeVarBoolExpr) {
      (expr1, expr2) => {
        val notExpr = Not(Or(expr1, expr2))
        val andExpr = boolexps.And(Not(expr1), Not(expr2))
        assert(notExpr.isEquivalentTo(andExpr))
        assert(andExpr.isEquivalentTo(notExpr))
      }
    }
  }

  // p AND (q AND r) == (p AND q) AND r
  "The AND operator" should "be associative" in {
    forAll(genThreeVarBoolExpr, genThreeVarBoolExpr, genThreeVarBoolExpr) {
      (expr1, expr2, expr3) => {
        assert(boolexps.And(expr1, boolexps.And(expr2, expr3)).isEquivalentTo(
          boolexps.And(boolexps.And(expr1, expr2), expr3)))
      }
    }
  }

  // p AND q == q AND p
  it should "be commutative" in {
    forAll(genThreeVarBoolExpr, genThreeVarBoolExpr) {
      (expr1, expr2) => {
        assert(boolexps.And(expr1, expr2).isEquivalentTo(boolexps.And(expr2, expr1)))
      }
    }
  }

  // p AND True == p
  it should "have true as its identity element" in {
    forAll(genThreeVarBoolExpr) {
      expr => {
        val andExpr1: And = boolexps.And(expr, True)
        assert(andExpr1.isEquivalentTo(expr))
        assert(expr.isEquivalentTo(andExpr1))

        val andExpr2: And = boolexps.And(True, expr)
        assert(andExpr2.isEquivalentTo(expr))
        assert(expr.isEquivalentTo(andExpr2))
      }
    }
  }

  // p AND False == False
  it should "have false as its annihilator" in {
    forAll(genThreeVarBoolExpr) {
      expr => {
        val andExpr1: And = boolexps.And(expr, False)
        assert(andExpr1.isEquivalentTo(False))
        assert(False.isEquivalentTo(andExpr1))

        val andExpr2: And = boolexps.And(False, expr)
        assert(andExpr2.isEquivalentTo(False))
        assert(False.isEquivalentTo(andExpr2))
      }
    }
  }

  // p AND p == p
  it should "be idempotent" in {
    forAll(genThreeVarBoolExpr) {
      expr => {
        assert(boolexps.And(expr, expr).isEquivalentTo(expr))
        assert(expr.isEquivalentTo(boolexps.And(expr, expr)))
      }
    }
  }

  // p AND (p OR q) == p
  it should "absorb OR" in {
    forAll(genThreeVarBoolExpr, genThreeVarBoolExpr) {
      (expr1, expr2) => {
        val andExpr1: And = boolexps.And(expr1, Or(expr1, expr2))
        assert(andExpr1.isEquivalentTo(expr1))
        assert(expr1.isEquivalentTo(andExpr1))

        val andExpr2: And = boolexps.And(Or(expr1, expr2), expr1)
        assert(andExpr2.isEquivalentTo(expr1))
        assert(expr1.isEquivalentTo(andExpr2))
      }
    }
  }

  // p AND (q OR r) == (p AND q) OR (p AND r)
  it should "distribute over OR" in {
    forAll(genThreeVarBoolExpr, genThreeVarBoolExpr, genThreeVarBoolExpr) {
      (expr1, expr2, expr3) => {
        val andExpr1: boolexps.And = boolexps.And(expr1, Or(expr2, expr3))
        val andExpr2: And = boolexps.And(expr1, expr2)
        val andExpr3: And = boolexps.And(expr1, expr3)
        val orExpr1: Or = Or(andExpr2, andExpr3)

        assert(andExpr1.isEquivalentTo(orExpr1))
        assert(orExpr1.isEquivalentTo(andExpr1))

        val andExpr4: And = boolexps.And(Or(expr2, expr3), expr1)
        assert(andExpr4.isEquivalentTo(orExpr1))
        assert(orExpr1.isEquivalentTo(andExpr4))
      }
    }
  }

  // p AND ~p == False
  it should "be false when its operands are complements of each other" in {
    forAll(genThreeVarBoolExpr) {
      e => assert(boolexps.And(e, Not(e)).isEquivalentTo(False))
    }
  }

  // ~(p AND q) == ~p OR ~q")
  it should "obey DeMorgan's Law" in {
    forAll(genThreeVarBoolExpr, genThreeVarBoolExpr) {
      (expr1, expr2) => {
        val notExpr = Not(boolexps.And(expr1, expr2))
        val orExpr = Or(Not(expr1), Not(expr2))

        assert(notExpr.isEquivalentTo(orExpr))
        assert(orExpr.isEquivalentTo(notExpr))
      }
    }
  }

  // ~(~p) == p
  "The NOT operator" should "obey the law of double negation" in {
    forAll(genThreeVarBoolExpr) {
      expr: BooleanExpression => {
        val doubleNegatedExpr = Not(Not(expr))

        assert(doubleNegatedExpr isEquivalentTo expr)
        assert(expr isEquivalentTo doubleNegatedExpr)
      }
    }
  }

  it should "have the same effect as calling negate()" in {
    forAll(genThreeVarBoolExpr) {
      expr: BooleanExpression => {
        assert(Not(expr) isEquivalentTo expr.negate)

        assert(expr.negate isEquivalentTo Not(expr))
      }
    }
  }

  // p == p
  "A BooleanExpression" should "always be equivalent to itself" in {
    forAll(genThreeVarBoolExpr) {
      b: BooleanExpression => assert(b isEquivalentTo b)
    }
  }

  // p != ~p
  it should "never be equivalent to its complement" in {
    forAll(genThreeVarBoolExpr) {
      b: BooleanExpression => {
        assert(!(b isEquivalentTo b.negate))
        assert(!(b.negate isEquivalentTo b))
      }
    }
  }

  it should "never be equivalent to the complement of its simplified form" in {
    forAll(genThreeVarBoolExpr) {
      b: BooleanExpression => {
        assert(!(b isEquivalentTo b.negate.simplify))
        assert(!(b.negate.simplify isEquivalentTo b))
      }
    }
  }

  it should "always produce an equivalent simplified form" in {
    forAll(genThreeVarBoolExpr) {
      b: BooleanExpression => {
        assert(b isEquivalentTo b.simplify)
        assert(b.simplify isEquivalentTo b)
      }
    }
  }

  it should "never simplify to an expression equivalent to its complement" in {
    forAll(genThreeVarBoolExpr) {
      b: BooleanExpression => {
        assert(!(b.simplify isEquivalentTo b.negate))
        assert(!(b.negate isEquivalentTo b.simplify))
      }
    }
  }

  it should "be reducible to negation normal form" in pending/*{
    forAll(genThreeVarBoolExpr) {
      b: BooleanExpression => assert(b.toNNF)
    }
  }*/

  it should "be equivalent to its negation normal form" in {
    forAll(genThreeVarBoolExpr) {
      b: BooleanExpression => assert(b isEquivalentTo b.toNNF)
        b: BooleanExpression => assert(b.toNNF isEquivalentTo b)
    }
  }

  it should "be reducible to disjunctive normal form" in {
    forAll(genThreeVarBoolExpr) {
      b: BooleanExpression => assert(b.toDNF.isDNF)
    }
  }

  it should "be equivalent to its disjunctive normal form" in {
    forAll(genThreeVarBoolExpr) {
      b: BooleanExpression => assert(b isEquivalentTo b.toDNF)
      b: BooleanExpression => assert(b.toDNF isEquivalentTo b)
    }
  }

  "Negation and simplification" should "be commutative" in {
    forAll(genThreeVarBoolExpr) {
      b: BooleanExpression => {
        assert(b.simplify.negate isEquivalentTo b.negate.simplify)
        assert(b.negate.simplify isEquivalentTo b.simplify.negate)
      }
    }
  }

}
