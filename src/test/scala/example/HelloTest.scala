package example

import org.scalatest._

class FirstSpect extends FlatSpec {

    "true" should "be true" in {
        assert(true == true)
    }

    "factorial of 5" should "be 120" in {
        assertResult(120) {
            MyMath.factorial(5)
        }
    }

    "factorial of 0" should "be 1" in {
        assertResult(1) {
            MyMath.factorial(0)
        }
    }

    "**x*" should "generate 8 combinations" in {
        assertResult(8) {
            Game.allPossibleSurroundings("**x*").length
        }
    }

    "**x*" should "contain Nxxx" in {
        assert(Game.allPossibleSurroundings("**x*").contains("Nxxx"))
    }

    "**x*" should "contain NExS" in {
        assert(Game.allPossibleSurroundings("**x*").contains("NExS"))
    }

    "" should "generate 0 combinations" in {
        assertResult(1) {
            Game.allPossibleSurroundings("").length
        }
    }

    "0 xxWx -> E 1" should "have state 1 as next state" in {
        assertResult(1) {
            Rule.parseRule("0 xxWx -> E 1").get.nextState
        }
    }

    "-1 xxWx -> E 1" should "fail" in {
        assertResult(None) {
            Rule.parseRule("-1 xxWx -> E 1")
        }
    }

    "1 xxWx -> E 100" should "fail" in {
        assertResult(None) {
            Rule.parseRule("1 xxWx -> E 100")
        }
    }

    "aa xxWx -> E 1" should "fail" in {
        assertResult(None) {
            Rule.parseRule("aa xxWx -> E 1")
        }
    }

    "1 xxwx -> e 1" should "succeed" in {
        assertResult("E") {
            Rule.parseRule("1 xxwx -> e 1").get.direction
        }
    }
}