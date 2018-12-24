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
}