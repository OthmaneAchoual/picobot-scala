package example

import javax.swing._
import scala.annotation.tailrec
import scala.io._
import scala.math._
import scala.util._

object MyMath {
    def product(x: Int, y: Int) = x * y

    def pascal(column: Int, row: Int): Int = {
        if (row == 1 || column == 1 || row == column) 1 else pascal(column-1, row-1) + pascal(column, row-1)        
    }

    def repeat(times: Int)(block: => Unit): Unit = {
        for { i <- 1 to times } block
    }

    def factorial(n: Int): Int = {

        @tailrec
        def _factorial(n: Int, acc: Int): Int = if (n == 0) acc else _factorial(n - 1, n * acc)

        require(n >= 0)
        _factorial(n, 1)
    }
}

trait Exchange {
    type Commodity;
}

class StockExchange extends Exchange {
    class Commodity
}

object Board

object Cell

object IO {
    def readLines(filename: String): Array[String] = Source.fromFile(filename).getLines.toArray
}

class Game(
    val rules: List[Rule]
)

object Game {
    def parseRules(rules: List[String]): List[Option[Rule]] = rules.map(Rule.parseRule)

    def allPossibleSurroundings(wildcard: String): List[String] = {
        if (wildcard.length == 0) 
            List("")
        else
            for {
                surrounding <- allPossibleSurroundings(wildcard.substring(1))
                chars = if (wildcard(0) == '*') "x" + "SWEN"(wildcard.length-1) else wildcard(0).toString
                char <- chars
            } yield char + surrounding 
    }
}

class Position(val x: Int, val y: Int) {
    def move(direction: String): Position = {
        direction match {
            case Position.NORTH => new Position(this.x, this.y + 1)
            case Position.SOUTH => new Position(this.x, this.y - 1)
            case Position.EAST => new Position(this.x + 1, this.y)
            case Position.WEST => new Position(this.x - 1, this.y)
        }
    }
}

object Position {
    val NORTH = "N"
    val SOUTH = "S"
    val EAST = "E"
    val WEST = "W"
}

class Bot(val position: Position) {
    def moveBot(direction: String): Bot = {
        new Bot(this.position.move(direction))
    }
}

class Rule(val currentState: Int, val surroundings: String, val direction: String, val nextState: Int)

object Rule {
    def parseRule(rule: String): Option[Rule] = {
        def validSurroundings(surroundings: String): Boolean = {
            val validLength = surroundings.length == 4 
            val validChars = surroundings.toList.zipWithIndex.map {
                case (c, i) => c == 'X' || c == "NEWS"(i)
            }.fold(true)(_ && _)

            validLength && validChars
        }
        def validDirection(direction: String): Boolean = direction == "N" || direction == "E" || direction == "W" || direction == "S"

        val ruleParts: Array[String] = rule.split("\\s+")
        val parsed = for {
            currentState <- Try(ruleParts(0).toInt).toOption
            if (currentState >= 0 && currentState <= 99)
            nextState <- Try(ruleParts(4).toInt).toOption
            if (nextState >= 0 && nextState <= 99)
            surroundings = ruleParts(1).toUpperCase
            if (validSurroundings(surroundings))
            direction = ruleParts(3).toUpperCase
            if (validDirection(direction))
        } yield new Rule(currentState, surroundings, direction, nextState)

        parsed
    }
}

object Hello extends App {
    println("Hello, scala!")
    println(s"5 * 3 = ${MyMath.product(5, 3)}")
    println(s"pascal(2, 3) = ${MyMath.pascal(2, 3)}")

    MyMath.repeat(3) {
        println("Hello from repeat")
    }

    def createAndShowGUI: Unit = {
            val frame = new JFrame("HelloWorldSwing")
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)

            val label = new JLabel("Hello Swing")
            frame.getContentPane.add(label)

            frame.pack
            frame.setVisible(true)
    }

    SwingUtilities.invokeLater(new Runnable() {
        def run: Unit = createAndShowGUI
    })
}