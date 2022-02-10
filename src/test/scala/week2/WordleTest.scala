package week2

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.util.{Failure, Try}

class WordleTest extends AnyFunSpec {

    describe("Wordle") {

        it("should score letters correctly") {

            val puzzle = WordlePuzzle("pears", Seq.empty)

            val puzzle2 = puzzle.doGuess("chomp")

            puzzle2.lines.head.letterStates shouldBe Seq(WRONG, WRONG, WRONG, WRONG, ALMOST)

            val puzzle3 = puzzle2.doGuess("point")

            puzzle3.lines(1).letterStates shouldBe Seq(CORRECT, WRONG, WRONG, WRONG, WRONG)

            val puzzle4 = puzzle3.doGuess("pears")

            puzzle4.lines(2).letterStates shouldBe Seq(CORRECT, CORRECT, CORRECT, CORRECT, CORRECT)

        }

        it("Not allow more than allowed guesses") {

            val game = WordleGame(WordlePuzzle("pears", Seq.empty), guesses = 3)

            val g = game.doGuess("chomp").doGuess("stomp").doGuess("abcde")

            val x = Try {
                g.doGuess("asdgf")
            }

            x shouldBe Failure

        }

        it("Not allow guesses with incorrect number of characters") {

            val t = Try {
                val game = WordleGame(WordlePuzzle("pears", Seq.empty), guesses = 3)
                game.doGuess("wordle")
            }

            t shouldBe Failure
        }

    }
}