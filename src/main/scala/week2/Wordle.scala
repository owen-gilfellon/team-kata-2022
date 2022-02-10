package week2

import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.{Failure, Success, Try}


sealed trait LetterState
case object CORRECT extends LetterState
case object WRONG extends LetterState
case object ALMOST extends LetterState

case class WordleGame(puzzle: WordlePuzzle, guesses: Int) {

  def isGameComplete = puzzle.lines.length >= guesses ||
    (puzzle.lines.nonEmpty && puzzle.lines.last.guess == puzzle.word)

  def doGuess(word: String): WordleGame = {
    if (word.length != puzzle.word.length)
      throw new IllegalArgumentException(s"Guess should be ${puzzle.word.length} characters")
    if (isGameComplete)
      throw new IllegalStateException(s"Only allowed ${guesses} guesses")

    this.copy(puzzle = puzzle.doGuess(word))

  }
}

case class WordlePuzzle(word: String, lines: Seq[WordleLine] = Seq.empty) {

  def doGuess(guess: String): WordlePuzzle = {
    val letters = guess.zipWithIndex.map(c => {
      if(!word.contains(c._1)) {
        WRONG
      } else if (word.charAt(c._2) == c._1) {
        CORRECT
      } else {
        ALMOST
      }
    })

    this.copy(word, lines :+ WordleLine(guess, letters))
  }
}

case class WordleLine(guess: String, letterStates: Seq[LetterState])

object Wordle {

  def main(args: Array[String]): Unit = {

    def getWords(): Seq[String] = {
      val filename = "src/main/resources/wordlewords.txt"
      val reader = Source.fromFile(filename)
      val words: Seq[String] = reader.getLines.toSeq
      reader.close()
      words.foreach(println)
      words
    }

    def playGame(game: WordleGame): Boolean = {

      def printGuesses(game: WordleGame): Unit = {
        game.puzzle.lines.foreach { line =>
          line.guess.toCharArray.zip(line.letterStates).foreach {
            case (character, CORRECT) =>
              print(Console.BLACK + Console.GREEN_B + character)
            case (character, WRONG) =>
              print(Console.WHITE + Console.RESET + character)
            case (character, ALMOST) =>
              print(Console.BLACK + Console.RED_B + character)
          }
          println(Console.RESET)
        }
      }

      def doGuess(game: WordleGame): WordleGame = {

        print(s"\nGuess ${game.puzzle.lines.length + 1}: ")
        val guess = readLine()
        println()

        Try(game.doGuess(guess)) match  {
          case Success(updated) => {
            printGuesses(updated)
            updated
          }
          case Failure(ex: IllegalArgumentException) => {
            println(Console.RED + ex.getMessage + Console.RESET)
            doGuess(game)
          }
        }
      }

      if(game.isGameComplete)
        return game.puzzle.lines.last.guess == game.puzzle.word

      playGame(doGuess(game))

    }

    val parsedArgs = args.grouped(2).map(x => (x(0).tail, x(1))).toMap

    val words = getWords()

    val score = words.foldLeft(0)((a, b) =>
      {
        if(playGame(WordleGame(WordlePuzzle(b), parsedArgs("guesses").toInt))) {
          println(Console.GREEN + "WON!" + Console.RESET)
          a + 1
        } else {
          println(Console.BLINK + Console.RED + "FAILED!" + Console.RESET)
          a
        }})

    println(s"Score: $score / ${words.length} wordles")

  }

}
