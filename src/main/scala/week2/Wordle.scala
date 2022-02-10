package week2

sealed trait LetterState
case object CORRECT extends LetterState
case object WRONG extends LetterState
case object ALMOST extends LetterState

case class WordleGame(puzzle: WordlePuzzle, guesses: Int) {

  def isGameComplete = puzzle.lines.length >= guesses

  def doGuess(word: String): WordleGame = {
    if (word.length != puzzle.word.length)
      throw new IllegalArgumentException(s"Guess should be ${puzzle.word.length} characters")
    if (isGameComplete)
      throw new IllegalStateException(s"Only allowed ${guesses} guesses")

    this.copy(puzzle = puzzle.doGuess(word))
  }

}

case class WordlePuzzle(word: String, lines: Seq[WordleLine]) {

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


  // word to guess (configurable externally)
  // six guesses - word + states (WRONG, ALMOST, CORRECT)

}
