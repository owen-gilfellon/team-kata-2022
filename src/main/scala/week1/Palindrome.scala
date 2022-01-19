package week1

object Palindrome {

  def isPalindrome(word: String): Boolean = {
    val sanitised = word.toLowerCase.filterNot(_.equals(' '))
    sanitised.reverse.equals(sanitised)
  }

  /**
   *
   * @param word
   * @return the first half of word, inclusive of middle character if odd number of characters
   */
  private def getFirstHalf(word: String): String = {
    val oddNumber = word.length % 2 != 0
    word.substring(0, if(oddNumber) (word.length / 2) + 1 else word.length / 2)
  }

  def scorePalindrome(word: String): Double = {

    val sanitised = word.toLowerCase.filterNot(_.equals(' '))

    val firstHalf = getFirstHalf(sanitised)
    val secondHalf = getFirstHalf(sanitised.reverse)

    val fullLength = sanitised.length
    val halfLength = firstHalf.length

    val matchedCharacters: Int = firstHalf.zip(secondHalf).count(x => x._1 == x._2)
    val palindromicByDefault: Int =  fullLength - halfLength

    ((palindromicByDefault.toDouble + matchedCharacters.toDouble) / fullLength) * 100.0
  }
}
