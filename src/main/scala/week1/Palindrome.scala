package week1

object Palindrome {

  def isPalindrome(word: String): Boolean = {

    val sanitised = sanitiseWord(word)

    if(sanitised.isEmpty)
      false
    else
      sanitised.reverse.equals(sanitised)
  }

  def scorePalindrome(word: String): Double = {

    val sanitised = sanitiseWord(word)

    if(sanitised.isEmpty)
      return 0.0

    val firstHalf = halfString(sanitised)
    val secondHalf = halfString(sanitised.reverse)

    val fullLength = sanitised.length

    val palindromicCharacters: Double = firstHalf.zip(secondHalf).count(x => x._1 == x._2)
    val palindromicByDefault: Double =  fullLength - firstHalf.length

    ((palindromicByDefault + palindromicCharacters) / fullLength) * 100.0
  }

  private def sanitiseWord(word: String) = word.toLowerCase.filterNot(_.equals(' '))

  private def halfString(word: String): String = {
    val oddNumber = word.length % 2 != 0
    word.substring(0, if(oddNumber) (word.length / 2) + 1 else word.length / 2)
  }
}
