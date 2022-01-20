package week1;

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class PalindromeTest extends AnyFunSpec {

    describe("Palindrome") {

        it("should identify palindromes") {

            Palindrome.isPalindrome("madam") shouldBe true
            Palindrome.isPalindrome("radar") shouldBe true
            Palindrome.isPalindrome("nurses run") shouldBe true
            Palindrome.isPalindrome("radas") shouldBe false
            Palindrome.isPalindrome("") shouldBe false
            Palindrome.isPalindrome("t") shouldBe true
        }

        it("should score palindromes") {

            Palindrome.scorePalindrome("madam") shouldBe 100.0
            Palindrome.scorePalindrome("radar") shouldBe 100.0
            Palindrome.scorePalindrome("nurses run") shouldBe 100.0
            Palindrome.scorePalindrome("radas") shouldBe 80.0
            Palindrome.scorePalindrome("rodas") shouldBe 60.0
            Palindrome.scorePalindrome("roxas") shouldBe 60.0
            Palindrome.scorePalindrome("odba") shouldBe 50.0
            Palindrome.scorePalindrome("odda") shouldBe 75.0
            Palindrome.scorePalindrome("roxas") shouldBe 60.0
            Palindrome.scorePalindrome("") shouldBe 0.0
            Palindrome.scorePalindrome("t") shouldBe 100.0
        }
    }
}