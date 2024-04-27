package ex1

import org.scalatest.matchers.should.Matchers.*

class ParserScalaTests extends org.scalatest.funsuite.AnyFunSuite:
    test("Basic parser should parse normally"):
        val parser = Parsers.basic(Set('a', 'b', 'c'))
        parser.parseAll("aabc".toList) should be equals true
        parser.parseAll("aabcdc".toList) should be equals false
        parser.parseAll("".toList) should be equals true

    test("NonEmpty parser should behave as basic but also do not accept empty strings"):
        val parser = Parsers.nonEmpty(Set('a', 'b', 'c'))
        parser.parseAll("aabc".toList) should be equals true
        parser.parseAll("aabcdc".toList) should be equals false
        parser.parseAll("".toList) should be equals false

    test("NotTwoConsecutive should behave as basic but also do not accept two consecutive equals chars"):
        val parser = Parsers.notTwoConsecutive(Set('X', 'Y', 'Z'))
        parser.parseAll("XYZ".toList) should be equals true
        parser.parseAll("XYYZ".toList) should be equals false
        parser.parseAll("".toList) should be equals true

    test("NonEmpty with not two consecutive should intersect both behaviours"):
        val parser = new BasicParser(Set('X', 'Y', 'Z')) with NonEmpty[Char] with NotTwoConsecutive[Char]
        parser.parseAll("XYZ".toList) should be equals true
        parser.parseAll("XYYZ".toList) should be equals false
        parser.parseAll("".toList) should be equals false

    test("String parser should accept all chars in the string"):
        val parser = "abc".charParser()
        parser.parseAll("aabc".toList) should be equals true
        parser.parseAll("aabcdc".toList) should be equals false
        parser.parseAll("".toList) should be equals true