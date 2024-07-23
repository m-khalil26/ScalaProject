package funProgTests

import progfun.InputParser

class InputParserTest extends munit.FunSuite {

  private def testParseDimensions(input: String, expected: (Int, Int)): Unit = {
    val parser = new InputParser(input)
    val parsed = parser.parse()
    assertEquals((parsed._1, parsed._2), expected)
  }

  private def testParseTondeuse(
      input: String,
      expected: List[(Int, Int, Char, String)]): Unit = {
    val parser = new InputParser(input)
    val parsed = parser.parse()
    assertEquals(parsed._3, expected)
  }

  test("InputParser should parse terrain dimensions correctly") {
    val input = "5 5\n1 2 N\nLFLFLFLFF"
    testParseDimensions(input, (5, 5))
  }

  test("InputParser should parse single mower correctly") {
    val input = "5 5\n1 2 N\nLFLFLFLFF"
    val expected = List((1, 2, 'N', "LFLFLFLFF"))
    testParseTondeuse(input, expected)
  }

  test("InputParser should parse multiple mowers correctly") {
    val input = "5 5\n1 2 N\nLFLFLFLFF\n3 3 E\nFFRFFRFRRF"
    val expected = List(
      (1, 2, 'N', "LFLFLFLFF"),
      (3, 3, 'E', "FFRFFRFRRF")
    )
    testParseTondeuse(input, expected)
  }
}
