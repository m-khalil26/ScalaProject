package funProgTests

import progfun.ConfigParser

import scala.util.{Success, Try}

class ConfigParserTest extends munit.FunSuite {

  private def testParse(
      input: String,
      expected: Try[Map[String, String]]): Unit = {
    val parser = new ConfigParser(input)
    val result = parser.parse()
    assertEquals(result, expected)
  }

  test("ConfigParser should parse a simple JSON object") {
    val input = """{"key1": "value1", "key2": "value2"}"""
    val expected = Success(Map("key1" -> "value1", "key2" -> "value2"))
    testParse(input, expected)
  }

  test("ConfigParser should handle empty JSON object") {
    val input = "{}"
    val expected = Success(Map.empty[String, String])
    testParse(input, expected)
  }

  test("ConfigParser should handle JSON object with whitespace") {
    val input = """{  "key1"  : "value1" , "key2" :  "value2"  }"""
    val expected = Success(Map("key1" -> "value1", "key2" -> "value2"))
    testParse(input, expected)
  }

  test("ConfigParser should parse JSON from file correctly") {
    val filePath = "src/main/resources/configuration.json"
    val parser = ConfigParser.fromFile(filePath)
    val expected = Success(
      Map(
        "name"      -> "funprog",
        "csvPath"   -> "tmp/output.csv",
        "jsonPath"  -> "tmp/output.json",
        "yamlPath"  -> "tmp/output.yaml",
        "inputPath" -> "src/main/resources/input.txt"
      )
    )
    assertEquals(parser.parse(), expected)
  }
}
