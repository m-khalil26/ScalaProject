package progfun

import better.files.File
import progfun.string_functions.StringFunctions
import progfun.string_functions.StringFunctions.{
  customTrim,
  extractSubstring,
  stripPrefix,
  stripSuffix
}

import scala.util.{Failure, Success, Try}

class ConfigParser(json: String) {

  private def removeWhitespace(json: String): String = {
    json.filterNot(_.isWhitespace)
  }

  private def parseObject(json: String): Try[Map[String, String]] = {
    def extractKeyValuePairs(json: String): List[(String, String)] = {
      val trimmedJson = stripSuffix(stripPrefix(customTrim(json), "{"), "}")

      def extractKey(json: String): (String, String) = {
        val keyStart = json.indexOf('"') + 1
        val keyEnd = json.indexOf('"', keyStart)
        val key = extractSubstring(json, keyStart, keyEnd)
        val jsonSubstring =
          extractSubstring(json, keyEnd + 1, StringFunctions.stringLength(json))
        (key, jsonSubstring)
      }

      def extractValue(json: String): (String, String) = {
        val valueStart = json.indexOf(':') + 1
        val jsonSubstring =
          extractSubstring(json, valueStart, StringFunctions.stringLength(json))

        val valueJson = customTrim(jsonSubstring)
        if (valueJson.startsWith("\"")) {
          val valueStart = valueJson.indexOf('"') + 1
          val valueEnd = valueJson.indexOf('"', valueStart)
          val value = extractSubstring(valueJson, valueStart, valueEnd)
          val jsonSubstring = extractSubstring(
            valueJson,
            valueEnd + 1,
            StringFunctions.stringLength(valueJson)
          )
          (value, jsonSubstring)
        } else {
          val valueEnd = valueJson.indexOf(',')
          val value =
            if (valueEnd == -1) valueJson
            else extractSubstring(valueJson, 0, valueEnd)
          val jsonSubstring = extractSubstring(
            valueJson,
            valueEnd + 1,
            StringFunctions.stringLength(valueJson)
          )
          (value, if (valueEnd == -1) "" else jsonSubstring)
        }
      }

      @scala.annotation.tailrec
      def parsePairs_aux(
          json: String,
          acc: List[(String, String)]): List[(String, String)] = {
        if (json.isEmpty) acc
        else {
          val (key, restAfterKey) = extractKey(json)
          val (value, restAfterValue) = extractValue(restAfterKey)
          parsePairs_aux(
            restAfterValue.drop(1),
            (key, value) :: acc
          ) 
        }
      }

      def parsePairs(json: String): List[(String, String)] = {
        parsePairs_aux(json, List.empty).reverse
      }

      parsePairs(trimmedJson)
    }

    Try(extractKeyValuePairs(json).toMap)
  }

  def parse(): Try[Map[String, String]] = {
    Try(removeWhitespace(json)).flatMap(parseObject)
  }
}

object ConfigParser {
  def fromFile(filePath: String): ConfigParser = {
    val file = File(filePath)
    val fileContents: String = Try(file.contentAsString) match {
      case Success(contents) =>
        contents
      case Failure(ex) =>
        println(s"Error reading the configuration file: ${ex.getMessage}")
        sys.exit(1)
    }
    new ConfigParser(fileContents)
  }
}
