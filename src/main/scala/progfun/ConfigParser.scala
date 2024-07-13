package progfun

import scala.util.{Try, Success, Failure}

class ConfigParser(json: String) {

  private def removeWhitespace(json: String): String = {
    json.filterNot(_.isWhitespace)
  }

  private def parseObject(json: String): Try[Map[String, String]] = {
    def extractKeyValuePairs(json: String): List[(String, String)] = {
      val trimmedJson = json.trim.stripPrefix("{").stripSuffix("}")
      val pairs = trimmedJson.split(",").toList
      pairs.flatMap { pair =>
        val keyValue = pair.split(":")
        if (keyValue.length == 2) {
          val key = keyValue(0).trim.stripPrefix("\"").stripSuffix("\"")
          val value = keyValue(1).trim.stripPrefix("\"").stripSuffix("\"")
          Some((key, value))
        } else {
          None
        }
      }
    }
    Try {
      extractKeyValuePairs(json).toMap
    }
  }

  def parse(): Try[Map[String, String]] = {
    Try(removeWhitespace(json)).flatMap(parseObject)
  }
}

object ConfigParser {
  def fromFile(filePath: String): Try[ConfigParser] = {
    Try {
      val fileContents = scala.io.Source.fromFile(filePath).mkString
      new ConfigParser(fileContents)
    }
  }
}
