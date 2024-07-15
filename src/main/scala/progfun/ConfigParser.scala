package progfun

import better.files.File
import progfun.string_functions.StringFunctions
import progfun.string_functions.StringFunctions.{stripPrefix, stripSuffix}

import scala.sys.exit
import scala.util.{Failure, Success, Try}

class ConfigParser(json: String) {

  private def removeWhitespace(json: String): String = {
    json.filterNot(_.isWhitespace)
  }

  private def parseObject(json: String): Try[Map[String, String]] = {
    def extractKeyValuePairs(json: String): List[(String, String)] = {
      val trimmedJson = stripSuffix(stripPrefix(StringFunctions.customTrim(json),"{"),"}")
      val pairs = StringFunctions.splitString(trimmedJson,',').toList
      pairs.flatMap { pair =>
        val keyValue = StringFunctions.splitString(pair,':')
        if (StringFunctions.arrayLength(keyValue) == 2) {
          val key = StringFunctions.stripSuffix(StringFunctions.stripPrefix(StringFunctions.customTrim(keyValue(0)),("\"")),"\"")
          val value =StringFunctions.stripSuffix(StringFunctions.stripPrefix(StringFunctions.customTrim(keyValue(1)),("\"")),"\"")
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
  def fromFile(filePath: String): ConfigParser = {
    val file = File(filePath)
    val fileContents: String = Try(file.contentAsString) match {
      case Success(contents) => contents
      case Failure(ex) =>
        System.exit(1)  
        ""  
    }
    new ConfigParser(fileContents)
  }
}
