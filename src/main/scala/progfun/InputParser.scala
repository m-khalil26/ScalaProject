package progfun

import scala.util.{Try, Success, Failure}

class InputParser(input: String) {

  private def parseFieldSize(line: String): Try[(Int, Int)] = {
    line.split(" ").toList match {
      case List(width, height) =>
        Try((width.toInt, height.toInt))
      case _ =>
        Failure(new IllegalArgumentException("Invalid field size format"))
    }
  }

  private def parseMower(line1: String, line2: String): Try[(Int, Int, Char, String)] = {
    val position = line1.split(" ").toList
    if (position.length == 3) {
      Try {
        val x = position(0).toInt
        val y = position(1).toInt
        val orientation = position(2).head // take first character as orientation
        (x, y, orientation, line2.trim)
      }
    } else {
      Failure(new IllegalArgumentException("Invalid mower position format"))
    }
  }

  def parse(): Try[(Int, Int, List[(Int, Int, Char, String)])] = {
    val lines = input.split("\n").toList
    if (lines.length < 2) {
      Failure(new IllegalArgumentException("Input file does not contain enough lines"))
    } else {
      parseFieldSize(lines.head).flatMap { case (width, height) =>
        val mowerLines = lines.tail
        val mowerPairs = mowerLines.sliding(2, 2).toList
        val parsedMowers = mowerPairs.map { case List(line1, line2) =>
          parseMower(line1, line2)
        }
        val validatedMowers = parsedMowers.foldRight(Try(List.empty[(Int, Int, Char, String)])) { (next, acc) =>
          for {
            mowers <- acc
            mower <- next
          } yield mower :: mowers
        }
        validatedMowers.map(mowerParams => (width, height, mowerParams))
      }
    }
  }
}

object InputParser {
  def fromFile(filePath: String): Try[InputParser] = {
    Try {
      val source = scala.io.Source.fromFile(filePath)
      val fileContents = try source.mkString finally source.close()
      new InputParser(fileContents)
    }
  }
}
