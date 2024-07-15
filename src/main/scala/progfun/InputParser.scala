import better.files.File
import progfun.string_functions.StringFunctions
import progfun.string_functions.StringFunctions.splitOnDelim

import scala.util.{Failure, Success, Try}
import java.nio.charset.Charset
import scala.sys.exit

class InputParser(input: String) {

  private def parseDimensionsTerrain(ligne: String): Try[(Int, Int)] = {
    val trimmedLine = StringFunctions.customTrim(ligne)
    val spaceIndex = StringFunctions.findFirstSpace(trimmedLine)

    if (spaceIndex >= 0) {
      val largeur = StringFunctions.extractSubstring(trimmedLine, 0, spaceIndex)
      val hauteur = StringFunctions.extractSubstring(trimmedLine, spaceIndex + 1, StringFunctions.stringLength(trimmedLine))

      Try {
        val largeurInt = largeur.toInt
        val hauteurInt = hauteur.toInt
        (largeurInt, hauteurInt)
      }.recoverWith {
        case _: NumberFormatException =>
          exit(1)
      }
    } else {
      exit(1)
    }
  }

  private def parseTondeuse(lines: Array[String]): Try[(Int, Int, Char, String)] = {
    if (lines.length == 2) {
      val positionLine = StringFunctions.customTrim(lines(0))
      val commands = StringFunctions.customTrim(lines(1))

      if (positionLine.nonEmpty && commands.nonEmpty) {
        val parts = positionLine.split(' ')
        if (parts.length == 3) {
          Try {
            val x = parts(0).toInt
            val y = parts(1).toInt
            val orientation = parts(2).charAt(0)
            (x, y, orientation, commands)
          }.recoverWith {
            case _: NumberFormatException =>
              exit(1)
          }
        } else {
          exit(1)
        }
      } else {
        exit(1)
      }
    } else {
      exit(1)
    }
  }



  def parse(): Try[(Int, Int, List[(Int, Int, Char, String)])] = {
    val splitted = splitOnDelim(input, '\n')
    val lignes = StringFunctions.trimParts(splitted)

    if (lignes.length < 2) {
      exit(1)
    } else {
      parseDimensionsTerrain(lignes(0)).flatMap { case (largeur, hauteur) =>
        val lignesTondeuses = lignes.tail
        val pairesTondeuses = lignesTondeuses.grouped(2).toArray
        val tondeusesAnalysees = pairesTondeuses.map(parseTondeuse)

        val tondeusesValidees = tondeusesAnalysees.foldLeft(Try(List.empty[(Int, Int, Char, String)])) { (acc, next) =>
          for {
            tondeuses <- acc
            tondeuse <- next
          } yield tondeuse :: tondeuses
        }

        tondeusesValidees.map(tondeuseParams => (largeur, hauteur, tondeuseParams))
      }
    }
  }
}

object InputParser {
  def fromFile(filePath: String): Try[InputParser] = {
    Try {
      val file = File(filePath)
      val fileContents = file.contentAsString
      new InputParser(fileContents)
    }.transform(
      inputParser => Success(inputParser),
      ex => exit(1)
    )
  }
}
