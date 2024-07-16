package progfun

import better.files.File
import progfun.string_functions.StringFunctions
import progfun.string_functions.StringFunctions.splitOnDelim

import scala.sys.exit

class InputParser(input: String) {

  private def parseDimensionsTerrain(ligne: String): (Int, Int) = {
    val trimmedLine = StringFunctions.customTrim(ligne)
    val spaceIndex = StringFunctions.findFirstSpace(trimmedLine)


    if (spaceIndex >= 0) {
      val largeur = StringFunctions.extractSubstring(trimmedLine, 0, spaceIndex)
      val hauteur = StringFunctions.extractSubstring(trimmedLine, spaceIndex + 1, StringFunctions.stringLength(trimmedLine))


      try {
        val largeurInt = largeur.toInt
        val hauteurInt = hauteur.toInt
        (largeurInt, hauteurInt)
      } catch {
        case _: NumberFormatException =>
          println(s"Les dimensions du terrain doivent être des entiers. Found: largeur='$largeur', hauteur='$hauteur'")
          exit(1)
      }
    } else {
      println("Les dimensions du terrain doivent être séparées par un espace.")
      exit(1)
    }
  }


  private def parseTondeuse(lines: Array[String]): (Int, Int, Char, String) = {

    if (lines.length == 2) {
      val positionLine = StringFunctions.customTrim(lines(0))
      val commands = StringFunctions.customTrim(lines(1))

      if (positionLine.nonEmpty && commands.nonEmpty) {
        val parts = positionLine.split(' ')
        if (parts.length == 3) {
          try {
            val x = parts(0).toInt
            val y = parts(1).toInt
            val orientation = parts(2).charAt(0)
            (x, y, orientation, commands)
          } catch {
            case _: NumberFormatException =>
              println("Les coordonnées de la tondeuse doivent être des entiers.")
              exit(1)
          }
        } else {
          println("La ligne de position de la tondeuse doit contenir 3 parties.")
          exit(1)
        }
      } else {
        println("La ligne de position de la tondeuse et les commandes ne doivent pas être vides.")
        exit(1)
      }
    } else {
      println("La tondeuse doit être définie par une ligne de position et une ligne de commandes.")
      exit(1)
    }
  }

  def parse(): (Int, Int, List[(Int, Int, Char, String)]) = {
    val splitted = splitOnDelim(input, '\n')
    val lignes = StringFunctions.trimParts(splitted)


    if (lignes.length < 2) {
      println("Input does not contain enough lines.")
      exit(1)
    } else {
      val (largeur, hauteur) = parseDimensionsTerrain(lignes(0))
      val lignesTondeuses = lignes.tail
      val pairesTondeuses = lignesTondeuses.grouped(2).toArray
      val tondeusesAnalysees = pairesTondeuses.map(parseTondeuse).toList

      (largeur, hauteur, tondeusesAnalysees)
    }
  }
}

object InputParser {
  def fromFile(filePath: String): InputParser = {
    val file = File(filePath)
    val fileContents = file.contentAsString
    new InputParser(fileContents)
  }
}
