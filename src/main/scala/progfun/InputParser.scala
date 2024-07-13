import scala.util.{Try, Success, Failure}

class InputParser(input: String) {

  private def parseDimensionsTerrain(ligne: String): Try[(Int, Int)] = {
    val trimmedLine = ligne.trim()

    trimmedLine.split(" ").toList match {
      case List(largeur, hauteur) =>
        Try {
          println(largeur + " " + hauteur)
          val largeurInt = largeur.toInt
          val hauteurInt = hauteur.toInt
          (largeurInt, hauteurInt)
        }.recoverWith {
          case _: NumberFormatException =>
            Failure(new IllegalArgumentException("Invalid terrain dimensions format: dimensions must be integers"))
        }
      case _ =>
        Failure(new IllegalArgumentException("Invalid terrain dimensions format"))
    }
  }


  private def parseTondeuse(lignes: List[String]): Try[(Int, Int, Char, String)] = {
    lignes match {
      case List(ligne1, ligne2) =>
        val position = ligne1.split(" ").toList
        if (position.length == 3) {
          Try {
            val x = position(0).toInt
            val y = position(1).toInt
            val orientation = position(2).head // prendre le premier caractère comme orientation
            (x, y, orientation, ligne2.trim)
          }.recoverWith {
            case _: NumberFormatException =>
              Failure(new IllegalArgumentException("Format de position de la tondeuse invalide : les coordonnées doivent être des entiers"))
          }
        } else {
          Failure(new IllegalArgumentException("Format de position de la tondeuse invalide"))
        }
      case _ =>
        Failure(new IllegalArgumentException("Deux lignes attendues pour la tondeuse, différent trouvé"))
    }
  }

  def parse(): Try[(Int, Int, List[(Int, Int, Char, String)])] = {
    val lignes = input.split("\n").toList
    if (lignes.length < 2) {
      Failure(new IllegalArgumentException("Le fichier d'entrée ne contient pas assez de lignes"))
    } else {
      parseDimensionsTerrain(lignes.head).flatMap { case (largeur, hauteur) =>
        val lignesTondeuses = lignes.tail
        val pairesTondeuses = lignesTondeuses.sliding(2, 2).toList
        val tondeusesAnalysees = pairesTondeuses.map(parseTondeuse)

        val tondeusesValidees = tondeusesAnalysees.foldRight(Try(List.empty[(Int, Int, Char, String)])) { (next, acc) =>
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
      val source = scala.io.Source.fromFile(filePath)
      val fileContents = try source.mkString finally source.close()
      new InputParser(fileContents)
    }
  }
}
