import progfun.{ConfigParser, Direction, InputParser, Position, Tondeuse}

import java.io.File
import scala.annotation.tailrec
import scala.util.{Failure, Success}

object Main {

  val configPath = "src/main/resources/configuration.json"

  def main(args: Array[String]): Unit = {
    val configParser = ConfigParser.fromFile(configPath)

    val configMap = configParser.parse() match {
      case Success(map) => map
      case Failure(ex) =>
        println(s"Error parsing the configuration file: ${ex.getMessage}")
        sys.exit(1)
    }
    val inputPath = configMap.getOrElse("inputPath", {
      println("Missing 'inputPath' in configuration file.")
      sys.exit(1)
    })

    if (isFileAccessible(inputPath)) {
      try {
        parseInputFile(inputPath)
      } catch {
        case e: IllegalArgumentException =>
          println(s"Error parsing input file: ${e.getMessage}")
          sys.exit(1)
      }
    } else {
      println(s"Input file at $inputPath is not accessible.")
      sys.exit(1)
    }
  }

  def parseInputFile(inputPath: String): Unit = {
    val inputParser = InputParser.fromFile(inputPath)
    val (width, height, mowerParams) = inputParser.parse()
    @tailrec
    def processMowerParams(params: List[(Int, Int, Char, String)]): Unit = {
      params match {
        case Nil => // Base case: no more mowers to process
        case (x, y, orientation, commands) :: tail =>
          val initialPosition = Position(x, y)
          val initialDirection = Direction.fromChar(orientation) match {
            case Some(dir) => dir
            case None =>
              println(s"Invalid orientation character: $orientation")
              sys.exit(1)
          }

          val terrainDimensions = (width, height)
          val tondeuse = new Tondeuse(initialPosition, initialDirection, terrainDimensions, commands)

          val finalTondeuse = tondeuse.executerCommandes()
          finalTondeuse.afficherEtat()
          processMowerParams(tail)
      }
    }


    processMowerParams(mowerParams)
  }

  def isFileAccessible(filePath: String): Boolean = {
    val file = new File(filePath)
    file.exists() && file.isFile && file.canRead
  }
}
