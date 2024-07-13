import progfun.{ConfigParser, Direction, Nord, Position, Tondeuse}

import java.io.File
import scala.util.{Failure, Success}

object Main extends App {

  val configPath = "src/main/resources/configuration.json"
  val configParserTry = ConfigParser.fromFile(configPath)

  configParserTry match {
    case Success(configParser) =>

      val configMapTry = configParser.parse()

      configMapTry match {
        case Success(configMap) =>
          val inputPath = configMap.getOrElse("inputPath", "N/A") // Default input path
          val jsonPath = configMap.getOrElse("jsonPath", "N/A")
          val csvPath = configMap.getOrElse("csvPath", "N/A")
          val yamlPath = configMap.getOrElse("yamlPath", "N/A")

          if (isFileAccessible(inputPath)) {
            parseInputFile(inputPath)
          } else {
            sys.exit(1)
          }

        case Failure(exception) =>
          sys.exit(1)
      }

    case Failure(exception) =>
      sys.exit(1)
  }
  def parseInputFile(inputPath: String): Unit = {
    val inputParserTry = InputParser.fromFile(inputPath)

    inputParserTry match {
      case Success(inputParser) =>

        val parsedDataTry = inputParser.parse()

        parsedDataTry match {
          case Success((width, height, mowerParams)) =>
            mowerParams.zipWithIndex.foreach { case ((x, y, orientation, commands), index) =>
              val initialPosition = Position(x, y)
              val initialDirection = Direction.fromChar(orientation) match {
                case Some(dir) => dir
                case None =>
                  sys.exit(1)
              }

              val terrainDimensions = (width, height)
              val tondeuse = new Tondeuse(initialPosition, initialDirection, terrainDimensions)

              tondeuse.commandes = commands
              tondeuse.executerCommandes()

              tondeuse.afficherEtat()
            }

          case Failure(exception) =>
            println(s"Error parsing input file: ${exception.getMessage}")
            exception.printStackTrace()
            sys.exit(1)
        }

      case Failure(exception) =>
        println(s"Error reading input file: ${exception.getMessage}")
        exception.printStackTrace()
        sys.exit(1)
    }
  }

  def isFileAccessible(filePath: String): Boolean = {
    val file = new File(filePath)
    file.exists() && file.isFile && file.canRead
  }
}
