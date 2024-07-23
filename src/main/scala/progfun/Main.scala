package funprog

import progfun.{ConfigParser, Direction, InputParser, Position, Tondeuse}
import java.io.File
import scala.util.{Failure, Success}

final case class Point(x: Int, y: Int)
final case class Debut(point: Point, direction: String)
final case class Fin(point: Point, direction: String)
final case class TondeuseResult(
    debut: Debut,
    instructions: List[String],
    fin: Fin)
final case class Limite(x: Int, y: Int)
final case class Result(limite: Limite, tondeuses: List[TondeuseResult])

object JsonImplicits {
  implicit val pointRW: upickle.default.ReadWriter[Point] =
    upickle.default.macroRW
  implicit val debutRW: upickle.default.ReadWriter[Debut] =
    upickle.default.macroRW
  implicit val finRW: upickle.default.ReadWriter[Fin] = upickle.default.macroRW
  implicit val tondeuseResultRW: upickle.default.ReadWriter[TondeuseResult] =
    upickle.default.macroRW
  implicit val limiteRW: upickle.default.ReadWriter[Limite] =
    upickle.default.macroRW
  implicit val resultRW: upickle.default.ReadWriter[Result] =
    upickle.default.macroRW
}

object Main {

  import JsonImplicits._

  val configPath = "src/main/resources/configuration.json"

  def main(args: Array[String]): Unit = {
    val configParser = ConfigParser.fromFile(configPath)

    val configMap = configParser.parse() match {
      case Success(map) => map
      case Failure(ex) =>
        println(s"Error parsing the configuration file: ${ex.getMessage}")
        sys.exit(1)
    }
    val jsonPath = configMap.getOrElse(
      "jsonPath", {
        println("Missing 'jsonPath' in configuration file.")
        sys.exit(1)
      }
    )
    println(s"output JSON: $jsonPath")
    val csvPath = configMap.getOrElse(
      "csvPath", {
        println("Missing 'csvPath' in configuration file.")
        sys.exit(1)
      }
    )
    println(s"output CSV: $csvPath")
    val yamlPath = configMap.getOrElse(
      "yamlPath", {
        println("Missing 'yamlPath' in configuration file.")
        sys.exit(1)
      }
    )
    println(s"output YAML: $yamlPath")
    val inputPath = configMap.getOrElse(
      "inputPath", {
        println("Missing 'inputPath' in configuration file.")
        sys.exit(1)
      }
    )

    val fileWriters = new FileWriters

    if (isFileAccessible(inputPath)) {
      try {
        val results = parseInputFile(inputPath)
        fileWriters.writeResultsToJson(jsonPath, results)
        fileWriters.writeResultsToCsv(csvPath, results)
        fileWriters.writeResultsToYaml(yamlPath, results)
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

  def parseInputFile(inputPath: String): Result = {
    val inputParser = InputParser.fromFile(inputPath)
    val (width, height, mowerParams) = inputParser.parse()

    val tondeusesResults = mowerParams.map {
      case (x, y, orientation, commands) =>
        val initialPosition = Position(x, y)
        val initialDirection = Direction.fromChar(orientation) match {
          case Some(dir) => dir
          case None =>
            println(s"Invalid orientation character: $orientation")
            sys.exit(1)
        }

        val terrainDimensions = (width, height)
        val tondeuse = new Tondeuse(
          initialPosition,
          initialDirection,
          terrainDimensions,
          commands
        )

        val finalTondeuse = tondeuse.executerCommandes()
        TondeuseResult(
          Debut(Point(x, y), orientation.toString),
          commands.toList.map(_.toString),
          Fin(
            Point(finalTondeuse.position.x, finalTondeuse.position.y),
            finalTondeuse.direction.code.toString
          )
        )
    }

    Result(Limite(width, height), tondeusesResults)
  }

  def isFileAccessible(filePath: String): Boolean = {
    val file = new File(filePath)
    file.exists() && file.isFile && file.canRead
  }
}
