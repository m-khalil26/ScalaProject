package funprog

import progfun.{ConfigParser, Direction, InputParser, Position, Tondeuse}
import upickle.default._
import java.io.{File, PrintWriter}
//import scala.annotation.tailrec
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
  implicit val pointRW: ReadWriter[Point] = macroRW
  implicit val debutRW: ReadWriter[Debut] = macroRW
  implicit val finRW: ReadWriter[Fin] = macroRW
  implicit val tondeuseResultRW: ReadWriter[TondeuseResult] = macroRW
  implicit val limiteRW: ReadWriter[Limite] = macroRW
  implicit val resultRW: ReadWriter[Result] = macroRW
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

    if (isFileAccessible(inputPath)) {
      try {
        val results = parseInputFile(inputPath)
        writeResultsToJson(jsonPath, results)
        writeResultsToCsv(csvPath, results)
        writeResultsToYaml(yamlPath, results)
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

  def writeResultsToJson(jsonPath: String, results: Result): Unit = {
    val file = new File(jsonPath)
    val writer = new PrintWriter(file)
    writer.write(write(results, indent = 4))
    writer.close()
  }

  def writeResultsToCsv(csvPath: String, results: Result): Unit = {
    val file = new File(csvPath)
    val writer = new PrintWriter(file)
    writer.write(
      "limite_x,limite_y,debut_x,debut_y,debut_direction,instructions,fin_x,fin_y,fin_direction\n"
    )
    results.tondeuses.foreach { tondeuse =>
      val debut = tondeuse.debut
      val fin = tondeuse.fin
      val instructions = tondeuse.instructions.mkString("")
      writer.write(
        s"${results.limite.x},${results.limite.y},${debut.point.x},${debut.point.y},${debut.direction},${instructions},${fin.point.x},${fin.point.y},${fin.direction}\n"
      )
    }
    writer.close()
  }

  def writeResultsToYaml(yamlPath: String, results: Result): Unit = {
    val file = new File(yamlPath)
    val writer = new PrintWriter(file)

    writer.write(
      s"limite:\n  x: ${results.limite.x}\n  y: ${results.limite.y}\n"
    )
    writer.write("tondeuses:\n")
    results.tondeuses.foreach { tondeuse =>
      writer.write(s"  - debut:\n")
      writer.write(s"      point:\n")
      writer.write(s"        x: ${tondeuse.debut.point.x}\n")
      writer.write(s"        y: ${tondeuse.debut.point.y}\n")
      writer.write(s"      direction: ${tondeuse.debut.direction}\n")
      writer.write(
        s"    instructions: [${tondeuse.instructions.mkString(", ")}]\n"
      )
      writer.write(s"    fin:\n")
      writer.write(s"      point:\n")
      writer.write(s"        x: ${tondeuse.fin.point.x}\n")
      writer.write(s"        y: ${tondeuse.fin.point.y}\n")
      writer.write(s"      direction: ${tondeuse.fin.direction}\n")
    }
    writer.close()
  }

  def isFileAccessible(filePath: String): Boolean = {
    val file = new File(filePath)
    file.exists() && file.isFile && file.canRead
  }
}
