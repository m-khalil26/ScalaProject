import progfun.{ConfigParser}

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

          println(s"Configuration loaded successfully:")
          println(s"Chosen input path: $inputPath")
          println(s"Chosen JSON output path: $jsonPath")
          println(s"Chosen CSV output path: $csvPath")
          println(s"Chosen YAML output path: $yamlPath")

          // Check if inputPath is reachable
          if (isFileAccessible(inputPath)) {
            parseInputFile(inputPath)
          } else {
            println(s"Error: Input file '$inputPath' is not accessible.")
            sys.exit(1)
          }

        case Failure(exception) =>
          println(s"Error reading configuration file: ${exception.getMessage}")
          sys.exit(1)
      }

    case Failure(exception) =>
      println(s"Error loading configuration parser: ${exception.getMessage}")
      sys.exit(1)
  }

  def parseInputFile(inputPath: String): Unit = {
    println("Before input file parser creation")
    val inputParserTry = InputParser.fromFile(inputPath)
    println("After input file parser creation")

    inputParserTry match {
      case Success(inputParser) =>
        println("After input file parser before parse")

        val parsedDataTry = inputParser.parse()
        println("After input file parse")

        parsedDataTry match {
          case Success((width, height, mowerParams)) =>
            println(s"Field size: $width x $height")
            mowerParams.zipWithIndex.foreach { case ((x, y, orientation, commands), index) =>
              println(s"Mower ${index + 1}: Position ($x, $y), Orientation $orientation, Commands $commands")
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

  // Function to check if a file is accessible
  def isFileAccessible(filePath: String): Boolean = {
    val file = new File(filePath)
    file.exists() && file.isFile && file.canRead
  }
}
