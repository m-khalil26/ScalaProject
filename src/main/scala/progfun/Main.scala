import progfun.{ConfigParser, Direction, Position, Tondeuse}

import java.io.File

object Main {

  val configPath = "src/main/resources/configuration.json"

  def main(args: Array[String]): Unit = {
    val configParser = ConfigParser.fromFile(configPath)

    val configMap = configParser.parse()

    val inputPath = configMap.getOrElse("inputPath", {
      println("Missing 'inputPath' in configuration file.")
      sys.exit(1)
    })
    val jsonPath = configMap.getOrElse("jsonPath", "N/A")
    val csvPath = configMap.getOrElse("csvPath", "N/A")
    val yamlPath = configMap.getOrElse("yamlPath", "N/A")

    if (isFileAccessible(inputPath.toString)) {
      parseInputFile(inputPath.toString)
    } else {
      println(s"Input file at $inputPath is not accessible.")
      sys.exit(1)
    }
  }

  def parseInputFile(inputPath: String): Unit = {
    val inputParser = InputParser.fromFile(inputPath)

    val (width, height, mowerParams) = inputParser.parse()

    mowerParams.zipWithIndex.foreach { case ((x, y, orientation, commands), index) =>
      val initialPosition = Position(x, y)
      val initialDirection = Direction.fromChar(orientation) match {
        case Some(dir) => dir
        case None =>
          println(s"Invalid orientation character: $orientation")
          sys.exit(1)
      }

      val terrainDimensions = (width, height)
      val tondeuse = new Tondeuse(initialPosition, initialDirection, terrainDimensions, commands)

      tondeuse.executerCommandes()
      tondeuse.afficherEtat()
    }
  }

  def isFileAccessible(filePath: String): Boolean = {
    val file = new File(filePath)
    file.exists() && file.isFile && file.canRead
  }
}
