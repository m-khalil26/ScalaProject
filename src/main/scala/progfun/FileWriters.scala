package funprog

import upickle.default._
import java.io.{File, PrintWriter}

class FileWriters {
  import JsonImplicits._

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
}
