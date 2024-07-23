package funProgTests

import better.files._
import munit.FunSuite
import funprog.{Debut, FileWriters, Fin, Limite, Point, Result, TondeuseResult}

class FileWritersTest extends FunSuite {

  val sampleResults = Result(
    Limite(5, 5),
    List(
      TondeuseResult(
        Debut(Point(1, 2), "N"),
        List("G", "A", "G", "A", "G", "A", "G", "A", "A"),
        Fin(Point(1, 3), "N")
      ),
      TondeuseResult(
        Debut(Point(3, 3), "E"),
        List("A", "A", "D", "A", "A", "D", "A", "D", "D", "A"),
        Fin(Point(5, 1), "E")
      )
    )
  )

  test("writeResultsToJson should write results to a JSON file") {
    val jsonFile = File.newTemporaryFile("results", ".json")
    try {
      val fileWriters = new FileWriters
      fileWriters.writeResultsToJson(jsonFile.pathAsString, sampleResults)

      val content = jsonFile.contentAsString
      assert(content.contains("\"limite\""))
      assert(content.contains("\"tondeuses\""))
    } finally {
      jsonFile.delete(swallowIOExceptions = true)
    }
  }

  test("writeResultsToCsv should write results to a CSV file") {
    val csvFile = File.newTemporaryFile("results", ".csv")
    try {
      val fileWriters = new FileWriters
      fileWriters.writeResultsToCsv(csvFile.pathAsString, sampleResults)

      val content = csvFile.contentAsString
      assert(content.contains("5,5,1,2,N,GAGAGAGAA,1,3,N"))
    } finally {
      csvFile.delete(swallowIOExceptions = true)
    }
  }

  test("writeResultsToYaml should write results to a YAML file") {
    val yamlFile = File.newTemporaryFile("results", ".yaml")
    try {
      val fileWriters = new FileWriters
      fileWriters.writeResultsToYaml(yamlFile.pathAsString, sampleResults)

      val content = yamlFile.contentAsString
      assert(content.contains("limite:"))
      assert(content.contains("tondeuses:"))
    } finally {
      yamlFile.delete(swallowIOExceptions = true)
    }
  }
}
