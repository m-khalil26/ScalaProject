package funProgTests

import progfun.{Est, Nord, Ouest, Position, Tondeuse}

class TondeuseTest extends munit.FunSuite{
  test("Tondeuse should turn left correctly") {
    val initialTondeuse = new Tondeuse(Position(0, 0), Nord, (5, 5), "")
    val turnedTondeuse = initialTondeuse.tournerGauche()
    assertEquals(turnedTondeuse.direction, Ouest)
  }

  test("Tondeuse should turn right correctly") {
    val initialTondeuse = new Tondeuse(Position(0, 0), Nord, (5, 5), "")
    val turnedTondeuse = initialTondeuse.tournerDroite()
    assertEquals(turnedTondeuse.direction, Est)
  }

  test("Tondeuse should advance correctly within terrain bounds") {
    val initialTondeuse = new Tondeuse(Position(0, 0), Nord, (5, 5), "")
    val advancedTondeuse = initialTondeuse.avancer()
    assertEquals(advancedTondeuse.position, Position(0, 1))
  }

  test("Tondeuse should not advance outside terrain bounds") {
    val initialTondeuse = new Tondeuse(Position(5, 5), Nord, (5, 5), "")
    val advancedTondeuse = initialTondeuse.avancer()
    assertEquals(advancedTondeuse.position, Position(5, 5))
  }

  test("Tondeuse should execute commands correctly") {
    val initialTondeuse = new Tondeuse(Position(1, 2), Nord, (5, 5), "GAGAGAGAA")
    val finalTondeuse = initialTondeuse.executerCommandes()
    assertEquals(finalTondeuse.position, Position(1, 3))
    assertEquals(finalTondeuse.direction, Nord)
  }

  test("Tondeuse should handle unknown commands gracefully") {
    val initialTondeuse = new Tondeuse(Position(0, 0), Nord, (5, 5), "XYZ")
    val finalTondeuse = initialTondeuse.executerCommandes()
    assertEquals(finalTondeuse.position, Position(0, 0))
    assertEquals(finalTondeuse.direction, Nord)
  }

  test("Tondeuse should display state correctly") {
    val tondeuse = new Tondeuse(Position(1, 1), Nord, (5, 5), "")
    tondeuse.afficherEtat()

  }
}
