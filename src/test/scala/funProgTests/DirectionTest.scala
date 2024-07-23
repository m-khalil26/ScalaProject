package funProgTests

import progfun.{Direction, Est, Nord, Ouest, Sud}

class DirectionTest extends munit.FunSuite {
  test("Nord should turn right to Est and left to Ouest") {
    assertEquals(Nord.tournerDroite, Est)
    assertEquals(Nord.tournerGauche, Ouest)
  }

  test("Sud should turn right to Ouest and left to Est") {
    assertEquals(Sud.tournerDroite, Ouest)
    assertEquals(Sud.tournerGauche, Est)
  }

  test("Est should turn right to Sud and left to Nord") {
    assertEquals(Est.tournerDroite, Sud)
    assertEquals(Est.tournerGauche, Nord)
  }

  test("Ouest should turn right to Nord and left to Sud") {
    assertEquals(Ouest.tournerDroite, Nord)
    assertEquals(Ouest.tournerGauche, Sud)
  }

  test("Direction codes should be correct") {
    assertEquals(Nord.code, 'N')
    assertEquals(Sud.code, 'S')
    assertEquals(Est.code, 'E')
    assertEquals(Ouest.code, 'O')
  }

  test("Direction.fromChar should map characters to directions") {
    assertEquals(Direction.fromChar('N'), Some(Nord))
    assertEquals(Direction.fromChar('S'), Some(Sud))
    assertEquals(Direction.fromChar('E'), Some(Est))
    assertEquals(Direction.fromChar('O'), Some(Ouest))
    assertEquals(Direction.fromChar('X'), None) // Invalid direction
  }
}
