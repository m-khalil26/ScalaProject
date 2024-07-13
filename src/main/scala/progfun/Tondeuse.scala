// src/main/scala/progfun/Tondeuse.scala

package progfun

class Tondeuse(var position: Position, var direction: Direction, val terrain: (Int, Int)) {

  def tournerGauche(): Unit = {
    direction = direction.tournerGauche
  }

  def tournerDroite(): Unit = {
    direction = direction.tournerDroite
  }

  def avancer(): Unit = {
    val (maxX, maxY) = terrain
    val (x, y) = (position.x, position.y)
    direction match {
      case Nord if y < maxY => position = Position(x, y + 1)
      case Sud if y > 0    => position = Position(x, y - 1)
      case Est if x < maxX => position = Position(x + 1, y)
      case Ouest if x > 0  => position = Position(x - 1, y)
      case _ => // Ne fait rien si l’on est sur le bord du terrain
    }
  }

  def afficherEtat(): Unit = {
    println(s"Position: (${position.x}, ${position.y})")
    println(s"Direction: ${direction.code}")
  }
}
