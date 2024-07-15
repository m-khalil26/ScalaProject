package progfun

class Tondeuse(val position: Position, val direction: Direction, val terrain: (Int, Int), private val _commandes: String) {

  def commandes: String = _commandes

  private def withPosition(newPosition: Position): Tondeuse = new Tondeuse(newPosition, direction, terrain, _commandes)

  private def withDirection(newDirection: Direction): Tondeuse = new Tondeuse(position, newDirection, terrain, _commandes)

  def tournerGauche(): Tondeuse = {
    val newDirection = direction.tournerGauche
    withDirection(newDirection)
  }

  def tournerDroite(): Tondeuse = {
    val newDirection = direction.tournerDroite
    withDirection(newDirection)
  }

  def avancer(): Tondeuse = {
    val (maxX, maxY) = terrain
    val (x, y) = (position.x, position.y)
    val newPosition = direction match {
      case Nord if y < maxY => Position(x, y + 1)
      case Sud if y > 0    => Position(x, y - 1)
      case Est if x < maxX => Position(x + 1, y)
      case Ouest if x > 0  => Position(x - 1, y)
      case _ => position 
    }
    withPosition(newPosition)
  }

  def executerCommandes(): Unit = {
    _commandes.foreach { cmd =>
      cmd match {
        case 'G' => tournerGauche()
        case 'D' => tournerDroite()
        case 'A' => avancer()
        case _ => println(s"Commande inconnue: $cmd")
      }
    }
  }

  def afficherEtat(): Unit = {
    println(s"Position: (${position.x}, ${position.y})")
    println(s"Direction: ${direction.code}")
  }
}
