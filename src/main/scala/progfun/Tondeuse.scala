package progfun

class Tondeuse(var position: Position, var direction: Direction, val terrain: (Int, Int)) {

  private var _commandes: String = ""

  def commandes: String = _commandes
  def commandes_=(value: String): Unit = {
    _commandes = value
  }

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
      case _ =>
    }
  }

  def executerCommandes(): Unit = {
    for (cmd <- _commandes) {
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
