// src/main/scala/progfun/Main.scala

package progfun

object Main extends App {
  // Taille du terrain (5x5)
  val terrain = (5, 5)

  // Création d'une nouvelle position
  val positionInitiale = Position(3, 3)
  // Création de la direction initiale
  val directionInitiale = Est

  // Création de l'ondeuse avec la position et la direction initiales
  val tondeuse = new Tondeuse(positionInitiale, directionInitiale, terrain)

  // Exécution des commandes
  "AADAADADDA".foreach {
    case 'G' => tondeuse.tournerGauche()
    case 'D' => tondeuse.tournerDroite()
    case 'A' => tondeuse.avancer()
  }

  // Affichage de l'état final
  tondeuse.afficherEtat()
}
