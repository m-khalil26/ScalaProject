
package progfun

sealed trait Direction {
  def tournerDroite: Direction
  def tournerGauche: Direction
  def code: Char
}

case object Nord extends Direction {
  def tournerDroite: Direction = Est
  def tournerGauche: Direction = Ouest
  val code: Char = 'N'
}

case object Sud extends Direction {
  def tournerDroite: Direction = Ouest
  def tournerGauche: Direction = Est
  val code: Char = 'S'
}

case object Est extends Direction {
  def tournerDroite: Direction = Sud
  def tournerGauche: Direction = Nord
  val code: Char = 'E'
}

case object Ouest extends Direction {
  def tournerDroite: Direction = Nord
  def tournerGauche: Direction = Sud
  val code: Char = 'O'
}
