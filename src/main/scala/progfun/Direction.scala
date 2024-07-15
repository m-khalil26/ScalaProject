package progfun

sealed trait Direction {
  def tournerDroite: Direction
  def tournerGauche: Direction
  def code: Char
}

object Direction {
  def fromChar(char: Char): Option[Direction] = char match {
    case 'N' => Some(Nord)
    case 'S' => Some(Sud)
    case 'E' => Some(Est)
    case 'O' => Some(Ouest)
    case _ => None
  }
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
