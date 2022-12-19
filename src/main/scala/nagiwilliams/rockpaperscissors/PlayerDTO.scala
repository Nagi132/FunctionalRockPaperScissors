package nagiwilliams.rockpaperscissors

case class Player(name: String, `type`: String, weights: Option[Map[String, Double]] = None)

