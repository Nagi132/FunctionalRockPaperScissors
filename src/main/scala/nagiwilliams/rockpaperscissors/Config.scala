package nagiwilliams.rockpaperscissors

import fpinscala.parsing.JSON

case class Config(tournaments: Int, roundsPerMatch: Int, randomSeed: Int, players: IndexedSeq[JSON])
