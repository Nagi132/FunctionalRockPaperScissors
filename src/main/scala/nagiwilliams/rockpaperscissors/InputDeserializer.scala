package nagiwilliams.rockpaperscissors

import fpinscala.parsing.JSON.{JArray, JNumber, JObject, JString}
import fpinscala.parsing.{JSON, Location, ParseError}
import fpinscala.parsing.ReferenceTypes.Parser

object InputDeserializer {
  val jsonTxt_3 =
    """
{
  "tournaments": 1000,
  "roundsPerMatch": 1000,
  "randomSeed": 12345,
  "players": [
  {
    "name": "Last Losing 1",
    "type": "LastLosingMovePlayer"
  },
  {
    "name": "Last Losing 2",
    "type": "LastLosingMovePlayer"
  },
  {
    "name": "Last Winning 1",
    "type": "LastWinningMovePlayer"
  },
  {
    "name": "Majority Losing 1",
    "type": "MajorityLosingMovePlayer"
  },
  {
    "name": "Majority Winning 1",
    "type": "MajorityWinningMovePlayer"
  },
  {
    "name": "Unbiased Random 1",
    "type": "RandomMovePlayer"
  },
  {
    "name": "Biased Random 1",
    "type": "BiasedRandomMovePlayer",
    "weights": {
      "rock": 0.5,
      "paper": 0.25,
      "scissors": 0.25
    }
  },
  {
    "name": "Biased Random 2",
    "type": "BiasedRandomMovePlayer",
    "weights": {
      "rock": 0.8,
      "paper": 0.1,
      "scissors": 0.1
    }
  }
  ]
}
  """
def go={
  val P = fpinscala.parsing.Reference
  val json: Parser[JSON] = JSON.jsonParser(P)
  val resultOfParsing=P.run(json)(jsonTxt_3)
  resultOfParsing.flatMap(j=>parseConfig(j).map(dto=>println(dto)).map(_=>()))
}
  def parseConfig(json: JSON): Either[ParseError, Config] = json match {
    case jObject: JObject =>
      for {
        tournaments <- jObject.get("tournaments") match {
          case jNumber: JNumber => Right(jNumber.get.toInt)
          case _ => Left(ParseError(List((Location("Could not unpack tournaments"), "tournaments"))))
        }
        roundsPerMatch <- jObject.get("roundsPerMatch") match {
          case jNumber: JNumber => Right(jNumber.get.toInt)
          case _ => Left(ParseError(List((Location("Could not unpack roundsPerMatch"), "roundsPerMatch"))))
        }
        randomSeed <- jObject.get("randomSeed") match {
          case jNumber: JNumber => Right(jNumber.get.toInt)
          case _ => Left(ParseError(List((Location("Could not unpack randomSeed"), "randomSeed"))))
        }
        players <- jObject.get("players") match {
          case jArray: JArray => Right(jArray.get)
          case _ => Left(ParseError(List((Location("Could not unpack players"), "players"))))
        }
      } yield Config(tournaments, roundsPerMatch, randomSeed, players)
    case _ => Left(ParseError(List((Location("Could not unpack JSON contents"), "Could not unpack JSON contents"))))
  }

  def parsePlayer(json: JSON): Either[ParseError, Player] = json match {
    case jObject: JObject =>
      for {
        name <- jObject.get("name") match {
          case jString: JString => Right(jString.get)
          case _ => Left(ParseError(List((Location("Could not unpack name"), "name"))))
        }
        playerType <- jObject.get("type") match {
          case jString: JString => Right(jString.get)
          case _ => Left(ParseError(List((Location("Could not unpack type"), "type"))))
        }
        weights <- jObject.get("weights") match {
          case jObject: JObject =>
            for {
              rock <- unpackNumber(jObject, "rock")
              paper <- unpackNumber(jObject, "paper")
              scissors <- unpackNumber(jObject, "scissors")
            } yield Some(Map("rock" -> rock, "paper" -> paper, "scissors" -> scissors))
          //case None => Right(None)
          case _ => Left(ParseError(List((Location("Could not unpack weights"), "weights"))))
        }
      } yield Player(name, playerType, weights)
    case _ => Left(ParseError(List((Location("Could not unpack players"), "players"))))
  }

  def unpackNumber(jObject: JObject, key: String): Either[ParseError, Double] = jObject.get(key) match {
    case jNumber: JNumber => Right(jNumber.get)
    case _ => Left(ParseError(List((Location("Could not unpack ticker"), "ticker"))))
  }


}