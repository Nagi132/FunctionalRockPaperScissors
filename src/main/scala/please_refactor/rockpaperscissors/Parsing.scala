import fpinscala.parsing.JSON.{JArray, JBool, JNumber, JObject, JString}
import fpinscala.parsing.{JSON, Location, ParseError}

import scala.::
import scala.collection.IterableOnce.iterableOnceExtensionMethods


object Parsing {

  val jsonTxt= {
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
  }

  def go = {
    val P = fpinscala.parsing.Reference
    import fpinscala.parsing.ReferenceTypes.Parser
    val json: Parser[JSON] = JSON.jsonParser(P)
    val resultOfParsing = P.run(json)(tournamentConfig.json) // this parses JSON input into a JSON object
    resultOfParsing.flatMap(j => betterUnpackUsingForComprehension(j)).map(dto => println(dto)).map(_ => ())
  }
  case class DTO(
                        tournaments: Int,
                        roundsPerMatch: Int,
                        randomSeed: Double,
                        players: List[name:String,type:String,weights:String,Map[rock:String,]]
                      )

  def betterUnpackUsingForComprehension(json: JSON): Either[ParseError, SampleDTO] =
    json match {
      case jObject: JObject =>
        for {
          tournaments <- unpackNumber(jObject, "tournaments")
          roundsPerMatch <- unpackNumber(jObject, "roundsPerMatch")
          randomSeed <- unpackNumber(jObject, "randomSeed")
          players <-
          for {
          name <- unpackString (jObject, "name")
          //'type'<- unpackString (jObject, "type")
          weights<-for{
            rock<-unpackNumber(jObject,"rock")
            paper<-unpackNumber(Object,"paper")
            scissors<-unpackNumber(Object,"scissors")
          }

        }
        } yield DTO(tournaments, roundsPerMatch, randomSeed, players,name,weights)
      case _ => Left(ParseError(List((Location("Could not unpack JSON contents"), "Could not unpack JSON contents"))))
    }

  def unpackString(jObject: JObject, key: String): Either[ParseError, String] = jObject.get(key) match {
    case jString: JString => Right(jString.get)
    case _ => Left(ParseError(List((Location("Could not unpack ticker"), "ticker"))))
  }

  def unpackBoolean(jObject: JObject, key: String): Either[ParseError, Boolean] = jObject.get(key) match {
    case jBool: JBool => Right(jBool.get)
    case _ => Left(ParseError(List((Location("Could not unpack ticker"), "ticker"))))
  }

  def unpackNumber(jObject: JObject, key: String): Either[ParseError, Double] = jObject.get(key) match {
    case jNumber: JNumber => Right(jNumber.get)
    case _ => Left(ParseError(List((Location("Could not unpack ticker"), "ticker"))))
  }

  def unpackArray(jObject: JObject, key: String): Either[ParseError, List[String]] = {
    for {
      relatedPacked <- jObject.get(key) match {
        case jArray: JArray => Right(jArray.get)
        case _ => Left(ParseError(List((Location("Could not unpack related"), "related"))))
      }
      related <- unpackList(relatedPacked.toList, Right(List.empty))
    } yield related
  }
}