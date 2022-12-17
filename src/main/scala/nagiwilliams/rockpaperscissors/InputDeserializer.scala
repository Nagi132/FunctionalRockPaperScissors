package nagiwilliams.rockpaperscissors

import fpinscala.parsing.JSON

object InputDeserializer {
val jsonTxt=
"""
    {
    "tournament":1000
    }
    """
  def parseInput(fname: String): SeasonDTO = SeasonDTO(10)

  def go = {
    val P = fpinscala.parsing.Reference
    import fpinscala.parsing.ReferenceTypes.Parser
    val json: Parser[JSON] = JSON.jsonParser(P)
    val resultOfParsing = P.run(json)(jsonTxt) // this parses JSON input into a JSON object
  }

}
