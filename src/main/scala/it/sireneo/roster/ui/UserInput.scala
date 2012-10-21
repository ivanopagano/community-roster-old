package it.sireneo.roster.ui

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.Parsers

//Queste classi sono dei wrapper per identificare il tipo di richiesta del client
sealed trait PreparationRequest
case class LiturgyPreparation(word: Int, mass: Int) extends PreparationRequest
case object PsalmPreparation extends PreparationRequest

object UserInput {

  private trait CounterParam
  private case class PsalmCount(val count: Int) extends CounterParam
  private case class WordCount(val count: Int) extends CounterParam
  private case class MassCount(val count: Int) extends CounterParam

  private object ParamRequestParser extends JavaTokenParsers {

    def paramString: Parser[List[CounterParam]] = repsep(param, ",")
    def param: Parser[CounterParam] = wordParam | psalmParam | massParam
    def psalmParam: Parser[CounterParam] = "salmo:" ~> wholeNumber ^^ (n => PsalmCount(n.toInt))
    def wordParam: Parser[CounterParam] = "parola:" ~> wholeNumber ^^ (n => WordCount(n.toInt))
    def massParam: Parser[CounterParam] = "eucarestia:" ~> wholeNumber ^^ (n => MassCount(n.toInt))

    def parse(toParse: String) = parseAll(paramString, toParse)
  }

  import ParamRequestParser._

  def parseRequestParams(params: String): PreparationRequest = ParamRequestParser.parse(params) match {
    case Success(parsed: List[CounterParam], rest) =>
      (parsed foldLeft (LiturgyPreparation(0, 0): PreparationRequest)) {
        case (PsalmPreparation, _)                    => PsalmPreparation
        case (_, PsalmCount(_))                       => PsalmPreparation
        case (LiturgyPreparation(_, m), WordCount(w)) => LiturgyPreparation(w, m)
        case (LiturgyPreparation(w, _), MassCount(m)) => LiturgyPreparation(w, m)
      }
    case Failure(error, rest) => sys.error("C'e' stato un errore:" + error); LiturgyPreparation(0, 0)
  }

}