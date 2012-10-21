package it.sireneo.roster

import controls.{ CommunityLoader, RuledPreparationController }
import ui._

object CommunityPreparationManager extends App {

  if (args.length == 0) throw new IllegalArgumentException("""Specificare le preparazioni da organizzare fra virgolette
  ad es. "parola:5, eucarestia:3" oppure "salmo:1"""")

  val communityFileName = if (args.length > 1) args(1) else "elenco_comunita.xml"

  printf("I gruppi da organizzare %s\n", args(0))

  val preparations = RuledPreparationController.makeStandardPreparationSequence(
    requested = UserInput.parseRequestParams(args(0)),
    communityList = CommunityLoader loadFromResource communityFileName)

  println("I gruppi di preparazione previsti:\n")
  //println(preparations filterNot (_.partecipants.isEmpty) mkString "\n\n")
  println(preparations mkString "\n\n")

}
