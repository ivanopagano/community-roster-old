package it.sireneo.roster.scripts

import it.sireneo.roster.model.{ Cathecumen, Male, Female }
import xml.XML

/**
 * @author home
 * Crea il database della comunita' in xml
 * <p>Utilizzo: SaveCommunity <nome_file.xml>
 */
object SaveCommunity extends App {

  if (args.length == 0) throw new IllegalArgumentException("Specificare il nome di un file xml per salvare i dati")

  var members: List[Cathecumen] =
    Cathecumen(
      name = "Peter",
      surname = "Pan",
      home = Some("Nowhere Island"),
      gender = Male,
      spouse = Some("Lisa Simpson"),
      hasCar = true,
      canHost = true,
      hasChildren = true) :: Nil

  members +:=
    Cathecumen(
      name = "Lisa",
      surname = "Simpson",
      home = Some("Nowhere Island"),
      gender = Female,
      spouse = Some("Peter Pan"),
      hasCar = true,
      canHost = true,
      hasChildren = true)

  members +:=
    Cathecumen(
      name = "Rob",
      surname = "Roy",
      home = Some("Dublin"),
      gender = Male,
      hasCar = true,
      canHost = true,
      available = false)

  val community =
    <comunita>
      { for (member <- members) yield member.toXml }
    </comunita>

  XML.save(args(0), community)

  printf("file salvato: %s", args(0))
}

