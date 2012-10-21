/**
 *
 */
package it.sireneo.roster.controls

import it.sireneo.roster.model.Cathecumen
import xml.XML

/**
 * Contiene i metodi per caricare da xml i dati
 *
 */
object CommunityLoader {

  import java.net.URL
  implicit def urlOption(resource: URL): Option[URL] = if (resource == null) None else Some(resource)

  /**
   * Carica gli elementi della comunita' partendo dalla serializzazione su file xml
   */
  def loadFromFile(fileName: String): List[Cathecumen] = {
    val community = XML loadFile (fileName)
    extract(community)
  }

  /**
   * Carica gli elementi della comunita' partendo dalla serializzazione su file xml ottenuto come risorsa
   */
  def loadFromResource(fileName: String): List[Cathecumen] = {
      def classLoad(name: String) = getClass.getClassLoader.getResource(name)
    val community = XML load (classLoad(fileName.replace(".xml", "-mod.xml")) getOrElse classLoad(fileName))
    extract(community)
  }

  private def extract(community: scala.xml.Elem): List[it.sireneo.roster.model.Cathecumen] = {
    community match {
      case <comunita>{ members @ _* }</comunita> =>
        val communityMembers = for (member @ <catecumeno>{ _* }</catecumeno> <- members) yield Cathecumen.fromXml(member)
        communityMembers.toList
      case _ => Nil
    }
  }

}