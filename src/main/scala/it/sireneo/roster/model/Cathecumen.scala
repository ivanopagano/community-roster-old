package it.sireneo.roster.model

import scala.xml._

trait Gender
case object Male extends Gender { override def toString = "uomo" }
case object Female extends Gender { override def toString = "donna" }

object Cathecumen {

  /**
   * Factory method
   */
  def apply(
    name: String,
    surname: String,
    home: Option[String],
    gender: Gender,
    spouse: Option[String] = None,
    hasCar: Boolean = false,
    canHost: Boolean = false,
    hasChildren: Boolean = false,
    available: Boolean = true) = new Cathecumen(name, surname, home, gender, spouse, hasCar, canHost, hasChildren, available)

  /**
   * Ricostruisce l'oggetto dal xml
   */
  def fromXml(xmlNode: Node): Cathecumen = Cathecumen(
    name = (xmlNode \ "nome").text,
    surname = (xmlNode \ "cognome").text,
    home = (xmlNode \ "abitazione").text match { case "nessuna" => None case somewhere => Some(somewhere) },
    gender = (xmlNode \ "sesso").text match { case "uomo" => Male case "donna" => Female },
    spouse = (xmlNode \ "coniuge").text match { case "nessuno" => None case someone => Some(someone) },
    hasCar = (xmlNode \ "macchina").text.toBoolean,
    canHost = (xmlNode \ "casa").text.toBoolean,
    hasChildren = (xmlNode \ "bambini").text.toBoolean,
    available = (xmlNode \ "disponibile").text.toBoolean)
}

/**
 * @author ivano
 * Un fratello o sorella della comunit�
 */
class Cathecumen private (
    val name: String,
    val surname: String,
    val home: Option[String],
    val gender: Gender,
    val spouse: Option[String],
    val hasCar: Boolean,
    val canHost: Boolean,
    val hasChildren: Boolean,
    val available: Boolean,
    var assigned: Boolean = false) {

  /**
   * @return true se e' spostato/a
   */
  def isMarried = spouse.isDefined

  /**
   * Verifica se e' sposato ad un alro/a fratello/sorella
   */
  def isMarriedTo(another: Cathecumen): Boolean = spouse match {
    case Some(spouseName) => spouseName == another.toString
    case _                => false
  }

  override def toString = name + " " + surname

  override def equals(other: Any) = other match {
    case that: Cathecumen =>
      (that canEqual this) &&
        name == that.name &&
        surname == that.surname
    case _ => false
  }

  def canEqual(other: Any) = other.isInstanceOf[Cathecumen]

  override def hashCode() = 41 * (41 + name.hashCode) + surname.hashCode

  /**
   * Converte in xml per facile serializzazione
   */
  def toXml() =
    <catecumeno>
      <nome>{ name }</nome>
      <cognome>{ surname }</cognome>
      <abitazione>{ home match { case None => "nessuna" case Some(address) => address } }</abitazione>
      <sesso>{ gender }</sesso>
      <coniuge>{ spouse match { case None => "nessuno" case Some(spouseName) => spouseName } }</coniuge>
      <macchina>{ hasCar }</macchina>
      <casa>{ canHost }</casa>
      <bambini>{ hasChildren }</bambini>
      <disponibile>{ available }</disponibile>
    </catecumeno>

}