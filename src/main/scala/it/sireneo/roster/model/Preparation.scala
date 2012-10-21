package it.sireneo.roster.model
import reflect.BeanProperty

/**
 * Il generico tipo di una preparazione
 */
abstract sealed class Preparation(@BeanProperty var cardinal: Int = Int.MinValue) {
  /** il nome della liturgia*/
  def name: String
  /** quante sono le persone minime*/
  def groupSize: Int
  /** i coniugi preparano insieme*/
  def spousesTogether: Boolean
  /** le persone assegnate*/
  def partecipants: List[Cathecumen]

  /**metodi di semplificazione per le conseguenze del rule engne, scritte in java*/
  def hasDriver: Boolean = partecipants exists { _.hasCar }
  def isFull: Boolean = partecipants.size >= groupSize
  def hasChildren: Boolean = partecipants exists { _.hasChildren }
  def hasHost: Boolean = partecipants exists { _.canHost }

  override def toString = name + partecipants.mkString(" [", ", ", "] ") + partecipants.length + " persone"

  override def equals(other: Any) = other match {
    case that: Preparation =>
      (that canEqual this) &&
        name == that.name &&
        groupSize == that.groupSize &&
        spousesTogether == that.spousesTogether &&
        partecipants == that.partecipants
    case _ => false
  }

  def canEqual(other: Any) = other.isInstanceOf[Preparation]

  override def hashCode() = 41 * (41 * (41 * (41 + name.hashCode) + groupSize) + spousesTogether.hashCode) + partecipants.hashCode
}

object Preparation {

  val WORD_LITURGY_SIZE = 5
  val MASS_LITURGY_SIZE = 4
  val PSALM_GROUP_SIZE = 6

}

/**
 * La liturgia settimanale della parola
 */
case class WordLiturgy(var partecipants: List[Cathecumen] = Nil) extends Preparation {
  val name = "Liturgia della Parola"
  val groupSize = Preparation.WORD_LITURGY_SIZE
  val spousesTogether = true
}

/**
 * La liturgia eucaristica del sabato sera
 */
case class MassLiturgy(var partecipants: List[Cathecumen] = Nil) extends Preparation {
  val name = "Eucarestia"
  val groupSize = Preparation.MASS_LITURGY_SIZE
  val spousesTogether = true
}

/**
 * La liturgia casalinga dei salmi
 */
case class PsalmGroup(var partecipants: List[Cathecumen] = Nil) extends Preparation {
  val name = "Salmo nelle case"
  val groupSize = Preparation.PSALM_GROUP_SIZE
  val spousesTogether = true
}