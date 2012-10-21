package it.sireneo.roster.controls

import it.sireneo.roster._
import model.{ Preparation, WordLiturgy, MassLiturgy, Cathecumen }
import ui.{ LiturgyPreparation, PreparationRequest, PsalmPreparation }
import reflect.BeanProperty
import org.drools.io.ResourceFactory
import org.drools._
import logger.{ KnowledgeRuntimeLogger, KnowledgeRuntimeLoggerFactory }
import org.drools.builder._
import scala.collection.JavaConversions._

trait PreparationsFactory {

  /**
   * Crea una sequenza di preparazioni, di tipo alternato fra liturgia della parola e eucaristia, oppure i salmi nelle case
   * @param requested che tipo di preparazione (liturgie o salmi) e nel caso quante per tipo (parola, eucarestia)
   * @param communityList gli elementi della comunita'
   * @param leaveout indica le persone eventuali da non considerare nelle prime preparazioni per le liturgie
   */
  def makeStandardPreparationSequence(requested: PreparationRequest, communityList: List[Cathecumen], leaveout: List[Cathecumen] = List()): List[Preparation]

}

package rules {

  import model.{ Preparation, PsalmGroup, MassLiturgy, WordLiturgy }

  object RuleEngineHelper {

    import org.drools.spi._

    def addToPreparation(member: Cathecumen, preparation: Preparation): (Cathecumen, Preparation) = {
      if (!(preparation.partecipants contains member)) {
        preparation match {
          case p: WordLiturgy => p.partecipants = member :: p.partecipants
          case p: MassLiturgy => p.partecipants = member :: p.partecipants
          case p: PsalmGroup  => p.partecipants = member :: p.partecipants
        }
        member.assigned = true
      }
      (member, preparation)
    }

    def removeFromPreparation(member: Cathecumen, preparation: Preparation): (Cathecumen, Preparation) = {
      preparation match {
        case p: WordLiturgy => p.partecipants = p.partecipants filter (_ == member)
        case p: MassLiturgy => p.partecipants = p.partecipants filter (_ == member)
        case p: PsalmGroup  => p.partecipants = p.partecipants filter (_ == member)
      }
      member.assigned = false
      (member, preparation)
    }

    def psalmGroup: PsalmGroup = PsalmGroup()

  }

  class State(@BeanProperty var state: String)

}

object RuledPreparationController extends PreparationsFactory {

  //Prepara il KB
  lazy val knowledgeBase = {
    val builder = KnowledgeBuilderFactory.newKnowledgeBuilder()
    builder add (ResourceFactory.newClassPathResource(rulesResource), ResourceType.DRL)
    if (builder.hasErrors)
      Left(builder.getErrors)
    else {
      val kbase = KnowledgeBaseFactory.newKnowledgeBase()
      kbase addKnowledgePackages (builder.getKnowledgePackages)
      Right(kbase)
    }
  }

  def rulesResource: String = "preparation_rules.drl"

  def makeStandardPreparationSequence(requested: PreparationRequest, communityList: List[Cathecumen], leaveout: List[Cathecumen] = List()): List[Preparation] = {

    val decTo0: Int => Int = n => (n - 1) max 0
    val prepSorter = (p1: Preparation, p2: Preparation) => p1.cardinal < p2.cardinal
    println("Sto preparando...")

      /*Applica le regole*/
      def executeRules(kbase: KnowledgeBase): List[Preparation] = {
        //Genera la session
        val session = kbase.newStatefulKnowledgeSession()
        //Aggancia un logger alla session
        //        val logger = KnowledgeRuntimeLoggerFactory.newConsoleLogger(session);
        //inserisce le preparazioni senza nessun partecipante, se necessario
        buildEmptyPreparations(requested, Nil) foreach { session insert _ }
        //inserisce i partecipanti interessati
        (scala.util.Random shuffle communityList).view filterNot { leaveout contains _ } foreach { session insert _ }
        //inserisce il controllo di stato
        session insert (new rules.State(if (requested == PsalmPreparation) "INIT_PSALM" else "INIT"))
        try {
          val maxRepeatWhile = repeatNoMoreThan(if (requested == PsalmPreparation) 0 else 3) _
          session.fireAllRules()
          //al secondo giro rimette tutti gli elementi come disponibili
          leaveout foreach { session insert _ }
          maxRepeatWhile(session.getQueryResults("unfinished preparations").size() > 0) {
            println("ancora un po' di attesa...")
            //Ognuno e' riassegnabile
            communityList foreach { member =>
              member.assigned = false
              session update (session getFactHandle member, member)
            }
            session.fireAllRules()
          }
          (session.getQueryResults("all preparations").iterator map { _.get("$preparation").asInstanceOf[Preparation] }).toList
        } catch {
          case e: Exception =>
            println("C'e' stato un errore durante l'esecuzione delle regole di assegnazione dei gruppi: " + e)
            e.printStackTrace()
            Nil
        } finally {
          session.dispose()
        }
      }

      /*ripete il codice finche' e' valida la condizione, per un massimo di 'times' volte. tail-recursive */
      def repeatNoMoreThan(times: Int)(condition: => Boolean)(code: => Unit) {
        if (times > 0 && condition) {
          code
          repeatNoMoreThan(times - 1)(condition)(code)
        }
      }

      /*Costruisce un elenco ordinato di preparazioni vuote da riempire*/
      def buildEmptyPreparations(left: PreparationRequest, preps: List[Preparation]): List[Preparation] = {
        left match {
          case PsalmPreparation         => Nil
          case LiturgyPreparation(0, 0) => fillCardinality(preps.reverse)
          case LiturgyPreparation(w, 0) => buildEmptyPreparations(LiturgyPreparation(decTo0(w), 0), WordLiturgy() :: preps)
          case LiturgyPreparation(0, m) => buildEmptyPreparations(LiturgyPreparation(0, decTo0(m)), MassLiturgy() :: preps)
          case LiturgyPreparation(w, m) => buildEmptyPreparations(LiturgyPreparation(decTo0(w), decTo0(m)), MassLiturgy() :: WordLiturgy() :: preps)
        }
      }

      /*Aggiunge un indice ad ogni preparazione, in base alla posizione nella lista*/
      def fillCardinality(preparations: List[Preparation]): List[Preparation] = preparations.zipWithIndex map { zipped =>
        val (prep, idx) = zipped
        prep.cardinal = idx
        prep
      }

    knowledgeBase match {
      case Left(errors) =>
        println("Failed to build rules: " + errors)
        Nil
      case Right(kbase) =>
        //se il kb e' stato correttamente preparato esegue le regole
        executeRules(kbase) sortWith prepSorter
    }
  }

}

