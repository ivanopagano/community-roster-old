package it.sireneo.roster.test

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import sys.SystemProperties
import it.sireneo.roster.scripts._
import it.sireneo.roster.controls._
import it.sireneo.roster.model._

/**
 * @author Ivano
 * Test sulle operazioni di creazione dei gruppi di preparazione specifici
 *
 */
class PreparationObjectTest extends FunSuite with ShouldMatchers {

  val sysprops = new SystemProperties
  val community: List[Cathecumen] = CommunityLoader loadFromResource "elenco_comunita.xml"

  test("Crea i gruppi di preparazione e verifica le proprieta'") {

    val mass = new MassLiturgy(community)
    val word = new WordLiturgy(community)
    mass should not equal (word)
    mass.groupSize should be(4)
    (mass name) should be("Eucarestia")
    (mass partecipants) should equal(community)
  }

}