/**
 *
 */
package it.sireneo.roster.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import sys.SystemProperties
import it.sireneo.roster.scripts._
import it.sireneo.roster.controls._
import it.sireneo.roster.model.{ Cathecumen, Male, Female }

/**
 * @author home
 *
 */
class CommunityMembersTest extends FunSuite with ShouldMatchers {

  val sysprops = new SystemProperties
  val community: List[Cathecumen] = CommunityLoader loadFromResource "test_elenco_comunita.xml"

  test("Numero di elementi nella comunita'") {
    community should have size (6)
  }

  test("Persone presenti") {
    val names = community map (_.toString)
    names should contain("Perry Mason")
    names should contain("Lucy Torwald")
    names should contain("Lucky Luke")
    names should contain("Mary Jane")
    names should contain("Ivan Hoe")
    names should contain("Carl Lewis")
  }

  test("Dettagli catecumeno") {
    (community withFilter { _.name == "Perry" }) foreach { cat =>
      (cat isMarried) should be(true)
      (cat spouse) should be(Some("Lucy Torwald"))
      (cat hasCar) should be(true)
      (cat hasChildren) should be(true)
      (cat available) should be(true)
      (cat gender) should be(Male)
    }
  }

}