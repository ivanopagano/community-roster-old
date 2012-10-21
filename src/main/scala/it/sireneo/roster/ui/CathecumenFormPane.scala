package it.sireneo.roster.ui

import it.sireneo.roster.model._
import scala.swing.{GridBagPanel, Label, TextField, Button}
import scala.swing.Swing._
import javax.swing.border.EmptyBorder

trait ClosingParent {
  def closeChild(): Unit
}


class CathecumenFormPane(val cat: Cathecumen, val parent: ClosingParent) extends GridBagPanel {

  import java.awt.{ Insets, GridBagConstraints }
  type GridPosition = this.Constraints

  border = new EmptyBorder(2, 8, 2, 4)

  trait DefaultInsets { self: GridPosition =>
    self.insets = new Insets(2, 3, 2, 2)
  }

  trait WidthRemainder { self: GridPosition =>
    self.gridwidth = GridBagConstraints.REMAINDER
  }

  trait LeftAlignment { self: GridPosition =>
    self.anchor = GridBagPanel.Anchor.LineStart
  }

  trait RightAlignment { self: GridPosition =>
    self.anchor = GridBagPanel.Anchor.LineEnd
  }

  trait Defaults extends DefaultInsets { self: GridPosition =>
    weightx = 1.0
  }

  val labelPositioning = new GridPosition with Defaults with RightAlignment
  val fieldPositioning = new GridPosition with Defaults with LeftAlignment with WidthRemainder
  val textLenght = 20

  layout(new Label("Nome:")) = labelPositioning
  layout(new Label(cat.name)) = fieldPositioning
  layout(new Label("Cognome:")) = labelPositioning
  layout(new Label(cat.surname)) = fieldPositioning
  layout(new Label("Sesso:")) = labelPositioning
  layout(new TextField(cat.gender.toString, textLenght)) = fieldPositioning
  layout(new Label("Coniuge:")) = labelPositioning
  layout(new TextField(cat.spouse getOrElse "", textLenght)) = fieldPositioning
  layout(Button("Chiudi") { parent.closeChild() }) = fieldPositioning

  /*
val name: String,
val surname: String,
val home: Option[String],
val gender: Gender,
val spouse: Option[String],
val hasCar: Boolean,
val canHost: Boolean,
val hasChildren: Boolean,
val available: Boolean,
var assigned: Boolean = false
*/

}

object CathecumenFormPane {
  def apply(c: Cathecumen, parent: ClosingParent) = new CathecumenFormPane(c, parent)
}
