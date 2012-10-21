package it.sireneo.roster.ui

import it.sireneo.roster.controls.{ CommunityLoader, RuledPreparationController }
import it.sireneo.roster.model.{ Preparation, Cathecumen }
import scala.actors._
import Actor._
import Futures._
import scala.swing._
import event._
import Swing._
import scala.collection.JavaConverters._
import javax.swing.UIManager.LookAndFeelInfo
import javax.swing.border.TitledBorder
import javax.swing.{ UIManager, ImageIcon, TransferHandler }
import javax.swing.SpringLayout.Constraints
import javax.swing.{ DefaultCellEditor, JComponent }
import java.awt.GridBagConstraints

object CommunityApplication extends SimpleSwingApplication {

  val communityFile = "elenco_comunita.xml"
  val communityList = (CommunityLoader loadFromResource communityFile) sortBy { _.toString }
  val maxNameSize = communityList map { _.toString.length } max

  //inizializza il RuledPreparationController per caricare e compilare in background il rulebase
  actor { RuledPreparationController.knowledgeBase }
  //anche la testata viene caricata in parallelo
  val headerIcon = future { new ImageIcon(resourceFromClassloader("testata.png")) }

  override def resourceFromClassloader(path: String) = this.getClass.getClassLoader.getResource(path)

  def top = new MainFrame { self =>
    val mainFrame: MainFrame = self

    try {
      (UIManager.getInstalledLookAndFeels find { _.getName == "Nimbus" }) match {
        case Some(info) => UIManager setLookAndFeel info.getClassName
        case _          =>
      }
    } catch {
      case e: Exception =>
      // If LAF is not available, you can set the GUI to another look and feel.
    }

    private class TwoDigitField extends TextField(2) { self =>
      enabled = false
      tooltip = "da 1 a 10"
      def validate(s: String): Boolean = {
        try {
          val intVal = s.toInt
          intVal >= 0 && intVal <= 10
        } catch {
          case e => false
        }
      }
      verifier = { validate(_) }
      inputVerifier = {
        case tdf: TwoDigitField => tdf validate tdf.text
        case _                  => false
      }
      listenTo(self.keys)
      reactions += {
        case KeyReleased(src: TwoDigitField, _, _, _) =>
          if (src.verifier(src.text)) src.foreground = java.awt.Color.BLACK
          else src.foreground = java.awt.Color.RED
      }
    }

    lazy val communityView: ListView[Cathecumen] = new ListView[Cathecumen](communityList) {
      tooltip = "doppio click per i dettagli"
    }
    lazy val leaveoutView: ListView[Cathecumen] = new ListView(Nil)
    leaveoutView.tooltip = "Aggiungi a questo elenco chi non deve essere inserito nei primi gruppi"
    lazy val psalmChoice = new RadioButton("Salmo nelle case")
    lazy val liturgyChoice = new RadioButton("Celebrazioni (indica quante per tipo)")
    lazy val preparationChoice = new ButtonGroup {
      buttons += (psalmChoice, liturgyChoice)
    }
    lazy val prepareButton = new Button("Prepara i gruppi") {
      enabled = false
      reactions += {
        case ButtonClicked(self) =>
          preparationChoice.selected match {
            case Some(`liturgyChoice`) => prepareLiturgy()
            case Some(`psalmChoice`)   => preparePsalm()
            case None                  =>
          }

      }
    }
    lazy val progress = new ProgressBar {
      value = 0
      max = 1
      preferredSize = (60, 20)
    }
    lazy val resultArea = new TextArea(12, 50) {
      border = Swing.EmptyBorder(4)
      editable = false
      lineWrap = true
      charWrap = false
    }
    lazy val resultPane = new ScrollPane(resultArea)

    lazy val wordCount = new TwoDigitField
    lazy val massCount = new TwoDigitField

    val updater = actor {
      loop {
        react {
          case done @ PreparationDone(preparations, message) => Swing.onEDT {
            //Crea una tabella dei risultati dinamicamente
            val resultTable = new ResultTable(done)
            resultPane.contents = resultTable

            println(message :: preparations mkString "\n\n")
            progress.indeterminate = false
            progress.value = 1
          }
        }
      }
    }
    val preparator = new LiturgyPreparator(updater)

    var editorWindow: Option[Window] = None

    title = "Gruppi di preparazione per la liturgia"
    resizable = false

    //Listeners
    listenTo(communityView.mouse.clicks)
    //reactions
    reactions += {
      case MouseClicked(s: ListView[Cathecumen], _, _, 2, _) if (editorWindow.isEmpty) =>
        val selected = s.selection.items(0).asInstanceOf[Cathecumen]
        println("Editing " + selected)

        editorWindow = Some(new Dialog(self) with ClosingParent {
          title = "Modifica " + selected
          contents = CathecumenFormPane(selected, this)
          centerOnScreen()

          def closeChild(): Unit = {
            close()
            dispose()
            editorWindow = None
          }
        })

        editorWindow foreach { _.pack().open() }
    }

    //    menuBar = new MenuBar() {
    //			contents += new Menu("Temi") {
    //				contents ++= (UIManager.getInstalledLookAndFeels map {
    //					info => new MenuItem(info.getName)
    //				})
    //			}
    //    }

    def prepareLiturgy() {
      if (wordCount.inputVerifier(wordCount) && massCount.inputVerifier(massCount)) {
        progress.indeterminate = true
        preparator ! LiturgyPrepare(
          wordCount.text.toInt,
          massCount.text.toInt,
          communityView.listData.toList,
          leaveoutView.listData.toList)
      }
    }
    def preparePsalm() {
      progress.indeterminate = true
      preparator ! PsalmPrepare(communityView.listData.toList)
    }

    contents = new BorderPanel() {
      def mainBox = new BoxPanel(Orientation.Horizontal) {
        border = Swing.EmptyBorder(10)
        contents += new ScrollPane(communityView) {
          border = new TitledBorder("VI Comunita' di S.Ireneo")
          preferredSize = (300, preferredSize.getHeight.toInt)
        }

        contents += new BoxPanel(Orientation.Vertical) {
          contents += Swing.VStrut(155)
          contents += Button(">>") {
            leaveoutView.listData = (leaveoutView.listData union communityView.selection.items).distinct sortBy { _.toString }
          }
          contents += Button("<<") {
            leaveoutView.listData = leaveoutView.listData diff (leaveoutView.selection.items)
          }
        }

        contents += new GridBagPanel {
          //Crea un insieme di constraint standard da riutilizzare per posizionare sul layout gridbag
          type GridPosition = this.Constraints
          import java.awt.{ Insets, Color, GridBagConstraints }
          trait DefaultInsets { self: GridPosition =>
            self.insets = new Insets(2, 2, 2, 2)
          }
          trait HorizontalFilling { self: GridPosition =>
            self.fill = GridBagPanel.Fill.Horizontal
          }
          trait BothFillings { self: GridPosition =>
            self.fill = GridBagPanel.Fill.Both
          }
          trait WidthRemainder { self: GridPosition =>
            self.gridwidth = GridBagConstraints.REMAINDER
          }
          trait LeftAlignment { self: GridPosition =>
            self.anchor = GridBagPanel.Anchor.LineStart
          }
          val standardLeftPositioning = new GridPosition with DefaultInsets with LeftAlignment with WidthRemainder {
            weightx = 1.0
          }
          val fillerPositioning = new GridPosition with DefaultInsets with HorizontalFilling with WidthRemainder
          val centeredPositioning = new GridPosition with DefaultInsets with WidthRemainder
          val countsPanel = new FlowPanel(FlowPanel.Alignment.Trailing)() {
            contents += new Label("della parola") { enabled = false }
            contents += wordCount
            contents += new Label("eucarestia") { enabled = false }
            contents += massCount
            preparationChoice.buttons foreach (b => listenTo(b))
            reactions += {
              case ButtonClicked(`liturgyChoice`) =>
                this.contents foreach { _.enabled = true }
                prepareButton.enabled = true
              case ButtonClicked(`psalmChoice`) =>
                this.contents foreach { _.enabled = false }
                prepareButton.enabled = true
            }
          }

          layout(new Label("Che tipo di preparazioni bisogna organizzare?")) = standardLeftPositioning
          layout(Swing.HGlue) = fillerPositioning
          preparationChoice.buttons foreach { b => layout(b) = standardLeftPositioning }
          layout(countsPanel) = centeredPositioning
          layout(Swing.VStrut(5)) = standardLeftPositioning
          layout(prepareButton) = new GridPosition with LeftAlignment with DefaultInsets
          layout(progress) = fillerPositioning
          layout(Swing.VStrut(35)) = standardLeftPositioning
          layout(new ScrollPane(leaveoutView) {
            border = new TitledBorder("Posticipati")
            leaveoutView.foreground = Color.LIGHT_GRAY
            preparationChoice.buttons foreach (b => listenTo(b))
            reactions += {
              case ButtonClicked(`liturgyChoice`) => leaveoutView.foreground = Color.BLACK
              case ButtonClicked(`psalmChoice`)   => leaveoutView.foreground = Color.LIGHT_GRAY
            }
          }) = new GridPosition with DefaultInsets with BothFillings with WidthRemainder
        }
      }
      layout(new Label(null, headerIcon(), Alignment.Center)) = BorderPanel.Position.North
      layout(mainBox) = BorderPanel.Position.Center
      layout(resultPane) = BorderPanel.Position.South
    }

    centerOnScreen()
  }

  case class LiturgyPrepare(word: Int, mass: Int, community: List[Cathecumen], leaveouts: List[Cathecumen])
  case class PsalmPrepare(community: List[Cathecumen])
  case class PreparationDone(preparations: List[Preparation], message: String)

  class LiturgyPreparator(val resultsReceiver: Actor) extends SwingWorker {
    start()
    def act() {
      loop {
        react {
          case LiturgyPrepare(word, mass, community, leaveout) =>
            (community ++ leaveout) foreach { _.assigned = false }
            val preparations = RuledPreparationController.makeStandardPreparationSequence(
              requested = LiturgyPreparation(word, mass),
              communityList = community,
              leaveout = leaveout)
            resultsReceiver ! PreparationDone(preparations, "I gruppi per la liturgia:\n")
          case PsalmPrepare(community) =>
            community foreach { _.assigned = false }
            val preparations = RuledPreparationController.makeStandardPreparationSequence(
              requested = PsalmPreparation,
              communityList = community)
            resultsReceiver ! PreparationDone(preparations, "I gruppi per il salmo:\n")
        }
      }
    }
  }

  /**
   * Una tabella per mostrare i risultati dell'elaborazione in modo leggibile e modificabile
   */
  private class ResultTable(results: PreparationDone) extends Table { //with TableModelListener { self =>
    import javax.swing.table.DefaultTableModel
    import java.awt.datatransfer.{ Transferable, StringSelection }
    tooltip = results.message
    model = new DefaultTableModel {
      val pa: Array[Preparation] = results.preparations.toArray
      val biggest = pa map { _.partecipants.size } max
      //trasforma l'elenco originale in un array bidimensionale quadrato ottenuto facendo riempimento con delle Option
      val invertedMatrix: Array[Array[Option[Cathecumen]]] = (pa map { _.partecipants.map { Some(_) } padTo (biggest, None) toArray }).toArray.asInstanceOf[Array[Array[Option[Cathecumen]]]]
      override def getColumnName(col: Int) = "%s [%d]" format (pa(col).name, pa(col).partecipants.size)
      override def getRowCount() = biggest
      override def getColumnCount() = pa.size
      override def getValueAt(row: Int, col: Int): String = invertedMatrix(col)(row) map { _.toString } getOrElse ("")
      override def setValueAt(value: Any, row: Int, col: Int) {
        value match {
          case cat: Cathecumen => invertedMatrix(col)(row) = Some(cat)
          case _               => invertedMatrix(col)(row) = None
        }
      }
      override def isCellEditable(row: Int, col: Int) = true
    }
    rowHeight = (font.getSize * 1.5).toInt
    autoResizeMode = Table.AutoResizeMode.Off
    peer.setDefaultEditor(classOf[String], new DefaultCellEditor(new ComboBox("" :: communityList).peer))
    peer.setRowSelectionAllowed(false)
    preferredSize = (maxNameSize * 7 * results.preparations.size, preferredSize.getHeight.toInt)

  }

}
