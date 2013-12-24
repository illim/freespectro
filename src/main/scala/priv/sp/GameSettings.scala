package priv.sp

import javax.swing._
import javax.swing.event._
import java.awt.event._
import priv.util.Utils._
import collection.JavaConversions._

class GameSettings(resources : GameResources) extends JPanel  {

  class PlayerChoice(id : PlayerId) extends JPanel {
    val specials = resources.sp.houses.special
    val choices = specials.map(_.name).toArray
    val l = new JList(choices)
    l.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
    l.setLayoutOrientation(JList.VERTICAL_WRAP)
    val selModel = l.getSelectionModel
    select(resources.playerChoices(id))
    updateResources()
    add(l)
    val btnPane = new JPanel with ActionListener {
      setLayout(new BoxLayout(this, BoxLayout.Y_AXIS))
      addBtn("all", this)
      addBtn("clear", this)
      addBtn("sinist", this)
      addBtn("others", this)
      addBtn("bs", this)
      def actionPerformed(e : ActionEvent){
        e.getActionCommand() match {
          case "clear" => selModel.clearSelection()
          case "all" => selModel.addSelectionInterval(0, choices.size -1)
          case "sinist" => select(resources.sp.houses.sinist)
          case "others" => select(resources.sp.houses.others)
          case "bs" => select(resources.sp.houses.bs)
        }
      }
    }
    add(btnPane)

    selModel.addListSelectionListener(new ListSelectionListener {
      override def valueChanged(e : ListSelectionEvent){ updateResources()  }
    })

    def updateResources(){
      val selecteds = l.getSelectedValuesList.toSet
      val choices = specials.filter(x => selecteds.contains(x.name))
      resources.playerChoices = resources.playerChoices.updated(id, choices)
    }

    def select(houses : List[House]){
      houses.foreach{ h =>
        val idx = choices.indexOf(h.name)
        if (idx != -1){
          selModel.addSelectionInterval(idx, idx)
        }
      }
    }

  }

  val choices = playerIds.map{ id =>
    val c = new PlayerChoice(id)
    add(c)
    c
  }

  class HeurisChoice extends ActionListener {
    val combo = new JComboBox((0 to 3).map(_.toString).toArray)
    combo.addActionListener(this)
    combo.setSelectedItem("3")
    def actionPerformed(e : ActionEvent){
      resources.heurisChoice = combo.getSelectedItem().toString.toInt
    }
  }

  val hc = new HeurisChoice
  //add(hc.combo)
}

class GameDebug(game : => Game) extends JPanel with ActionListener {
  val showCards = addBtn("showCards", this)
  val dump = addBtn("dump", this)
  val gimmeMana = addBtn("gimmeMana", this)

  def actionPerformed(e : ActionEvent){
    e.getActionCommand() match {
      case "showCards" =>
        game.cardPanels(game.otherPlayerId).cardButtons.foreach(_.visible = true)
      case "gimmeMana" =>
        game.giveMeMana()
      case "dump" =>
        println(game.state)
    }
  }
}

