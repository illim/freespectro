package priv.sp

import javax.swing._
import java.awt.event._
import priv.util.Utils._

class GameSettings(resources : GameResources) extends JPanel  {

  class PlayerChoice(id : PlayerId) extends ActionListener {
    val specials = resources.sp.houses.special
    val choices = ("Random" :: specials.map(_.name)).toArray
    val combo = new JComboBox(choices)
    combo.setSelectedItem(resources.playerChoices(id).map(_.name).getOrElse("Random"))
    combo.addActionListener(this)
    def actionPerformed(e : ActionEvent){
      val choice = specials.find(_.name == combo.getSelectedItem())
      resources.playerChoices = resources.playerChoices.updated(id, choice)
    }
  }

  val choices = playerIds.map{ id =>
    val c = new PlayerChoice(id)
    add(c.combo)
    c
  }
}

class GameDebug(game : => Game) extends JPanel with ActionListener {
  val showCards = addBtn("showCards", this)

  def actionPerformed(e : ActionEvent){
    e.getActionCommand() match {
      case "showCards" =>
        game.cardPanels(game.otherPlayerId).cardButtons.foreach(_.visible = true)
    }
  }
}

