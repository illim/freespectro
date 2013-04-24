package priv.sp

import javax.swing._
import java.awt.event._

class GameSettings(resources : GameResources) extends JPanel  {

  class PlayerChoice(id : PlayerId) extends ActionListener {
    val combo = new JComboBox(resources.sp.houses.special.toArray)
    combo.addActionListener(this)
    def actionPerformed(e : ActionEvent){
      resources.playerChoices = resources.playerChoices.updated(id, combo.getSelectedItem().asInstanceOf[House])
    }
  }

  val choices = playerIds.map{ id =>
    val c = new PlayerChoice(id)
    add(c.combo)
    c
  }
}

