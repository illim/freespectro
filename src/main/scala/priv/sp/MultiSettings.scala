package priv.sp

import javax.swing._
import java.awt.event._
import priv.util.Utils._
import priv.World

class MultiSettings(
  world: World,
  resources: GameResources, abort: () ⇒ Unit, resetGame: GameServer ⇒ Unit)
    extends JPanel with ActionListener {
  val ipTxt = new JTextField(15)
  add(ipTxt)
  val portTxt = new JTextField(4)
  add(portTxt)
  val connectBtn = addBtn("connect", this)
  val serveBtn = addBtn("serve", this)
  val connectLocalBtn = addBtn("connectLocal", this)

  val combo = new JComboBox(resources.networkInterfaces.map(_.getName).toArray)
  add(combo)
  combo.addActionListener(this)

  setSize(1024, 200)

  def setEnable(b: Boolean) {
    List(connectBtn, serveBtn, connectLocalBtn).foreach { btn ⇒
      btn.setEnabled(b)
    }
  }

  def actionPerformed(e: ActionEvent) {
    val port = portTxt.getText.trim
    if (!port.isEmpty) {
      resources.port = port.toInt
    }
    e.getActionCommand() match {
      case "serve" ⇒
        newServer(k ⇒ new MasterBoot(k, resources))
      case cmd @ ("connect" | "connectLocal") ⇒
        val address = java.net.InetAddress.getByName(
          if (cmd == "connect") {
            ipTxt.getText().trim()
          } else {
            "127.0.0.1"
          })
        newServer(k ⇒ thread("connectserver") { new SlaveBoot(k, address, resources) })
      case _ ⇒
        resources.networkInterface = resources.networkInterfaces.find(_.getName == combo.getSelectedItem())
    }
  }

  def newServer(boot: ((Option[GameServer] ⇒ Unit) ⇒ Any)) {
    abort()
    setEnable(false)
    boot { gameServerOpt ⇒
      setEnable(true)
      gameServerOpt.foreach { gameServer ⇒
        resetGame(gameServer)
      }
    }
  }
}
