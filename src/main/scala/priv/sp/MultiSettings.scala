package priv.sp

import javax.swing._
import java.awt.event._
import scala.util.continuations._
import priv.util.Utils._
import priv.World

class MultiSettings(
  world : World,
  resources : GameResources, resetGame : GameServer => Unit)
 extends JPanel with ActionListener {
  val ipTxt = new JTextField(15)
  add(ipTxt)
  val connectBtn = addBtn("connect", this)
  val serveBtn = addBtn("serve", this)
  val connectLocalBtn = addBtn("connectLocal", this)

  setSize(1024, 200)

  def setEnable(b : Boolean){
    List(connectBtn, serveBtn, connectLocalBtn).foreach { btn =>
      btn.setEnabled(b)
    }
  }

  def actionPerformed(e : ActionEvent){
    e.getActionCommand() match {
      case "serve" =>
        newServer(k => new MasterBoot(k, resources))
      case cmd @ ("connect" | "connectLocal") =>
        val address = java.net.InetAddress.getByName(
          if (cmd == "connect") {
            ipTxt.getText().trim()
          } else {
            "127.0.0.1"
          })
        newServer(k => thread("connectserver") { new SlaveBoot(k, address, resources) })
    }
  }

  def newServer(boot : ((Option[GameServer] => Unit) => Any)) {
    reset {
      val gameServerOpt = shift { k: (Option[GameServer] => Unit) =>
        setEnable(false)
        boot(k)
      }
      setEnable(true)
      gameServerOpt.foreach{ gameServer =>
        world.doInRenderThread {
          resetGame(gameServer)
        }
      }
    }
  }

}
