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
  val ipTxts = (0 to 3).map{ _ =>
    val t = new JTextField(3)
    add(t)
    t
  }
  val connectBtn = addBtn("connect", this)
  val serveBtn = addBtn("serve", this)
  setSize(1024, 200)

  def setEnable(b : Boolean){
    List(connectBtn, serveBtn).foreach { btn =>
      btn.setEnabled(b)
    }
  }

  def actionPerformed(e : ActionEvent){
    e.getActionCommand() match {
      case "serve" =>
        newServer(k => new MasterBoot(k, resources))
      case "connect" =>
        val address = java.net.InetAddress.getByAddress(ipTxts.map{_.getText().toByte}.toArray)
        newServer(k => new SlaveBoot(k, address, resources))
    }
  }

  def newServer(boot : ((GameServer => Unit) => Any)) {
    reset {
      val gameServer = shift { k: (GameServer => Unit) =>
        setEnable(false)
        boot(k)
      }
      setEnable(true)
      world.doInRenderThread {
        resetGame(gameServer)
      }
    }
  }

}
