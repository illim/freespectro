package priv.sp

import java.io._
import org.scalatest._
import org.scalatest.matchers._
import java.net._
import java.nio.channels._
import java.nio._
import priv.util.Utils._

class NetSpec extends FlatSpec with ShouldMatchers {
  val startMsg = new Message("start")

  /** "client" should "serialize data" in {
    val bos = new ByteArrayOutputStream()
    val client = new Client(bos)
    client.send(startMsg)
    val arr = bos.toByteArray()
    val buffer = ByteBuffer.wrap(arr)
    buffer.rewind()
    val capacity = buffer.getInt
    capacity should equal(102)
    val a = new Array[Byte](capacity)
    buffer.get(a)
    fromBytes(a) should equal(startMsg)
  }

  "we" should "be able to send a message" in {
    val serverSocketAddr = new InetSocketAddress(12345)
    val serverChannel = ServerSocketChannel.open()
    var ended = false
    val server= new Server(serverSocketAddr, serverChannel, ended, { case (c, m) =>
      assert(m.name == "start")
      println("great")
      ended = true
    })
    val socket = new Socket
    socket.connect(serverSocketAddr, 1000)
    val out = socket.getOutputStream
    val client = new Client(out)
    client.send(startMsg)
    Thread.sleep(1000)
    socket.close()
  }*/

  "we" should "be able to send junk" in {
    val serverSocketAddr = new InetSocketAddress(12345)
    val serverChannel = ServerSocketChannel.open()
    var ended = false
    val server= new Server(serverSocketAddr, serverChannel, ended, { case (c, m) =>
      assert(m.name == "start")
      println("great")
      ended = true
    })
    val socket = new Socket
    socket.connect(serverSocketAddr, 1000)
    val out = socket.getOutputStream
    val client = new DummyClient(out)
    client.yo()
    val is = socket.getInputStream
    val bis = new BufferedReader(new InputStreamReader(is))
    Thread.sleep(1000)
    while(is.available > 0){
      println("client receive " + bis.readLine)
    }
    socket.close()
  }
}

class DummyClient(out : OutputStream) {
  def yo() {
    out.write("yo man".getBytes)
  }
}
