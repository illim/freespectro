package priv.sp

import java.io._
import java.net._
import java.nio.channels._
import java.nio._
import priv.util.Utils._
import priv.util.GBufferUtils._
import collection.JavaConversions._
import java.lang.reflect.{ Proxy, Method, InvocationHandler }

class Client(out: OutputStream) {
  val b = ByteBuffer allocate 4
  val channel = Channels newChannel out
  def send(m: Message) {
    println("client send " + m.name)
    val bytes = toBytes(m)
    b.rewind()
    b putInt bytes.length
    b.flip()
    channel write b
    try {
      out.write(bytes)
    } catch {
      case t: Throwable ⇒ t.printStackTrace
    }
  }
}

class ClientPeerOut(out: OutputStream) extends InvocationHandler {
  val client = new Client(out)

  def invoke(obj: Any, m: Method, args: Array[Object]) = {
    client send new Message(m.getName, args)
    assert(m.getReturnType() == classOf[Unit], "not managing return value for " + m.getName)
    null
  }
}

