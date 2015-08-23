package priv.sp

import java.io._
import java.net._
import java.nio.channels._
import java.nio._
import priv.util.Utils._
import priv.util.GBufferUtils._
import collection.JavaConversions._

/**
 * Lot of BS here
 * deserialize client game msg or act as an echo server
 */
case class Message(name: String, args: Array[Object] = null)

class Server(serverSocketAddr: InetSocketAddress, serverChannel: ServerSocketChannel, ended: ⇒ Boolean, handleMessage: (Connection, Message) ⇒ Unit) {
  val timeout = 3 * 60 * 1000
  serverChannel.configureBlocking(false)
  val serverSocket = serverChannel.socket()
  serverSocket.setReuseAddress(true)
  serverSocket.setSoTimeout(timeout)
  serverSocket.bind(serverSocketAddr)
  println("Listening (" + serverSocketAddr + "), waiting for client ...")
  val selector = Selector.open()
  serverChannel.register(selector, SelectionKey.OP_ACCEPT)
  var serverUp = true

  thread("select") {
    while (selector.select(timeout) > 0 && !ended && (serverUp || selector.keys.isEmpty)) {
      val i = selector.selectedKeys().iterator
      while (i.hasNext) {
        val key = i.next()
        i.remove()

        if (key.isValid()) {
          if (key.isAcceptable()) {
            println("accept")
            val server = key.channel().asInstanceOf[ServerSocketChannel]
            val client = server.accept()
            if (client != null) {
              client.configureBlocking(false)
              client.register(selector, SelectionKey.OP_READ | SelectionKey.OP_WRITE)
            }
          } else if (key.isReadable()) {
            try {
              val client = key.channel().asInstanceOf[SocketChannel]
              if (client.isConnected) {
                val c = getOrAttach(key) {
                  new Connection(client, this)
                }
                println("init read " + c.buffer.remaining)
                var count = -1
                if (c.readHeader() == -1) {
                  close(key)
                } else {
                  count = client.read(c.buffer)
                  while (client.isOpen() && count > 0) {
                    count = client.read(c.buffer)
                  }
                  if (count == -1) close(key) else {
                    println("count " + count + ",pos" + c.buffer.position + "," + c.buffer.remaining)
                    c.expectedLength match {
                      case Some(x) if x == c.buffer.position ⇒
                        c.buffer.flip
                        val bytes = bufToArray(c.buffer)
                        fromBytes(bytes) match {
                          case m: Message ⇒
                            println("received msg " + m.name)
                            handleMessage(c, m)
                          case x ⇒ println("msg received not managed " + x)
                        }
                      case _ if c.headBuff.position > 0 || c.buffer.position > 0 ⇒
                        c.buffer.flip
                        val echo = ("echo: "
                          + new String(bufToArray(c.headBuff))
                          + new String(bufToArray(c.buffer)) + "\n")
                        println("echoing")
                        val buff = ByteBuffer.wrap(echo.getBytes)
                        client.write(buff)
                        c.clear()
                        client.close()
                      case _ ⇒
                    }
                  }
                  if (c.isPacketEnded) {
                    c.clear()
                  }
                }
              }
            } catch {
              case t: Throwable ⇒ t.printStackTrace
            }
          }
        }
      }
    }
    println("close selector")
    selector.close()
    serverSocket.close()
  }

  def close(key: SelectionKey) {
    println("close key")
    key.attach(null)
    key.channel().close()
  }

  def bufToArray(b: ByteBuffer) = {
    val bytes = new Array[Byte](b.limit)
    b.rewind
    b.get(bytes)
    bytes
  }

  def getOrAttach[A <: AnyRef](key: SelectionKey)(f: ⇒ A) = {
    var a = key.attachment
    if (a == null) {
      a = f
      key.attach(a)
    }
    a.asInstanceOf[A]
  }
}

class Connection(channel: SocketChannel, server: Server) {
  val maxSize = 8192
  val address = channel.socket.getInetAddress().toString
  val headBuff = ByteBuffer.allocateDirect(4).order(ByteOrder.BIG_ENDIAN) // network byte order

  // WTF IS THAT
  val junk = ByteBuffer.allocateDirect(4).order(ByteOrder.BIG_ENDIAN)
  val buffer = createByteBuffer(maxSize)
  var expectedLength = Option.empty[Int]
  val peer = new MasterPeerInterface[SlaveInterface](channel, server)
  println("Server init connection with " + address)

  def readHeader() = {
    var count = 0L
    if (expectedLength.isEmpty) {
      count = channel.read(Array(junk, headBuff, buffer))
      println("read header" + count)
      if (headBuff.position == 4) {
        headBuff.flip()
        val l = headBuff.getInt(0)
        if (l > 0 && l < maxSize) {
          expectedLength = Some(l)
          println("expected length " + expectedLength)
          buffer.limit(l)
        } else {
          println("unknown length" + l)
        }
      }
    }
    count
  }

  def isPacketEnded = {
    expectedLength.exists(_ == buffer.position)
  }

  def clear() {
    println("clearing")
    headBuff.clear()
    buffer.clear()
    expectedLength = None
  }
}
