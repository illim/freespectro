package priv.sp

import java.io._
import java.net._
import priv.sp.bot._
import priv.util.Utils._
import collection.JavaConversions._
import java.util.concurrent.atomic.AtomicInteger

trait GameServer {
  def initState() : GameState
  def desc() : GameDesc
  def playerId : PlayerId

  def waitNextCommand(k : (Option[Command] => Unit), state : GameState)
  def submitCommand(commandOption : Option[Command])
}

class Local(resources : GameResources) extends GameServer {
  private val shuffle = new CardShuffle(resources.sp)
  private val List((p1Desc, p1State), (p2Desc, p2State)) = shuffle.get()

  def initState = GameState(List(PlayerState(p1State), PlayerState(p2State)))
  val desc = GameDesc(Array(p1Desc, p2Desc))
  val playerId = opponent
  val bot = new BoundedBot(playerId, desc, resources.sp)
  def waitNextCommand(k : (Option[Command] => Unit), state : GameState) = {
    resources.aiExecutor.submit(
      runnable(k(bot.executeAI(state))))
  }

  def submitCommand(commandOption : Option[Command]) = {
    commandOption.foreach{ c =>
      val cardIdx = desc.players(owner).getIndexOfCardInHouse(c.card)
      resources.aiExecutor.submit(runnable {
        bot.updateKnowledge(c, cardIdx)
      })
    }
  }
}

class MasterBoot(k: GameServer => Unit, resources : GameResources)   {
  private val shuffle = new CardShuffle(resources.sp)
  private val List((p1Desc, p1State), (p2Desc, p2State)) = shuffle.get()
  def initState = GameState(List(PlayerState(p1State), PlayerState(p2State)))
  val desc = GameDesc(Array(p1Desc, p2Desc))

  val serverSocketAddr = new InetSocketAddress(4444)
  val serverSocket = resources.serverSocket(new ServerSocket())
  serverSocket.setReuseAddress(true)
  serverSocket.setSoTimeout(3 * 60 * 1000)
  serverSocket.bind(serverSocketAddr)
  println("Listening, waiting for client ...")

  priv.util.Utils.threaded {
    val cs = resources.clientSocket(serverSocket.accept())
    println("Connected to "+ cs.getInetAddress())
    val peer = new PeerInterface[SlaveInterface](cs, this)
    peer.proxy.init(initState, desc)
    k(new CommonGameServer(opponent, initState, desc, peer))
  }
}

class SlaveBoot(k: GameServer => Unit, address : InetAddress, resources : GameResources) {
  val socket = new Socket(address, 4444)
  println("Connected to " + socket.getInetAddress)
  val peer = new PeerInterface[MasterInterface](socket, this)

  def init(state : GameState, desc : GameDesc) = {
    k(new CommonGameServer(owner, state, desc, peer))
  }
}

class CommonGameServer(val playerId : PlayerId, val initState : GameState, val desc : GameDesc, peer : PeerInterface[CommonInterface]) extends GameServer with PeerCommon {
  peer.updateImpl(this)

  def remoteSubmit(turnId : Int, commandOption : Option[Command]) = {
    peer.proxy.submit(turnId, commandOption)
  }
}

trait PeerCommon {
  val currentTurnId = new AtomicInteger
  val defaultCont : (Option[Command] => Unit) =  { c : Option[Command] => ()}
  var cont = defaultCont

  def waitNextCommand(k : (Option[Command] => Unit), state : GameState) {
    cont = k
    currentTurnId.incrementAndGet
  }
  def submitCommand(commandOption : Option[Command]){
    remoteSubmit(currentTurnId.incrementAndGet, commandOption)
  }

  // out
  def remoteSubmit(turnId : Int, commandOption : Option[Command])

  // in
  def submit(turnId : Int, commandOption : Option[Command]) {
    assert(turnId == currentTurnId.get)
    cont(commandOption)
    cont = defaultCont
  }
}

trait CommonInterface {
  def submit(turnId : Int, c: Option[Command])
}

trait SlaveInterface extends CommonInterface {
  def init(state : GameState, desc : GameDesc)
}

trait MasterInterface extends CommonInterface


import java.lang.reflect.{Proxy, Method, InvocationHandler}

class PeerInterface[+O](val socket : Socket, impl : AnyRef) (implicit co : reflect.ClassTag[O]){
  private var currentImpl : AnyRef = impl
  private var methods = impl.getClass.getMethods.toList
  val in = socket.getInputStream
  val out = socket.getOutputStream
  val oos = new ObjectOutputStream(out)
  val ois = new ObjectInputStream(in)
  var ended = false

  val proxy = Proxy.newProxyInstance(
    getClass.getClassLoader(),
		Array(co.erasure),
		new PeerOut(ois, oos)).asInstanceOf[O]

  priv.util.Utils.threaded { // /!\ blocking thread
    var obj = ois.readObject()
    while ( obj != null && !ended) {
      val message = obj.asInstanceOf[Message]
      println("received " + message.name + "/" + Option(message.args).toList.flatten)
      val m = methods.find{ m => m.getName == message.name  }.getOrElse(sys.error(message.name + " method not found"))
      val res = m.invoke(currentImpl, message.args : _*)
      assert(m.getReturnType() == classOf[Unit], "not managing return value for "+m.getName)
      obj = ois.readObject()
    }
  }

  def updateImpl(impl : AnyRef){
    currentImpl = impl
    methods = currentImpl.getClass.getMethods.toList
  }

  def release(){
    ended = true
    socket.close()
  }
}

class PeerOut(in : ObjectInputStream, out : ObjectOutputStream) extends InvocationHandler {

  def invoke(obj : Any, m : Method, args : Array[Object]) = {
    out.writeObject(new Message(m.getName, args))
    out.flush()
    assert(m.getReturnType() == classOf[Unit], "not managing return value for "+m.getName)
    null
  }
}

class Message(val name : String, val args : Array[Object]) extends java.io.Serializable
