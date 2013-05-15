package priv.sp

import java.io._
import java.net._
import priv.sp.bot._
import priv.util.Utils._
import priv.util.TVar
import collection.JavaConversions._
import java.util.concurrent.atomic.AtomicInteger

/**
 * Player plays against game server hiding that it can be local(ai) or remote.
 * In remote play, the master shuffle the cards and send to the slave.
 */
trait GameServer {
  def initState() : GameState
  def desc() : GameDesc
  def playerId : PlayerId
  def name : String

  def waitNextCommand(c : TVar[Option[Command]], state : GameState)
  def submitCommand(commandOption : Option[Command])
}

class Local(resources : GameResources) extends GameServer {
  private val shuffle = new CardShuffle(resources.sp.houses)
  private val List((p1Desc, p1State), (p2Desc, p2State)) = shuffle.get(resources.playerChoices)

  def initState = GameState(List(PlayerState.init(p1State, p1Desc), PlayerState.init(p2State, p2Desc)))
  val desc = GameDesc(Vector(p1Desc, p2Desc))
  val playerId = opponent
  val name = "AI"
  val bot = new BoundedBot(playerId, desc, resources.sp)
  def waitNextCommand(c : TVar[Option[Command]], state : GameState) = {
    resources.aiExecutor.submit(
      runnable(c.set(bot.executeAI(state))))
  }

  def submitCommand(commandOption : Option[Command]) = {
    commandOption.foreach{ c =>
      val cardIdx = desc.players(owner).getIndexOfCardInHouse(c.card)
      if (cardIdx != -1){
        resources.aiExecutor.submit(runnable {
          bot.updateKnowledge(c, cardIdx)
        })
      }
    }
  }
}

// remote game server common for master or slave
// retarded code, assuming that the continuation is set before receiving the message
// todo use something like a syncvar
class CommonGameServer(val playerId : PlayerId, val name : String, val initState : GameState, val desc : GameDesc, peer : PeerInterface[CommonInterface]) extends GameServer {
  peer.updateImpl(this)

  val currentTurnId = new AtomicInteger
  var cont = Option.empty[TVar[Option[Command]]]

  def waitNextCommand(c : TVar[Option[Command]], state : GameState) {
    cont = Some(c)
    currentTurnId.incrementAndGet
  }
  def submitCommand(commandOption : Option[Command]){
    remoteSubmit(currentTurnId.incrementAndGet, commandOption)
  }

  // out
  def remoteSubmit(turnId : Int, commandOption : Option[Command]) = {
    peer.proxy.submit(turnId, commandOption)
  }

  // in
  def submit(turnId : Int, commandOption : Option[Command]) {
    require(turnId == currentTurnId.get, turnId + "!=" + currentTurnId.get)
    cont.get.set(commandOption)
    cont = None
  }
}


class MasterBoot(k: GameServer => Unit, resources : GameResources)   {
  private val shuffle = new CardShuffle(resources.sp.houses)
  private val List((p1Desc, p1State), (p2Desc, p2State)) = shuffle.get(resources.playerChoices)
  def initState = GameState(List(PlayerState.init(p1State, p1Desc), PlayerState.init(p2State, p2Desc)))
  val desc = GameDesc(Vector(p1Desc, p2Desc))

  val serverSocketAddr = new InetSocketAddress(4444)
  val serverSocket = resources.serverSocket(new ServerSocket())
  serverSocket.setReuseAddress(true)
  serverSocket.setSoTimeout(3 * 60 * 1000)
  serverSocket.bind(serverSocketAddr)
  println("Listening, waiting for client ...")

  thread("waitclient") {
    val cs = resources.clientSocket(serverSocket.accept())
    val peer = new PeerInterface[SlaveInterface](cs, this)
    peer.proxy.init(initState, desc)
    k(new CommonGameServer(opponent, cs.getInetAddress().toString, initState, desc, peer))
  }
}

class SlaveBoot(k: GameServer => Unit, address : InetAddress, resources : GameResources) {
  val socket = new Socket(address, 4444)
  val peer = new PeerInterface[MasterInterface](socket, this)

  def init(state : GameState, desc : GameDesc) = {
    k(new CommonGameServer(owner, socket.getInetAddress().toString, state, desc, peer))
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

  thread("listenpeer") { // /!\ blocking thread
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
