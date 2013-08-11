package priv.sp

import java.io._
import java.net._
import priv.sp.bot._
import priv.util.Utils._
import priv.util.TVar
import collection.JavaConversions._
import java.util.concurrent.atomic.AtomicInteger
import scala.util.Random

/**
 * Player plays against game server hiding that it can be local(ai) or remote.
 * In remote play, the master shuffle the cards and send to the slave.
 *
 * TODO clean the code(release sockets...)
 */
trait GameServer {
  def initState() : GameState
  def desc() : GameDesc
  def playerId : PlayerId
  def startingPlayer : PlayerId
  def name : String
  def seed : Long
  resetSeed()

  // None implies surrender
  def waitNextCommand(c : TVar[Option[Option[Command]]], state : GameState)
  def submitCommand(commandOption : Option[Command])
  def resetSeed(){  Random.setSeed(seed) }
  def reset(){ }
  def surrender(){ }
  var abort = { () => }
}

class Local(resources : GameResources) extends GameServer {
  Random.setSeed(System.currentTimeMillis)
  private val shuffle = new CardShuffle(resources.sp.houses)
  val startingPlayer = playerIds(scala.util.Random.nextInt(2))
  private val List((p1Desc, p1State), (p2Desc, p2State)) = shuffle.get(resources.resolveChoices, startingPlayer)

  def initState = GameState(List(PlayerState.init(p1State, p1Desc), PlayerState.init(p2State, p2Desc)))
  val desc = GameDesc(Vector(p1Desc, p2Desc))
  val playerId = opponent
  val bot = new BoundedBot(playerId, desc, resources.sp.houses, resources.heurisChoice)
  val name = "AI-" + bot.heuris.name
  val seed = System.currentTimeMillis
  def waitNextCommand(c : TVar[Option[Option[Command]]], state : GameState) = {
    resources.aiExecutor.submit(
      runnable(c.set(Some(bot.executeAI(state)))))
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

  override def reset(){
    super.reset()
    resources.aiExecutor.submit(runnable(bot.reset()))
  }
}

// remote game server common for master or slave
// retarded code, assuming that the continuation is set before receiving the message
// todo use something like a syncvar
class CommonGameServer(val playerId : PlayerId, val name : String, val initState : GameState, val desc : GameDesc, val startingPlayer : PlayerId, val seed : Long, peer : PeerInterface[CommonInterface]) extends GameServer {
  peer.updateImpl(this)

  val currentTurnId = new AtomicInteger
  @volatile private var ended = false
  var cont = Option.empty[TVar[Option[Option[Command]]]]

  def waitNextCommand(c : TVar[Option[Option[Command]]], state : GameState) {
    this.synchronized{
      if (! ended){
        cont = Some(c)
        currentTurnId.incrementAndGet
      } else c.set(None)
    }
  }
  def submitCommand(commandOption : Option[Command]){
    remoteSubmit(currentTurnId.incrementAndGet, commandOption)
  }

  def end(){
    peer.release()
    this.synchronized{
      ended = true
      cont match {
        case Some(c) => c.set(None) // bullshit not safe (should be none of option(option(...))
        case None => abort()
      }
    }
  }

  // out
  def remoteSubmit(turnId : Int, commandOption : Option[Command]) = {
    peer.proxy.submit(turnId, commandOption)
  }

  // in
  def submit(turnId : Int, commandOption : Option[Command]) {
    require(turnId == currentTurnId.get, turnId + "!=" + currentTurnId.get)
    cont.get.set(Some(commandOption))
    cont = None
  }
  override def surrender(){
    peer.proxy.end()
    peer.ended = true
  }
}


class MasterBoot(k: Option[GameServer] => Unit, resources : GameResources)   {
  private val shuffle = new CardShuffle(resources.sp.houses)
  private val List((p1Desc, p1State), (p2Desc, p2State)) = shuffle.get(resources.resolveChoices)
  def initState = GameState(List(PlayerState.init(p1State, p1Desc), PlayerState.init(p2State, p2Desc)))
  def startingPlayer = owner
  val desc = GameDesc(Vector(p1Desc, p2Desc))
  val seed = System.currentTimeMillis

  val serverSocketAddr = resources.getAddr(resources.port)
  val serverSocket = resources.serverSocket(new ServerSocket())
  serverSocket.setReuseAddress(true)
  serverSocket.setSoTimeout(3 * 60 * 1000)
  serverSocket.bind(serverSocketAddr)
  println("Listening ("+serverSocketAddr+"), waiting for client ...")

  thread("waitclient") {
    val cs = resources.clientSocket(serverSocket.accept())
    val peer = new PeerInterface[SlaveInterface](cs, this)
    peer.proxy.init(initState, desc, seed)
    k(Some(new CommonGameServer(opponent, cs.getInetAddress().toString, initState, desc, startingPlayer, seed, peer)))
  }
}

class SlaveBoot(k: Option[GameServer] => Unit, address : InetAddress, resources : GameResources) {
  resources.multi.release() // BS FIXME
  val socketAddress = new InetSocketAddress(address, resources.port)
  val socketOption = connect()
  val peerOption = socketOption.map{ socket =>
    new PeerInterface[MasterInterface](socket, this)
  }

  def init(state : GameState, desc : GameDesc, seed : Long) = {
    peerOption.foreach{ peer =>
      k(Some(new CommonGameServer(owner, socketAddress.toString, state, desc, owner, seed, peer)))
    }
  }

  def connect(i : Int = 3) : Option[Socket] = {
    if (resources.ended) None else {
      val addr = resources.getAddr(0)
      val socket = new Socket()
      socket.bind(addr)
      try {
        println("Bound to "+addr+", connect to " + socketAddress)
        socket.connect(socketAddress, 10 * 3000)
        Some(socket)
      } catch {
        case t :SocketTimeoutException if i > 0 =>
          println(t.getMessage + " retrying...")
          connect(i -1)
        case t : Throwable =>
          if (i > 0){
            println(t.getMessage + " retrying in 3s...")
            Thread.sleep(3 * 1000)
            connect(i -1)
          } else {
            t.printStackTrace()
            k(None)
            None
          }
      }
    }
  }
}

trait CommonInterface {
  def submit(turnId : Int, c: Option[Command])
  def end()
}

trait SlaveInterface extends CommonInterface {
  def init(state : GameState, desc : GameDesc, seed : Long)
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
      if (!ended){
        obj = ois.readObject()
      }
    }
    println("stop reading stream")
  }

  def updateImpl(impl : AnyRef){
    currentImpl = impl
    methods = currentImpl.getClass.getMethods.toList
  }

  def release(){
    ended = true
    socket.close() // BS TODO use holder in resources
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
