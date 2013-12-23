package priv.sp

import java.io._
import java.net._
import java.nio.channels._
import java.nio._
import priv.sp.bot._
import priv.util.Utils._
import priv.util.GBufferUtils._
import priv.util.TVar
import collection.JavaConversions._
import java.util.concurrent.atomic.AtomicInteger
import java.lang.reflect.{Proxy, Method, InvocationHandler}
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
  def resetSeed(){ Random.setSeed(seed) }
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
//  val bot = new BoundedBot(playerId, desc, resources.sp.houses, resources.heurisChoice)
//  val name = "AI-" + bot.heuris.name
  val bot = new BoundedBot2(playerId, desc, resources.sp.houses)
  val name = "AI"
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

trait CommonInterface {
  def submit(turnId : Int, c: Option[Command])
  def end()
}

trait SlaveInterface extends CommonInterface {
  def init(state : GameState, desc : GameDesc, seed : Long)
}

trait MasterInterface extends CommonInterface {
  def start()
}

// remote game server common for master or slave
// retarded code, assuming that the continuation is set before receiving the message
// todo use something like a syncvar
class CommonGameServer(val playerId : PlayerId, val name : String, val initState : GameState, val desc : GameDesc, val startingPlayer : PlayerId, val seed : Long, peer : PeerInterface[CommonInterface]) extends GameServer {
  peer.delegate.updateImpl(this)

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
    surrender()
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
    if (!peer.ended){
      peer.ended = true
      peer.proxy.end()
    }
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
  val serverChannel = resources.serverSocket(ServerSocketChannel.open())
  val server = new Server(serverSocketAddr, serverChannel, resources.ended, {
    case (c , m) =>
    if (m.name == "start"){
      c.peer.proxy.init(initState, desc, seed)
      k(Some(new CommonGameServer(opponent, c.address, initState, desc, startingPlayer, seed, c.peer)))
    } else {
      c.peer.delegate.send(m)
    }
  })
}

trait PeerInterface[+O] {
  def proxy : O
  def release()
  def delegate : Delegate
  var ended = false
}

class MasterPeerInterface[+O](channel : SocketChannel, server : Server)(implicit co : reflect.ClassTag[O]) extends PeerInterface[O]{
  val proxy = Proxy.newProxyInstance(
    getClass.getClassLoader(),
		Array(co.erasure),
		new MasterPeerOut(channel)).asInstanceOf[O]
  val delegate = new Delegate(this)

  def release(){
    server.serverUp = false
    println("master interface released")
  }
}

class MasterPeerOut(channel : SocketChannel) extends InvocationHandler {

  def invoke(obj : Any, m : Method, args : Array[Object]) = {
    val bytes = ByteBuffer.wrap(toBytes(new Message(m.getName, args)))
    channel.write(bytes)
    println("server send " + m.getName)
    assert(m.getReturnType() == classOf[Unit], "not managing return value for "+m.getName)
    null
  }
}

class SlaveBoot(k: Option[GameServer] => Unit, address : InetAddress, resources : GameResources) {
  resources.multi.release() // BS FIXME
  val socketAddress = new InetSocketAddress(address, resources.port)
  val socketOption = connect()
  val peerOption = socketOption.map{ socket =>
    val interface = new SlavePeerInterface[MasterInterface](socket, this)
    interface.proxy.start()
    interface
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
        resources.clientSocket(socket)
        println("Connected")
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

class Delegate(private var impl : AnyRef) {
  private var methods = impl.getClass.getMethods.toList

  def send(message : Message) {
    println("received " + message.name + "/" + Option(message.args).toList.flatten)
    val m = methods.find{ m => m.getName == message.name  }.getOrElse(sys.error(message.name + " method not found"))
    val res = m.invoke(impl, message.args : _*)
    assert(m.getReturnType() == classOf[Unit], "not managing return value for "+m.getName)
  }

  def updateImpl(newImpl : AnyRef){
    impl = newImpl
    methods = impl.getClass.getMethods.toList
  }
}

class SlavePeerInterface[+O](val socket : Socket, impl : AnyRef)(implicit co : reflect.ClassTag[O]) extends PeerInterface[O] {
  val out = socket.getOutputStream
  val oos = new ObjectOutputStream(out)
  val delegate = new Delegate(impl)

  val proxy = Proxy.newProxyInstance(
    getClass.getClassLoader(),
		Array(co.erasure),
		new ClientPeerOut(out)).asInstanceOf[O]

  thread("listenpeer") { // /!\ blocking thread
    val in = socket.getInputStream
    var ois = new ObjectInputStream(in)
    try {
      var obj = ois.readObject()
      while ( obj != null) {
        delegate send obj.asInstanceOf[Message]
        ois = new ObjectInputStream(in) // I'm just soo bored
        obj = ois.readObject()
      }
    } catch { case t : Throwable =>
      if (ended){
        println("stop reading stream " + t)
      } else {
        t.printStackTrace
      }
    }
  }

  println("listening")

  def release(){
    ended = true
    socket.close() // BS TODO use holder in resources
  }
}
