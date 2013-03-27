package priv.sp

import java.io._
import org.scalatest._
import org.scalatest.matchers._

class SerialSpec extends FlatSpec with ShouldMatchers {
  val houses = HouseSingleton

  "we" should "be able to serialize card" in {
    val c = houses.Water.cards(6)
    val bos = new ByteArrayOutputStream
    val oos = new ObjectOutputStream(bos)
    oos.writeObject(c)
    oos.flush()
    val barr = bos.toByteArray
    val bis = new ByteArrayInputStream(barr)
    val ois = new ObjectInputStream(bis)
    val card = ois.readObject()
    println(card)
  }

}
