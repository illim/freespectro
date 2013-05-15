package priv

package object sp {

  def owner = 0
  def opponent = 1
  val playerIds = List(owner, opponent)
  val nbSlots = 6
  val slotRange = 0 until nbSlots
  def inSlotRange(n : Int) = n > -1 && n < 6
  def adjacents(n : Int) = List(n -1, n+1).filter(inSlotRange _)
  def slotInterval(n : Int, m : Int) = (math.max(0, n) to math.min(5, m))

  type PlayerId = Int

  def other(id: PlayerId) = if (id == owner) opponent else owner
}
