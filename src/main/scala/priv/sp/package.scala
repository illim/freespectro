package priv

package object sp {

  def owner = 0
  def opponent = 1
  val playerIds = List(owner, opponent)
  val slotRange = 0 to 5

  type PlayerId = Int

  def other(id: PlayerId) = if (id == owner) opponent else owner
}
