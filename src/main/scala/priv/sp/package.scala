package priv

package object sp {
  
  def owner = 0
  def opponent = 1
  val playerIds = List(owner, opponent)

  type PlayerId = Int

  def swapPlayer(id: PlayerId) = if (id == owner) opponent else owner
}