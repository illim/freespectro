package priv.sp

package object house {
  import priv.sp.update._

  def lowestLife(s1 : SlotUpdate, s2 : SlotUpdate) = if (s2.get.life < s1.get.life) s2 else s1
  def highestLife(s1 : SlotUpdate, s2 : SlotUpdate) = if (s2.get.life > s1.get.life) s2 else s1
  def strongest(s1 : SlotUpdate, s2 : SlotUpdate) = if (s2.get.attack > s1.get.attack) s2 else s1
}
