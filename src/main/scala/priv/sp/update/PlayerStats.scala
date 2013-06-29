package priv.sp.update

import priv.sp._

object PlayerStats {
  def basicKillValue(slot : SlotState) = slot.card.cost * (if (slot.card.houseIndex == 4) 1.5f else 1f)
  def hpManaRatio(slot : SlotState) : Float = (slot.card.life / (0.5 + getCostPowMana(slot.card.cost, slot.card.houseIndex))).toFloat

  def getCostPowMana(m : Int, houseIndex : Int) = {
    if (houseIndex == 4) math.pow(m, 3) else math.pow(m, 2)
  }
}

// Some bs for heuristic
case class PlayerStats(var nbSummon : Int = 0, var nbKill : Int = 0, var nbDead : Int = 0, var killValue : Float = 0){
  var getKillValue : Function[SlotState, Float] = PlayerStats.hpManaRatio _

  def reset(){
    nbSummon = 0
    nbKill = 0
    nbDead = 0
    killValue = 0
  }

  def addKill(slot : SlotState){
    nbKill += 1
    killValue += getKillValue(slot)
  }

  def + (stats : PlayerStats) = PlayerStats(
    nbSummon + stats.nbSummon,
    nbKill + stats.nbKill,
    nbDead + stats.nbDead,
    killValue + stats.killValue)
}
