package priv.sp.update

import priv.sp._

// Some bs for heuristic
case class PlayerStats(var nbSummon : Int = 0, var nbKill : Int = 0, var nbDead : Int = 0, var killCost : Int = 0){
  def reset(){
    nbSummon = 0
    nbKill = 0
    nbDead = 0
    killCost = 0
  }

  def addKill(card : Creature){
    nbKill += 1
    killCost += (card.cost * ((if (card.houseIndex == 4) 1.5 else 1))).toInt // bs
  }

  def + (stats : PlayerStats) = PlayerStats(
    nbSummon + stats.nbSummon,
    nbKill + stats.nbKill,
    nbDead + stats.nbDead,
    killCost + stats.killCost)
}
