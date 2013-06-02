package priv.sp.house

import collection._
import priv.sp._

trait EarthHouse {
  import CardSpec._
  import GameCardEffect._

  val Earth : House = House("earth", List(
    Creature("Elven", Attack(2), 12, "Heals owner by 3 every turn", effects = effects(OnTurn -> focus(heal(3)))),
    Spell("NaturesCare", "heals target and owner by 8",
      inputSpec = Some(SelectOwnerCreature),
      effects = effects(Direct -> heal(8), Direct -> healCreature(8))),
    Creature("ForestSprite", Attack(1), 22, "attack all opponent creatures", runAttack = MultiTargetAttack),
    Spell("PlantTherapy", "heals by 2 * earth mana",
      effects = effects(Direct -> { env : Env => env.player.heal(2 * env.getMana(3))})),
    Creature("woodsHermit", Attack(1), 13, "Increase earth mana growth by 2", effects = effects(OnTurn -> addMana(2, 3))),
    Spell("Fury", "Deals to opponent the sum of the attacks of the 2 strongest owner creatures", effects = effects(Direct -> fury)),
    Creature("HugeSpider", Attack(4), 21, "Spawn 2 forest spiders around him", effects = effects(Direct -> spider)),
    Creature("Troll", Attack(6), 25, "Every turn heals himself by 4", effects = effects(OnTurn -> focus(healCreature(4)))),
    Spell("StoneShower", "Deals 25 damage to any creature", effects = effects(Direct -> massDamage(25, isSpell = true))),
    Creature("EarthElemental", AttackSources().add(ManaAttack(3)), 49, effects = effects(OnTurn -> addMana(1, 3))),
    Creature("MasterHealer", Attack(3), 35, "Every turn heals by 3 owner and his creatures",
      effects = effects(OnTurn -> focus(heal(3)), OnTurn -> healCreatures(3))),
    Creature("Hydra", Attack(3), 40, "Attack all opponent creatures",
      runAttack = MultiTargetAttack,
      effects = effects(OnTurn -> healCreature(4)))), houseIndex = 3)

  private val forestSpider = new Creature("ForestSpider", Attack(2), 11){
    cost = 1
    houseIndex = Earth.houseIndex
    houseId = Earth.houseId
  }

  private def fury = { env: Env =>
    import env._

    val attack = (player.slots().values.map(_.attack)(breakOut) : Seq[Int]).sorted(math.Ordering.Int.reverse).take(2).sum
    env.otherPlayer.inflict(Damage(attack, env, isSpell = true))
  }

  private def spider = { env: Env =>
    def spawnForestSpiderAt(num : Int){
      if (env.player.value.isInSlotRange(num)){
        val slot = env.player.slots(num)
        if (slot.value.isEmpty){
          slot.add(forestSpider)
        }
      }
    }
    spawnForestSpiderAt(env.selected - 1)
    spawnForestSpiderAt(env.selected + 1)
  }
}
