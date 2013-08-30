package priv.sp.house

import collection._
import priv.sp._
import priv.sp.update._
import GameCardEffect._

object Darksider {
  import CardSpec._

  val wolfSpider = new Creature("Wolf spider", Attack(2), 15, "when opponent summons card, deals to opposite creature damage\nequal to cost of that card.", reaction = new WolfSpiderReaction)
  val warlock = new Creature("Warlock", Attack(5), 34, "when opponent casts spell, blocks it and loses 3X life\n(X - cost of spell).", reaction = new WarlockReaction)
  val blackKnight = new Creature("Black knight", Attack(4), 18, "his neighbors attack only slot opposite to black knight.", reaction = new BlackKnightReaction)

  val Darksider : House = House("Darksider", List(
    new Creature("Fallen mage", Attack(4), 12, "when attacks, deals the same damage to all enemy creatures of element\nof opposite creature.", runAttack = new ElemAttack),
    wolfSpider,
    blackKnight,
    new Creature("Dark mystic", AttackSources().add(new ManaAttack(4)), 15, "attack is equal to owner special power.\nWhen friendly elemental creature dies,\nincreases owner special power by 1.", reaction = new DarkMysticReaction),
    new Creature("Faceless horror", Attack(7), 34, "when summoned, opponent loses card of opposite creature.",
      effects = effects(Direct -> horror)),
    new Creature("Lake of oblivion", Attack(0), 45, "when opponent summons card, blocks it till the death of lake.", reaction = new LakeReaction, data = LakeData()),
    warlock,
    new Creature("Black dragon", Attack(8), 39, "immune to all spells and creatures abilities with no exception\n(for example, air 10, earth 2-11, fire 5).\nWhen enters the game, stuns all enemy creatures.",
      reaction = new DragonReaction,
      effects = effects(Direct -> dragon))), eventListener = Some(new CustomListener(new DarksiderEventListener)))

  Darksider.initCards(Houses.basicCostFunc)

  class WolfSpiderReaction extends Reaction {

      final override def onSummon(summoned : SummonEvent) {
        import summoned._
        if (selected.playerId != player.id){
          val damage = Damage(card.cost, Context(selected.playerId, Some(wolfSpider), selected.num), isAbility = true)
          selected.focus()
          player.slots(num).inflict(damage)
        }
      }
  }

  class BlackKnightReaction extends Reaction {
      def cond(num : Int) : Boolean = math.abs(selected.num - num) == 1
      def applyEffect(s : SlotUpdate) = {
        val slotState = s.get
        if (slotState.card != blackKnight){
          val target = if (slotState.target == List(s.num)){
            List(selected.num)
          } else {
            selected.num :: slotState.target
          }
          s.setTarget(target)
        }
      }

      def undoEffect(s : SlotUpdate) = {
        val slotState = s.get
        if (slotState.card != blackKnight){
          val target = slotState.target.filterNot(_ == selected.num)
          if (target.isEmpty) s.setTarget(List(s.num))
          else s.setTarget(target)
        }
      }

      final override def onAdd(slot : SlotUpdate) = {
        if (selected.num == slot.num){
          selected.filledAdjacents.foreach{ s =>
            applyEffect(s)
          }
        } else if (cond(slot.num)) {
          applyEffect(slot)
        }
      }

      final override def onRemove(slot : SlotUpdate){
        if (cond(slot.num)) {
          undoEffect(slot)
        }
      }

      final override def onMyRemove(dead : Option[Dead]) = {
        selected.filledAdjacents.foreach{ s =>
          undoEffect(s)
        }
      }
  }

  class DarkMysticReaction extends Reaction {
      final override def onDeath(dead : Dead) {
        selected.player.houses.incrMana(1, 4)
      }
  }

  def horror = { env : Env =>
    env.getSelectedSlot().oppositeSlot.value.foreach{ s =>
      env.otherPlayer.addDescMod(Destroyed(s.card))
    }
  }

  trait OnCommand{
    def onCommand(c : Command)
  }

  case class LakeData(destroyeds : List[Destroyed] = Nil)
  class LakeReaction extends ReactionWithData[LakeData] with OnCommand {
      def onCommand(c : Command) {
        val destroyed = Destroyed(c.card)
        selected.otherPlayer.addDescMod(destroyed)
        updateData[LakeData](d => d.copy(destroyeds = destroyed :: d.destroyeds))
      }
      override def cleanUp(){
        getData[LakeData].destroyeds.foreach{ d =>
          selected.otherPlayer.removeDescMod(d)
        }
      }
  }

  class WarlockReaction extends Reaction with OnCommand  {
      def onCommand(c : Command) {
        if (c.card.isSpell){
          selected.otherPlayer.addDescMod(Destroyed(c.card))
          selected.otherPlayer.inflict(Damage(3 * c.card.cost, Context(selected.playerId, Some(warlock), selected.num), isAbility = true))
        }
      }
  }

  def dragon = { env : Env =>
    env.otherPlayer.slots.foreach(_.stun())
  }

  class DragonReaction extends Reaction {
    override def heal(amount : Int) { }
    override def inflict(damage : Damage){
      if (!damage.isEffect){  super.inflict(damage)  }
    }
    override def destroy() {  }
    override def stun() {  }
  }

  class DarksiderEventListener extends OwnerDeathEventListener{

    override def init(p : PlayerUpdate){
      super.init(p)
      p.otherPlayer.submitCommand.after { c =>
        player.slots.foreach{ s =>
          s.get.reaction match {
            case r : OnCommand => r.onCommand(c)
            case _ =>
          }
        }
      }
    }
  }
}
