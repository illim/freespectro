package priv.sp


sealed trait Card {
  def image : String
  def isSpell : Boolean
  def inputSpecs : CardInputSpecs
  var cost = 0
}
case class Creature(name: String, attack : Option[Int], life : Int, 
    inputSpecs : CardInputSpecs = CardInputSpecs(List(SelectOwnerSlot))) extends Card {
  def isSpell = false
  def image = name + ".JPG"
}
case class Spell(name: String, inputSpecs : CardInputSpecs = CardInputSpecs(Nil)) extends Card{
  def isSpell = true
  def image = name + ".tga"
}

case class House(name: String, cards: List[Card]) {
  cards.zipWithIndex.foreach { case (c, i) => c.cost = i + 1 }
}

object Houses {

  val FireHouse = House("fire", List(
    Creature("GoblinBerserker"  , Some(4), 16),
    Creature("WallofFire"       , Some(0), 5),
    Creature("PriestOfFire"     , Some(3), 13),
    Creature("FireDrake"        , Some(4), 18),
    Creature("OrcChieftain"     , Some(3), 16),
    Spell("FlameWave"),
    Creature("MinotaurCommander", Some(6), 20),
    Creature("Bargul"           , Some(8), 25),
    Spell("Inferno"),
    Creature("FireElemental"    , None, 36),
    Spell("Armageddon"),
    Creature("Dragon"           , Some(9), 41)))

  val WaterHouse = House("water", List(
    Spell("Meditation"),
    Creature("SeaSprite"        , Some(5), 22),
    Creature("MerfolkApostate"  , Some(3), 10),
    Creature("IceGolem"         , Some(4), 12),
    Creature("MerfolkElder"     , Some(3), 16),
    Creature("IceGuard"         , Some(4), 20),
    Creature("GiantTurtle"      , Some(5), 17),
    Spell("AcidicRain"),
    Creature("MerfolkOverlord",   Some(7), 34),
    Creature("WaterElemental",    None,38),
    Creature("MindMaster",        Some(6), 22),
    Creature("AstralGuard",       Some(1), 17)))

  val AirHouse = House("air", List(
    Creature("FaerieApprentice",  Some(4), 11),
    Creature("Griffin",           Some(3), 15),
    Spell("CalltoThunder"),
    Creature("FaerieSage",        Some(4), 19),
    Creature("WallOfLightning",   Some(0), 28),
    Spell("LightningBolt"),
    Creature("Phoenix",           Some(6), 18),
    Spell("ChainLightning"),
    Creature("LightningCloud",    Some(4), 20),
    Spell("Tornado"),
    Creature("AirElemental",      None, 44),
    Creature("Titan",             Some(9), 40)))

  val EarthHouse = House("earth", List(
    Creature("ElvenHealer",       Some(2), 12),
    Spell("NaturesRitual"),
    Creature("ForestSprite",      Some(1), 22),
    Spell("Rejuvenation"),
    Creature("elfHermit",         Some(1), 13),
    Spell("NaturesFury"),
    Creature("GiantSpider",       Some(4), 21),
    Creature("Troll",             Some(6), 25),
    Spell("StoneRain"),
    Creature("EarthElemental",    None, 49),
    Creature("MasterHealer",      Some(3), 35),
    Creature("Hydra",             Some(3), 40)))

}
