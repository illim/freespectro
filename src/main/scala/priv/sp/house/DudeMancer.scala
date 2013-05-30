package priv.sp.house

import priv.sp._

class DudeMancer {

  val Dude = House("Dudes", List(
    Creature("Bred", Attack(4), 17),
    Creature("TwoP", Attack(4), 27),
    Creature("Axl", Attack(5), 26),
    Creature("BillBull", Attack(7), 31),
    Creature("ElGado", Attack(8), 46),
    Creature("Andore", Attack(9), 52),
    Creature("Rolento", Attack(10), 65),
    Creature("EdiE", Attack(10), 99)))

  Dude.initCards(Houses.basicCostFunc)
}
