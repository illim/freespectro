package priv.sp.house

import priv.sp._

class DudeMancer {

  val Dude = House("Dudes", List(
    Creature("Bred", Attack(4), 17),
    Creature("Two P", Attack(4), 27),
    Creature("Axl", Attack(5), 26),
    Creature("Bill Bull", Attack(7), 31),
    Creature("El Gado", Attack(8), 46),
    Creature("Andore", Attack(9), 52),
    Creature("Rolento", Attack(10), 65),
    Creature("Edi E", Attack(10), 99)))

  Dude.initCards(Houses.basicCostFunc)
}
