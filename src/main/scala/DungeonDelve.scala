class DungeonDelve {
 var overallDead = false
}

trait TheDoEverything{

  var Strength:Int
  var Dexterity:Int
  var Constitution:Int
  var Intelligence:Int
  var Wisdom:Int
  var Charisma:Int
  //def attack={}
 // def return_stats={}
}
trait Inventory{
}
trait Magic{}
trait Dice{
  var randInt = scala.util.Random
  def rollDice(numberOf:Int,size:Int,bonus:Int=0):Int={
    return (randInt.nextInt((numberOf*size)-1)+numberOf+bonus)
  }
}

trait RacialBonuses{
  var str:Int
  var dex:Int
  var con:Int
  var intel:Int
  var wis:Int
  var cha:Int

}
class Elf extends RacialBonuses {
  str = (1,6)
  dex = (1,10)
  con = (2,8)
  intel = (1,8)
  wis = (1,6)
  cha = (1,8)

}
class MundaneElf extends TheDoEverything with Inventory with Dice{
  var race = new Elf()
  var Strength = 0
  var Dexterity = 0
  var Constitution = 0
  var Intelligence = 0
  var Wisdom = 0
  var Charisma = 0
  var name = ""

  def createCharacterAttributes() = {
    this.name = readLine("What are you called brave idiot?")
    this.Strength = rollDice(1,race.str)
    this.Dexterity = rollDice(1,race.dex)
    this.Constitution = rollDice(2,race.con._2)
    this.Intelligence = rollDice(1,race.intel._2)
    this.Wisdom = rollDice(1,8)
    this.Charisma = rollDice(1,8)
  }
  createCharacterAttributes()
}
//class Mundane extends Character{}
//object initialiser{
//}
object DungeonDelveGame extends App{
  var ElfChara = new MundaneElf()
  println(ElfChara.Strength)

}
