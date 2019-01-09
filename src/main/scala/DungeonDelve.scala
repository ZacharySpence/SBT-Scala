class DungeonDelve {
 var overallDead = false
}

trait TheDoEverything extends Dice with Items{

  var Strength:Int = 0
  var Dexterity:Int  = 0
  var Constitution:Int  = 0
  var Intelligence:Int  = 0
  var Wisdom:Int  = 0
  var Charisma:Int  = 0

  def Combat(playerNADH:Tuple4[String,Tuple3[Int,Int,String],Int,Int],enemyNADH:Tuple4[String,Tuple3[Int,Int,String],Int,Int])={
    println(s"${playerNADH._1} is under attack by ${enemyNADH._1}")
    //1time instantiating variables
    var ehealth = enemyNADH._4
    var phealth = playerNADH._4
    var combat = true
    //Attack phase => +ve is player does damage, -ve is enemy damage
    //multi-instantiating variables
    while (combat){
      var dmg = 0
      var pattack = rollDice((playerNADH._2._1,playerNADH._2._2),weapons(playerNADH._2._3))
      var eattack = rollDice((enemyNADH._2._1,enemyNADH._2._2),weapons(enemyNADH._2._3))
      var attack = pattack-eattack

      if (attack> 0){//Player hits
        dmg = attack - enemyNADH._3
        if (dmg > 0){
          ehealth - dmg
        }
      }
      else if (attack < 0){
        dmg = attack + playerNADH._3
        if (dmg > 0){
          phealth - dmg
        }

      }

      //Resolve round



    }
  }
 // def return_stats={}
}

trait Items {
  var weapons:Map[String,Int]= Map("Sword"->2,"Club"->0,"Dagger"->1).withDefaultValue(0)
}

trait Inventory{
}
trait Magic{}
trait Dice{
  var randInt = scala.util.Random
  def rollDice(diceTuple:Tuple2[Int,Int],bonus:Int=0):Int={
    return (randInt.nextInt((diceTuple._1*diceTuple._2)-1)+diceTuple._1+bonus)
  }
}

abstract class RacialBonuses{
  var str: Tuple2[Int,Int]
  var dex: Tuple2[Int,Int]
  var con: Tuple2[Int,Int]
  var intel: Tuple2[Int,Int]
  var wis: Tuple2[Int,Int]
  var cha: Tuple2[Int,Int]

}
//class Dwarf extends RacialBonuses{}
case class Elf(
                var str: Tuple2[Int,Int],
                var dex: Tuple2[Int,Int],
                var con: Tuple2[Int,Int],
                var intel: Tuple2[Int,Int],
                var wis: Tuple2[Int,Int],
                var cha: Tuple2[Int,Int]
) extends RacialBonuses {
  //str = (1,6)
  //dex = (1,10)
  //con = (2,8)
  //intel = (1,8)
  //wis = (1,6)
  //cha = (1,8)

}

case class Human(var str: Tuple2[Int,Int],
                 var dex: Tuple2[Int,Int],
                 var con: Tuple2[Int,Int],
                 var intel: Tuple2[Int,Int],
                 var wis: Tuple2[Int,Int],
                 var cha: Tuple2[Int,Int]) extends RacialBonuses{
//  str = (1,6)
//  dex = (1,6)
//  con = (2,6)
//  intel = (1,6)
//  wis = (1,6)
//  cha = (1,6)
}


abstract class Mundane extends TheDoEverything  with Dice  with Inventory {

  var race = Elf((1,6), (1,6), (1,6), (1,6), (1,6), (1,6))
  var name = ""

  def createCharacterAttributes() = {
    name = readLine("What are you called brave idiot?")
    Strength = rollDice((race.str._1,race.str._2))
    Dexterity = rollDice((race.dex._1,race.dex._2))
    Constitution = rollDice((race.con._2,race.con._2))
    Intelligence = rollDice((race.intel._1,race.intel._2))
    Wisdom = rollDice((race.wis._1,race.wis._2))
    Charisma = rollDice((race.cha._1,race.cha._2))
  }
}
class MundaneElf extends Mundane{
   race = Elf((1,6), (1,6), (1,6), (1,6), (1,6), (1,6))

}
//class MundaneDwarf extends Mundane{
//  override val race = new Dwarf()
//}
//object initialiser{
//}
object DungeonDelveGame extends App{
  var ElfChara = new MundaneElf()
  ElfChara.createCharacterAttributes()
  println(ElfChara.Strength)

}
