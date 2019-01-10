class DungeonDelve {
 var overallDead = false
}

trait TheDoEverything extends Dice with Items{
  var Name: String = " "
  var raece: String = " "
  var Strength: Int = 0
  var Dexterity: Int = 0
  var Constitution: Int = 0
  var Intelligence: Int = 0
  var Wisdom: Int = 0
  var Charisma: Int = 0
  var Defence: Int = 0
  var currentRoom: Int = 0
  var combatTuple: Tuple3[String, Tuple3[Int, Int, String], Tuple7[Int, Int, Int, Int, Int, Int, Int]] = ("", (0, 0, ""), (0, 0, 0, 0, 0, 0, 0))
  //var roomEncounter: List[Tuple2[List[String], Tuple2[List[String], List[Int]]]] = List((List(), (List(), List())))

  def Combat(characterSheet:Mundane,
              playerNAS: Tuple3[String, Tuple3[Int, Int, String], Tuple7[Int, Int, Int, Int, Int, Int, Int]],
             enemyNAS: Tuple3[String, Tuple3[Int, Int, String], Tuple7[Int, Int, Int, Int, Int, Int, Int]]):Boolean = {
    var dead = false
    println(s"${playerNAS._1} is under attack by ${enemyNAS._1}\n")

    //1time instantiating variables
    var ehealth = enemyNAS._3._3
    var phealth = playerNAS._3._3
    var combat = true
    //Attack phase => +ve is player does damage, -ve is enemy damage
    //multi-instantiating variables
    while (combat) {

      def checkAttack(pattack: Tuple3[Int, Int, String], eattack: Tuple3[Int, Int, String]): Int = {
        var playerAttack = rollDice((pattack._1, pattack._2), weapons(pattack._3))
        var enemyAttack = rollDice((eattack._1, eattack._2), weapons(eattack._3))
        return (playerAttack - enemyAttack)
      }

      def checkDamage(attack: Int, edefence: Int, pdefence: Int): Tuple2[Boolean, Int] = {
        var dmg = 0
        if (attack > 0) {
          //Player hits
          dmg = attack - edefence
          if (dmg > 0) {
            println(s"You have dealt $dmg damage ")
            return (true, dmg)
          }
        }
        else if (attack < 0) {
          dmg = (attack * -1) - pdefence
          if (dmg > 0) {
            println(s"You took $dmg damage ")
            return (false, dmg)
          }
        }
        println("No damage was dealt ")
        return (false, 0)

      }


      //Resolve round
      def checkHealth(ename: String, ehealth: Int, phealth: Int,alreadyRan:Boolean): Boolean = {
        if (!alreadyRan){
          return false
        }

        else{
          if (ehealth <= 0) {
            println(s"You have killed the $ename")
            return false
          }
          else if (phealth <= 0) {
            println(s"You have been killed")
            dead = true
            return false
          }
          else if (phealth <= 0 && ehealth <= 0) {
            println(s"You die in a blaze of glory killing the $ename too.")
            dead = true
            return false
          }
          else {
            return true
          }
        }
      }

      def checkRun(pdex: Int, edex: Int): Boolean = {
        var run = rollDice((1, 20), pdex) - rollDice((1, 20), edex)
        if (run <= 0) {
          return false
        }
        currentRoom -= 1
        return true
      }

      //Main Function Bit
      def Attack() = {
        var attack = checkAttack(playerNAS._2, enemyNAS._2)
        println("Attack: "+attack)
        var damage = checkDamage(attack, enemyNAS._3._7, playerNAS._3._7)
        //Deal Damage
        damage._1 match {
          case true => ehealth -= damage._2
          case false => phealth -= damage._2
        }
      }
      def runAway() = {
        var running = checkRun(playerNAS._3._2, enemyNAS._3._2)
        running match {
          case true => println("You got away")
            combat = false
          case false => println("You got caught")
            Attack()
        }

      }

      def usePotion(Character:Mundane)= {
        if (Character.inventory.inventory.length > 0){
          print(s"What Potion would you like to use? [")
          Character.inventory.inventory.foreach(value => print(value + ","))
          print("]")
          var item = readLine()
          var potion = item.split(" ")
          println("Potion chosen: " + Potions(potion(1))(potion(0)))
          potion(1) match {
            case "Healing" => phealth += rollDice((Potions(potion(1))(potion(0))._1, Potions(potion(1))(potion(0))._2))
            case "Damage" => ehealth -= rollDice((Potions(potion(1))(potion(0))._1, Potions(potion(1))(potion(0))._2))
            case _ => println("You have an empty glass which is quite shiny...not much use though")
          }
        }
        else{
          println("You don't have anything in your backpack!")
        }

      }

      def checkInventory(Character:Mundane)={
        println(Character.inventory.inventory.length)
        println(Character.inventory.inventory)
      }
      println(s"Enemy HP: $ehealth, Your HP: $phealth")
      var chooseOption = (readLine("[Attack,Run Away,Use Item,Check Inventory]").toLowerCase() + " ").substring(0, 1)
      chooseOption match {
        case "a" => Attack()
        case "r" => runAway()
        case "u" => usePotion(characterSheet)
        case "c" => checkInventory(characterSheet)
        case _ => Attack()
      }
      combat = checkHealth(enemyNAS._1, ehealth, phealth,combat)
    }
    return dead
  }

  def lootRoll(loot: String): String = {
    loot.toLowerCase() match {
      case "armour" => return armKeys(randInt.nextInt(armKeys.size))
      case "weapon" => return wepKeys(randInt.nextInt(wepKeys.size))
      case "potion" =>
        potKeys(randInt.nextInt(potKeys.size)).toLowerCase() match{
          case "healing" => return potHealKeys(randInt.nextInt(potHealKeys.size)) + " Healing Potion"
          case "damage" => return potDmgKeys(randInt.nextInt(potDmgKeys.size)) + " Damage Potion"
          case _ => return loot //specific potion
        }
      case _ => return loot //So specific loot
      }
  }
}

trait Items {
  var itemList:List[String] = List("Potions")
  var weapons:Map[String,Int]= Map("Sword"-> 2,"Club"->0,"Dagger"->1,"GreatAxe" ->4).withDefaultValue(0)
  var wepKeys = weapons.keys.toList
  var armours:Map[String,Int] = Map("None"->0,"Leather"->1,"ChainMail"->2,"PlateMail"->4).withDefaultValue(1)
  var armKeys = armours.keys.toList
  var Potions:Map[String,Map[String,Tuple2[Int,Int]]] = Map(
    "Healing"->Map("Light"->(1,8),"Moderate"->(2,8),"Critical" -> (4,8)),
    "Damage"->Map("Acid" -> (1,4),"Explosive" ->(1,8),"Frost"->(1,4)))
  var potKeys = Potions.keys.toList
  var potHealKeys = Potions("Healing").keys.toList
  var potDmgKeys = Potions("Damage").keys.toList
}

class Inventory extends Items{
  var weapon = "Sword"
  var armour = "Leather"
  var inventory:List[String] = List()
}
trait Magic{}
trait Dice{
  var randInt = scala.util.Random
  def rollDice(diceTuple:Tuple2[Int,Int],bonus:Int=0):Int={
    if (diceTuple._1*diceTuple._2 == 1) {//So can only roll a 1
      return (diceTuple._2+bonus)
    }
    return (randInt.nextInt((diceTuple._1*diceTuple._2)-1)+diceTuple._1+bonus)
  }
}


class  DungeonTile {
  var randInt = scala.util.Random
  def makeDungeon(Size:Int):List[List[String]]={
    var dungeonList:List[List[String]] = List()
    var dungeonPreviousList:List[String] = List()
    for(k <- 0 to Size-1){
      var dungeonSquare:List[String] = List()
      def addPreviousPosition(dungeonSquarePrevious:List[String])={
        def matchToPrevious(position:String):String={
          position match{
            case "Forwards" => "Backwards"
            case "Right" => "Left"
            case "Backwards" => "Forwards"
            case "Left" => "Right"
            case _ => "Errore"
          }

        }
        dungeonSquare ::= matchToPrevious(dungeonSquarePrevious(0))
        //dungeonSquarePrevious.foreach(position => dungeonSquare::= matchToPrevious(position))

      }
      if (k > 0 ){ //For all cases except the first
        addPreviousPosition(dungeonPreviousList)
      }

      def choosePosition(dungeonSquare:List[String]):Int={
        var choiceList:Tuple4[Int,Int,Int,Int] = (0,0,0,0)
        def matchToRemove(position:String)={
          position match{
            case "Forwards" => choiceList = choiceList.copy(_1 = 1)
            case "Right" => choiceList = choiceList.copy(_2 = 2)
            case "Backwards" => choiceList = choiceList.copy(_3 = 3)
            case "Left" => choiceList = choiceList.copy(_4 = 4)
          }
        }
        if (k > 0){//For all cases but the first
          dungeonSquare.foreach(position => matchToRemove(position) )//creates a list of all already taken positions
        }

        var choice = 0
        while(choiceList.productIterator.contains(choice)){//

          choice = (randInt.nextInt(4)+1)

        }
        return choice
      }
      var position = choosePosition(dungeonSquare) //So 1 to 4 1=> North, 2 => East, 3=>South, 4=> West
      position match{
        case 1 => dungeonSquare ::= "Forwards"
        case 2 => dungeonSquare ::= "Right"
        case 3 => dungeonSquare ::= "Backwards"
        case 4 => dungeonSquare ::= "Left"
        case _ => print("Error")
      }
      dungeonList ::= dungeonSquare
      dungeonPreviousList = List()
      dungeonPreviousList = dungeonSquare
    }
    return dungeonList
  }

}

//Characters
 abstract class RacialBonuses{
  var race:String
  var str: Tuple2[Int,Int]
  var dex: Tuple2[Int,Int]
  var con: Tuple2[Int,Int]
  var intel: Tuple2[Int,Int]
  var wis: Tuple2[Int,Int]
  var cha: Tuple2[Int,Int]

}

case class Race(var race:String,var str: Tuple2[Int,Int], var dex: Tuple2[Int,Int], var con: Tuple2[Int,Int],
                 var intel: Tuple2[Int,Int], var wis: Tuple2[Int,Int], var cha: Tuple2[Int,Int])
  extends RacialBonuses{}

abstract class Mundane extends TheDoEverything{

  var race = Race("Human",(1,6), (1,6), (1,6), (1,6), (1,6), (1,6))
  var bonuses  = (0,0,0,0,0,0)
  Name = ""
  var inventory = new Inventory()
  var loot:List[String] = List()
  var characterPoints = 0


  def createCharacterAttributes(name:Boolean = false) = {
    if (name){
      Name = readLine("What are you called brave idiot?")
    }

    Strength = rollDice((race.str._1,race.str._2))+bonuses._1
    Dexterity = rollDice((race.dex._1,race.dex._2))+bonuses._2
    Constitution = rollDice((race.con._1,race.con._2))+bonuses._3
    Intelligence = rollDice((race.intel._1,race.intel._2))+bonuses._4
    Wisdom = rollDice((race.wis._1,race.wis._2))+bonuses._5
    Charisma = rollDice((race.cha._1,race.cha._2))+bonuses._6
    Defence = armours(inventory.armour)
    raece = race.race
    combatTuple = (Name,(1,Strength,inventory.weapon),(Strength,Dexterity,Constitution,Intelligence,Wisdom,Charisma,Defence))
  }

  def chooseItems()={

  }
}
class MundaneElf extends Mundane{
  race = Race("Elf",(1,6),(1,8),(2,6),(1,8),(1,6),(1,8))
  bonuses =(0,2,6,2,0,0)
  inventory.weapon = "Sword"
  inventory.armour = "Leather"

}
class MundaneDwarf extends Mundane{
  race = Race("Dwarf",(1,8),(1,4),(2,10),(1,6),(1,6),(1,4))
  bonuses = (2,0,10,0,0,0)
  inventory.weapon = "Club"
  inventory.armour = "PlateMail"

}
class MundaneHuman extends Mundane{
  race = Race("Human",(1,6),(1,6),(2,6),(1,6),(1,6),(1,6))
  bonuses = (1,1,6,1,1,1)
  inventory.weapon = "Club"
  inventory.armour = "PlateMail"

}
class MundaneGoblin extends Mundane{
  race = Race("Goblin",(1,3),(1,3),(1,4),(1,1),(1,1),(1,1))
  inventory.weapon = "Club"
  inventory.armour = "None"
  Name = "Goblin"
}
class MundaneGoblinThief extends Mundane{
  race = Race("GoblinThief",(1,3),(1,5),(1,3),(1,1),(1,1),(1,1))
  inventory.weapon = "Dagger"
  inventory.armour = "None"
  Name = "Goblin Thief"
  inventory.inventory = List("Potion")
}
class MundaneMinotaur extends Mundane{
  race = Race("Goblin",(1,8),(1,4),(1,10),(1,4),(1,2),(1,2))
  bonuses = (1,0,10,0,0,0)
  inventory.weapon = "GreatAxe"
  inventory.armour = "ThickHide"
  Name = "Minotaur"
  inventory.inventory = List("GreatAxe")
}

//Main Game
object DungeonDelveGame extends App with TheDoEverything {
  def createCharacter(): Mundane = {
    var chooseRace = readLine("Pick a Race:[Human,Dwarf,Elf]")
    var Player = initialiseRace(chooseRace)
    Player.createCharacterAttributes(true)
    return Player
  }

  def initialiseRace(chooseRaces: String): Mundane = {
    var chooseRace = chooseRaces
    if (chooseRace.length == 0){
      chooseRace += " "
    }
    chooseRace.toLowerCase().substring(0,1) match {
      case "h" => new MundaneHuman()
      case "d" => new MundaneDwarf()
      case "e" => new MundaneElf()
      case "m" => new MundaneMinotaur()
      case "g" => chooseRace.toLowerCase() match{
        case "goblin" => new MundaneGoblin()
        case "goblinthief" => new MundaneGoblinThief()
      }
      case _ =>
        println("You left  it into the hands of the Demons below.")
        new MundaneGoblin()
    }
  }

  def createEnemy(enemyName: String): Mundane = {
    var Enemy = initialiseRace(enemyName)
    Enemy.createCharacterAttributes()
    return Enemy
  }

  //create Player Character
  var Character = createCharacter()

  //Movement,Monsters
  def fillDungeon(dungeon:List[List[String]]):List[Tuple2[List[String], Tuple2[List[String], List[Int]]]]={

    var MonsterList:List[Tuple2[List[String],List[Int]]] = List(
      (List("Goblin"),List(1)),
      (List("Goblin"),List(3)),
      (List("Goblin","GoblinThief"),List(4,1)),
      (List("Minotaur"),List(1)))
    var filledRooms:List[Tuple2[List[String],Tuple2[List[String],List[Int]]]] = List()
    for (k <- 0 until dungeon.length){
      filledRooms ::= (dungeon(k),MonsterList(k))
    }
    filledRooms = filledRooms.reverse
    return filledRooms
  }

  //Create dungeon
  var dungeonTile = new DungeonTile()
  var theDungeon = dungeonTile.makeDungeon(4).reverse
  var roomEncounter = fillDungeon(theDungeon)

  println(roomEncounter(3))

  def Encounter(encounterTheRoom: Tuple2[List[String], Tuple2[List[String], List[Int]]])= {
    def monsterCombat(roomMonster:Tuple2[List[String],List[Int]])= {
      //Tell player how many monsters they are fighting
      for (p<-0 until roomMonster._1.length){
      print(roomMonster._2(p)+" "+roomMonster._1(p))
      }
      print("\n")
      //Combat per monster
      for (k <- 0 until roomMonster._1.length) {
        var Monster = createEnemy(roomMonster._1(k))
        for (m <- 0 until roomMonster._2(k)) {

          dead = Combat(Character,Character.combatTuple, Monster.combatTuple)
          Monster.inventory.inventory.foreach(lootItem => {
            var loot = lootRoll(lootItem)
            Character.inventory.inventory ::= loot
            println(s"You have looted a $loot")})
        }
      }
    }
    def move(moveRoom: List[String]) = {
      var room = moveRoom
      if (room.length == 1) {
        room ::= " "
        room = room.reverse
      }
      print(s"You can go [")
      room.foreach(value => print(value + ","))
      print("]")
      val whereMoving:String = (readLine().toLowerCase()+ " ").substring(0, 1)  //append to string
      if (whereMoving == room.head.toLowerCase().substring(0, 1)) {
        currentRoom += 1
      }

      else if (whereMoving == room(1).toLowerCase().substring(0,1)) {
        currentRoom -= 1
      }
      if (currentRoom < 0) {
        currentRoom = 0
        println("There's Nowhere to run")
      }
    }

    monsterCombat(encounterTheRoom._2)
    if (!dead){
      move(encounterTheRoom._1)
    }

  }

  var dead = false
  while(!dead){
    Encounter(roomEncounter(currentRoom))
    println("the current room:"+currentRoom)
  }
  println("YOU DIED")






}
