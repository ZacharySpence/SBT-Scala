class DungeonDelve {
 var overallDead = false
}

trait TheDoEverything extends Dice with Items with Magic {
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

  def checkInventory(Character: Mundane) = {
    print("\n")
    println(s"Inventory ${Character.inventory.inventory}")
    println(s"Weapon: ${Character.inventory.weapon}")
    println(s"Armour: ${Character.inventory.armour}")
    println(s"Spell Book: ${Character.spellBook.spellBook}")
    print("\n")
  }

  def Combat(characterSheet:Mundane,
             playerNAS: Tuple3[String, Tuple3[Int, Int, String], Tuple7[Int, Int, Int, Int, Int, Int, Int]],
             enemyNAS: Tuple3[String, Tuple3[Int, Int, String], Tuple7[Int, Int, Int, Int, Int, Int, Int]]): Boolean = {
    var dead = false
    println(s"${playerNAS._1} is under attack by ${enemyNAS._1}\n")

    //1time instantiating variables
    var ehealth = enemyNAS._3._3
    var phealth = playerNAS._3._3 //need to have player health change
    var combat = true
    //Attack phase => +ve is player does damage, -ve is enemy damage
    //multi-instantiating variables
    while (combat) {

      def checkAttack(pattack: Tuple3[Int, Int, String], eattack: Tuple3[Int, Int, String]): Int = {
        var playerAttack = rollDice((pattack._1, pattack._2), weapons(pattack._3)._1)
        var enemyAttack = rollDice((eattack._1, eattack._2), weapons(eattack._3)._1)
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
      def checkHealth(ename: String, ehealth: Int, phealth: Int, alreadyRan: Boolean): Boolean = {
        if (!alreadyRan) {
          return false
        }

        else {
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
        println("Attack: " + attack)
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

      def usePotion(Character: Mundane) = {
        if (Character.inventory.inventory.length > 0) {
          print(s"What Potion would you like to use? [")
          Character.inventory.inventory.foreach(value => print(value + ","))
          print("]")
          var item = readLine()
          var potion = item.split(" ")
          println("Potion chosen: " + Potions(potion(1))(potion(0)))
          potion(1) match {
            case "Healing" => phealth += rollDice((Potions(potion(1))(potion(0))._1._1, Potions(potion(1))(potion(0))._1._2))
            case "Damage" => ehealth -= rollDice((Potions(potion(1))(potion(0))._1._1, Potions(potion(1))(potion(0))._1._2))
            case _ => println("You have an empty glass which is quite shiny...not much use though")
          }
        }
        else {
          println("You don't have anything in your backpack!")
        }

      }

      def castSpell(Character:Mundane)={
        var spellChoice = " "
        //RollDice
        var spellCheck = rollDice((1,20),playerNAS._3._4)

        while(!allSpellKeys.contains(spellChoice)){
          spellChoice = readLine(createChoiceString(Character.spellBook.spellBook,"Choose a spell From:")).toLowerCase().split(" ").map(_.capitalize).mkString("")
        }
        def getDC(spellChoice:String):Int= {
          spellChoice.substring(0, 1).toLowerCase() match {
            case "h" => return 0
            case _ => return damageSpellMap(spellChoice.substring(0,1))(spellChoice)._2._1
          }
        }
        var DC = getDC(spellChoice)
        def castHealSpell(spellChoice:String)={
          var heal = rollDice(healingSpellMap(spellChoice),playerNAS._3._4)
          println(s"You healed yourself for $heal")
          phealth += heal
        }
        def castDamageSpell(spellChoice:String)={
          var damage = rollDice(damageSpellMap(spellChoice.substring(0,1))(spellChoice)._1,playerNAS._3._4)
          if (spellCheck >= DC){
            println(s"You cast $spellChoice for $damage damage")
            ehealth -= damage
          }
          else{
            println(s"your spell backfires causing you $damage damage")
            phealth -= damage
          }

        }
        if (DC == 0){
          castHealSpell(spellChoice)
        }
        else{
            castDamageSpell(spellChoice)
        }
      }

      println(s"Enemy HP: $ehealth, Your HP: $phealth")
      var chooseOption = (readLine("[Attack,Run Away,Use Item,Inventory,Case Magic]").toLowerCase() + " ").substring(0, 1)
      chooseOption match {
        case "a" => Attack()
        case "r" => runAway()
        case "u" => usePotion(characterSheet)
        case "i" => checkInventory(characterSheet)
        case "m"|"c" => castSpell(characterSheet)
        case _ => Attack()
      }
      combat = checkHealth(enemyNAS._1, ehealth, phealth, combat)
    }
    characterSheet.health = phealth
    return dead
  }

  def lootRoll(loot:String):String= {
    loot.toLowerCase() match {
      case "armour" => return armKeys(randInt.nextInt(armKeys.size))
      case "weapon" => return wepKeys(randInt.nextInt(wepKeys.size))
      case "potion" =>
        potKeys(randInt.nextInt(potKeys.size)).toLowerCase() match {
          case "healing" => return potHealKeys(randInt.nextInt(potHealKeys.size)) + " Healing Potion"
          case "damage" => return potDmgKeys(randInt.nextInt(potDmgKeys.size)) + " Damage Potion"
          case _ => return loot //specific potion
        }
      case _ => return loot //So specific loot
    }
  }


  def createChoiceString(listOfStuff:List[String],firstText:String):String={
    var theString = firstText + " ["
    listOfStuff.foreach(value => theString += (value + ","))
    theString = theString.substring(0,theString.length-1)
    theString += "]"
    return theString
  }
}

trait Items {
  var itemList:List[String] = List("Potions")
  var weapons:Map[String,Tuple2[Int,Int]]= Map("Sword"-> (2,3),"Club"->(0,0),"Dagger"->(1,1),"GreatAxe" ->(4,6)).withDefaultValue(0,0)
  var wepKeys = weapons.keys.toList
  var armours:Map[String,Tuple2[Int,Int]] = Map("None"->(0,0),"Leather"->(1,2),"ChainMail"->(2,5),"PlateMail"->(4,12)).withDefaultValue(1,2)
  var armKeys = armours.keys.toList
  var Potions:Map[String,Map[String,Tuple2[Tuple2[Int,Int],Int]]] = Map(
    "Healing"->Map("Light"->((1,8),2),"Moderate"->((2,8),5),"Critical" -> ((4,8),15)),
    "Damage"->Map("Acid" -> ((1,4),4),"Explosive" ->((1,8),10),"Frost"->((1,6),7)))
  var potKeys = Potions.keys.toList
  var potHealKeys = Potions("Healing").keys.toList
  var potDmgKeys = Potions("Damage").keys.toList
}

class Inventory extends Items{
  var weapon = "Club"
  var armour = "None"
  var inventory:List[String] = List()
}

class SpellBook extends Magic{
  var spellBook:List[String] = List()
}

trait Magic{

  var healingSpellMap:Map[String,Tuple2[Int,Int]] = Map("HealingLight"->(1,6),"HealingModerate"->(2,6),"HealingCritical"->(4,6))
  var healSpellKeys = healingSpellMap.keys.toList
  var damageSpellMap:Map[String,Map[String,Tuple2[Tuple2[Int,Int],Tuple2[Int,String]]]]= Map(//(rollDice),(castingDC,effect(or damage type))
    "F"->Map("FlamingHands"->((1,8),(12,"Fire")),"FlameBolt"->((3,3),(15,"Fire")),"Fireball"->((2,10),(18,"Fire"))),
    "W"-> Map("WaterSpray"->((1,4),(6,"Water")),"WaterBolt"->((2,4),(10,"Water")),"WaterLance"->((1,10),(13,"Water"))))
  var damSpellKeys = damageSpellMap.keys.toList
  var damSpellFireKeys = damageSpellMap("F").keys.toList
  var damSpellWaterKeys = damageSpellMap("W").keys.toList
  var allSpellKeys = healSpellKeys ::: damSpellFireKeys ::: damSpellWaterKeys
}
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
  var spellBook = new SpellBook()
  var inventory = new Inventory()
  var loot:List[String] = List()
  var characterPoints = 5
  var health = 0


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
    Defence = armours(inventory.armour)._1
    raece = race.race
    combatTuple = (Name,(1,Strength,inventory.weapon),(Strength,Dexterity,Constitution,Intelligence,Wisdom,Charisma,Defence))
    health = Constitution
  }

  def chooseItems()={
    //Choose what to buy
    def buyChoice() ={
      var choice = (readLine("What would you like to spend points on [Weapons,Armour,Potions] or [Finished]").toLowerCase + " ").substring(0,1)
      choice match{
        case "w" => if(inventory.weapon == ""){thingChoice(wepKeys,"Weapon")}else{println("You already have a weapon")}
        case "a" => if(inventory.armour == ""){thingChoice(armKeys,"Armour")}else{println("You already have armour")}
        case "p" =>
          var choice2 = (readLine(createChoiceString(potKeys,"What type of potion")).toLowerCase() + " ").substring(0,1)
          choice2 match {
            case "h" => thingChoice(potHealKeys,"Potion"," Healing ")
            case "d" => thingChoice(potDmgKeys,"Potion"," Damage ")
            case _ => println("We don't sell that here")
          }
        case _ => characterPoints = 0
      }
    }
    def checkPoints(chosenItemCost:Int):Boolean={
      if (chosenItemCost <= characterPoints){
        characterPoints -= chosenItemCost
        return true
      }
      println("You don't have enough points for that!")
      return false
    }
    //Buying certain thing
    def thingChoice(keyList:List[String],thing:String,moreDetail:String="")= {
      var choose = ""
      while (!keyList.contains(choose)) {
        choose = readLine(createChoiceString(keyList, s"Choose a $thing from")).toLowerCase().split(" ").map(_.capitalize).mkString("") //Doesn't work with greatAxe(capital mid letter
      }
      thing match {
        case "Weapon" => if(checkPoints(weapons(choose)._2)){inventory.weapon = choose}
        case "Armour" => if(checkPoints(armours(choose)._2)){inventory.armour = choose}
        case "Potion" => if(checkPoints(Potions(moreDetail.replaceAll(" ",""))(choose)._2)){inventory.inventory ::= (choose + moreDetail + thing)}
      }
    }
    while (characterPoints > 0){
      println("Your points to spends: "+characterPoints)
      buyChoice()
    }

  }
}


//class MagicalElf extends MundaneElf with Magicaly{
//  spellbook.spellBook = List("Flame Bolt","Water Bolt")
//}

class MundaneElf extends Mundane{
  race = Race("Elf",(1,6),(1,8),(2,6),(1,8),(1,6),(1,8))
  bonuses =(0,2,6,2,0,0)
  characterPoints = 5
  inventory.weapon = "Sword"
  inventory.armour = "Leather"

}
class MagicalElf extends MundaneElf{
  spellBook.spellBook = List("Flame Bolt","Water Bolt")
}
class MundaneDwarf extends Mundane{
  race = Race("Dwarf",(1,8),(1,4),(2,10),(1,6),(1,6),(1,4))
  bonuses = (2,0,10,0,0,0)
  characterPoints = 5
  inventory.weapon = "Club"
  inventory.armour = "PlateMail"

}
class MagicalDwarf extends MundaneDwarf{
  spellBook.spellBook = List("Water Spray")
}
class MundaneHuman extends Mundane{
  race = Race("Human",(1,6),(1,6),(2,6),(1,6),(1,6),(1,6))
  bonuses = (1,1,6,1,1,1)
  characterPoints = 20
  //inventory.weapon = "Club"
  //inventory.armour = "PlateMail"

}
class MagicalHuman extends MundaneHuman{
  spellBook.spellBook = List("Fireball")
}
class MundaneGoblin extends Mundane{
  race = Race("Goblin",(1,3),(1,3),(1,4),(1,1),(1,1),(1,1))
  inventory.weapon = "Club"
  inventory.armour = "None"
  Name = "Goblin"
}
class MagicalGoblin extends MundaneGoblin{
  spellBook.spellBook = List("Flaming Hands")
}
class MundaneGoblinThief extends Mundane{
  race = Race("GoblinThief",(1,3),(1,5),(1,3),(1,1),(1,1),(1,1))
  inventory.weapon = "Dagger"
  inventory.armour = "None"
  Name = "Goblin Thief"
  inventory.inventory = List("Potion")
}
class MundaneMinitaur extends Mundane{
  race = Race("Minitaur",(1,6),(1,2),(1,8),(1,4),(1,2),(1,2))
  bonuses = (1,0,6,0,0,0)
  inventory.weapon = "GreatAxe"
  inventory.armour = "ThickHide"
  Name = "Minitaur"
}
class MundaneMinotaur extends Mundane{
  race = Race("Minotaur",(1,8),(1,4),(1,10),(1,4),(1,2),(1,2))
  bonuses = (3,0,20,0,0,0)
  inventory.weapon = "GreatAxe"
  inventory.armour = "ThickHide"
  Name = "Minotaur"
  inventory.inventory = List("The Key")
}

//Main Game
object DungeonDelveGame extends App with TheDoEverything {
  def createCharacter(): Mundane = {
    var chooseRace = readLine("Pick a Race:[Human,Dwarf,Elf]")
    var Player = initialiseRace(chooseRace, true)
    Player.createCharacterAttributes(true)
    Player.chooseItems()
    return Player
  }

  def initialiseRace(chooseRaces: String, player:Boolean = false): Mundane = {

    var chooseMagical = "mu"
    if (player){
      chooseMagical = (readLine("Magical or Mundane?").toLowerCase()+ "  ").substring(0,2)
    }
    var chooseRace = chooseRaces
    if (chooseRace.length == 0){
      chooseRace += " "
    }

    var raceList:Map[String,Mundane] = Map()
    chooseMagical match{
      case "mu" => raceList = Map("h" -> new MundaneHuman(),"d" -> new MundaneDwarf(), "e" -> new MundaneElf(),"g" -> chooseGoblin(chooseRace), "m" -> chooseMinotaur(chooseRace)).withDefaultValue(new MundaneGoblin())
      case "ma" => raceList = Map("e" -> new MagicalElf).withDefaultValue(new MundaneGoblin)
    }

    def chooseGoblin(race:String):Mundane={
      race.toLowerCase() match{
        case "goblin" => new MundaneGoblin()
        case "goblinthief" => new MundaneGoblinThief()
        case _ => new MundaneGoblin()
      }
    }

    def chooseMinotaur(race:String):Mundane ={
      race.toLowerCase() match{
        case "minitaur" => new MundaneMinitaur()
        case "minotaur" => new MundaneMinotaur()
        case _ => new MundaneGoblin()
      }
    }

    raceList((chooseRace.toLowerCase()+" ").substring(0,1))

//    chooseRace.toLowerCase().substring(0,1) match {
//      case "h" => new MundaneHuman()
//      case "d" => new MundaneDwarf()
//      case "e" => new MundaneElf()
//      case "m" => chooseRace.toLowerCase() match{
//        case "minotaur" => new MundaneMinotaur()
//        case "minitaur" => new MundaneMinitaur()
//      }
//      case "g" => chooseRace.toLowerCase() match{
//        case "goblin" => new MundaneGoblin()
//        case "goblinthief" => new MundaneGoblinThief()
//      }
//      case _ =>
//        println("You left  it into the hands of the Demons below.")
//        new MundaneGoblin()
//    }
  }

  def createEnemy(enemyName: String): Mundane = {
    var Enemy = initialiseRace(enemyName)
    Enemy.createCharacterAttributes()
    return Enemy
  }

  //create Player Character
  var Character = createCharacter()
  checkInventory(Character)

  //MonsterList Premade
  var MonsterList:List[Tuple2[List[String],List[Int]]] = List((List("Goblin"),List(1)),(List("Goblin"),List(3)),(List("Goblin","GoblinThief"),List(4,1)),(List("Minotaur"),List(1)))
  //Create dungeon
  var dungeonThings:Tuple2[Int,List[Tuple2[List[String],List[Int]]]] = (4,MonsterList)
  var dungeonTile = new DungeonTile()
  var theDungeon:List[List[String]] = List()
  var roomEncounter:List[Tuple2[List[String], Tuple2[List[String], List[Int]]]] = List()




  var customize = (readLine("Would you like to create your own dungeon? or randomly generate: [Own,Generate]").toLowerCase()+ " ").substring(0,1)
  customize match{
    case "o"|"m" => doItYourself()
    case "g"|"r" => doItYourself(generate=true)
    case _ => doItYourself(dungeonThings,false)
  }
  def doItYourself(things:Tuple2[Int,List[Tuple2[List[String],List[Int]]]]=(0,List()),doIt:Boolean=true,generate:Boolean=false)={
    var MonsterGeneration= Map("Goblin"->(1,5),"GoblinThief"->(1,3),"Minitaur"->(1,1))
    var MonGenKeys = MonsterGeneration.keys.toList
    println(MonGenKeys)

    if(!doIt){
      theDungeon = dungeonTile.makeDungeon(things._1).reverse
      roomEncounter = fillDungeon(theDungeon,things._2)
    }
    else{
      println("How many rooms 1-∞")
      var numberOfRooms = readInt()
      var monsterList:List[Tuple2[List[String],List[Int]]] = List()
      def chooseRoomMonsters(generate:Boolean,k:Int)={
          var done = false
          var mList:List[String] = List()
          var mCount:List[Int] = List()
          var timesRun = 0
          while(!done){
            timesRun +=1
            var monster = ""
            var howMany = 0
            var checkDone = ""
            if (generate){
              println("Generating")
             // monster = MonGenKeys(randInt.nextInt(MonGenKeys.length))
              //println(mList.contains(monster))
              do {//Stops duplicates
                monster = MonGenKeys(randInt.nextInt(MonGenKeys.length))
                println("monmon"+monster)
                if (mList.contains(monster)){//Add 1 to that monster group
                  println("Here")
                  mCount.updated(mList.indexOf(monster),(mCount(mList.indexOf(monster))+1))
                }
              }while(mList.contains(monster))
              howMany = rollDice((MonsterGeneration(monster)._1,MonsterGeneration(monster)._2))
              var checkIt = randInt.nextInt(5-timesRun)+1
              if (mList.length == MonGenKeys.length-1){
                checkIt = 0
              }
              println("check"+checkIt)
              checkIt match {
                case 0|1|2 => checkDone = "y"
                case _ => checkDone = "n"
              }

            }
            else{
              while(!MonGenKeys.contains(monster) | mList.contains(monster)){
              monster = readLine(createChoiceString(MonGenKeys,"Choose a Monster from:"))
              }
              println(s"How many $monster will there be in that room")
              howMany = readInt()
              checkDone = (readLine("Are you done?").toLowerCase+ " ").substring(0,1)
            }
              if (k-1 == numberOfRooms){//forces Minotaur boss in last room
                monster = "Minotaur"
                howMany = 1
              }
            mList ::= monster
            mCount ::= howMany

            checkDone match{
              case "y" => done = true
              case _ => done = false
            }
          }
          println((mList,mCount))
          monsterList ::= (mList,mCount)
        }

        for (k <- 0 until numberOfRooms){
          println("Start"+k)
          chooseRoomMonsters(generate,k)
        }
      //Visual of dungeon for feedback
      theDungeon = dungeonTile.makeDungeon(numberOfRooms).reverse
      print("hEEEERE")
      roomEncounter = fillDungeon(theDungeon,monsterList.reverse)
      println("Your dungeon Has:")
      for (k <- 1 to numberOfRooms){
        println(s"Room $k: ${monsterList(k-1)}")
      }
    }

  }
  //Movement,Monsters
  def fillDungeon(dungeon:List[List[String]],monsterList:List[Tuple2[List[String],List[Int]]]):List[Tuple2[List[String], Tuple2[List[String], List[Int]]]]={

    var filledRooms:List[Tuple2[List[String],Tuple2[List[String],List[Int]]]] = List()
    for (k <- 0 until dungeon.length){
      filledRooms ::= (dungeon(k),monsterList(k))
    }
    filledRooms = filledRooms.reverse
    return filledRooms
  }


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
          if (!dead){
            Monster.inventory.inventory.foreach(lootItem => {
              var loot = lootRoll(lootItem)
              Character.inventory.inventory ::= loot
              println(s"You have looted a $loot")})
          }
        }
      }
    }
    def move(moveRoom: List[String]) = {
      var room = moveRoom
      if (room.length == 1) {
        room ::= " "
        room = room.reverse
      }
//      print(s"You can go")
//      room.foreach(value => print(value + ","))
//      print("]")
      val whereMoving = (readLine(createChoiceString(room,"You can go")).toLowerCase()+ " ").substring(0, 1)  //append to string
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
  while(!dead && !Character.inventory.inventory.contains("The Key")){
    Encounter(roomEncounter(currentRoom))
    println("the current room:"+currentRoom)
  }
  if (Character.inventory.inventory.contains("The Key")){
    println("You won!")
  }
}
