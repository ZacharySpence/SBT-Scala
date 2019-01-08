class DeathRun {
var overallDead = false
  def rollDice(numberOf:Int,size:Int,bonuses:Int):Int={
    var randInt = scala.util.Random
    var roll = randInt.nextInt((size*numberOf)-1)+numberOf
    //println("roll",roll+bonuses)
    return (roll+bonuses)

  }
  def choices(Chara:Character, location:Tuple5[String,Int,Int,Int,Int], success:String = "somehow it worked"):String={
    var Description = location._1
    var Difficulty = location._2
    var StrDC =  location._3
    var AgiDC =  location._4
    var SpeedDC =location._5


    //player chooses 1 of 4 actions.
    println(Description)
    var choice = readLine("[Fight,Dodge,Run,Cast Magic]").toLowerCase + " "//space is to get empty
    choice.substring(0,1) match {
      case "f" =>
        if (rollDice(1,6,Chara.strength) < Difficulty+StrDC){
          this.overallDead = true
          return "You died"
        }
        else{
          return success
        }
      case "d" =>
        if (rollDice(1,6,Chara.agility) < Difficulty+AgiDC){
          this.overallDead = true
          return "You died"
        }
        else{
          return success
        }
      case "r" =>
        if(rollDice(1,6,Chara.speed) < Difficulty+SpeedDC){
          this.overallDead = true
          return "You died"
        }
        else{
          return success
        }
      case "c"|"m" =>
        if(mainGame.gameMagic == 0){
          this.overallDead = true
          return "You died"
        }
        else{
          mainGame.gameMagic -= 1
          return success
        }
      case _ => println("You stumble, stumped on what to do...and die")
        this.overallDead = true
        return "You died"
    }
  }

  def chooseRun():String={
    println("Where are you running?")
    var choice1 = readLine("[Beach,Jungle,Volcano]").toLowerCase() + " "
    choice1.substring(0,1) match{
      case "b" => return "Beach"
      case "j" => return "Jungle"
      case "v" => return "Volcano"
      case _ => return "The VoidZone"
    }
  }
}
class Jungle{
  //var name = (Description,Difficulty, strengthDC,AgiDC,SpeedDC,deathbyFighting,deathbyDodging,deathbyRunning,deathbyCastingMagic)
  var monkeyBusiness = ("Monkeys start pelting you with nuts and rocks. begone intruder!!",5,0,0,0)
  var forestFire = ("the trees are set ablaze, whole canopies meteoring to the ground, blazing",7,100,100,-3)
  var treantMeeting = ("Stumbling into an Entmoot, the tree shepherds demand retribution for interrupting their hellos!",8,2,-2,-4)
}
class Volcano{
  var eruption = ("The volcano erupts in a plume of molten lava. Run!!",10,100,4,-2)
  var quake = ("The volcano quakes violently, cracks splitting the earth below your feet!",8,100,2,-1)
  var salamanders = ("You've run into the population of fire salamanders, and are they angry!!",6,-1,2,3)
}
class Beach{
  var lobsterAttack = ("You've been attacked by were-lobsters!",8,1,0,-2)
  var tsunami = ("A tsunami rises in the distance...",6,100,100,3)
  var burningSand = ("The sand is so hot it burns!",3,100,0,0)
}
class The_VoidZone{
  var multidimensionalPortalHopping = ("You portal into a mesh of inter-spliced portals",0,100,100,100)
  var theDarkness = ("utter darkness envelops you",9,0,100,100)
  var voidwalkers = ("A dark blue ghost with thunder in its eyes wails your demise",7,1,100,-1)
  var VolcanoTeleportation = "You swim into a molten portal and land at a volcano"
  var JungleTeleportation = "You swing into a grassy portal and smack into a a jungle tree"
  var BeachTeleportation = "You dig through a sandy portal bursting out of the coastline"
}
abstract class Attributes{
  var strength:Int
  var agility:Int
  var speed:Int
  var magic:Int
}

class Character extends Attributes{
  var randInt = scala.util.Random
  var strength = 0
  var agility = 0
  var speed = 0
  var magic = 0

  def setAttributes()={
    this.strength = randInt.nextInt(6)+1
    this.agility = randInt.nextInt(6)+1
    this.speed = randInt.nextInt(6)+1
    this.magic = randInt.nextInt(8)
  }
  setAttributes()
  var name:String = "Nobody"
  var personalRecord = 0
}

class Locations{
  var Beach = new Beach()
  var Jungle = new Jungle()
  var Volcano = new Volcano()
  var The_VoidZone = new The_VoidZone()
}

object mainGame extends App{
  var randInt = scala.util.Random
  var deathRun = new DeathRun()
  var Chara:Character = new Character()
  var location = new Locations()
  var end = true
  var gameMagic = Chara.magic
  //Main game
  while(end){
    var choice1 = readLine("Press Enter to start your Run")
    println(s"Your furthest Run: ${Chara.personalRecord}")
    var runSpot = deathRun.chooseRun
    var currentRecord = 0
    var listSituation:List[Int] = List()
    def getAnInt(maxsituations:Int):Int={

      var theInt = randInt.nextInt(maxsituations)+1
      while(listSituation.contains(theInt)&& listSituation.length < maxsituations){
        theInt = randInt.nextInt(maxsituations)+1
      }
      if(listSituation.length >= maxsituations){
        return 0
      }
      listSituation ::= theInt
      return theInt


    }
    while (!deathRun.overallDead){
      println(s"\nStats:" +
        s"\nStr= ${Chara.strength}"+
        s"\nAgi=${Chara.agility}"+
        s"\nSpeed = ${Chara.speed}"+
        s"\nMagic = ${gameMagic}\n")
      runSpot match{
        case "Beach" => getAnInt(3) match{//so 1 to 3
          case 1 => println(deathRun.choices(Chara,location.Beach.lobsterAttack))
          case 2 => println(deathRun.choices(Chara,location.Beach.burningSand))
          case 3 => println(deathRun.choices(Chara,location.Beach.tsunami))
          case _ => println("Errror")
            deathRun.overallDead = true
        }
        case "Volcano" => getAnInt(3) match{
          case 1 => println(deathRun.choices(Chara,location.Volcano.eruption))
          case 2 => println(deathRun.choices(Chara,location.Volcano.quake))
          case 3 => println(deathRun.choices(Chara,location.Volcano.salamanders))
          case _ => println("Errore")
            deathRun.overallDead = true
        }
        case "Jungle" => getAnInt(3) match{
          case 1 => println(deathRun.choices(Chara,location.Jungle.forestFire))
          case 2 => println(deathRun.choices(Chara,location.Jungle.monkeyBusiness))
          case 3 => println(deathRun.choices(Chara,location.Jungle.treantMeeting))
          case _ => println("Errrror")
            deathRun.overallDead = true
        }
        case "The VoidZone" => getAnInt(9) match{
          case 1|7 => println(deathRun.choices(Chara,location.The_VoidZone.multidimensionalPortalHopping))
          case 2|8=> println(deathRun.choices(Chara,location.The_VoidZone.theDarkness))
          case 3|9 => println(deathRun.choices(Chara,location.The_VoidZone.voidwalkers))
          case 4 => println(location.The_VoidZone.BeachTeleportation)
            runSpot = "Beach"
            listSituation = List()
          case 5 => println(location.The_VoidZone.JungleTeleportation)
            runSpot = "Jungle"
            listSituation = List()
          case 6 => println(location.The_VoidZone.VolcanoTeleportation)
            runSpot = "Volcano"
            listSituation = List()

        }
      }
      if (!deathRun.overallDead){ //only adds if not dead
        currentRecord += 1
      }
    }
    if (currentRecord > Chara.personalRecord){ //Update record
      Chara.personalRecord = currentRecord
    }
    //Reset
    def reset()={
      deathRun.overallDead = false
      var newAttributes = readLine("Use the same Character? yes or no").toLowerCase() + " "
      newAttributes.substring(0,1) match{
        case "y" => gameMagic = Chara.magic
        case _ => Chara.setAttributes()
      }
    }

    var choiceFinish = readLine("If finished playing, please type finished").toLowerCase() + " "
    choiceFinish.substring(0,1) match{
      case "e"|"f" => end = false
      case _ => reset()
    }
    println("You record for that run was: "+
      currentRecord)
  }


}

