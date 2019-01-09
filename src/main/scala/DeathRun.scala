class DeathRun {
var overallDead = false
  def choices(Chara:Character, deathf:Boolean = false,deatha:Boolean = false, deaths:Boolean = false, deathm:Boolean = false,success:String = "somehow it worked"):String={
    //player chooses 1 of 4 actions.
    var choice = readLine("[Fight,Dodge,Run,Cast Magic]").toLowerCase
    choice.substring(1,1) match {
      case "f" =>
        if (deathf | Chara.attributes.strength < 4){
          this.overallDead = true
          return "You died"
        }
        else{
          return success
        }
      case "d" =>
        if (deatha| Chara.attributes.agility < 4){
          this.overallDead = true
          return "You died"
        }
        else{
          return success
        }
      case "r" =>
        if(deaths | Chara.attributes.speed < 4){
          this.overallDead = true
          return "You died"
        }
        else{
          return success
        }
      case "c" =>
        if(deathm | Chara.attributes.magic == 0){
          this.overallDead = true
          return "You died"
        }
        else{
          Chara.attributes.magic -= 1
          return success
        }
    }
  }

  def blka(a: String) = {




  }

}
class Jungle{

}
class Volcano{

}
class Beach{
  var lobsterAttack = "You've been attacked by were-lobsters!"
  var tsunami = "A tsunami rises in the distance"
  var burningSand = "The sand is so hot it burns!"
}


class Character{
  var attributes = new Attributes
   var name:String = "Nobody"
  var personalRecord = 0
}
class mainGame extends App{
  var deathRun = new DeathRun()
  var Chara:Character = new Character()
  while (!deathRun.overallDead){
    deathRun.choices(Chara)
  }

}



class Attributes{
  var strength:Int = 0
  var agility:Int = 0
  var speed:Int = 5
  var magic:Int = 1
}
