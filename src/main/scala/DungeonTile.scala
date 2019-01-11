object  DungeonTile extends App {
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
        println("dsp "+dungeonPreviousList)
        addPreviousPosition(dungeonPreviousList)
        println("After Dsp "+dungeonSquare)
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
        println(choice)
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
      println("After new choice ds "+ dungeonSquare)
      dungeonList ::= dungeonSquare
      dungeonPreviousList = List()
      dungeonPreviousList = dungeonSquare
    }
    return dungeonList
  }
  println(makeDungeon(8).reverse)
}