object  RockPaperScissors extends App {
    var randInt = scala.util.Random
    var choiceList = List("r","p","s")

    var playingList:List[String] = List()
    addAI(2)
    addPlayer()

    //playingList = playingList.reverse
    def addAI(numberOf:Int)={
        println("How many AI do you want to play against")
        var howMany = readInt()
        for (k <- 1 to howMany){
            playingList ::= ("AI"+k)
        }
    }

    def addPlayer()={
        var yesOrNo = (readLine("Add a player? [Yes,No]").toLowerCase()+ " ").substring(0,1)
        yesOrNo match{
            case "y" => playingList ::= "Player"
            case _ => playingList ::= "AIPlayer"
        }
    }

    var used:List[Int] = List()

    def chooseRPS(player:String):String={
        player match{
            case "Player" =>{
                var choice = ""
                var choice2 = 0
                while (!choiceList.contains(choice)) {
                    choice = (readLine("Rock Paper or Scissors?").toLowerCase).substring(0, 1)
                    choice match{
                        case "r" => choice2 = 1
                        case "p" => choice2 = 2
                        case "s" => choice2 = 3

                    }
                }
                return chooseIt(choice2)
            }
            case _ => chooseIt(randInt.nextInt(3)+1)

        }
    }

    def chooseIt(choice:Int,unused:Int = 0):String= {
        var choice2 = choice
        used = used.distinct
        while (!used.contains(choice2) && used.length > 1){
            choice2 = randInt.nextInt(3)+1
        }
        used ::= choice2
        choice2 match {
            case 1 => return "Rock"
            case 2 => return "Paper"
            case 3 => return "Scissors"
        }
    }

    def InOrOut(playerChoice:String,AIChoice:String):String={
        //println(s"You chose $playerChoice")
        var pC = playerChoice.toLowerCase().substring(0,1)
        var aC = AIChoice.toLowerCase().substring(0,1)
        pC match{
            case "r" => aC match{
                //returns winning choice
                case "s"|"r" => "Rock"
                case "p" => "Paper"
            }
            case "p" => aC match{
                case "p"|"r" => "Paper"
                case "s" => "Scissors"
            }
            case "s" => aC match{
                case "s"|"p" => "Scissors"
                case "r" => "Rock"
            }
        }
    }

    def checkWin(AllAIChoices:List[String],chosenList:List[String]):List[String]= {
        var keepPlayingList: List[String] = List()
        var winningChoice = InOrOut(AllAIChoices(0),AllAIChoices(1))
        println(s"The winning choice was $winningChoice")
        if (AllAIChoices(0) == winningChoice) {
            keepPlayingList ::= chosenList(0)
        }
        for (k <- 1 until AllAIChoices.length) {
            if (AllAIChoices(k) == winningChoice) {
                keepPlayingList ::= chosenList(k)
            }
        }
        return keepPlayingList
    }


    def Game(playList:List[String]):List[String]={
        used = List()
        var whatEveryoneChose:List[String] = List()
        playList.foreach(player => whatEveryoneChose ::= chooseRPS(player))
        whatEveryoneChose = whatEveryoneChose.reverse

        playList.foreach(player => print(s"[$player]"))
        println("")
        whatEveryoneChose.foreach(choice => print(s"[$choice]"))
        println("")

        var newplayingList:List[String] = List()
         checkWin(whatEveryoneChose,playList).foreach(player => newplayingList ::= player)
        return newplayingList
    }


    var play = true
    while(play){
        var numberOfPlayers:List[String] = Game(playingList)
        while(numberOfPlayers.length > 1){
            numberOfPlayers = Game(numberOfPlayers)
        }
        println("")
        println("The Winner is: "+numberOfPlayers(0))

        var choice2 = readLine("Do you want to continue playing? [Yes,No]").toLowerCase().substring(0,1)
            choice2 match{
                case "n" => play = false
                case _ => play = true
            }
    }
}
