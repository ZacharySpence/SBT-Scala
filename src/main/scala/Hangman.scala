object Hangman extends App{
  var alphabet = "abcdefghijlkmnopqrstuvwxyz".toList
  def theGame(word:String)={

    var theWordArray = word.toArray
    var size = theWordArray.length
    var hangManArray = Array.fill(size)(" _ ")
    var correctAnswers = 0
    var gettingHung = 0
    var finished = false
    var chosenAnswers:List[Char] = List()

    def checkWord()={
      var chosenCharacter:Char = '0'
      var correct = false
      while(!alphabet.contains(chosenCharacter) || chosenAnswers.contains(chosenCharacter)){
        println("Choose A Letter from a-z")
        chosenCharacter= readChar
      }
      chosenAnswers ::= chosenCharacter
      for (k <- 0 until theWordArray.length()){
        if (theWordArray(k).toString().toLowerCase() == chosenCharacter.toString()){
          correctAnswers += 1
          hangManArray(k) = " "+chosenCharacter+" "
          correct = true
        }
      }
      if (!correct){
        gettingHung += 1
      }
    }

    def checkWinOrLose()={
      //Lose
      if (gettingHung >= 7){
        println("You Were Hung. better luck next time")
        //fill in the array to show the correct answer
        theWordArray.foreach(letter => print(letter))
        finished = true
      }
      else if (correctAnswers >= size){
        println("You guessed it! well done.")
        hangManArray.foreach(letter => print(letter))
        finished = true
      }
      else{
        hangManArray.foreach(letter => print(letter))
      }

    }

    hangManArray.foreach(letter => print(letter))
    while (!finished){
      checkWord()
      checkWinOrLose()
    }
  }

  theGame("SpeedWagon")

}
