//All the scala newbie things
object scalaNewbieProjects extends App{
  println("Hello World")
  var hW = "Hello World"
  def tryThemAllOut={
    var word = "Hello"
    var word1 = "Ha"
    var word2 = "llo"
    var char1 = "e"
    var char2 = "a"
    var num1 = 4
    var num2 = 6
    var TorF = true
    println(this.returnWord(word))
    println(this.returnAny(char1))
    println(this.partOfString(word,2))
    println(this.replaceAndConcatenateString(word1,word2,char1,char2))
    println(this.sumOrMultiply(num1,num2,TorF))
    this.repeatMyself(word,num2)
    this.wordSquare(word,num2)
  }
  def returnWord(word:String):String ={return(word)}
  def returnAny(thing:Any):Any={return(thing)}
  def partOfString(word:String,number:Int):String={return(word.substring(number))}
  def replaceAndConcatenateString(word1:String,word2:String,
                                  replacingCharacter:String,
                                  replacedCharacter:String):String= {
    word1.replace(replacedCharacter,replacingCharacter)
    word2.replace(replacedCharacter,replacingCharacter)
    return(word1+word2)
  }
  def sumOrMultiply(number1:Int,number2:Int,TorF:Boolean):Int= {
    if (TorF) {
      return (number1 + number2)
    }
    if (number1 == 0 && number2 > 0){
      return(number2)
    }
    else if (number2 == 0 && number1 > 0){
      return (number1)
    }
    else if(number1 == 0 && number2 == 0){
      return(0)
    }
    return (number1 * number2)
  }

  def repeatMyself(word:String,repeats:Int)={
    for (k <- 1 to repeats){
      println(word)
    }
  }
  def wordSquare(word:String,squareSize:Int)={
    for (k <- 1 to squareSize/(squareSize/2)){
      for (m <-1 to squareSize/2) {
        print(" "+word+" ")
      }
      print("\n")
    }
  }
  def FizzBuzz(word1:String,word2:String,number1:Int)={
    for(k <- 1 to number1){
      if (k%3 == 0 && k%5 == 0){
        print(word1+word2)
      }
      else if (k%3 == 0){
        print(word1)
      }
      else if (k%5 == 0){
        print(word2)
      }
      else{
        print(k)
      }
    }
  }

  //PatternMatching
  def swapTheInts(ALT:Any)={

  }

  //games
  def blackjack(number1:Int,number2:Int):Int={
    if ((number1 > number2 || number2 > 21)&& number1 <= 21 ){
      return (number1)
    }
    else if ((number2 > number1 || number1 > 21) && number2 <= 21){
      return (number2)
    }
    else{
      return (0)
    }
  }

  def uniqueSum(number1:Int,number2:Int,number3:Int):Int={
    if (number1 == number2 && number2 == number3){
      return (0)
    }
    else if (number1 == number2){
      return(number3)
    }
    else if (number1 == number3){
      return(number2)
    }
    else if (number2 == number3){
      return(number1)
    }
    else{
      return(number1+number2+number3)
    }
  }

  def tooHot(number1:Int,TorF:Boolean):Boolean={
    if (TorF) {
      if (number1 >= 60 && number1 <= 100) {
        return true
      }
    }
    else{
      if (number1 >= 60 && number1 <= 90){
        return true
      }
    }
    return false
  }


  tryThemAllOut
}
