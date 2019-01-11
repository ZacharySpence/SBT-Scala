object LuhnAlgorithm extends App{
  //var digit = "5204408086566492"
  def checkCard(digit:String)={
    var digitStringList = digit.toString.toList.reverse
    var extraWorkStuff = false
    var addition = 0
    def addNumbers(doExtraWork:Boolean,elem:Int)={
      var number= elem
      if (doExtraWork){
        number = number*2
        var numberList = number.toString.toList
        numberList.foreach(num => addition+=num.toInt-48)



      }
      else{
        addition += number
      }
    }

    def checkOddOrEven(extraWork:Boolean):Boolean={
      val doExtraWork = extraWork
      if (doExtraWork){
        extraWorkStuff = false
      }
      else{
        extraWorkStuff = true
      }
      doExtraWork
    }

    digitStringList.foreach(elem => addNumbers(checkOddOrEven(extraWorkStuff),elem.toInt-48))
  println(addition)
    if (addition%10 == 0){
      println("Validated")
    }
    else{
      println("Invalid Card")
    }
  }

  var digit = readLine("Input A Card")
  checkCard(digit)
}
