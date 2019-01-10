object  ReverseFactorial extends App {
  var chosenNumber = 120
  def checkFactorial(chosenNumber:Int):String={
    var factorial = 1
    var increasingNumber = 1
    while (factorial < chosenNumber){
      factorial *= increasingNumber
      increasingNumber += 1
    }
    factorial match{
      case chosenNumber => return ((increasingNumber-1).toString()+"!")
      case _ => return "NONE"
    }
  }
  println(checkFactorial(3628800))
}
