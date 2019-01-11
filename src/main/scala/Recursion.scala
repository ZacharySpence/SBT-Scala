object Recursion extends App {
  def sum(num: Int): Int = {
    var number = 0
    if (num > 0) {
      number += num + sum(num - 1)
    }
    return number
  }

  def factorial(num: Int): Int = {
    var number = 1
    if (num > 0){
      number = num * factorial(num - 1)
    }
    return number
  }

  def countZeros(numArray:Array[Int],usedIndex:Int=0):Int ={
    var count = 0
    var tempCount = 0
    if (numArray(usedIndex) == 0){

      tempCount = 1
    }

    if (usedIndex < numArray.length-1){
      count += tempCount+ countZeros(numArray,usedIndex+1)
    }
    return count
  }


  def findMinElement(numArray:Array[Int],usedIndex:Int=0):Int={
    var min = numArray(usedIndex)
    if (numArray(usedIndex)> numArray(usedIndex+1)){
      min = numArray(usedIndex+1)

    }
    if (usedIndex < numArray.length-2) {
       min = findMinElement(numArray, usedIndex + 1)
    }
    return min
  }
  println("\n"+countZeros(Array(0,0,4,0,1,2)))
  println(findMinElement(Array(13,12,22,6,9,2,2,0,44,86)))
  println(factorial(5))
  println(sum(120))
}

