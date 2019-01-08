
object getDuplicates extends App {
  var firstHalf = 1 to 1000000 toArray
  var secondHalf:Array[Int] = Array(42)
  var testArray:Array[Int] = firstHalf ++ secondHalf
  //def getSum(sumArray:Array[Int]):Int={
  //  var sum =(sumArray.length/2)*(sumArray.reduceLeft(_ min _)+sumArray.reduceLeft(_ max _))
   // println(sum)
  //  return sum
  //}
  def findDuplicate(chosenArray:Array[Int]):Int ={
    var oneToMillion:Array[Int] = 1 to 1000000 toArray
    var sum = oneToMillion.reduceLeft(_ + _)
    var newSum = chosenArray.reduceLeft(_ + _)
    return(newSum-sum)
  }
  print(findDuplicate(testArray))


  //for(k <- 0 to theArray.length-1){

   // newSum += theArray(k)
  //  println(newSum)
  //}
  //var duplicatedNumber =(newSum - sum)
 // println(duplicatedNumber)
}
