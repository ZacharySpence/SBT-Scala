package euler

object Problems1To10 extends App {
  //Sum all multiples of 3 and 5
  def sumMultiples3And5(num:Int):Int={
    var sum = 0
    for (k <- 1 until num){
      if (k%3==0 || k%5==0){
        sum += k
      }
    }
    return sum
  }
  //println(sumMultiples3And5(1000))


//  //def evenFibonacci(num:Int)={//
//    def Fibonacci(num:Int)={
//      var sum = 0
//      if (num > 0){
//        number =
//      }
//    }
//  } //Scre

  def primeFactors(num:Long):Long={
    def getUniquePrimes(num:Long):List[Long]={
      var primeList:List[Long] = List(2L,3L,5L,7L).reverse
      var k = 2L
      while(k < num){
        var primefactor = true
        primeList.foreach(numbers => if(k%numbers ==0){primefactor = false})
        if (primefactor){
          primeList ::= k
        }
        k+=1
        println(primeList)
      }

      return primeList
    }

    var uniquePrimeList:List[Long] = getUniquePrimes(num).reverse
    println("unique prime List: "+uniquePrimeList)
    var listOfPrimeFactors:List[Long] = List()



    def checkPrime(num:Long):Long={
      for (k <- 0 until uniquePrimeList.length){
        var divisionNumber = uniquePrimeList(k)
        var isItPrime = num%divisionNumber
        if (isItPrime==0){
          listOfPrimeFactors ::= divisionNumber
          return num/divisionNumber
        }
      }
      return num
    }


    var newNum = num
    var k = 0
    def timesList(ListOf:List[Long]):Long={
      var times = 1L
      ListOf.foreach(element => times *= element)
      return times
    }
    while (timesList(listOfPrimeFactors)!= num){
      //println(timesList(listOfPrimeFactors))
      newNum = checkPrime(newNum)
    }
    println(listOfPrimeFactors)
    return listOfPrimeFactors.max
  }
  //println(primeFactors(600851475143L))

 // println(999*999)

  def getPalindrome(num1:Int,num2:Int):Int= {
    var num = 0
    var numList:List[Int] = List()
   for (k <- num1 to 1 by -1;p <- num2 to 1 by -1){
      num = biggestPalindrome(k,p)
      if (num != 0){
        numList ::= num
      }
    }

    def biggestPalindrome(num1: Int, num2: Int): Int = {
      var numList = (num1 * num2).toString.map(_.asDigit)
      var palindrome = true
      if (numList.length % 2 == 0) {
        for (k <- 0 until numList.length / 2){
          if (numList(k) != numList(numList.length-1 - k)) {
            palindrome = false
          }
        }
        if (palindrome) {
          return num1 * num2
        }
      }
      return 0
    }

  return numList.max
  }

  //println(getPalindrome(999,999))


  def smallestMultiple():Int={
    var smallest = 19
    var multipleList:List[Int]=List(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
    var found = false
    while(!found){
      smallest +=1
      var isSmallest = true
        multipleList.foreach(multiple => if (smallest%multiple != 0){isSmallest = false})
      if (isSmallest){
        found = true
      }

    }
    return smallest
  }

  //println(smallestMultiple())


  def sumSquareDifference(num:Int):Int={

    def squareSum(num:Int):Int={
      var squaresum = 0
      for (k <- 1 to num){
        squaresum += k * k
      }
      return squaresum
    }

    def sumOfSquares(num:Int):Int={
      var sumofsquares = 0
      for (k <- 1 to num){
        sumofsquares += k
      }
      return sumofsquares * sumofsquares
    }
    return sumOfSquares(num) - squareSum(num)
  }
  //println(sumSquareDifference(100))

  def tenthousenthPrime():Int= {
    var primeList: List[Int] = List(2, 3, 5, 7)

    def getUniquePrimes(num: Int)= {
      var k = 1
      while (primeList.length < 10002) {
        var primefactor = true
        primeList.foreach(primes => if (k % primes == 0) {
          primefactor = false
        })
        if (primefactor) {
          primeList ::= k
        }
        k += 1
        println(primeList)
      }
    }
    return primeList(10000)
  }
 // println(tenthousenthPrime())

  def greatestProductFromDigits(size:Int):Int={
    var digitField = "7316717653133062491922511967442" +
      "6574742355349194934969835203127745063262395783" +
      "1801698480186947885184385861560789112949495459" +
      "5017379583319528532088055111254069874715852386" +
      "3050715693290963295227443043557668966489504452" +
      "4452316173185640309871112172238311362229893423" +
      "3803081353362766142828064444866452387493035890" +
      "7296290491560440772390713810515859307960866701" +
      "7242712188399879790879227492190169972088809377" +
      "6657273330010533678812202354218097512545405947" +
      "5224352584907711670556013604839586446706324415" +
      "7221553975369781797784617406495514929086256932" +
      "1978468622482839722413756570560574902614079729" +
      "6865241453510047482166370484403199890008895243" +
      "4506585412275886668811642717147992444292823086" +
      "3465674813919123162824586178664583591245665294" +
      "7654568284891288314260769004224219022671055626" +
      "3211111093705442175069416589604080719840385096" +
      "2455444362981230987879927244284909188845801561" +
      "6609791913387549920052406368991256071760605886" +
      "1164671094050775410022569831552000559357297257" +
      "1636269561882670428252483600823257530420752963" +
      "450"

    var digitList = digitField.toList.map(_.toString.toInt)
    var MAX = 0

    for (k <- 0 until digitList.length-size){
      var multiplication = 1
      for (p <- k until size+k){
        multiplication *= digitList(p)
      }
      if (multiplication > MAX){
        MAX = multiplication
      }
    }
    return MAX
  }
  //println(greatestProductFromDigits(13)) Should be working!

  def pythagoramTriplet(num:Int):Int={
    for (k <- 1 to num;p <- 1 to num){
      if ((k*k)+(p*p) == (num-k-p)*(num-k-p)){
        println(k+" "+p+" "+(num -k-p))
        return (k * p * num-k-p)
      }
    }
    return 0
  }
  //println(pythagoramTriplet(1000))

  def triangularDivisors():Int={
    var triangleSize = 0
    var theLength = 1
    var k = 2
    while (theLength <= 500) {
      var divisorList: List[Int] = List(1)
      triangleSize += k
      var p = 2
      while (divisorList.length <= 500 && p != k) {
        if (triangleSize % p == 0) {
          divisorList ::= p
        }
        p += 1
      }
      k += 1
      theLength = divisorList.length
      if (divisorList.length > 500){
        println("it exists")
      }
    }
    return triangleSize
  }

  println(triangularDivisors)

}
