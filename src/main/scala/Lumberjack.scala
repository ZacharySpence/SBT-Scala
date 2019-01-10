object Lumberjack extends App{
  var randInt = scala.util.Random

  def createLogStorage(size:Int):Array[Int]={
    val logStorage =  Array.fill(size*size+1){randInt.nextInt(4)+1}
//    for (k <- 0 until (size*size)){
//      logStorage(k) =(randInt.nextInt(4)+1)
//    }
    return logStorage
  }

  def fillLogStorage(logs:Int,logStorage:Array[Int]):Array[Int]={
    var logsLeft = logs
    var smallestPile = logStorage.min
    while (logsLeft > 0){
      for(k <- 0 until logStorage.length){
        if (logStorage(k) == smallestPile){
          logStorage(k) += 1
          logsLeft -= 1
          smallestPile = logStorage.min
        }
      }
    }
    return logStorage
  }

  def printLogStorage(size:Int,logStorage:Array[Int])={
    for (k <- 1 to size*size){
      print(logStorage(k)+",")
      if (k%3 == 0){
        println("")
      }
    }
  }

  def doItAll(size:Int,numberOfLogs:Int): Unit ={
    var logStorage = fillLogStorage(numberOfLogs,createLogStorage(size))
    printLogStorage(size,logStorage)
  }

  doItAll(3,7)

}
