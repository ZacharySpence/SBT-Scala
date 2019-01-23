object Bullshevick extends App {
  var rand = scala.util.Random
  var CardList:List[String]= List("A","2","3","4","5","6","7","8","9","10","J","Q","K")
  var SuitList:List[String] = List("S","C","H","D")
  var fullList:List[String] = List()
  var PlayerHand:List[String] = List()
  var EnemyHand:List[String] = List()

  def createList()={
    for(k <- 0 until SuitList.length){
      for (p <- 0 until CardList.length){
        fullList ::= SuitList(k)+CardList(p)
      }
    }
  }
  createList()
  def randomCardDraw():String={
    var chosenCard = fullList(rand.nextInt(fullList.length))
    println(chosenCard)
    fullList = fullList.filter(_ != chosenCard)
    return chosenCard
  }

  def fillHands()={
    while(fullList.length > 0){
      PlayerHand ::= randomCardDraw()
      EnemyHand ::= randomCardDraw()
    }
  }

  fillHands()
  println(PlayerHand)
  println(EnemyHand)


  def chooseCards(player:Boolean = false)= {
    var cardtype = EnemyHand(rand.nextInt(EnemyHand.length))
    var choice = rand.nextInt(4) + 1
    if (player) {
      cardtype = readLine("Choose a card")+ " "``````
      println("How many of them?")
      choice = readInt()
    }
  }
}
