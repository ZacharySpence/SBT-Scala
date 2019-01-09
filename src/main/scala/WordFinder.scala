import scala.io.Source
object WordFinder extends App {
  val words = Source.fromFile("C:\\Users\\Admin\\Documents\\Zach Scala\\enable1.txt").getLines.toList
  val anagrams = Source.fromFile("C:\\Users\\Admin\\Documents\\Zach Scala\\tryout.txt").getLines.toList
  var newWordsList:List[String]= List()

  def matching(word:String, anagramWord:String):Tuple2[Boolean,String]={
    for (k <- anagramWord.toList){
     // println("word: "+word)
      //println("k: "+k)
      if (word == k.toString){
        return (true,k.toString)
      }

    }
    return (false,"")
  }

  def check(k:Int)={
    var newWord = ""
    var index = 0
    var anagramIndex = 0
    while (index != words(k).length && matching(words(k)(index).toString,anagrams(anagramIndex))._1){
      newWord += matching(words(k)(index).toString,anagrams(anagramIndex))._2
      index+=1
      //println("newWord:"+newWord+" "+index)
    }
    //newWordsList ::: newWord
    if (index == words(k).length){//so reached the end of the word
      println("newWord: "+newWord)
    }

  }

  //matching(words(0)(0).toString,anagrams(0))._1
  for (m<- 0 until anagrams.length){
    for (k <- 0 until words.length
         if (checkContain(k,m))){
      // for (m <- 0 until anagrams(0).length){
      check(k)
  }

   // }

    //Get sublist.

  }
  def checkContain(k:Int,m:Int):Boolean ={
    var result = false
    for(p <- 0 until anagrams(m).length) {
      if (words(k)(0).toString.contains(anagrams(m)(p).toString)) {
        result =  true
      }
    }
    result
  }
}
