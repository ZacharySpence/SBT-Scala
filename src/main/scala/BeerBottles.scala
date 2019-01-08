object BeerBottles extends App{
  var totalBeers = 99
  def beerSong(bigBeer:Int):Unit={
    var beer = bigBeer

    if (beer > 0){
      println(s"$beer bottles of beer on the wall, $beer bottles of beer.")
      beer -= 1
      println(s"Take one down and pass it around, $beer bottles of beer on the wall. \n")
      beerSong(beer)
    }
    else{
      println("No more bottles of beer on the wall, no more bottles of beer. \n" +
        s"Go to the store and buy some more, $totalBeers bottles of beer on the wall.")
    }
  }
  beerSong(totalBeers)
}
