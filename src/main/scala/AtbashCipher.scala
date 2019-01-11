object AtbashCipher extends App{
  //Atbash and MorseCode Cipher



  var atbashCipher = (("z","y","x","w","v","u","t","s","r","q","p","n","m"),("o","l","k","j","i","h","g","f","e","d","c","b","a"),(0,1,2,3,4,5,6,7,8,9),"")

  var morseCode = (
    (".-","-...","-.-.","-..",".","..-.","--.","....","..",".---","-.-",".-..","--"),
    ("-.","---",".--.","--.-",".-.", "...","-","..-","...-",".--","-..-","-.--.","--.."),
    ("-,-,-,-,-",".----","..---","...--","....-",".....","-....","--...","---..","----."),"/")

  def Ciphering(letter:Any,
                cipher:Tuple4[
                  Tuple13[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any],
                  Tuple13[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any],
                  Tuple10[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any],String]):String={
      letter.toString.toLowerCase() match{
        case "a" => cipher._1._1 + cipher._4
        case "b" => cipher._1._2 + cipher._4
        case "c" => cipher._1._3 + cipher._4
        case "d" => cipher._1._4 + cipher._4
        case "e" => cipher._1._5 + cipher._4
        case "f" => cipher._1._6 + cipher._4
        case "g" => cipher._1._7 + cipher._4
        case "h" => cipher._1._8 + cipher._4
        case "i" => cipher._1._9 + cipher._4
        case "j" => cipher._1._10 + cipher._4
        case "k" => cipher._1._11 + cipher._4
        case "l" => cipher._1._12 + cipher._4
        case "m" => cipher._1._13 + cipher._4
        case "n" => cipher._2._1 + cipher._4
        case "o" => cipher._2._2 + cipher._4
        case "p" => cipher._2._3 + cipher._4
        case "q" => cipher._2._4 + cipher._4
        case "r" => cipher._2._5 + cipher._4
        case "s" => cipher._2._6 + cipher._4
        case "t" => cipher._2._7 + cipher._4
        case "u" => cipher._2._8 + cipher._4
        case "v" => cipher._2._9 + cipher._4
        case "w" => cipher._2._10 + cipher._4
        case "x" => cipher._2._11 + cipher._4
        case "y" => cipher._2._12 + cipher._4
        case "z" => cipher._2._13 + cipher._4
        case "0" => cipher._3._1+cipher._4
        case "1" => cipher._3._2+cipher._4
        case "2" => cipher._3._3+cipher._4
        case "3" => cipher._3._4+cipher._4
        case "4" => cipher._3._5+cipher._4
        case "5" => cipher._3._6+cipher._4
        case "6" => cipher._3._7+cipher._4
        case "7" => cipher._3._8+cipher._4
        case "8" => cipher._3._9+cipher._4
        case "9" => cipher._3._10+cipher._4
        case _ =>  letter.toString()+cipher._4
      }
  }


  def Deciphering(letter:Any,
                cipher:Tuple4[
                  Tuple13[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any],
                  Tuple13[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any],
                  Tuple10[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any],String]):String={
    letter.toString().toLowerCase()match{
        case cipher._1._1 =>"a"
        case cipher._1._2 =>"b"
        case cipher._1._3 =>"c"
        case cipher._1._4 =>"d"
        case cipher._1._5 =>"e"
        case cipher._1._6 =>"f"
        case cipher._1._7 =>"g"
        case cipher._1._8 =>"h"
        case cipher._1._9 =>"i"
        case cipher._1._10 =>"j"
        case cipher._1._11 =>"k"
        case cipher._1._12 =>"l"
        case cipher._1._13 =>"m"
        case cipher._2._1 =>"n"
        case cipher._2._2 =>"o"
        case cipher._2._3 =>"p"
        case cipher._2._4 =>"q"
        case cipher._2._5 =>"r"
        case cipher._2._6 =>"s"
        case cipher._2._7 =>"t"
        case cipher._2._8 =>"u"
        case cipher._2._9 =>"v"
        case cipher._2._10 =>"w"
        case cipher._2._11 =>"x"
        case cipher._2._12 =>"y"
        case cipher._2._13 =>"z"
        case cipher._3._1 => "0"
        case cipher._3._2 => "1"
        case cipher._3._3 => "2"
        case cipher._3._4 => "3"
        case cipher._3._5 => "4"
        case cipher._3._6 => "5"
        case cipher._3._7 => "6"
        case cipher._3._8 => "7"
        case cipher._3._9 => "8"
        case cipher._3._10 => "9"
        case cipher._4 =>  ""
        case _ => letter.toString()
    }
  }

  def Cipher(word:String,cipher:Tuple4[
    Tuple13[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any],
    Tuple13[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any],
    Tuple10[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any],String]):String ={
    var cipheredWord = ""
    word.toList.foreach(letter => cipheredWord+=Ciphering(letter,cipher))
    return cipheredWord
  }

  def Decipher(stringSplit:String="",cipheredWord:String,cipher:Tuple4[
    Tuple13[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any],
    Tuple13[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any],
    Tuple10[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any],String]):String ={
    var decipheredWord = ""

    if (stringSplit != ""){
      var changedWord = cipheredWord.split(stringSplit)
      changedWord.foreach(letter => decipheredWord+=Deciphering(letter,cipher))
      return decipheredWord
    }
    var changedWord= cipheredWord
    changedWord.toArray.foreach(letter => decipheredWord+=Deciphering(letter,cipher))
    return decipheredWord
  }

  def cipherAndDecipher(word:String,cipher:Tuple4[
    Tuple13[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any],
    Tuple13[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any],
    Tuple10[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any],String]): Unit ={
    var cipheredWord = Cipher(word,cipher)
    var decipheredWord = Decipher(cipher._4,cipheredWord,cipher)
    println(s"Word: $word, \n Ciphered Word: $cipheredWord,\n Deciphered Word: $decipheredWord")
  }

  //Works for Character Ciphers
  cipherAndDecipher("Mark is Mac's Speed Wagon",atbashCipher)
  cipherAndDecipher("Maggie Mee's make me pleased",morseCode)

}
