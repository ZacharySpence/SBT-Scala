object AtbashCipher extends App{
  //var alphabet ="abcdefghijklmnopqrstuvwxyz".toList
  //var cipher = alphabet.reverse

  def Cipher(word:String):String ={
    var cipheredWord = ""
    word.toList.foreach(letter => cipheredWord+=AtbashCiphering(letter))
    println(cipheredWord)
    return cipheredWord
  }

  def AtbashCiphering(letter:Char):String={
    letter.toString.toLowerCase() match{
      case "a" => "z"
      case "b" => "y"
      case "c" => "x"
      case "d" => "w"
      case "e" => "v"
      case "f" => "u"
      case "g" => "t"
      case "h" => "s"
      case "i" => "r"
      case "j" => "q"
      case "k" => "p"
      case "l" => "o"
      case "m" => "n"
      case "n" => "m"
      case "o" => "l"
      case "p" => "k"
      case "q" => "j"
      case "r" => "i"
      case "s" => "h"
      case "t" => "g"
      case "u" => "f"
      case "v" => "e"
      case "w" => "d"
      case "x" => "c"
      case "y" => "b"
      case "z" => "a"
      case _ =>  letter.toString().toLowerCase()
    }
  }

  def Decipher(cipheredWord:String):String ={
    return Cipher(cipheredWord)
  }

  var word = Cipher("Mark is Mac's speed wagon")
  Decipher(word)
}
