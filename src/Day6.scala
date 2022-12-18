import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

/*
  1: How many characters need to be processed before the first start-of-packet marker is detected?
  2: How many characters need to be processed before the first start-of-message marker is detected?
 */
object Day6 extends Filereader {

  Using(Source.fromFile(filename)) {
    source =>
      for (line <- source.getLines()){
        val list: List[Char] = List[Char]{' '}
        val endChar=0
        println("1: "+getEndCharIndex(line, list, endChar))
        println("2: "+getEndCharIndex(line, list, endChar, 14))
      }
  }

  @tailrec
  def getEndCharIndex(tLine: String, list: List[Char], endChar: Int, size: Int = 4): Int = {
    val c: Char = tLine.head
    var newList = list ++ List(c)
    var newLine = tLine
    val char = endChar+1
    if (endChar>size-2){
      newList = newList.drop(1)
      newLine = newLine.drop(1)
    }
    else {
      newLine = newLine.drop(1)
    }
    if (endChar>2 && !containsDuplicates(newList)) char else getEndCharIndex(newLine, newList, char, size)
  }

  def containsDuplicates(chars: List[Char]): Boolean = {
      val duplicates = chars.groupBy(identity).collect {
        case (char,occurrence) if occurrence.length > 1 => char
      }
    if (duplicates.nonEmpty) true else false
  }



}
