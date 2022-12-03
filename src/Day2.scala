import scala.io.Source

/*  *** rock paper scissors ***
  1: What would your total score be if everything goes exactly according to your strategy guide?
  2: Following the Elf's instructions for the second column, what would your total score be if
  everything goes exactly according to your strategy guide? */

object Day2 extends Filereader {

  def shapeScore(shape: Char): Int = {
    shape match{
      case 'A' | 'X' => 1
      case 'B' | 'Y' => 2
      case 'C' | 'Z' => 3
    }
  }

  def resultScore(play: (Char, Char)): Int = {
    play match {
      case ('A','X') | ('B','Y') | ('C','Z') => 3
      case ('A','Y') | ('B','Z') | ('C','X') => 6
      case _ => 0
    }
  }

  /* (opponent_score, my_score) */
  def getScores: (Int, Int) = {
    var scores: (Int, Int) = (0,0)
    for (line <- Source.fromFile(filename).getLines()) {
      val opponent_play = line.charAt(0)
      val my_play = line.charAt(2)
      val play = (opponent_play, my_play)
      val result = resultScore(play)
      scores=(scores._1+shapeScore(opponent_play)+result, scores._2+shapeScore(my_play)+result)
    }
    scores
  }

  println("1: "+getScores._2)

  def shape(opponent: Char, result: Int): Char = {
    (opponent, result) match {
//      case (a: Char, b:Int) if (b==3) => (a.toInt+23).toChar
      case ('A',0) => 'Z'
      case ('A',3) => 'X'
      case ('A',6) => 'Y'
      case ('B',0) => 'X'
      case ('B',3) => 'Y'
      case ('B',6) => 'Z'
      case ('C',0) => 'Y'
      case ('C',3) => 'Z'
      case ('C',6) => 'X'
    }
  }

  def getScores_2: (Int, Int) = {
    var scores: (Int, Int) = (0,0)
    for (line <- Source.fromFile(filename).getLines()) {
      val opponent_play = line.charAt(0)
      val result = line.charAt(2) match {
        case 'X' => 0
        case 'Y' => 3
        case 'Z' => 6
      }
      val my_play = shape(opponent_play,result)
      scores=(scores._1+shapeScore(opponent_play)+result, scores._2+shapeScore(my_play)+result)
    }
    scores
  }

  println("2: "+getScores_2._2)

}
