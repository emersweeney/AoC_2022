import scala.io.Source

/*
  1: In how many assignment pairs does one range fully contain the other?
  2: In how many assignment pairs do the ranges overlap?
 */
object Day4 extends Filereader {

  implicit def boolAsInt(b: Boolean): Int = if (b) 1 else 0
  var containsCounter=0
  var overlapsCounter=0

  for (line <- Source.fromFile(filename).getLines()){
    val (range1, range2) = getRangesFromLine(line)
    containsCounter+=rangeContainsAnother(range1, range2)
    overlapsCounter+=rangesOverlap(range1, range2)
  }

  println("1: "+containsCounter)
  println("2: "+overlapsCounter)

  def getRangesFromLine(line: String): ((Int, Int), (Int, Int)) = {
    val array = line.split("[,-]")
    ((array(0).toInt, array(1).toInt), (array(2).toInt, array(3).toInt))
  }

  def rangeContainsAnother(r1: (Int, Int), r2: (Int, Int)): Boolean = {
    val y = r2._1
    val (inner, outer) = r1._1 match {
      case x if (x > y || (x == y && r2._2>r1._2)) => (r1, r2)
      case _ => (r2, r1)
    }
    inner._2 <= outer._2
  }

  def rangesOverlap(r1: (Int, Int), r2: (Int, Int)): Boolean = {
    val overlap = r1 match {
      case r if (r._1 <= r2._1 && r._2 >= r2._1) => true
      case r if (r._1 <= r2._2 && r._2 >= r2._2) => true
      case r if (r2._1 <= r._1 && r2._2 >= r._1) => true
      case r if (r2._1 <= r._2 && r2._2 >= r._2) => true
      case _ => false
    }
    overlap || rangeContainsAnother(r1, r2)
  }

}
