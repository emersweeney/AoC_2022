import java.util.Scanner
import scala.collection.mutable.Map
object A1 extends App {
  import scala.io.Source

  val filename = "resources/a1"
  var elfNumber = 1
  var calorieCount = 0
  val calorieMap = Map.empty[Int, Int]

  for (line <- Source.fromFile(filename).getLines()) {
    if (!line.isBlank) {
      calorieCount+=line.toInt
    } else {
      calorieMap.addOne(elfNumber, calorieCount)
      calorieCount=0
      elfNumber+=1
    }
  }

  println(calorieMap.values.max)
}