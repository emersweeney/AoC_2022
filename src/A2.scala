object A2 extends App{
  import java.util.Scanner
  import scala.collection.mutable.Map
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

  val first = calorieMap.values.max
  val sansFirstElf = calorieMap.filter(v => v._2!=first)
  val second = sansFirstElf.values.max
  val sansSecondElf = sansFirstElf.filter(v => v._2!=second)

  val third = sansSecondElf.values.max
  println(first+second+third)
}
