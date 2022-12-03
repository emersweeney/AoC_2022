import java.util.Scanner
import scala.collection.mutable
import scala.collection.mutable.Map

/*  *** elves carrying snacks ***
  1: Find the Elf carrying the most Calories. How many total Calories is that Elf carrying?
  2: Find the top three Elves carrying the most Calories. How many Calories are those Elves carrying in total? */

object Day1 extends Filereader {
  import scala.io.Source

  var elfNumber = 1
  var calorieCount = 0
  val calorieMap = mutable.Map.empty[Int, Int]

  for (line <- Source.fromFile(filename).getLines()) {
    if (!line.isBlank) {
      calorieCount+=line.toInt
    } else {
      calorieMap.addOne(elfNumber, calorieCount)
      calorieCount=0
      elfNumber+=1
    }
  }

  println("1: "+calorieMap.values.max)

  val first = calorieMap.values.max
  val sansFirstElf = calorieMap.filter(v => v._2!=first)
  val second = sansFirstElf.values.max
  val sansSecondElf = sansFirstElf.filter(v => v._2!=second)
  val third = sansSecondElf.values.max
  println("2: "+ (first+second+third).toInt)
  // this only works cause there is no case of two elves within top 3 calorie counts carrying same amount ...
}