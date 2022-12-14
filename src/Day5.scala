import scala.collection.immutable.ListMap
import scala.io.Source
import scala.util.Using

/*
  1: After the rearrangement procedure completes, what crate ends up on top of each stack? (Moving one at a time)
  2: After the rearrangement procedure completes, what crate ends up on top of each stack? (Moving multiple at a time)
 */
object Day5 extends Filereader {

  solve()

  var stackMap1: Map[Int, List[Char]] = Map.empty
  var stackMap2: Map[Int, List[Char]] = Map.empty

  def operate(stackMap: Map[Int, List[Char]], oneAtATime: Boolean): Map[Int, List[Char]] ={
    var stackMap: Map[Int, List[Char]] = Map.empty
    Using(Source.fromFile(filename)) {
      source => {
        for (line <-  source.getLines) {
          line match {
            case s"move $quantity from $stack1 to $stack2" => stackMap = moveCrates(stackMap, quantity.toInt, stack1.toInt, stack2.toInt, oneAtATime)
            case s"$empty[$l" => stackMap = mapStacks(stackMap, line)
            case _ => ()
          }
        }
      }
    }
    stackMap
  }

  def solve(): Unit = {
    val map1 = operate(stackMap1, oneAtATime = true)
    val sorted1 = ListMap(map1.toSeq.sortBy(_._1):_*)

    val map2 = operate(stackMap2, oneAtATime = false)
    val sorted2 = ListMap(map2.toSeq.sortBy(_._1):_*)

    print("1: ")
    for (s <- sorted1){
      print(s._2.take(1).head)
    }

    println("")

    print("2: ")
    for (s <- sorted2){
      print(s._2.take(1).head)
    }
  }


  def mapStacks(map: Map[Int, List[Char]], line: String): Map[Int, List[Char]] = {
    var stackMap = map
    var stackNum: Int = 1
    var charCount: Int = 0
    var nextIndex: Int = 1
    for (c <- line) {
      if (charCount == nextIndex) {
        if (c!=' ') {
          val crates = stackMap.getOrElse(stackNum, List.empty).++(List(c))
          stackMap = stackMap.+(stackNum -> crates)
        }
        nextIndex += 4
        stackNum += 1
      }

      charCount += 1
    }
   stackMap
  }

  def moveCrates(map: Map[Int, List[Char]], quantity: Int, stack1: Int, stack2: Int, oneAtATime: Boolean): Map[Int, List[Char]] = {
    val stackMap = map
    val crates1 = stackMap(stack1)
    val cratesToMove = if (oneAtATime) {
      crates1.take(quantity).reverse
    } else {
      crates1.take(quantity)
    }
    val newCrates = crates1.drop(quantity)
    val crates2 = cratesToMove.++(stackMap(stack2))
    stackMap + (stack1 -> newCrates, stack2 -> crates2)
  }

}
