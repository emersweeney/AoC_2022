import scala.::
import scala.collection.mutable
import scala.io.Source
/*
  1: Find the item type that appears in both compartments of each rucksack. What is the sum of the priorities of those
  item types?
  2: Find the item type that corresponds to the badges of each three-Elf group. What is the sum of the priorities of
  those item types?
 */
object Day3 extends Filereader {

  def getCommonItem(compartment1: String, compartment2: String): Char = {
    compartment1.filter(c => compartment2.contains(c)).charAt(0)
  }

  def getPriority(c: Char): Int = {
    val lowercase = "[a-z]".r
    c match {
      case lowercase() => c.toInt-96
      case _ => c.toInt-38
    }
  }

  def getPrioritySum: Int = {
    var sum=0;
    for (line <- Source.fromFile(filename).getLines) {
      val midIndex=line.length/2
      val compartment1 = line.substring(0,midIndex)
      val compartment2 = line.substring(midIndex, line.length)
      val commonItem = getCommonItem(compartment1, compartment2)
      sum+=getPriority(commonItem)
    }
    sum
  }

  println("1: "+getPrioritySum)

  /*
    1. get group lines
    2. find common item in lines
    3. get priority of common item
    4. get sum of priorities
   */

  def getCommonItem(rucksack1: String, rucksack2: String, rucksack3: String): Char = {
    rucksack1.filter(c => rucksack2.contains(c) && rucksack3.contains(c)).charAt(0)
  }

  def getBadgePrioritySum: Int = {
    var sum: Int = 0
    var count: Int = 0
    var group: mutable.HashMap[Int, String] = mutable.HashMap.empty
    for (line <- Source.fromFile(filename).getLines()) {
      group.put(count, line)
      if (count==2) {
        val commonItem = getCommonItem(group(0), group(1), group(2))
        sum += getPriority(commonItem)
        count = -1
        group = mutable.HashMap.empty
      }
      count += 1
    }
    sum
  }

  println("2: "+getBadgePrioritySum)
}
