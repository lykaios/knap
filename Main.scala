import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.concurrent._
import scala.concurrent.duration.Duration
import ExecutionContext.Implicits.global
import java.io._

import scala.annotation.tailrec

/*----------------- CLASSES -----------------*/
case class Item(value: Int, weight: Int, id: Int) {
  val density = value / weight

  def ratioValue(curCapacity: Int): Int = {
    val ratio = curCapacity / weight.toDouble
    math.ceil(ratio * value).toInt
  }
}

abstract class Knapsack(itemCount: Int, capacity: Int, itemsIn: List[Item]) {
  //block long operations
  val TIMEOUT = 60 * 60

  def fullPrint: String = {
    s"$this\nitems: $itemsIn"
  }

  def valueForItems(input: List[Item]): Int = {
    input.map(i => i.value).sum
  }

  def outputForItems(results: List[Item]): String = {
    itemsIn.map(i => if (results.contains(i)) 1 else 0).mkString(" ")
  }

  override def toString: String = {
    s"$itemCount $capacity"
  }
}

//Branch and bound
case class KnapBB(itemCount: Int, capacity: Int, itemsIn: List[Item]) extends Knapsack(itemCount, capacity, itemsIn) {

  @tailrec
  final def relaxationAt(index: Int, remK: Int, curVal: Int = 0): Int = {
    if (remK < 1 || index >= itemsIn.size)
      curVal
    else {
      val searchSpace = itemsIn.dropWhile(i => i.id < index).sortBy(i => -i.density)
      searchSpace match {
        case s :: ss => {
          if (s.weight <= remK)
            relaxationAt(index + 1, remK - s.weight, curVal + s.value)
          else {
            val partial = ((s.weight - remK) / s.weight.toDouble) * s.value
            curVal + partial.toInt
          }
        }
        case _ => throw new Exception("Unmatched case in relaxationAt")

      }
    }
  }


  def performRelaxedSearch: (Int, String) = {
    val greedyResults = relaxedSearch(0, SearchObj(List()), capacity)
    val greedyValue = valueForItems(greedyResults)
    if (greedyValue > relaxationAt(1, capacity))
      (greedyValue, outputForItems(greedyResults))
    else {
      val branch = relaxedSearch(1, SearchObj(List()), capacity)
      (valueForItems(branch), outputForItems(branch))
    }

  }

  def relaxedSearch(index: Int, cur: SearchObj, rCapacity: Int): List[Item] = {
    val relaxed = relaxationAt(index, rCapacity, cur.value)
    //println(s"relaxedAt: $index $relaxed")
    if (relaxed < cur.value || index >= itemsIn.size)
      cur.choices
    else if (itemsIn(index).weight < rCapacity) {
      relaxedSearch(index + 1, SearchObj(itemsIn(index) :: cur.choices), rCapacity - itemsIn(index).weight)
    }
    else
      relaxedSearch(index + 1, cur, rCapacity)
  }

  case class SearchObj(choices: List[Item]) {
    val value = choices.map(i => i.value).sum
  }

}

case class KnapDP(itemCount: Int, capacity: Int, itemsIn: List[Item]) extends Knapsack(itemCount: Int, capacity: Int, itemsIn: List[Item]) {

  //offset one due to empty first column
  val items = itemsIn.map(i => (i.id + 1, i)).toMap
  val table = fillTable

  def fillDynamicTable(arr: Array[Array[Int]], column: Int, k: Int): Array[Array[Int]] = {
    val item = items(column)

    for (r <- 0 to k) {
      val prev = arr(r)(column - 1)

      if (item.weight > r)
        arr(r)(column) = arr(r)(column - 1)
      else {
        val ifTaken = item.value + valWithObjTaken(arr, column, r)
        if (prev > ifTaken)
          arr(r)(column) = prev
        else
          arr(r)(column) = ifTaken
      }
    }
    arr
  }

  def valWithObjTaken(arr: Array[Array[Int]], itemIndex: Int, curCapacity: Int): Int = {
    val item = items(itemIndex)
    val leftoverCapacity = curCapacity - item.weight

    if (leftoverCapacity < 0)
      0
    else
      arr(leftoverCapacity)(itemIndex - 1)
  }

  def fillTable: Array[Array[Int]] = {
    var dp = Array.ofDim[Int](capacity + 1, items.size + 1)
    for (count <- 1 to items.size) {
      fillDynamicTable(dp, count, capacity)
    }
    dp
  }

  @tailrec
  final def decideTable(results: List[Int], curCap: Int, curItem: Int): List[Int] = {
    if (curItem == 0 || curCap == 0)
      results
    else if (table(curCap)(curItem - 1) != table(curCap)(curItem))
      decideTable(1 :: results, curCap - items(curItem).weight, curItem - 1)
    else
      decideTable(0 :: results, curCap, curItem - 1)
  }

  def decideDp: String = {
    decideTable(List(), capacity, items.size).mkString(" ")
  }

  def printTable = {
    print(table.map(e => e.mkString("  ")).mkString("\n-------------\n"))
  }

}

object Parser {
  def readFile(path: String): ParsedInput = {
    val lines = Source.fromFile(path).getLines().toList
    val header = lines.head.split(" ")

    val itemsObj = lines.tail.zipWithIndex.map(i => {
      val split = i._1.split(' ')
      Item(split(0).toInt, split(1).toInt, i._2)
    })

    ParsedInput(header(0).toInt, header(1).toInt, itemsObj)
  }

  case class ParsedInput(itemCount: Int, capacity: Int, items: List[Item])

}

/*-------------------------------------------*/

//invocation: scala Main.scala ks_1000_0
//TODO:
// * I think the issue is that we are not consiering the 'no' choices correctly for the objects (grader outputs required obj value for improvement)
// * refactor to ensure DP works on small capacity

object Main extends App {

  val logFile = "run.log"
  val file = new File(logFile)
  val bw = new BufferedWriter(new FileWriter(file))

  val filePath = args(0)
  val input = Parser.readFile(filePath)
  bw.append(s"-----\n${input.itemCount} cap: ${input.capacity}")
  //DP on small space
  if (input.capacity < 10000 || input.itemCount <= 200) {
    val knap = KnapDP(input.itemCount, input.capacity, input.items)
    val result = knap.decideDp
    println(s"1 0")
    println(s"$result")
  }
  else {
    val knap = KnapBB(input.itemCount, input.capacity, input.items)
    //println(s"relax: ${knap.relaxationAt(0, knap.capacity)}")
    val relaxedResult = knap.performRelaxedSearch
    println(s"${relaxedResult._1} 0")
    println(s"${relaxedResult._2}")
  }

  bw.close()
}

Main.main(args)