import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.concurrent._
import scala.concurrent.duration.Duration
import ExecutionContext.Implicits.global
import java.io._

import scala.annotation.tailrec

/*----------------- CLASSES -----------------*/
case class Item(value: Int, weight: Int) {
  val density = value / weight

  def ratioValue(curCapacity: Int): Int = {
    val ratio = curCapacity / weight.toDouble
    math.ceil(ratio * value).toInt
  }
}

case class Knapsack(itemCount: Int, capacity: Int, items: Map[Int, Item]) {
  //block long operations
  val TIMEOUT = 60 * 60
  //  val itemsDescDensity = items.sortBy(i => -i.density)
  val dpTable = fillTable

  def performChoices: List[Item] = {
    List()
  }

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
    if(curItem == 0 || curCap == 0)
      results
    else if(dpTable(curCap)(curItem-1) != dpTable(curCap)(curItem))
      decideTable(1::results, curCap - items(curItem).weight, curItem - 1)
    else
      decideTable(0:: results, curCap, curItem -1)
  }

  def decideDp: List[Int] = {
    decideTable(List(), capacity, items.size)
  }

  def printTable = {
    print(dpTable.map(e => e.mkString("  ")).mkString("\n-------------\n"))
  }


  def fullPrint: String = {
    s"$this\nitems: $items"
  }

  override def toString: String = {
    s"$itemCount $capacity"
  }
}

object Parser {
  def readFile(path: String): Knapsack = {
    val lines = Source.fromFile(path).getLines().toList
    val header = lines.head.split(" ")

    val itemsObj = lines.tail.zipWithIndex.map(i => {
      val split = i._1.split(' ')
      (i._2 + 1, Item(split(0).toInt, split(1).toInt))
    }).toMap

    //    val xs = List(2, 4, 6, 8, 10)
    //    (xs.indices zip xs).toMap

    Knapsack(header(0).toInt, header(1).toInt, itemsObj)
  }

}

/*-------------------------------------------*/

//invocation: scala Main.scala ks_1000_0

// need to re-think the exploration model for relaxed search
// list is not right represenation as it keeps returning the first, weightiest item.
// need to build up a tree of choices *as needed*, ie ignoring right branch when it's clear it does
// not contain a more optimal solution

object Main extends App {

  val logFile = "run.log"
  val file = new File(logFile)
  val bw = new BufferedWriter(new FileWriter(file))

  val filePath = args(0)
  val knap = Parser.readFile(filePath)
  val decided = knap.decideDp.mkString(" ")
  println(knap)
  println(s"$decided")
  bw.write(s"sourceFile: ${filePath}")
  bw.write(s"knapsack:\n${knap.fullPrint}")



  bw.close()
}

Main.main(args)