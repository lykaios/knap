import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.concurrent._
import scala.concurrent.duration.Duration
import ExecutionContext.Implicits.global
import java.io._

/*----------------- CLASSES -----------------*/
case class Item(value: Int, weight: Int, id: Int) {
  val density = value / weight
}

case class Knapsack(itemCount: Int, capacity: Int, items: List[Item]) {
  //block long operations
  val TIMEOUT = 60 * 2

  def greedySearch(implicit ec: ExecutionContext): List[Item] = {
    def takeItem(cur: List[Item], localItems: List[Item], localCapacity: Int): List[Item] = {
      localItems match {
        case i :: is if i.weight > localCapacity => cur
        case i :: is => takeItem(i :: cur, is, localCapacity - i.weight)
      }
    }

    val densitySorted = items.sortBy(i => -i.density)
    lazy val f = future { takeItem(List(), densitySorted, capacity) }

    Await.result(f, Duration.create(TIMEOUT, "seconds"))
  }

  //actually bool but made int for printing
  def performChoices: List[Int] = {
    val taken = greedySearch
    items.map(i => if (taken.contains(i)) 1 else 0)
  }

  def print = {
    println(s"$this\nitems: $items")
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
      val id = i._2
      Item(split(0).toInt, split(1).toInt, id)
    })

    Knapsack(header(0).toInt, header(1).toInt, itemsObj)
  }

}

/*-------------------------------------------*/

//invocation: scala Main.scala ks_1000_0
object Main extends App {

  val logFile = "run.log"
  val file = new File(logFile)
  val bw = new BufferedWriter(new FileWriter(file))

  val filePath = args(0)
  val knap = Parser.readFile(filePath)

  bw.write(s"knapsack\n$knap")
  println(knap)
  println(knap.performChoices.mkString(" "))

  bw.close()
}

Main.main(args)