import scala.io.Source

/*----------------- CLASSES -----------------*/
case class Item(value: Int, weight: Int, id: Int) {
  val density = value / weight
}

case class Knapsack(itemCount: Int, capacity: Int, items: List[Item]) {
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


object Main extends App {
  val filePath = s"data/${args(0)}"
  val knap = Parser.readFile(filePath)

  println(knap)
}
Main.main(args)