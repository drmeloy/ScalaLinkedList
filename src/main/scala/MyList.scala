sealed trait MyList[T] {
  def length: Int =
    this match {
      case Node(_, t) => 1 + t.length
      case End() => 0
    }

  def sum: BigDecimal =
    this match {
      case node: Node[Int] => BigDecimal(node.head) + node.tail.sum
      case node: Node[Double] => BigDecimal(node.head) + node.tail.sum
      case _ => throw new Exception("Can only sum numbers")
    }
}

case class Node[T](head: T, tail: MyList[T]) extends MyList[T]
case class End[T]() extends MyList[T]

object Main extends App {
  val list: MyList[String] = Node("1", Node("Hi", Node("3", End())))
  println(list.length)
}