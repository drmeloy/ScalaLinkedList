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

  def foldLeft[U](identity: U)(op: (U, T) => U): U =
    this match {
      case Node(h, t) => val accumulator = op(identity, h)
        t.foldLeft(accumulator)(op)
      case End() => identity
    }

  def contains(item: T): Boolean =
    this match {
      case Node(h, t) => h == item || t.contains(item)
      case End() => false
    }
}

case class Node[T](head: T, tail: MyList[T]) extends MyList[T]
case class End[T]() extends MyList[T]

object Main extends App {
  val list: MyList[Int] = Node(1, Node(2, Node(3, End())))
  val names: MyList[String] = Node("Jake", Node("John", Node("Mike", End())))
  println(names.contains("Mike"))
}