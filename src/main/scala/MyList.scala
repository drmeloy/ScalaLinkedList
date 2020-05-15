object Main extends App {
  val list1: MyList[Int] = Node(1, Node(2, Node(3, End())))
  val list2: MyList[Int] = Node(4, Node(5, Node(6, End())))
  val names: MyList[String] = Node("Jake", Node("John", Node("Mike", End())))
  println(names.contains("Mike"))
  println(names.find(name => name.startsWith("M")))
  println(names.filter(name => name.startsWith("J")))
  println(names.filter(name => name.startsWith("T")))
  println(names.map(name => name.toUpperCase))
  println(names.lastOption)
  println(End().lastOption)
//  println(list1 ++ list2)
  println(names.flatmap(name => Node(name.head.toString, Node(name.tail, End()))))
}

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

  def find(func: T => Boolean): Option[T] =
    this match {
      case Node(h, t) =>
        if(func(h)) Some(h)
        else t.find(func)
      case End() =>
        None
    }

  def filter(func: T => Boolean): MyList[T] =
    this match {
      case Node(h, t) =>
        if(func(h)) Node(h, t.filter(func))
        else t.filter(func)
      case End() =>
        End()
    }

  def map[U](func: T => U): MyList[U] =
    this match {
      case Node(h, t) =>
        Node(func(h), t.map(func))
      case End() =>
        End()
    }

  def lastOption: Option[T] =
    this match {
      case Node(h, t) =>
        t match {
          case Node(_, _) =>
            t.lastOption
          case End() =>
            Some(h)
        }
      case End() =>
        None
    }

  def ++(that: MyList[T]): MyList[T] = {

    def getNewTail(thisTail: MyList[T]) =
      thisTail match {
        case Node(_, _) =>
          thisTail ++ that // Recursive step
        case End() =>
          that
      }

    this match {
      case Node(thisHead, thisTail) =>
        that match {
          case Node(_, _) =>
            val newTail: MyList[T] = getNewTail(thisTail)
            Node(thisHead, newTail)
          case End() =>
            this
        }
      case End() =>
        that
    }
  }

  /**
   * flatMap takes a function that turns an object of type T into a MyList (collection) of type U, and concatenates the new MyList to the MyList that called flatMap (this)
   * @param func function applied to every object in MyList[T}, converts object of type T into MyList[U]
   * @tparam U
   * @return a flattened MyList[U]
   */
  def flatmap[U](func: T => MyList[U]): MyList[U] =
    this match {
      case Node(h, t) =>
        func(h) ++ t.flatmap(func)
      case End() =>
        End()
    }
}

case class Node[T](head: T, tail: MyList[T]) extends MyList[T]
case class End[T]() extends MyList[T]