object TreeApp extends App {

  val s1 = new Sum(1, 2)
  val n = Num(3).include(s1)
  val s2 = new Sum(1, 1)
  val s3 = new Sum(4, 1)
  val s4 = new Sum(2, 2)
  val n2 = Num(5).include(Sum(n, Num(2)))
  println(n2.include(s2).include(s3).include(s4))

}

trait Node {

  val value: Int
  val children: List[Node]
  def include(node: Node): Node
  val included: List[Int]
}

case class Sum(a: Node, b: Node) extends Node {

  def this(a: Int, b: Int) {
    this(Num(a), Num(b))
  }

  override val included: List[Int] = (List(value) ::: a.included ::: b.included).distinct

  override val value: Int = a.value + b.value
  override val children: List[Node] = Nil

  override def include(node: Node): Node = {
    if (node.value == value) {
      this
    } else {
      copy(a.include(node), b.include(node))
    }
  }

  override def toString: String = s"$a + $b"

}

case class Num(value: Int, children: List[Node] = Nil) extends Node {

  override val included: List[Int] = (List(value) ::: children.flatMap(_.included)).distinct

  override def include(node: Node): Node = {
    if (included.contains(node.value)) {
      println(s"already included $node")
    }
    if (node.value == value) {
      copy(children = node :: children)
    } else {
      copy(children = children.map(_.include(node)))
    }
  }

  override def toString: String = if (children.nonEmpty) {
    s"$value = (${children.mkString(" | ")})"
  } else {
    value.toString
  }

  assume(children.forall(_.value == value))

}