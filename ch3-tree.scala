sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(value) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

}

object TreeApp {

  def main(args: Array[String]): Unit = {
    val sampleTree: Tree[Int] = Branch(
      Branch(
        Leaf(1), 
        Leaf(2)
      ),
      Leaf(3)
    )
    println(Tree.size(sampleTree))

  }
}
