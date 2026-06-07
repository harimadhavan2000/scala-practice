sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeApp {

  def main(args: Array[String]): Unit = {
    val sampleTree: Tree[Int] = Branch(
      Branch(
        Leaf(1), 
        Leaf(2)
      ),
      Leaf(3)
    )

  }
}
