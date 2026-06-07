sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, t) => t
  }
     
}

object Main {
  def main(args: Array[String]): Unit = {
    val intArray = List(1, 2, 3, 4, 5)
    val doubleArray = List(1.5, 2.0, 3.0)
    val doubleArraywithZero = List(5.0, 0.0, 10.0)

    val tailVariable = List.tail(intArray)
    println(s"The tail of intArray is: $tailVariable")
  }
}
