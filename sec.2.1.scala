import scala.annotation.tailrec

object HelloWorld {

  def factorial(n: Int): BigInt = {
    @tailrec
    def loop(acc: BigInt, current: Int): BigInt = {
      if (current <= 1) acc
      else loop(acc * current, current - 1)
    }    
    loop(1, n)
  }

  def fibonacci_naive(n: Int) : Int = {
    if(n == 1) 0
    else if(n == 2) 1
    else fibonacci_naive(n - 1) + fibonacci_naive(n - 2)
  }

  def fibonacci_tail(n: Int): Int = {
    def helper(n: Int, acc: Int, m: Int): Int = {
      if(m == 1) n
      else helper(acc, acc + n, m - 1)
    }
    helper(0, 1, n)
  }

  def intComparator(n: Int, m: Int): Boolean = {
    n > m
  }

  def isSortedInt(is: Array[Int]): Boolean = {
    def helper( n: Int): Boolean = {
      if(n == is.length - 1) true
      else if(is(n) > is(n+1)) false
      else helper(n + 1)
    }
    helper(0)
  }

  def isSortedIntwithComparator(is: Array[Int]): Boolean = {
    def helper( n: Int): Boolean = {
      if(n == is.length - 1) true
      else if(intComparator(is(n), is(n+1))) false
      else helper(n + 1)
    }
    helper(0)
  }

  def isSorted[A](as: Array[A], p: (A, A) => Boolean): Boolean = {
    def helper(n: Int): Boolean = {
      if(n == as.length - 1) true
      else if(p(as(n), as(n+1))) false
      else helper(n + 1)
    }
    return helper(0)
  }

  def main(args: Array[String]): Unit = {
    
    val smallNumber = 5
    println(s"Fibonacci of $smallNumber is: ${fibonacci_naive(smallNumber)}")
    println(s"Fibonacci, tailrec of $smallNumber is: ${fibonacci_tail(smallNumber)}")
    
    val largeNumber = 20
    println(s"Fibonacci of $largeNumber is: ${fibonacci_naive(largeNumber)}")
    println(s"Fibonacci, tailrec of $largeNumber is: ${fibonacci_tail(largeNumber)}")

    val sortedArray: Array[Int] = Array(1, 2, 3, 4, 5)
    var unsortedArray: Array[Int] = Array(2, 1, 4, 5 , 3)
    println(s"The first array sorted is: ${isSortedInt(sortedArray)}")
    println(s"The second array sorted is: ${isSortedInt(unsortedArray)}")

    println(s"The first array sorted is: ${isSortedIntwithComparator(sortedArray)}")
    println(s"The second array sorted is: ${isSortedIntwithComparator(unsortedArray)}")

    println(s"The first array using general sorted is: ${isSorted(sortedArray, intComparator)}")
    println(s"The second array using general sorted is: ${isSorted(unsortedArray, intComparator)}")

    val sortedCharArray: Array[Char] = Array('a', 'b', 'c', 'd', 'e')
    var unsortedCharArray: Array[Char] = Array('f', 'g', 'i', 'b' , 'a')
    println(s"The first char array using general sorted is: ${isSorted(sortedCharArray, intComparator)}")
    println(s"The second char array using general sorted is: ${isSorted(unsortedCharArray, intComparator)}")

    
  }
}
