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

  def main(args: Array[String]): Unit = {
    
    val smallNumber = 5
    println(s"Fibonacci of $smallNumber is: ${fibonacci_naive(smallNumber)}")
    println(s"Fibonacci, tailrec of $smallNumber is: ${fibonacci_tail(smallNumber)}")
    
    val largeNumber = 20
    println(s"Fibonacci of $largeNumber is: ${fibonacci_naive(largeNumber)}")
    println(s"Fibonacci, tailrec of $largeNumber is: ${fibonacci_tail(largeNumber)}")
  }
}
