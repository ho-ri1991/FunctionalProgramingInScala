package chapter2
object Chapter2 {
  def fib(n: Int): Long ={
    @annotation.tailrec
    def go(n: Int, acc1: Long, acc2: Long): Long = {
      if (n <= 1)
        acc1
      else
        go(n - 1, acc2, acc1 + acc2)
    }

    return go(n, 0, 1)
  }

  def isSorted[A](as:Array[A], ordered: (A,A) => Boolean): Boolean = {
    def go(n: Int): Boolean = {
      if (n >= as.length) true
      else {
        if (!ordered(as(n-1), as(n)))false
        else go(n + 1)
      }
    }
    return go(1)
  }

  def curry[A,B,C](f: (A,B)=>C): A=>(B=>C) = (a: A)=>{f(a, _:B)}
  def uncurry[A,B,C](f: A=>B=>C): (A,B)=>C = (a:A, b:B)=>{f(a)(b)}
  def compose[A,B,C](f: B=>C, g: A=>B): A=>C = (a: A)=>{f(g(a))}
}
