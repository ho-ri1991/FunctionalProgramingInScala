import chapter2.Chapter2._

object Main {

  def main(args: Array[String]): Unit = {
    val arr = Array[Int](1,2,3,1)
    println(isSorted(arr, (x:Int, y: Int)=>{x<=y}))

    def f1(x:Int, y:Int):Int = x + y
    val cf1 = curry(f1)
    val g = uncurry(cf1)
    println(g(1,2))

    val fibs = fpinscala.laziness.Stream.fibs.take(10).toList
    println(fibs)
    println(fpinscala.laziness.Stream(1,2,3,4) startsWith fpinscala.laziness.Stream(1,2))
    println(fpinscala.laziness.Stream(1,2,3).scanRight(0)(_ + _).toList)
  }
}
