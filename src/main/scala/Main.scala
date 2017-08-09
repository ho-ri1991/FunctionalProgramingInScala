import java.util.concurrent.{Executor, ExecutorService, Executors}

import chapter2.Chapter2._
import fpinscala._
import fpinscala.testing._
import fpinscala.parallelism._

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

    import fpinscala.state._
    val m = Machine(true, 2, 0)
    println(Machine.simulateMachine(List(Coin, Turn, Coin, Turn, Turn, Coin, Coin, Turn)).run(m))

//    val a = parallelism.Par.lazyUnit(42 + 1)
//    val S = Executors.newFixedThreadPool(1)
//    val b = parallelism.Par.fork(a)
//    println(b(S).get)

    val smallInt = Gen.choose(-10, 10)
    val maxProp = Prop.forAll(Gen.listOf1(smallInt)){ ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    Prop.run(maxProp)

    val ES: ExecutorService = Executors.newCachedThreadPool
    val p2 = Prop.check {
      val p = Par.map(Par.unit(1))(_ + 1)
      val p2 = Par.unit(2)
      p(ES).get == p2(ES).get
    }
    Prop.run(p2)

    val p3 = Prop.check {
      Prop.equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      )(ES).get
    }
    Prop.run(p3)
  }
}
