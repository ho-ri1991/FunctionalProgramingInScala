package fpinscala.testing

import fpinscala.state._
import fpinscala.laziness._
import fpinscala.testing.Prop._

import scala.ref.ReferenceWrapper

case class Gen[A](sample: State[RNG, A]){
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def ListOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen(State.sequence(List.fill(n)(sample))))
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if(b)g1 else g2)

  def double0to1: Gen[Double] = Gen(State(RNG.double))

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    double0to1.flatMap(d => if(d < g1._2 / (g1._2 + g2._2))g1._1 else g2._1)
}

object Prop{
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type Result = Option[(FailedCase, SuccessCount)]

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if(f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception =>  Falsified(buildMsg(a, e), i)}
    }.find(_.isFalsified).getOrElse(Passed)
  }
}

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    case (n, rng) => {
      run(n, rng) match {
        case Passed => p.run(n, rng)
        case Falsified(failure, successes) => Falsified(failure, successes)
      }
    }
  }

  def ||(p: Prop): Prop = Prop {
    case (n, rng) => {
      run(n, rng) match {
        case Passed => Passed
        case _ => p.run(n, rng)
      }
    }
  }
}

sealed trait Result{
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result{
  def isFalsified = true
}