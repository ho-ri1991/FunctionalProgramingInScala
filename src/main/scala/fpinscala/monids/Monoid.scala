package fpinscala.monids

import fpinscala.testing._
import fpinscala.testing.Prop._

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    def zero = ""
  }

  def listMonid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    def zero = Nil
  }

  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    def zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int)= a1 * a2
    def zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    def zero = false
  }

  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    def zero = true
  }

  def OptionMonoid[A](monoid: Monoid[A]) = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1.flatMap(a => a2.map(monoid.op(a, _)))
    def zero = Some(monoid.zero)
  }

  def endoMonoid[A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A) = a1 andThen a2
    def zero = a => a
  }

  object MonoidLaws {
    def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
      forAll(for{x <- gen
                 y <- gen
                 z <- gen
      } yield (x, y ,z)){case(x, y, z) =>
      m.op(x, m.op(y, z)) == m.op(m.op(x, y), z)} &&
      forAll(gen)(a => m.op(a, m.zero) == a)
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if(as.isEmpty) m.zero
    else if(as.length == 1) f(as.head)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMap(l, m)(f), foldMap(r, m)(f))
    }
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val wcMonoid = new Monoid[WC] {
    def op(a1: WC, a2: WC) = (a1, a2) match{
      case (Stub(cs1), Stub(cs2)) => Stub(cs1 + cs2)
      case (Stub(cs1), Part(lStub, words, rStub)) =>
        Part(cs1 + lStub, words, rStub)
      case (Part(lStub, words, rStub), Stub(cs)) =>
        Part(lStub, words, rStub + cs)
      case (Part(lStub1, words1, rStub1), Part(lStub2, words2, rStub2)) =>
        Part(lStub1, words1 + words2 + (if((rStub1+lStub2).isEmpty)0 else 1), rStub2)
    }
    def zero = Stub("")
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
    val m = new Monoid[Map[A, Int]] {
      def zero = Map[A, Int]()
      def op(a: Map[A, Int], b: Map[A, Int]) =
        (a.keys ++ b.keys).foldLeft(zero) { (acc, k) =>
          acc.updated(k, a.getOrElse(k, 0) + b.getOrElse(k, 0))
        }
    }

    foldMap(as, m)(a => Map(a -> 1))
  }
}


trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A
}

object ListFoldable extends Foldable[List] {
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
  def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    Monoid.foldMap(as, mb)(f)
  def concatenate[A](as: List[A])(m: Monoid[A]): A =
    Monoid.concatenate(as, m)
}
