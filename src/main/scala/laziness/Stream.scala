package fpinscala.laziness

import Stream._

trait Stream[+A]{
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toListRec: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListRec
  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(stream: Stream[A], ls: List[A]): List[A] = stream match {
      case Empty => ls
      case Cons(h, t) => go(t(), h() :: ls)
    }

    go(this, Nil).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((a, b) => if(p(a)) cons(a, b) else empty)

  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](empty)((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty){(a, b) =>
      if (f(a)) cons(a, b)
      else b
    }

  def append[B >: A](x: => Stream[B]): Stream[B] =
    foldRight[Stream[B]](x){
      (a, b) => cons(a, b)
    }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](empty){(a, b) =>
      f(a).append(b)
    }

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def mapViaUnFold[B](f: A => B): Stream[B] =
    unfold(this){
        case Cons(h, t) => Some((f(h()), t()))
        case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((n, this)){case (cnt, s) =>
      s match {
        case Cons(h, t) if n > 0 => Some((h(), (n - 1, t())))
        case _ => None
      }
    }

  def takeWhileViaUnfold(f: A => Boolean): Stream[A] =
    unfold(this){
      case Cons(h, t) if f(h()) => Some((h(), t()))
      case _ => None
    }

  def zipWith[B](s2: Stream[B]): Stream[(A, B)] =
    unfold((this, s2)){
        case (Cons(h1, t1), Cons(h2, t2)) => Some(((h1(), h2()), (t1(), t2())))
        case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, s2)){
        case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
        case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), empty)))
        case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (empty, t2())))
        case _ => None
    }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile{ !_._2.isEmpty }.forAll{ case(h1, h2) => h1 == h2 }

  def tails: Stream[Stream[A]] =
    Stream.unfold(this){
      case Empty => None
      case s => Some((s, s.drop(1)))
    }.append(empty)

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z))){(a, p0) =>
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    }._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream{
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  def fibs: Stream[Int] = {
    def go(a0: Int, a1: Int): Stream[Int] = {
      cons(a0 + a1, go(a1, a0 + a1))
    }
    cons(0, cons(1, go(0, 1)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case _ => empty[A]
  }

  def fibsViaUnFold: Stream[Int] =
    unfold((0, 0, 1)){case (n, a0, a1) =>
      n match {
        case 0 => Some(0, (1, 0, 1))
        case 1 => Some(1, (2, 0, 1))
        case _ => Some(a0 + a1, (n + 1, a1, a0 + a1))
      }
    }

  def fromViaUnFold(n: Int): Stream[Int] =
    unfold(n){s => Some((s, s + 1))}

  def constantViaUnFold[A](a: A): Stream[A] =
    unfold(a){a => Some(a, a)}
}