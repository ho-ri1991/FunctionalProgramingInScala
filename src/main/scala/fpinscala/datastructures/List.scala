package fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def prod(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * prod(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def isEmpty[A](l: List[A]): Boolean = l match {
    case Nil => true
    case _ => false
  }

  def head[A](as: List[A]): A = as match {
    case Nil => ???
    case Cons(h, _) => h
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](as: List[A], a: A): List[A] = as match {
    case Nil => Nil
    case Cons(_, t) => Cons[A](a, t)
  }

  def drop[A](as: List[A], n: Int): List[A] = {
    if (n <= 0) as
    else as match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  @annotation.tailrec
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => as
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(x, xs) => Cons(x, append(xs, a2))
  }

  def init[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](l: List[A]): Int = {
    foldLeft(l, 0)((b, _) => (b + 1))
  }

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], b: B)(f: (B,A)=>B): B = as match {
    case Nil => b
    case Cons(h,t) => foldLeft(t,f(b,h))(f)
  }

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((acc, a)=>Cons(a, acc))

  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B)=>b)((a,g) => b => g(f(b,a)))(z)

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((a,b) => Cons(a,b))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, List[A]())(append)

  def add1(ints: List[Int]): List[Int] =
    foldRight(ints, List[Int]())((a, acc) => Cons(a + 1, acc))

  def doublesToStings(ds: List[Double]): List[String] =
    foldRight(ds, Nil:List[String])((a, acc) => Cons(a.toString, acc))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil:List[B])((a, acc) => Cons(f(a), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, acc) => if (f(a))Cons(a, acc) else acc)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))
   // foldRight(as, Nil: List[B])((a, acc) => append(f(a), acc))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def addPairwise(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Cons(x,xs), Cons(y,ys)) => Cons(x + y, addPairwise(xs, ys))
    case _ => Nil
  }

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] = (as, bs) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith(xs, ys)(f))
    case _ => Nil
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def go1(a1: List[A], a2: List[A]): Boolean = (a1, a2) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => go1(t1, t2)
      case _ => false
    }

    @annotation.tailrec
    def go2(a1: List[A], a2: List[A]): Boolean = (a1, a2) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2 && go1(t1, t2)) => true
      case (Cons(h1, t1), Cons(h2, t2)) => go2(t1, sub)
    }

    go2(sup, sub)
  }
}
