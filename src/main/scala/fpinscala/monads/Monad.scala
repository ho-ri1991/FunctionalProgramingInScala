package fpinscala.monads

import fpinscala.testing._
import fpinscala.state._

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(fa) => map(fa)(Left(_))
      case Right(fb) => map(fb)(Right(_))
    }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as.map(f)
  }
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]())){(ma, acc) => map2(ma, acc)(_ :: _)}
  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List[B]())){(a, acc) => map2(f(a), acc)(_ :: _)}
//    sequence(la.map(f(_)))
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  //flatMap(flatMap(x)(f))(g) == flatMap(x)(a => flatMap(f(a))(g))
  //flatMap(flatMap(x)(f))(g) == flatMap(x)(compose(f, g))
  //flatMap(flatMap(x)(f))(g) == compose(h, compose(f, g))(y) // define x := h(y)
  //flatMap(flatMap(h(y))(f))(g) ==
  //flatMap(compose(h, f)(y))(g) ==
  //compose(compose(h, f), g)(y) ==
  //i.e. compose(compose(h, f), g) == compose(h, compose(f, g))

  def flatMapViaCompose[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => ma, f)()

  //flatMap(x)(unit) == x
  //flatMap(h(y))(unit) == h(y)
  //compose(h, unit)(y) == h(y)
  //i.e. compose (h, unit) == h

  //flatMap(unit(y))(f) == f(y)
  //compose(unit, f)(y) == f(y) //by definition

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(ma => ma)

  def flatMapViaJoinMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  //flatMap(flatMap(x)(f))(g) == flatMap(x)(a => flatMap(f(a))(g))
  //flatMap(join(map(x))(f)))(g) == flatMap(x)(a => join(map(f(a))(g)))
  //join(map(join(map(x))(f)))(g)) == join(map(x)(a => join(map(f(a))(g))) //define x := h(y)
}

case class Id[A](value: A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A) = Gen.unit(a)
    def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma flatMap f
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A) = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]) = ma flatMap f
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A) = Some(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
  }

  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = Id(a)
    def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = f(ma.value)
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    def flatMap[A, B](st: State[S, A])(f: A => State[S, B]) =
      st flatMap f
  }

  val F = stateMonad[Int]

  def zipWithIndex[A](as: List[A]): List[(Int, A)] =
    as.foldLeft(F.unit(List[(Int, A)]())){(acc, a) =>
      acc.flatMap(xs =>
        State.get.flatMap(n =>
          State.set(n + 1).map((_: Unit) => (n,a) :: xs)))
    }.run(0)._1.reverse

  //init: State(s => (Nil, s))
  //   1: State(s => (((s, a0)), s + 1))
  //   2: State(s => (((s + 1, a1), (s, a0)), s + 2)
  //   3: State(s => (((s + 2, a2), (s + 1, a1), (s, a0)), s + 2))
}