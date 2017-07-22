package fpinscala.errorhandling

/**
  * Created by iwahori on 2017/07/14.
  */

sealed trait Either[+E, +A]{
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => this
    case Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap(a => b map(f(a, _)))

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either{
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if(xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum/xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x/y)
    catch {case e: Exception => Left(e)}

  def Try[A](a: => A): Either[Exception, A]=
    try Right(a)
    catch {case e: Exception => Left(e)}

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight[Either[E, List[A]]](Right(List[A]())){(a, b) => a.map2(b)(_::_)}

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil)){(a, bs) => f(a).map2(bs)(_::_)}

}