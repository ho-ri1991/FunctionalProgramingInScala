package fpinscala.parsing

import fpinscala.testing._
import fpinscala.testing.Prop._

import scala.util.matching.Regex

trait Parsers[Parser[+_]] { self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps(p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  def succeed[A](a: A): Parser[A]
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def slice[A](p: Parser[A]):Parser[String]
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)
//  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
//    p ** p2 map f.tupled  //p ** p2 map{case (a, b) => f(a, b)}
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = n match {
    case 0 => succeed(List())
    case _ => map2(p, listOfN(n - 1, p))(_ :: _)
  }
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    p flatMap(a => p2.map(b => (a, b)))
  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    p.flatMap(a => p2.map(b => f(a, b)))
  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    a.flatMap(a => succeed(f(a)))
  implicit def regex(r: Regex): Parser[String]
  def attempt[A](p: Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]){
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def **[B](p1: => Parser[B]): Parser[(A, B)] = self.product(p, p1)
    def product[B](p1: => Parser[B]): Parser[(A, B)] = self.product(p, p1)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def slice[A]: Parser[String] = self.slice(p)
  }

  def label[A](msg: String)(p: Parser[A]): Parser[A]
  def errorLocation(e: ParseError): Location
  def errorMessage(e: ParseError): Location

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in){s => run(p1)(s) == run(p2)(s)}

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }
}

case class Location(input: String, offset: Int = 0) {
  //this lazy means that if error does not occur, we need not calculate these values
  lazy val line = input.slice(0, offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }
}

case class ParseError(stack: List[(Location, String)])