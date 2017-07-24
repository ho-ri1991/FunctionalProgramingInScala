package fpinscala.state

trait RNG{
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >> 16).toInt
    (n, nextRNG)
  }
}

object RNG{
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, next) = rng.nextInt
    val res = if(n < 0) -(n + 1) else n
    (res, next)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, next) = nonNegativeInt(rng)
    (n.toDouble / (Int.MaxValue.toDouble + 1.0), next)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), next) = intDouble(rng)
    ((d, i), next)
  }

  def doubl3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(cnt: Int, ls: List[Int], rng: RNG): (List[Int], RNG) = {
      if(cnt == 0) (ls, rng)
      else if (cnt < 0) (ls, rng)
      else {
         val (n, next) = rng.nextInt
        go(cnt - 1, n :: ls, next)
      }
    }

    go(count, List[Int](), rng)
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  val _double: Rand[Double] = map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue.toDouble + 1.0))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, _double)

  val randDoubleInt: Rand[(Double, Int)] = both(_double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]())){(a, acc) => map2(a, acc)(_ :: _)}

  def sequence_[A](fs: List[Rand[A]]): Rand[List[A]] =
    rnd => {
      @annotation.tailrec
      def go(trans: List[Rand[A]], ls: List[A], rng: RNG): (List[A], RNG) = {
        trans match {
          case Nil => (ls.reverse, rng)
          case h :: t =>{
            val (a, next) = h(rng)
            go(t, a :: ls, next)
          }
        }
      }

      go(fs, List[A](), rnd)
    }
}