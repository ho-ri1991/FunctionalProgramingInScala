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

  def boolean: Rand[Boolean] =
    map(_.nextInt){n => if(n % 2 == 0)true else false}

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

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
      val (a, next) = f(rng)
      g(a)(next)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt){i =>
      val mod = i % n
      if (i + (n - 1) - mod >=0) unit(mod)
      else nonNegativeLessThan(n)
  }

  def mapViaFlatMap[A, B](trans: Rand[A])(f: A => B) =
    flatMap(trans){a => unit(f(a))}

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra){a => map(rb)(b => f(a, b))}
}

case class State[S, +A](run: S => (A, S)){
  def flatMap[B](f: A => State[S, B]): State[S, B] = State({ state =>
    val (a, next) = run(state)
    f(a).run(next)
  })

  def map[B](f: A => B): State[S, B] =
    flatMap{a => State.unit(f(a))}

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
}

object State{
  def unit[S, A](a: A) = State[S, A](state => (a, state))

  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] =
    states.foldRight(unit[S, List[A]](List[A]())){(s, acc) => s.map2(acc)(_ :: _)}

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int){
  def transition(input: Input): ((Int, Int), Machine) = input match {
    case Turn if locked => ((candies, coins), this)
    case Turn if !locked => ((candies - 1, coins), Machine(!locked, candies - 1, coins))
    case Coin if !locked => ((candies, coins), Machine(locked, candies, coins))
    case Coin if locked && candies > 0 => ((candies, coins + 1), Machine(!locked, candies, coins + 1))
    case _ => ((candies, coins), this)
  }
}

object Machine{
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs map (in => State.modify[Machine](_.transition(in)._2)))
    s <- State.get
  } yield (s.candies, s.coins)

  def _simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    State(s => {
      inputs.foldLeft((s.candies, s.coins), s){
        (p, in) => p._2.transition(in)}
    })
  }
}
