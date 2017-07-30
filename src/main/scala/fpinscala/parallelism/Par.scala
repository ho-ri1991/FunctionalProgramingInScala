package fpinscala.parallelism

import java.util.concurrent.{ExecutorService, Future, TimeUnit, Callable}

object Par {
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone: Boolean = true

    override def get(timeout: Long, unit: TimeUnit): A = get

    override def isCancelled: Boolean = false

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call(): A = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  // This implementation executes each Par[A] from right to left successively.
  // Therefore each calculation is not executed parallel.
  // It is not we intend.
  def _sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List[A]())){(pa, acc) => map2(pa, acc)(_ :: _) }

  // Evaluations start from left to right parallel in this implementation.
  def sequenceRight[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case Nil => unit(Nil)
    case hp :: tp => map2(hp, fork(sequenceRight(tp)))(_ :: _)
  }

  def sequenceBalanced[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] =
  fork {
    if(ps.isEmpty) unit(Vector())
    else if(ps.length == 1) map(ps.head)(Vector(_))
    else {
      val (l, r) = ps.splitAt(ps.length / 2)
      map2(sequenceBalanced(r), sequenceBalanced(l))(_ ++ _)
    }
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(ps.toIndexedSeq))(_.toList)

  // If the `fork` in the begging of this function does not exist,
  // The evaluation of List[A]#map (not evaluation of f) which takes O(N) are done immediately.
  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilterBalanced[A](ps: IndexedSeq[A])(f: A => Boolean): Par[IndexedSeq[A]] = fork {
    if(ps.isEmpty) unit(Vector())
    else if (ps.length == 1) unit(if(f(ps.head))Vector(ps.head)else Vector())
    else {
      val (l, r) = ps.splitAt(ps.length / 2)
      map2(parFilterBalanced(l)(f), parFilterBalanced(r)(f))(_ ++ _)
    }
  }

  def _parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    map(parFilterBalanced(as.toIndexedSeq)(f))(_.toList)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val ps: List[Par[List[A]]] = as map (asyncF(a => if(f(a))List(a) else List()))
    map(sequence(ps))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean =
    p1(e).get == p2(e).get

  // This procedure produces deadlock because b uses the only one therad and wait for the end of the work a, but there are no thread reamined for work a.
  //    val a = parallelism.Par.lazyUnit(42 + 1)
  //    val S = Executors.newFixedThreadPool(1)
  //    val b = parallelism.Par.fork(a)
  //    println(b(S).get)

  def join[A](a: Par[Par[A]]): Par[A] = es => run(es)(run(es)(a).get())

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = join(map(a)(f))
}
