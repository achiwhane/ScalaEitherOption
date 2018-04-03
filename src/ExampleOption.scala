sealed trait ExOption[+A] {
  def map[B](f: A => B): ExOption[B] = this match {
    case ExSome(x) => ExSome(f(x))
    case ExNone => ExNone
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case ExSome(x) => x
    case ExNone => default
  }

  def flatMap[B](f: A => ExOption[B]): ExOption[B] = this match {
    case ExSome(x) => f(x)
    case ExNone => ExNone
  }

  def orElse[B>:A](ob: => ExOption[B]): ExOption[B] = this match {
    case ExSome(x) => ExSome(x)
    case ExNone => ob
  }

  def filter(f: A => Boolean): ExOption[A] = this match {
    case ExSome(x) if f(x) => ExSome(x)
    case _ => ExNone
  }
}
case class ExSome[+A](get: A) extends ExOption[A]
case object ExNone extends ExOption[Nothing]


object Test {
  def variance(xs: Seq[Double]): ExOption[Double] = {
    def avg(xs: Seq[Double]): ExOption[Double] = {
      def sum(xs: Seq[Double], res: ExOption[Double]): ExOption[Double] = {
        if (xs.isEmpty) res
        else sum(xs.tail, res.map(x => x + xs.head))
      }

      sum(xs, ExSome(0)).map(x => x / xs.length)
    }

    val avgVal = avg(xs)

    def helper(xs: Seq[Double], res: ExOption[Double]): ExOption[Double] = {
      if (xs.isEmpty) res
      else helper(xs.tail, avgVal.map(x => xs.head - x).map(x => x * x).flatMap(x => res.map(y => x + y)))
    }

    helper(xs, ExSome(0.0))
  }


  def lift[A, B](f: A => B): ExOption[A] => ExOption[B] = _ map f

  def map2[A, B, C](a: ExOption[A], b: ExOption[B], c: ExOption[C])(f: (A, B) => C): ExOption[C] = a.flatMap(x => b.map(y => f(x, y)))

  def map2WithFor[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)
  }

  def sequence[A](a: List[ExOption[A]]): ExOption[List[A]] = {
    def helper(xs: List[ExOption[A]], res: ExOption[List[A]]): ExOption[List[A]] =
      if (xs.isEmpty) res
      else if (xs.head == ExNone) ExNone
      else helper(xs.tail, res.flatMap(tempList => xs.head.map(currVal => currVal :: tempList)))


    helper(a, ExSome[List[A]](List[A]()))
  }

  def sequenceImplementedWithTraverse[A](a: List[ExOption[A]]): ExOption[List[A]] =
    traverse(a)(optionVal => optionVal.orElse(ExNone))


  def traverse[A, B](a: List[A])(f: A => ExOption[B]): ExOption[List[B]] = {
    def helper(xs: List[A], res: ExOption[List[B]]): ExOption[List[B]] =
      if (xs.isEmpty) res
      else if (xs.head == ExNone) ExNone
      else helper(xs.tail, res.flatMap(tempList => f(xs.head).map(currVal => currVal :: tempList)))

    helper(a, ExSome[List[B]](List[B]()))
  }


  def main(args: Array[String]): Unit = {
    val foo: List[Double] = 1.0 :: 2.0 :: 3.0 :: Nil
    println(variance(foo))
  }
}



