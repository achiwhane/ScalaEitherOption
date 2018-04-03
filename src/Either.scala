sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(x) => Left(x)
    case Right(x) => Right(f(x)) // TODO: fixme -- what if f(x) throws error??
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(x) => Left(x)
    case Right(x) => f(x)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(x) => Right(x)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    case Right(x) => Right(f(x, b))
    case Left(x) => Left(x)
  }

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object EitherTest {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)
  }

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = {
    try Right(x / y)
    catch { case e: Exception => Left(e)}
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    def helper(es: List[Either[E, A]], res: Either[E, List[A]]): Either[E, List[A]] = {
      if (es.isEmpty) res
      else es.head match {
        case Left(x) => Left(x)
        case _ => helper(es.tail, res.flatMap(tail => es.head.map(head => head :: tail)))
      }
    }

    helper(es, Right(List[A]()))
  }

  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch { case e: Exception => Left(e) }
  }
}