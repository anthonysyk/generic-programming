package functionnal

import java.util.UUID

sealed trait Monoid[T] {
  def zero: T

  def op(a: T, b: T): T
}

object Monoid {

  implicit val vMonoid: Monoid[Visit] = new Monoid[Visit] {
    override def zero: Visit = Visit(0, 0, None, 0)

    override def op(a: Visit, b: Visit): Visit = {
      Visit(
        b.start,
        b.end,
        if (a.sessionId.isEmpty) Some(UUID.randomUUID().toString) else a.sessionId,
        a.nbPages + b.nbPages
      )
    }
  }

  implicit def optMonoid[A](implicit m: Monoid[A]): Monoid[Option[A]] = new Monoid[Option[A]] {
    def zero = None

    def op(a: Option[A], b: Option[A]): Option[A] = (a, b) match {
      case (_, None) => a
      case (None, _) => b
      case (Some(x), Some(y)) => Option(m.op(x, y))
    }
  }

  implicit def seqMonoid(implicit m: Monoid[Visit]): Monoid[Seq[Visit]] = new Monoid[Seq[Visit]] {
    def zero = Nil

    def op(acc: Seq[Visit], next: Seq[Visit]): Seq[Visit] = (acc, next) match {
      //      case (Nil, Nil) => Nil
      //      case (_, Nil) => acc
      case (Nil, y :: _) => Seq(y.copy(sessionId = Some(UUID.randomUUID().toString)))
      case (_ :: xs, y :: _) if xs.nonEmpty && Visit.isNotSameSession(y.start, xs.last.end) =>
        acc :+ y.copy(sessionId = Some(UUID.randomUUID().toString))
      case (x :: Nil, y :: _) if Visit.isSameSession(y.start, x.end) =>
        acc.dropRight(1) :+ m.op(x, y)
      case (x :: Nil, y :: _) =>
        acc :+ m.op(x, y)
      case (_ :: xs, y :: _) if xs.nonEmpty =>
        acc.dropRight(1) :+ m.op(xs.last, y)
    }
  }

}

case class Visit(start: Long, end: Long, sessionId: Option[String], nbPages: Int)

object Visit {

  def isSameSession(nextStartVisitTS: Long, previousEndVisitTS: Long): Boolean = {
    nextStartVisitTS < previousEndVisitTS + 3600
  }

  def isNotSameSession(nextStartVisitTS: Long, previousEndVisitTS: Long): Boolean = {
    !isSameSession(nextStartVisitTS, previousEndVisitTS)
  }

}

object MonoidTest {


  def main(args: Array[String]): Unit = {

    implicit def op[A](a: A, b: A)(implicit m: Monoid[A]) = m.op(a, b)

    val visit1 = Visit(1521067376, 1521067856, None, 3)
    val visit2 = Visit(1521069856, 1521075856, None, 2)
    val visit3 = Visit(1521175856, 1521176856, None, 1)
    val visit4 = Visit(1521186856, 1521189856, None, 6)
    val visit5 = Visit(1521189956, 1521192956, None, 2)
    val visit6 = Visit(1521292956, 1521293256, None, 1)

    val visits = Seq(visit1, visit2, visit3, visit4, visit5, visit6).sortBy(_.start)

    val fold: Seq[Visit] = visits.foldLeft(Seq.empty[Visit])((a, b) => op(a, Seq(b)))

    fold.foreach(println)

  }

}
