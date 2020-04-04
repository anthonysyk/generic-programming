
// my type class
trait SuperOperator[T] {
  def zero: T

  def op(a: T, b: T): T
}

object SuperOperator {
  object Implicits {
    implicit val intConversion: SuperOperator[Int] = new SuperOperator[Int] {
      override def zero = 0

      override def op(a: Int, b: Int): Int = a + b
    }

    implicit def listConversion[T](implicit superOp: SuperOperator[T]): SuperOperator[List[T]] = new SuperOperator[List[T]] {
      override def zero = List.empty[T]

      override def op(a: List[T], b: List[T]): List[T] = a.zip(b).map { case (aElement, bElement) => superOp.op(aElement, bElement) }
    }

    implicit def optConversion[T](implicit superOp: SuperOperator[T]): SuperOperator[Option[T]] = new SuperOperator[Option[T]] {
      override def zero = Option.empty[T]

      override def op(a: Option[T], b: Option[T]): Option[T] = (a, b) match {
        case (Some(aValue), Some(bValue)) => Some(superOp.op(aValue, bValue))
        case (Some(aValue), None) => Some(aValue)
        case (None, Some(bValue)) => Some(bValue)
        case (None, None) => None
      }
    }
  }
}


// my generic function
def genericOp[T](a: T, b: T)(implicit superOp: SuperOperator[T]): T = superOp.op(a, b)

import SuperOperator.Implicits._

val test1 = genericOp(1, 4)
println(test1)

val test2 = genericOp(List(1, 2, 3, 4), List(4, 3, 2, 1))
test2.foreach(println)

val test3 = genericOp(
  Option(List(1, 2, 3, 4)),
  Option(List(4, 3, 2, 1))
)
test3.foreach(println)

val test4 = genericOp(
  List(Option(1), Option(2), Option(3), Option(4)),
  List(Option(4), Option(3), Option(2), Option(1))
)
test4.foreach(println)
