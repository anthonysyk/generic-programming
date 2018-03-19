package functionnal

sealed trait State[T] {
  type NextDependantType

  def value: NextDependantType
}

class Sale {
  def value = "sale"
}

class Nettoye {
  def value = "nettoyer"
}

class Propre {
  def value = "propre"
}

object State {

  type Aux[T, R] = State[T] {type NextDependantType = R}

  implicit def dirtyState = new State[Sale] {
    override type NextDependantType = Nettoye

    override def value: Nettoye = new Nettoye
  }

  implicit def cleanedState = new State[Nettoye] {
    override type NextDependantType = Propre

    override def value: Propre = new Propre

  }

  implicit def propreState = new State[Propre] {
    override type NextDependantType = Sale

    override def value: Sale = new Sale

  }

}

sealed trait Monoid[T] {
  def id: T

  def op(x: T, y: T): T
}

object Monoid {
  implicit def sMonoid = new Monoid[Sale] {
    override def id: Sale = new Sale

    override def op(x: Sale, y: Sale): Sale = x
  }

  implicit def nMonoid = new Monoid[Nettoye] {
    override def id: Nettoye = new Nettoye

    override def op(x: Nettoye, y: Nettoye): Nettoye = x
  }
}


object DependantType {
  def changeState[T, R](state: T)
                       (implicit nextState: State.Aux[T, R], m: Monoid[R]): R = m.id

  def main(args: Array[String]): Unit = {

    val dirtyObject = new Sale

    println(changeState(dirtyObject).value)

  }


}
