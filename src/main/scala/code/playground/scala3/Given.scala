package code.playground.scala3

object Given {

  case class User(username: String)

  given userOrdering: Ordering[User] = new Ordering[User]:
    override def compare(x: User, y: User): Int =
      x.username.compareTo(y.username)



}
