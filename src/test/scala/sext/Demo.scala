package sext

object Demo extends App {

  import sext.Sext._

  case class A(a: Int, b: String, c: Seq[A])
  val a = A(1, "adsf", A(5, "oiweur", Nil) :: A(4, "", A(6, "dsf", Nil) :: Nil) :: A(9, "sdlfkjw", Nil) :: Nil)
  a.treeStringValue.trace()

}
