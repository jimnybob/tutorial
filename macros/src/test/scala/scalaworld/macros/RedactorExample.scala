package scalaworld.macros

@Redactor
case class Test(i: Int, b: Boolean, c: Nested)

case class Nested(n: String)

object RedactorExample extends App {

  val a = new Test(1, true, Nested("umm")).redact
  println(a)
}
