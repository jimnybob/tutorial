package scalaworld.macros

@Redactor
class Test(i: Int)

object RedactorExample extends App {

  Test.redact
  val a = new Test(1)
  println(a)
}
