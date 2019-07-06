package com.me.examples


/**
  * @author sondv
  */
object HigherKindType {
  // ### Bad => Not composable ####
  trait TerminalSyncBad {
    def read(): String
    def write(t: String): Unit
  }

  trait TerminalAsyncBad {
    import scala.concurrent.Future
    def read(): Future[String]
    def write(t: String): Future[Unit]
  }

  // #####################
  trait Foo[C[_]] {
    def create(i: Int): C[Int]
  }

  object FooList extends Foo[List] {
    override def create(i: Int): List[Int] = List(i)
  }

//  This won't compile
//  object FooEither extends Foo[Either[String, ?]] {
//
//  }

  type EitherString[A] = Either[String, A]
  object FooEitherString extends Foo[EitherString] {
    override def create(i: Int): EitherString[Int] = Right(i)
  }

  // Trick to ignore the type constructor
  type Id[A] = A
  object FooId extends Foo[Id] {
    override def create(i: Int): Id[Int] = i
  }

  // Terminal improvement using higher-kinded type
  trait Terminal[C[_]] {
    def read(): C[String]
    def write(t: String): C[Unit]
  }

  trait Execution[C[_]] {
    def chain[A, B](c: C[A])(f: A => C[B]): C[B]
    def create[B](b: B): C[B]
  }

  def echo[C[_]](t: Terminal[C], e: Execution[C]): C[String] =
    e.chain(t.read()) { in: String =>
      e.chain(t.write(in)) { _: Unit => e.create(in)}
    }

  // Improve readability by defining Execution's extension
  // Define 2 methods flatMap and map so that we can use for-comprehension
  implicit class Ops[A, C[_]](c: C[A]) {
    def flatMap[B](f: A => C[B])(implicit e: Execution[C]): C[B] =
      e.chain(c)(f)

    def map[B](f: A => B)(implicit e: Execution[C]): C[B] =
      e.chain(c)(f andThen e.create)
  }

  def echo2[C[_]](implicit t: Terminal[C], e: Execution[C]): C[String] =
    for {
      in <- t.read()
      _  <- t.write(in)
    } yield in

}
