package com.me.examples.app0

import scala.io.StdIn.readLine
import scala.util.Try


object App2 {


  trait Program[F[_]] {
    def finish[A](a: A): F[A]

    def chain[A, B](fa: F[A], f: A => F[B]): F[B]

    def map[A, B](fa: F[A], f: A => B): F[B]
  }

  object Program {
    def apply[F[_]](implicit F: Program[F]): Program[F] = F
  }

  trait Console[F[_]] {
    def getStrLn: F[String]
    def putStrLn(line: String): F[Unit]
  }

  object Console {
    def apply[F[_]](implicit F: Console[F]): Console[F] = F
  }

  trait Random[F[_]] {
    def nextInt(upper: Int): F[Int]
  }

  object Random {
    def apply[F[_]](implicit F: Random[F]): Random[F] = F
  }

  implicit class ProgramSyntax[F[_]: Program, A](fa: F[A]) {
    def map[B](f: A => B): F[B] = Program[F].map(fa, f)

    def flatMap[B](afb: A => F[B]): F[B] = Program[F].chain(fa, afb)
  }

  case class IO[A](unsafeRun: () => A) { self =>
    def map[B](f: A => B): IO[B] = IO(() => f(self.unsafeRun()))

    def flatMap[B](fb: A => IO[B]): IO[B] = IO(() => fb(self.unsafeRun()).unsafeRun())
  }

  object IO {
    def point[A](a: A): IO[A] = IO(() => a)

    implicit val ProgramIO: Program[IO] = new Program[IO] {
      override def finish[A](a: A): IO[A] = IO.point(a)

      override def chain[A, B](fa: IO[A], afb: A => IO[B]): IO[B] = fa.flatMap(afb)

      override def map[A, B](fa: IO[A], f: A => B): IO[B] = fa.map(f)
    }

    implicit val ConsoleIO: Console[IO] = new Console[IO] {
      override def getStrLn: IO[String] = IO(() => readLine())

      override def putStrLn(line: String): IO[Unit] = IO(() => println(line))
    }

    implicit val RandomIO: Random[IO] = new Random[IO] {
      override def nextInt(upper: Int): IO[Int] = IO(() => scala.util.Random.nextInt(upper))
    }
  }

  def putStrLn[F[_]: Console](line: String): F[Unit] = Console[F].putStrLn(line)

  def getStrLn[F[_]: Console]: F[String] = Console[F].getStrLn

  def nextInt[F[_]: Random](upper: Int): F[Int] = Random[F].nextInt(upper)

  def finish[F[_]: Program, A](a: A): F[A] = Program[F].finish(a)

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  def checkContinue[F[_]: Program: Console](name: String): F[Boolean] =
    for {
      _     <- putStrLn(s"Do you want to continue, $name ?")
      input <- getStrLn.map(_.toLowerCase)
      cont  <- input match {
                 case "y" => finish(true)
                 case "n" => finish(false)
                 case _   => checkContinue(name)
               }
    } yield cont

  def gameLoop[F[_]: Program: Console: Random](name: String): F[Unit] =
    for {
      num   <- nextInt(5).map(_ + 1)
      _     <- putStrLn(s"Dear $name, please guess a number from 1 to 5")
      input <- getStrLn
      _     <- parseInt(input) match {
                 case None => putStrLn("You did not enter a number")
                 case Some(n) =>
                   if (n == num) putStrLn(s"You guessed right, $name !")
                   else putStrLn(s"You guessed wrong, $name ! The number was: $num")
               }
      cont  <- checkContinue(name)
      _     <- if (cont) gameLoop(name)
               else finish(())
    } yield ()

  def guessGame[F[_]: Program: Console: Random]: F[Unit] = {
    for {
      _    <- putStrLn("What is your name?")
      name <- getStrLn
      _    <- putStrLn("Hello, " + name + ", welcome to the game!")
      _    <- gameLoop(name)
    } yield ()
  }

  def main(args: Array[String]): Unit = {
    guessGame[IO].unsafeRun()
  }

  case class TestData(inputs: List[String], outputs: List[String], nums: List[Int])

  case class TestIO[A](run: TestData => (TestData, A)) { self =>
    def map[B](f: A => B): TestIO[B] =
      TestIO(t => self.run(t) match { case (t_, a) => (t_, f(a)) })

    def flatMap[B](afb: A => TestIO[B]): TestIO[B] =
      TestIO(t => self.run(t) match { case (t_, a) => afb(a).run(t_) })

  }
}
