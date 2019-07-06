import java.util.function.Predicate
import java.util.regex.Pattern

import Chapter4.MyMonad

import scala.io.StdIn.readLine
import scala.util.Try

sealed abstract class Formula

  final case class NumberValue(n: Double) extends Formula
  final case class PosixNotation(exp: String) extends Formula
  final case class PosixFormula(exp: String) extends Formula
  final case class Reference(cell: String) extends Formula


  final case class IO[A](run: () => A) {
    self =>
    def map[B](f: A => B): IO[B] = IO(() => f(self.run()))

    def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(self.run()).run())
  }

  object IO {
    def point[A](value: A): IO[A] = IO(() => value)

    def putStrLn(str: String): IO[Unit] =
      IO(() => println(str))

    def getStrLn(): IO[String] =
      IO(() => readLine())

    implicit val ioMonad: MyMonad[IO] = new MyMonad[IO] {
      override def pure[A](value: A): IO[A] = IO.point(value)

      override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
    }
  }

  object ExcelApp {

    import IO._

    type RawMap = Map[String, Formula]
    type Output = Map[String, String]

    val posixPred: Predicate[String] = Pattern.compile("^\\-?(\\d+(\\.\\d+)?)\\s+\\-?(\\d+(\\.\\d+)?)\\s+[+\\-*\\/](\\s+(\\-?)(\\d+(\\.\\d+)?)\\s+[+\\-*\\/])*$").asPredicate
    val formulaPred: Predicate[String] = Pattern.compile("^(\\-?((\\d+(\\.\\d+)?)|([A-Za-z][0-9]+)))\\s+(\\-?((\\d+(\\.\\d+)?)|([A-Za-z][0-9]+)))\\s+[\\+\\-\\*\\/](\\s+(\\-?((\\d+(\\.\\d+)?)|([A-Za-z][0-9]+)))\\s+[\\+\\-\\*\\/])*$").asPredicate
    val cellNamePred: Predicate[String] = Pattern.compile("^([A-Za-z][0-9]+)$").asPredicate

    def main(args: Array[String]): Unit = {
      val io = for {
        amount <- getCellAmount()
        rawMap <- getCells(amount)
      } yield rawMap

      io.run().foreach { case (name, value) => println(s"$name: $value") }
    }

    def evalCells(input: RawMap): Output = ???

    private def findDep(evaluatingCells: List[String], dependCells: Array[String]): Option[String] =
      evaluatingCells.find(dependCells.contains(_))

    private def getMessage(c1: String, c2: String): String = s"Circular dependencies between $c1 and $c2 detected."

    def evalPosix(exp: String): String = ???

    private def getCellName(): IO[String] = {
      def checkRaw(str: String): IO[String] =
        if (isValidCellName(str)) IO.point(str)
        else putStrLn("You entered an invalid name. Please try again.").flatMap(_ => getCellName())

      for {
        _     <- putStrLn("Enter a cell name: ")
        str   <- getStrLn()
        valid <- checkRaw(str)
      } yield valid
    }

    private def getCellFormula(): IO[Formula] = {
      def checkRaw(raw: String): IO[Formula] = {
        if (isNumber(raw)) IO.point(NumberValue(raw.toDouble))
        else if (isPosixNotation(raw)) IO.point(PosixNotation(raw))
        else if (isValidCellName(raw)) IO.point(Reference(raw))
        else if (formulaPred.test(raw)) IO.point(PosixFormula(raw))
        else putStrLn("You entered an invalid formula. Please try again.").flatMap(_ => getCellFormula())
      }

      for {
        _     <- putStrLn("Enter a cell formula: ")
        str   <- getStrLn()
        valid <- checkRaw(str)
      } yield valid
    }

    private def getCellAmount(): IO[Int] = {
      def checkRaw(raw: String): IO[Int] =
        Try(raw.toInt).fold(
          _ => putStrLn("You did not enter a number. Please try again.").flatMap(_ => getCellAmount()),
          n => IO.point(n)
        )

      for {
        _     <- putStrLn("Enter an amount of cells: ")
        str   <- getStrLn()
        valid <- checkRaw(str)
      } yield valid
    }

    private def getCells(amount: Int): IO[RawMap] = {
      def get(remain: Int, acc: RawMap): IO[RawMap] = {
        if (remain <= 0) IO.point(acc)
        else for {
          name    <- getCellName()
          formula <- getCellFormula()
          cells   <- get(remain - 1, acc + (name -> formula))
        } yield cells
      }

      get(amount, Map.empty)
    }

    def isValidCellName(name: String): Boolean =
      cellNamePred.test(name)

    def isValidFormula(formula: String): Boolean =
      isNumber(formula) || isValidCellName(formula) || formulaPred.test(formula)

    def isNumber(str: String): Boolean =
      Try(BigDecimal(str)).isSuccess

    def isPosixNotation(expression: String): Boolean =
      posixPred.test(expression)

  }
