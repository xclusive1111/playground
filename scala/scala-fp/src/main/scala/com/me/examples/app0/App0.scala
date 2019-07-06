package com.me.examples.app0
import scala.io.StdIn.readLine

object App0 {
  def main(args: Array[String]): Unit = {
    println("What is your name?")
    val name = readLine()

    println("Hello, " + name + ", welcome to the game!")

    var exec = true
    while (exec) {
      val num = scala.util.Random.nextInt(5) + 1

      println(s"Dear $name, please guess a number from 1 to 5")
      val guess = readLine().toInt
      if (guess == num) println(s"You guessed right, $name !")
      else println(s"You guessed wrong, $name ! The number was: $num")

      println(s"Do you want to continue, $name ?")
      readLine() match {
        case "y" => exec = true
        case "n" => exec = false
      }
    }

  }

}
