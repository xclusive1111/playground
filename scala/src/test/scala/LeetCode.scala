import org.scalatest.FunSuite

import scala.annotation.tailrec

class LeetCode extends FunSuite {

  test("Two sum") {
    def twoSum(nums: Array[Int], target: Int): Array[Int] = {
      @tailrec
      def find(xs: Array[(Int, Int)]): Array[Int] =
        xs.headOption match {
          case Some((a, ia)) =>
            val tails = xs.tail
            val opt = tails.find { case (b, _) => a + b == target }
            opt match {
              case Some((_, ib)) => Array(ia, ib)
              case None => find(tails)
            }

          case None    => Array.empty
        }

      find(nums.zipWithIndex)
    }

    assert(twoSum(Array(2,5,5,11), 10) === Array(1, 2))
  }

  test("Add two numbers") {
    class ListNode(var _x: Int = 0) {
      var next: ListNode = _
      var x: Int = _x
    }

//    object ListNode {
//      def apply(_x: Int): ListNode = new ListNode(_x)
//    }

    def getNum(l: ListNode): BigInt = {
      def go(ln: ListNode, acc: String): String = {
        if (ln.next == null) ln._x + acc
        else go(ln.next, ln._x + acc)
      }
      BigInt(go(l, ""))
    }

    def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
      val xs   = getNum(l1) + getNum(l2)
      val ys = xs.toString.split("").map(s => new ListNode(s.toInt))
      val buildNodes: (ListNode, ListNode) => ListNode = (tails, head) => {
        head.next = tails
        head
      }

      val head = ys.head
      ys.tail.foldLeft(head)(buildNodes)
    }

    val a1 = new ListNode(2)
    val a2 = new ListNode(4)
    val a3 = new ListNode(3)
    a1.next = a2
    a2.next = a3

    val b1 = new ListNode(5)
    val b2 = new ListNode(6)
    val b3 = new ListNode(4)
    b1.next = b2
    b2.next = b3
    val out = addTwoNumbers(a1, b1)
  }

  test("Sum two integers") {
    def getSum(a: Int, b: Int): Int = {
      if (b == 0) a
      if (a == 0) b

      def sum(x: Int, y: Int): Int = {
        if (y == 0) x
        else {
          val c  = x & y
          val x_ = x ^ y
          val y_ = c << 1
          sum(x_, y_)
        }
      }
      sum(a, b)
    }

    print(getSum(3, 2))

  }

  test("Find longest substring") {
    def lengthOfLongestSubstring(s: String): Int = {
      @tailrec
      def find(xs: List[String], acc: List[String], str: String): List[String] = {
        val lastOpt = str.lastOption.map(_.toString)
        (xs, lastOpt) match {
          case (c :: Nil, Some(last)) =>
            if (c == last || str.contains(c)) acc :+ str
            else acc :+ (str + c)
          case (list @ c :: remains, Some(last)) =>
            if (c == last) find(remains, acc :+ str, c)
            else if (str.contains(c)) {
              val strTail = partitionStr(str, c)
              find(strTail.split("").toList ++ list, acc :+ str, last)
            } else find(remains, acc, str + c)
          case _ => acc :+ str
        }
      }

      val list = s.split("").toList

      find(list.tail, List.empty, list.head).map(_.length) match {
        case Nil         => 0
        case numbers @ _ => numbers.max
      }
    }

    def partitionStr(str: String, s: String): String = {
      val idx = str.indexOf(s)
      str.splitAt(idx)._2
    }

    assert(lengthOfLongestSubstring("dvdf") == 3)

    assert(lengthOfLongestSubstring(" ") == 1)

    assert(lengthOfLongestSubstring("") == 0)

    assert(lengthOfLongestSubstring("abcabcbb") == 3)

    assert(lengthOfLongestSubstring("bbbbbbb") == 1)

    assert(lengthOfLongestSubstring("pwwkew") == 3)

  }

}
