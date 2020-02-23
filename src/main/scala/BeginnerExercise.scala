// https://blog.tmorris.net/posts/scala-exercises-for-beginners/

// You are not permitted to use these List methods:
// * length
// * map
// * filter
// * ::: (and variations such as ++)
// * flatten
// * flatMap
// * reverse (and variations i.e. reverseMap, reverse_:::)
// This also means you are not permitted to use for-comprehensions on Lists.
// You are permitted to use the functions you write yourself. For example, Exercise 2 may use Exercise 1 or Exercise 3.
// Using permitted existing methods where appropriate will attract marks for elegance.

// TOTAL marks:    /66

object Tester extends App {
  import BeginnerExercise._

  println("Add: " + add(4,0))
  println("Sum: " + sum(List(1,2,3)))
  println("Length: " + length(List(1,2,3,4)))
  println("Map: " + map(List("a", "b", "c"), (a: String) => a + "x"))
  println("Filter: " + filter(List(1,2,3,4), (a: Int) => a % 2 == 0))
  println("Append: " + append(List("a", "b"), List("c")))
  println("Concat: " + concat(List(List(1,2),List(5,6),List(8,9))))
  println("ConcatMap: " + concatMap(List(1,2,3,4,5), (i: Int) => List(i.toString + i.toString)))
  println("Maximum: " + maximum(List(1,2,3)))
  println("Reverse: " + reverse(List(1,2,3,4,5)))
}

object BeginnerExercise {
  def succ(n: Int) = n + 1
  def pred(n: Int) = n - 1

  // Exercise 1
  // Relative Difficulty: 1
  // Correctness: 2.0 marks
  // Performance: 0.5 mark
  // Elegance: 0.5 marks
  // Total: 3
  //  def add(x: Int, y: Int): Int = error("todo: Assume x and y are 0 or positive. Do not use + or - on Int. Only permitted to use succ/pred (above).")
  def add(x: Int, y: Int): Int = {
    if (x == 0) y else add(pred(x), succ(y))
  }

  // Exercise 2
  // Relative Difficulty: 2
  // Correctness: 2.5 marks
  // Performance: 1 mark
  // Elegance: 0.5 marks
  // Total: 4
  //  def sum(x: List[Int]): Int = error("todo")
  def sum(x: List[Int]): Int = {
    x match {
      case head :: tail => add(head, sum(tail))
      case Nil => add(0,0)
    }
  }

  // Exercise 3
  // Relative Difficulty: 2
  // Correctness: 2.5 marks
  // Performance: 1 mark
  // Elegance: 0.5 marks
  // Total: 4
  //  def length[A](x: List[A]): Int = error("todo")
  def length[A](x: List[A]): Int = {
    x match {
      case _ :: tail => add(1, length(tail))
      case Nil => add(0, 0)
    }
  }

  // Exercise 4
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.0 mark
  // Elegance: 1.5 marks
  // Total: 7
  //  def map[A, B](x: List[A], f: A => B): List[B] = error("todo")
  def map[A, B](x: List[A], f: A => B): List[B] = {
    x match {
      case head :: tail => f(head) :: map(tail, f)
      case Nil => Nil
    }
  }

  // Exercise 5
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  //  def filter[A](x: List[A], f: A => Boolean): List[A] = error("todo")
  def filter[A](x: List[A], f: A => Boolean): List[A] = {
    x match {
      case head :: tail => if (f(head)) head :: filter(tail, f) else filter(tail, f)
      case Nil => Nil
    }
  }

  // Exercise 6
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  //  def append[A](x: List[A], y: List[A]): List[A] = error("todo")
  def append[A](x: List[A], y: List[A]): List[A] = {
    x match {
      case last :: Nil => last :: y
      case head :: tail => head :: append(tail, y)
      case Nil => y
    }
  }

  // Exercise 7
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  //  def concat[A](x: List[List[A]]): List[A] = error("todo")
  def concat[A](x: List[List[A]]): List[A] = {
    x match {
      case head :: tail => append(head, concat(tail))
      case Nil => Nil
    }
  }

  // Exercise 8
  // Relative Difficulty: 7
  // Correctness: 5.0 marks
  // Performance: 1.5 marks
  // Elegance: 1.5 mark
  // Total: 8
  //  def concatMap[A, B](x: List[A], f: A => List[B]): List[B] = error("todo")
  def concatMap[A, B](x: List[A], f: A => List[B]): List[B] = {
    x match {
      case head :: tail => append(f(head), concatMap(tail, f))
      case Nil => Nil
    }
  }

  // Exercise 9
  // Relative Difficulty: 8
  // Correctness: 3.5 marks
  // Performance: 3.0 marks
  // Elegance: 2.5 marks
  // Total: 9
  //  def maximum(x: List[Int]): Int = error("todo")
  def maximum(x: List[Int]): Int = {
    x match {
      case head :: Nil => head
      case head :: tail =>
        val filtered = filter(tail, (i: Int) => i > head)
        val result = maximum(filtered)
        if (head > result) head else result
      case Nil => 0
    }
  }

  // Exercise 10
  // Relative Difficulty: 10
  // Correctness: 5.0 marks
  // Performance: 2.5 marks
  // Elegance: 2.5 marks
  // Total: 10
  //  def reverse[A](x: List[A]): List[A] = error("todo")
  def reverse[A](x: List[A]): List[A] = {
    x match {
      case head :: Nil => List(head)
      case head :: tail => append(reverse(tail), List(head))
      case Nil => Nil
    }
  }
}