package fpinscala.errorhandling


// hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Some => _, Either => _, _}

sealed trait Option[+A] {
	def map[B](f: A => B): Option[B] = {
		sys.error("todo")
	}

	def getOrElse[B>:A](default: => B): B = {
		sys.error("todo")
	}

	def flatMap[B](f: A => Option[B]): Option[B] = {
		sys.error("todo")
	}

	def orElse[B>:A](ob: => Option[B]): Option[B] = {
		sys.error("todo")
	}

	def filter(f: A => Boolean): Option[A] = {
		sys.error("todo")
	}
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
	def failingFn(i: Int): Int = {
		// `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
		val y: Int = throw new Exception("fail!")
		try {
			val x = 42 + 5
			x + y
		}
		catch {
			// A `catch` block is just a pattern matching block like the ones we've seen.
			// `case e: Exception` is a pattern that matches any `Exception`,
			// and it binds this value to the identifier `e`. The match returns the value 43.
			case e: Exception => 43
		}
	}

	def failingFn2(i: Int): Int = {
		try {
			val x = 42 + 5
			// A thrown Exception can be given any type; here we're annotating it with the type `Int`
			x + ((throw new Exception("fail!")): Int)
		}
		catch { case e: Exception => 43 }
	}

	def mean(xs: Seq[Double]): Option[Double] = {
		if (xs.isEmpty) None
		else Some(xs.sum / xs.length)
	}
	
	def variance(xs: Seq[Double]): Option[Double] = {
		sys.error("todo")
	}

	def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
		sys.error("todo")
	}

	def sequence[A](a: List[Option[A]]): Option[List[A]] = {
		sys.error("todo")
	}

	def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
		sys.error("todo")
	}
}