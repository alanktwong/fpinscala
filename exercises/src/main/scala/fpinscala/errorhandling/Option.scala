package fpinscala.errorhandling


// hide standard library `Option`, `Some` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Some => _, Either => _, _}
import util.{Try, Success, Failure}

sealed trait Option[+A] {
	def map[B](f: A => B): Option[B] = {
		this match {
			case None => None
			case Some(a) => Some(f(a))
		}
	}

	def getOrElse[B>:A](default: => B): B = {
		this match {
			case Some(a) => a
			case None => default
		}
	}

	def flatMap[B](f: A => Option[B]): Option[B] = {
		map(f) getOrElse (None)
	}

	def flatMapViaPatternMatch[B](f: A => Option[B]): Option[B] = {
		this match {
			case Some(a) => f(a)
			case None => None
		}
	}
	
	def orElse[B>:A](ob: => Option[B]): Option[B] = {
		this map (Some(_)) getOrElse ob
	}
	
	def orElseViaPatternMatch[B>:A](ob: => Option[B]): Option[B] = {
		this match {
			case Some(a) => this
			case None => ob
		}
	}

	def filter(f: A => Boolean): Option[A] = {
		this match {
			case Some(a) if (f(a)) => this
			case _ => None
		}
	}
	def filterViaFlatMap(f: A => Boolean): Option[A] = {
		flatMap(a => if (f(a)) Some(a) else None)
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
		mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
	}
	
	def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
	
	
	def apply[A](t: Try[A]): Option[A] = {
		t match {
			case Success(t) => Some(t)
			case Failure(e) => None
		}
	}

	def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
		a flatMap { aa =>
			b map { bb =>
				f(aa, bb)
			}
		}
	}

	// Here's an explicit recursive version:
	def sequence[A](list: List[Option[A]]): Option[List[A]] = {
		list match {
			case Nil => Some(Nil)
			case a::as => a flatMap{aa => sequence(as) map (aa :: _)}
		}
	}
	/*
	 * It can also be implemented using `foldRight` and `map2`.
	 * The type annotation on `foldRight` is needed here; otherwise Scala wrongly
	 * infers the result type of the fold as `Some[Nil.type]` and reports a type error (try it!).
	 * This is an unfortunate consequence of Scala using subtyping to encode algebraic data types.
	 */
	def sequenceViaHigherOrder[A](list: List[Option[A]]): Option[List[A]] = {
		list.foldRight[Option[List[A]]](Some(Nil)){ (x,y) =>
			map2(x,y)(_ :: _)
		}
	}
	
	
	def traverse[A, B](list: List[A])(f: A => Option[B]): Option[List[B]] = {
		list match {
			case Nil => Some(Nil)
			case a::as => map2(f(a), traverse(as)(f))(_ :: _)
		}
	}
	
	def traverseViaHigherOrder[A, B](list: List[A])(f: A => Option[B]): Option[List[B]] = {
		list.foldRight[Option[List[B]]](Some(Nil)){ (a,as) =>
			map2(f(a),as)(_ :: _)
		}
	}
}