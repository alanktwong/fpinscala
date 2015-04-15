package fpinscala.errorhandling


/*
 * EX 4.8
 * The implementation of map2 on the Either trait is only able to report one error,
 * even if both name and age are invalid. What would you need to change in order
 * to report both errors? Would you change the signature of map2 or the signature
 * of mkPerson? Or could you create a new data type that captures this requirement
 * better than Either does, with some additional structure? How would orElse,
 * traverse and sequence behave differently fot his data type?
 * 
 * There are a number of variations on `Option` and `Either`. If
 * we want to accumulate multiple errors, a simple approach is a new data type
 * that lets us keep a list of errors in the data constructor that represents failures:
 * 
 * This type is called `Validation` in the Scalaz library.
 * You can implement `map`, `map2`, `sequence`, and so on for this type in such a way
 * that errors are accumulated when possible. (This idea can even be generalized further -
 * we don't need to accumulate failing values into a list, we can accumulate values using
 * any user-supplied binary function)
 * 
 * It's also possible to use `Either[List[E],_]` directly to accumulate errors,
 * using different implementations of helper functions like `map2` and `sequence`.
*/


sealed trait Partial[+E,+A] {
	/**
	 * Map on the success of this validation.
	 */
	def map[B](f: A => B): Partial[E, B] = {
		???
	}
	
	/**
	 * Binary functor map on this validation.
	 */
	def bimap[C, D](f: E => C, g: A => D): Partial[C, D] = {
		???
	}

	/**
	 * flatMap on the success of this validation.
	 */
	def flatMap[EE >: E, B](f: A => Partial[EE, B]): Partial[EE, B] = {
		???
	}
	/**
	 * Filter on the success of this validation.
	 */
	def filter[EE >: E](p: A => Boolean)(implicit m: E => EE): Partial[EE, A] = {
		???
	}
	
	/**
	 * Run the given function on the left value.
	 */
	def leftMap[C](f: E => C): Partial[C, A] = {
		???
	}
	
	/**
	 * Return the success value of this validation or the given default if failure.
	 */
	def getOrElse[AA >: A](x: => AA): AA = {
		???
	}
	
	def orElse[EE >: E, AA >: A](x: => Partial[EE, AA]): Partial[EE, AA] = {
		???
	}
	
	/**
	 * Flip the failure/success values in this validation.
	 */
	def swap: Partial[A, E] = {
		???
	}
	/**
	 * Run the given function on this swapped value.
	 */
	def swapped[EE, AA](k: Partial[A, E] ⇒ Partial[AA, EE]): Partial[EE, AA] = {
		???
	}
}

case class Losses[+A](get: Seq[A]) extends Partial[A,Nothing]

case class Won[+B](get: B) extends Partial[Nothing,B]


object Partial {
  
}