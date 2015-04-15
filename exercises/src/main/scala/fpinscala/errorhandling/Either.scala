package fpinscala.errorhandling


// hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.{Option => _, Either => _, Left => _, Right => _, _}

sealed trait Either[+E,+A] {
	def map[B](f: A => B): Either[E, B] = {
		this match {
			case Left(e) => Left(e)
			case Right(a) => Right(f(a))
		}
	}

	def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
		this match {
			case Left(e) => Left(e)
			case Right(a) =>f(a)
		}
	}

	def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
		this match {
			case Left(e) => b
			case Right(a) => Right(a)
		}
	}

	def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
		for (a <- this; bb <- b) yield f(a,bb)
	}
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
	def mean(xs: IndexedSeq[Double]): Either[String, Double] =  {
		if (xs.isEmpty) {
			Left("mean of empty list!")
		} else {
			Right(xs.sum / xs.length)
		}
	}

	def safeDiv(x: Int, y: Int): Either[Exception, Int] = {
		try Right(x / y)
		catch { case e: Exception => Left(e) }
	}

	def Try[A](a: => A): Either[Exception, A] = {
		try Right(a)
		catch { case e: Exception => Left(e) }
	}
	
	
	def sequence[E,A](list: List[Either[E,A]]): Either[E, List[A]] = {
		list match {
			case Nil => Right(Nil)
			case e::es => e.flatMap{ aa => sequence(es) map (aa :: _) }
		}
	}
	
	def traverse[E,A,B](list: List[A])(f: A => Either[E,B]): Either[E, List[B]] = {
		list match {
			case Nil => Right(Nil)
			case a::as => f(a).map2(traverse(as)(f)){ _ :: _ }
		}
	}
}

