package fpinscala.laziness

import Stream._

trait Stream[+A] {

	/**
	 *The natural recursive solution
	 */
	def toListRecursive: List[A] = this match {
		case Cons(h,t) => h() :: t().toListRecursive
		case _ => List()
	}

	/**
	 * The above solution will stack overflow for large streams, since it's
	 * not tail-recursive. Here is a tail-recursive implementation. At each
	 * step we cons onto the front of the `acc` list, which will result in the
	 * reverse of the stream. Then at the end we reverse the result to get the
	 * correct order again.
	 */
	def toList: List[A] = {
		@annotation.tailrec
		def go(s: Stream[A], acc: List[A]): List[A] = s match {
			case Cons(h,t) => go(t(), h() :: acc)
			case _ => acc
		}
		go(this, List()).reverse
	}

	/**
	 * In order to avoid the `reverse` at the end, we could write it using a
	 * mutable list buffer and an explicit loop instead. Note that the mutable
	 * list buffer never escapes our `toList` method, so this function is
	 * still _pure_.
	 */
	def toListFast: List[A] = {
		val buf = new collection.mutable.ListBuffer[A]
		@annotation.tailrec
		def go(s: Stream[A]): List[A] = s match {
			case Cons(h,t) => {
				buf += h()
				go(t())
			}
			case _ => buf.toList
		}
		go(this)
	}
	
	/*
	 * The arrow `=>` in front of the argument type `B` means that the function `f` takes
	 * its second argument by name and may choose not to evaluate it.
	 */
	def foldRight[B](z: => B)(f: (A, => B) => B): B = {
		this match {
			case Cons(h,t) => f(h(), t().foldRight(z)(f))
			// If `f` doesn't evaluate its second argument, the recursion never occurs.
			case _ => z
		}
	}
	

	@annotation.tailrec
	final def find(f: A => Boolean): Option[A] = this match {
		case Empty => None
		case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
	}
	
	/**
	 * `take` first checks if n==0. In that case we need not look at the stream at all.
	 */
	def take(n: Int): Stream[A] = {
		if (n > 0) {
			this match {
				case Cons(h, t) if n == 1 => cons(h(), Stream.empty) // we can say Stream.empty
				case Cons(h, t) => cons(h(), t().take(n-1))
				case _ => Stream.empty
			}
		} else {
			Stream()
		}
	}

	/**
	 *  Unlike `take`, `drop` is not incremental. That is,
	 *  it doesn't generate the answer lazily. It must
	 *  traverse the first `n` elements of the stream eagerly.
	 */
	def drop(n: Int): Stream[A] = {
		@annotation.tailrec
		def go(s: Stream[A], n: Int): Stream[A] = {
			if (n <= 0) s
			else s match {
				case Cons(h,t) => go(t(), n-1)
				case _ => Stream()
			}
		}
		go(this, n)
	}

	def takeWhile(p: A => Boolean): Stream[A] = {
		this match { 
			case Cons(h,t) if p(h()) => cons(h(), t() takeWhile p)
			case _ => empty 
		}
	}

	/*
	 * Here `b` is the unevaluated recursive step that folds the tail of the stream.
	 * If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.
	 */
	def exists(p: A => Boolean): Boolean = {
		foldRight(false){ (a, b) => p(a) || b }
	}
	
	def forAll(p: A => Boolean): Boolean = {
		foldRight(true)((a,b) => p(a) && b)
	}
	
	def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
		foldRight(empty[A]){ (h,t) => 
			if (p(h)) cons(h,t)
			else      empty
		}
	}

	def headOption: Option[A] = {
		foldRight(None: Option[A])((h,_) => Some(h))
	}
	
	def map[B](f: A => B): Stream[B] = {
		foldRight(empty[B]){ (h,t) => cons(f(h), t) }
	}
	
	def filter[B](p: A => Boolean): Stream[A] = {
		foldRight(empty[A]){ (h,t) =>
			if (p(h)) cons(h, t)
			else t
		}
	}
	
	def flatMap[B](f: A => Stream[B]): Stream[B] = {
		foldRight(empty[B]){ (h,t) => f(h) append t }
	}

	def append[B>:A](s: => Stream[B]): Stream[B] = {
		foldRight(s){ (h,t) => cons(h,t) }
	}

	def startsWith[B](s: Stream[B]): Boolean = {
		???
	}
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
	def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
		lazy val head = hd
		lazy val tail = tl
		Cons(() => head, () => tail)
	}

	def empty[A]: Stream[A] = Empty

	def apply[A](as: A*): Stream[A] =
		if (as.isEmpty) empty 
		else cons(as.head, apply(as.tail: _*))

	val ones: Stream[Int] = Stream.cons(1, ones)
	
	def from(n: Int): Stream[Int] = {
		cons(n, from(n+1))
	}

	lazy val fibs = {
		def go(f0: Int, f1: Int): Stream[Int] = 
			cons(f0, go(f1, f0+f1))
		go(0, 1)
	}
	
	def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
		f(z) match {
			case Some((h,s)) => cons(h, unfold(s)(f))
			case None => empty
		}
	}
}