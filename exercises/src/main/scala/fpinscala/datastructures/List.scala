package fpinscala.datastructures

// `List` data type, parameterized on a type, `A`
sealed trait List[+A]
// A `List` data constructor representing the empty list
case object Nil extends List[Nothing]
// Another data constructor, representing nonempty lists.
// Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// `List` companion object. Contains functions for creating and working with lists.
object List { 
	// A function that uses pattern matching to add up a list of integers
	def sum(ints: List[Int]): Int = {
		ints match {
			// The sum of the empty list is 0.
			case Nil => 0 
			// The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
			case Cons(x,xs) => x + sum(xs)
		}
	}
	
	def product(ds: List[Double]): Double = {
		ds match {
			case Nil => 1.0
			case Cons(0.0, _) => 0.0
			case Cons(x,xs) => x * product(xs)
		}
	}
	
	// Variadic function syntax
	def apply[A](as: A*): List[A] = {
		if (as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))
	}

	def patternMatch: Int = {
		val x = List(1,2,3,4,5) match {
			case Cons(x, Cons(2, Cons(4, _))) => x
			case Nil => 42 
			case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
			case Cons(h, t) => h + sum(t)
			case _ => 101 
		}
		x
	}

	def append[A](a1: List[A], a2: List[A]): List[A] = {
		a1 match {
			case Nil => a2
			case Cons(h,t) => Cons(h, append(t, a2))
		}
	}
	

	// Utility functions
	def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
		as match {
			case Nil => z
			case Cons(x, xs) => f(x, foldRight(xs, z)(f))
		}
	}
	
	def sum2(ns: List[Int]) = {
		foldRight(ns, 0)((x,y) => x + y)
	}
	
	def product2(ns: List[Double]) =  {
		// `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar
		foldRight(ns, 1.0)(_ * _)
	}
	/*
	 * Although we could return `Nil` when the input list is empty, we choose to throw an exception instead.
	 * This is a somewhat subjective choice. In our experience, taking the tail of an empty list is often a bug,
	 * and silently returning a value just means this bug will be discovered later, further from the place where
	 * it was introduced. 
	 * 
	 * It's generally good practice when pattern matching to use `_` for any variables
	 * you don't intend to use on the right hand side of a pattern. This makes it clear the value isn't relevant.
	 */
	def tail[A](l: List[A]): List[A] = {
		l match {
			case Nil => throw new IllegalArgumentException("Cannot get the tail of an empty list")
			case Cons(_, as) => as
		}
	}
	def head[A](l: List[A]): A = {
		l match {
			case Nil => throw new IllegalArgumentException("Cannot get the head of an empty list")
			case Cons(a, _) => a
		}
	}

	def setHead[A](l: List[A], h: A): List[A] = {
		l match {
			case Nil => List(h)
			case Cons(a,as) => List.append(List(h), as)
		}
	}

	/* 
	 * Again, it's somewhat subjective whether to throw an exception when asked to drop more
	 * elements than the list contains. The usual default for `drop` is not to throw an exception,
	 * since it's typically used in cases where this is not indicative of a programming error.
	 * If you pay attention to how you use `drop`, it's often in cases where the length of the input list is
	 * unknown, and the number of elements to be dropped is being computed from something else.
	 * If `drop` threw an exception, we'd have to first compute or check the length and only drop up
	 * to that many elements.
	 */
	def drop[A](l: List[A], n: Int): List[A] = {
		if (n <= 0) {
			l
		} else {
			l match {
				case Nil => Nil
				case Cons(_, as) => drop(as, n - 1)
			}
		}
	}

	/*
	 * Somewhat overkill, but to illustrate the feature we're using a _pattern guard_, to
	 * only match a `Cons` whose head satisfies our predicate, `p`.
	 * The syntax is to add `if <cond>` after the pattern, before the `=>`, where `<cond>`
	 * can use any of the variables introduced by the pattern.
	 */
	def dropWhile[A](l: List[A], p: A => Boolean): List[A] = {
		l match {
			case Cons(a, as) if p(a) => dropWhile(as, p)
			case _ => l
		}
	}


	/*
	 * Note that we're copying the entire list up until the last element.
	 * Besides being inefficient, the natural recursive solution will use a stack frame
	 * for each element of the list, which can lead to stack overflows for large lists
	 * (can you see why?).
	 * 
	 * With lists, it's common to use a temporary, mutable buffer internal to the function
	 * (with lazy lists or streams, which we discuss in chapter 5, we don't normally do this).
	 * So long as the buffer is allocated internal to the function, the mutation is not observable and
	 * RT is preserved.
	 * 
	 * Another common convention is to accumulate the output list in reverse order,
	 * then reverse it at the end, which doesn't require even local mutation. We'll write a
	 * reverse function later in this chapter.
	 */
	def init[A](l: List[A]): List[A] = {
		l match {
			case Nil => throw new IllegalArgumentException("Cannot init an empty list")
			case Cons(_, Nil) => Nil
			case Cons(a, as) => Cons(a, init(as))
		}
	}
	
	def init2[A](l: List[A]): List[A] = {
		import collection.mutable.ListBuffer
		val buf = new ListBuffer[A]
		
		@annotation.tailrec
		def go(cur: List[A]): List[A] = cur match {
			case Nil => throw new IllegalArgumentException("Cannot init an empty list")
			case Cons(_,Nil) => List(buf.toList: _*)
			case Cons(a,as) => buf += a; go(as)
		}
		
		go(l)
	}

	/*
	 * EXERCISE 3.7
	 * No, this is not possible! The reason is because _before_ we ever call our function, `f`,
	 * we evaluate its argument, which in the case of `foldRight` means traversing the list
	 * all the way to the end. We need _non-strict_ evaluation to support early termination
	 * --- we discuss this in chapter 5.
	 * 
	 * We get back the original list! Why is that? As we mentioned earlier,
	 * one way of thinking about what `foldRight` "does" is it replaces the `Nil`
	 * constructor of the list with the `z` argument, and it replaces the `Cons`
	 * constructor with the given function, `f`. If we just supply `Nil` for `z`
	 * and `Cons` for `f`, then we get back the input list. 
	 * 
	 * foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_))
	 * Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil:List[Int])(Cons(_,_)))
	 * Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil:List[Int])(Cons(_,_))))
	 * Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int])(Cons(_,_)))))
	 * Cons(1, Cons(2, Cons(3, Nil))) 
	 */
	def length[A](l: List[A]): Int = {
		foldRight(l,0)( (_,length) => length + 1)
	}

	// def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
	@annotation.tailrec
	def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
		l match {
			case Nil => z
			case Cons(a,as) => foldLeft(as, f(z,a))(f)
		}
	}

	def map[A,B](l: List[A])(f: A => B): List[B] = {
		val z: List[B] = Nil
		foldLeft(l, z)( (bs,a) => {
			append(List(f(a)), bs)
		})
	}
}