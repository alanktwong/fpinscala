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
		foldRight(ns, 0){_ + _}
	}
	
	def product2(ns: List[Double]) =  {
		// `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar
		foldRight(ns, 1.0){_ * _}
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
	 * Can product, implemented with foldRight, halt recursion and return 0.0
	 * if it is encounters 0.0 as one of the terms.
	 * 
	 * No, this is not possible! The reason is because _before_ we ever call our function, `f`,
	 * we evaluate its argument, which in the case of `foldRight` means traversing the list
	 * all the way to the end. We need _non-strict_ evaluation to support early termination
	 * --- we discuss this in chapter 5.
	 * 
	 * EXERCISE 3.8
	 * Seee what happens when you pass Cons/Nil to foldRight.
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
	
	// implement length with foldRight
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
	
	def lengthLeft[A](l: List[A]): Int = {
		foldLeft(l,0){ (length,_) => length + 1 }
	}
	def sumLeft(l: List[Int]): Int = {
		foldLeft(l,0){ _ + _ }
	}
	def productLeft(l: List[Double]): Double = {
		foldLeft(l,1.0){ _ * _ }
	}

	
	/*
	 * EX 3.12
	 * reverse a list using fold (e.g. given List(1,2,3) return List(3,2,1)
	 */
	def reverse[A](l: List[A]): List[A]  = {
		foldLeft(l, Nil: List[A])( (bs,a) => {
			append( List(a),bs)
		})
	}
	/*
	 * EX 3.13
	 * Write foldLeft in terms of foldRight. Write foldRight in terms of foldLeft.
	 * The latter is useful because it would be stack-safe
	 */
	
	/*
	 * The implementation of `foldRight` in terms of `reverse` and `foldLeft` is a
	 * common trick for avoiding stack overflows when implementing a strict `foldRight`
	 * function as we've done in this chapter. (We'll revisit this in a later chapter, when we discuss laziness).
	 * 
	 * The other implementations build up a chain of functions which, when called, results in
	 * the operations being performed with the correct associativity. We are calling `foldRight`
	 * with the `B` type being instantiated to `B => B`, then calling the built up function with
	 * the `z` argument. Try expanding the definitions by substituting equals for equals
	 * using a simple example, like `foldLeft(List(1,2,3), 0)(_ + _)` if this isn't clear.
	 * Note these implementations are more of theoretical interest - they aren't stack-safe
	 * and won't work for large lists.
	 */
	def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
		foldLeft(reverse(l), z)((b,a) => f(a,b))
	}
	def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
		val identity: B => B = b => b
		foldLeft(l, identity){ (id,a) =>
			b => id(f(a,b))
		}(z)
	}
	def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
		val identity: B => B = b => b
		foldRight(l, identity){ (a,id) =>
			b => id(f(b,a))
		}(z)
	}
	
	
	
	/*
	 * EX 3.14
	 * Implement append in terms of either foldLeft or foldRight
	 * 
	 * `append` simply replaces the `Nil` constructor of the first list with the second list,
	 * which is exactly the operation performed by `foldRight`.
	 */
	def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = {
		foldLeft(reverse(a1), a2){ (as,b) => Cons(b,as) }
	}
	
	def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
		foldRight(a1, a2){ (a,bs) => Cons(a,bs) }
	}
	
	/*
	 * EX 3.15
	 * Write a function that concatenate a list of lists into a single list. its
	 * runtime should be linear in the total length of all lists.
	 * 
	 * Since `append` takes time proportional to its first argument, and this
	 * first argument never grows because of the right-associativity of `foldRight`,
	 * this function is linear in the total length of all lists. You may want to try tracing
	 * the execution of the implementation on paper to convince yourself that this works.
	 * 
	 * Note that we're simply referencing the `append` function, without writing
	 * something like `(x,y) => append(x,y)` or `append(_,_)`. In Scala there
	 * is a rather arbitrary distinction between functions defined as _methods_,
	 * which are introduced with the `def` keyword, and function values,
	 * which are the first-class objects we can pass to other functions, put in collections,
	 * and so on. This is a case where Scala lets us pretend the distinction doesn't exist.
	 * In other cases, you'll be forced to write `append _` (to convert a `def` to a function value)
	 * or even `(x: List[A], y: List[A]) => append(x,y)` if the function is polymorphic and
	 * the type arguments aren't known. 
	 * 
	 */
	def concat[A](l: List[List[A]]): List[A] = {
		foldRight(l, Nil:List[A])(append)
	}
	
	/*
	 * Skipped 3.16 & 3.17 b/c they're just versions of map
	 * 
	 * EX 3.18
	 * 
	 * A natural solution is using `foldRight`, but our implementation of `foldRight`
	 * is not stack-safe. We can use `foldRightViaFoldLeft` or `foldLeft`
	 * to avoid the stack overflow (variation 1), but more commonly, with
	 * our current implementation of `List`, `map` will just be implemented using
	 * local mutation (variation 2). Again, note that the mutation isn't observable
	 * outside the function, since we're only mutating a buffer that we've allocated. 
	 * 
	 */
	def map[A,B](l: List[A])(f: A => B): List[B] = {
		foldLeft(l, Nil:List[B])( (bs,a) => {
			append(bs, List(f(a)))
		})
	}
	
	def map_2[A,B](l: List[A])(f: A => B): List[B] = {
		val buf = new collection.mutable.ListBuffer[B]
		def go(l: List[A]): Unit = l match {
			case Nil => ()
			case Cons(h,t) => {
				buf += f(h)
				go(t)
			}
		}
		go(l)
		// converting from the standard Scala list to the list we've defined here
		List(buf.toList: _*)
	}
	/*
	 * EX 3.19
	 */
	def filter[A](l: List[A])(p: A => Boolean): List[A] = {
		val xs = foldLeft(l, Nil:List[A]){ (as,a) => if (p(a)) Cons(a,as) else as }
		reverse(xs)
	}
	def filter_2[A](l: List[A])(p: A => Boolean): List[A] = {
		val buf = new collection.mutable.ListBuffer[A]
		def go(l: List[A]): Unit = l match {
			case Nil => ()
			case Cons(h,t) => {
				if (p(h))  buf += h
				go(t)
			}
		}
		go(l)
		List(buf.toList: _*)
	}
	/*
	 * EX 3.20
	 */
	def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
		concat(map(l)(f))
	}
	
	/*
	 * EX 3.21
	 * Use flatMap to implement filter
	 */
	def filterViaFlatMap[A](l: List[A])(p: A => Boolean): List[A] = {
		flatMap(l){ a => if (p(a)) List(a) else Nil }
	}
	
	/*
	 * EX 3.23
	 */
	def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] = {
		(as,bs)  match {
			case (Nil,_) => Nil
			case (_,Nil) => Nil
			case (Cons(a,as1), Cons(b,bs1)) => {
				Cons( f(a,b), zipWith(as1,bs1)(f) )
			}
		}
	}
	/*
	 * There's nothing particularly bad about this implementation,
	 * except that it's somewhat monolithic and easy to get wrong.
	 * Where possible, we prefer to assemble functions like this using
	 * combinations of other functions. It makes the code more obviously
	 * correct and easier to read and understand. Notice that in this
	 * implementation we need special purpose logic to break out of our
	 * loops early. In Chapter 5 we'll discuss ways of composing functions
	 * like this from simpler components, without giving up the efficiency
	 * of having the resulting functions work in one pass over the data.
	 */
	def startsWith[A](l: List[A], prefix: List[A]): Boolean = {
		(l,prefix) match {
			case (_,Nil) => true
			case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
			case _ => false
		}
	}
	
	@annotation.tailrec
	def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
		l match {
			case Nil => false
			case Cons(h,t) if startsWith(l, sub) => true
			case Cons(h,t) => hasSubsequence(t, sub) 
		}
	}
}