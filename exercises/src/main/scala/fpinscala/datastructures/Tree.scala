package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

	def size[A](tree: Tree[A]): Int = {
		tree match {
			case Leaf(v) => 1
			case Branch(l, r) => 1 + size(l) + size(r)
		}
	}

	def maximum(tree: Tree[Int]):Int = {
		tree match {
			case Leaf(v) => v
			case Branch(l,r) => {
				maximum(l) max maximum(r)
			}
		}
	}
	
	def depth[A](tree: Tree[A]): Int = {
		tree match {
			case Leaf(v) => 0
			case Branch(l,r) => 1 + (depth(l) max size(r))
		}
	}
	
	def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = {
		tree match {
			case Leaf(v) => Leaf(f(v))
			case Branch(l,r) => Branch(map(l)(f),map(r)(f))
		}
	}

	/*
	 * Like `foldRight` for lists, `fold` receives a "handler" for each of the
	 * data constructors of the type, and recursively accumulates some value
	 * using these handlers. As with `foldRight`, `fold(t)(Leaf(_))(Branch(_,_)) == t`,
	 * and we can use this function to implement just about any recursive function
	 * that would otherwise be defined by pattern matching.
 	 */
	def fold[A,B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B = {
		tree match {
			case Leaf(v) => f(v)
			case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
		}
	}
	
	def sizeViaFold[A](tree: Tree[A]): Int = {
		fold(tree)(a => 1)(1 + _ + _)
	}
	
	def maximumViaFold(tree: Tree[Int]): Int = {
		fold(tree)(a => a)(_ max _)
	}
	
	/*
	 * Note the type annotation required on the expression `Leaf(f(a))`.
	 * 
	 * This error is an unfortunate consequence of Scala using subtyping
	 * to encode algebraic data types. Without the annotation, the result
	 * type of the fold gets inferred as `Leaf[B]` and it is then expected
	 * that the second argument to `fold` will return `Leaf[B]`, which
	 * it doesn't (it returns `Branch[B]`). Really, we'd prefer Scala to
	 * infer `Tree[B]` as the result type in both cases.
	 * When working with algebraic data types in Scala, it's somewhat common to
	 * define helper functions that simply call the corresponding data constructors
	 * but give the less specific result type
	 * 
	 * def leaf[A](a: A): Tree[A] = Leaf(a)
	 * def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
	 */
	def mapViaFold[A,B](tree: Tree[A])(f: A => B): Tree[B] = {
		fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
	}
}