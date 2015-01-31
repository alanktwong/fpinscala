package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

	def size[A](tree: Tree[A]): Int = {
		???
	}

	def maximum[Int](tree: Tree[Int]): Tree[Int] = {
		???
	}
	
	def depth[A](tree: Tree[A]): Int = {
		???
	}
	def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = {
		???
	}

	def fold[A,B](tree: Tree[A], z: B)(f: (A,B) => B): B = {
		???
	}
	
	def sizeViaFold[A](tree: Tree[A]): Int = {
		???
	}
	
	def maximumViaVold(tree: Tree[Int]): Tree[Int] = {
		???
	}
	
	def mapViaFold[A,B](tree: Tree[A])(f: A => B): Tree[B] = {
		???
	}
}