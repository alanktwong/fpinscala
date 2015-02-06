package fpinscala.datastructures

import fpinscala.AbstractWordSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith


@RunWith(classOf[JUnitRunner])
class TreeSpec extends AbstractWordSpec {
	val singleton: Tree[Int] = Leaf(1)
	val branch2: Tree[Int] = Branch(Leaf(1), Leaf(2))
	val branch4: Tree[Int] = Branch(branch2, Branch(Leaf(3), Leaf(4)))
	def identity[A]: A => A = a => a
	
	"size" should {
		"work on a singleton" in {
			Tree.size(singleton) should be (1)
		}
		"work on a 2-leaf tree"  in {
			Tree.size(branch2) should be (3)
		} 
		"work on a 4-leaf tree"  in {
			Tree.size(branch4) should be (7)
		}
	}
	
	"maximum" should {
		"work on a singleton" in {
			Tree.maximum(singleton) should be (1)
		}
		"work on a 2-leaf tree" in {
			Tree.maximum(branch2) should be (2)
		}
		"work on a 4-leaf tree" in {
			Tree.maximum(branch4) should be (4)
		}
	}
	
	"depth" should {
		"work on a singleton" in {
			Tree.depth(singleton) should be (0)
		}
		"work on a 2-leaf tree" in {
			Tree.depth(branch2) should be (2)
		}
		"work on a 4-leaf tree" in {
			Tree.depth(branch4) should be (4)
		}
	}
	
	"map" should {
		"work on a singleton/2-leaf/4-leaf trees with the identity function" in {
			Tree.map(singleton)(identity) should be (singleton)
			Tree.map(branch2)(identity) should be (branch2)
			Tree.map(branch4)(identity) should be (branch4)
		}
		"work on a singleton/2-leaf/4-leaf trees with the doubling function" in {
			Tree.map(singleton)(2 * _) should be (Leaf(2))
			val branch24 = Branch(Leaf(2), Leaf(4))
			Tree.map(branch2)(2 * _) should be (branch24)
			val branch68 = Branch(Leaf(6), Leaf(8))
			Tree.map(branch4)(2 * _) should be (Branch(branch24, branch68))
		}
	}
	
	"fold" should {
		"work just like size" in {
			Tree.sizeViaFold(branch4) should be (Tree.size(branch4))
		}
		"work just like maximum" in {
			Tree.maximumViaFold(branch4) should be (Tree.maximum(branch4))
		}
		"work on a just like map" in {
			Tree.mapViaFold(branch4)(identity) should be (Tree.map(branch4)(identity))
		}
	}
}