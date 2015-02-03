package fpinscala.datastructures

import fpinscala.AbstractWordSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith


@RunWith(classOf[JUnitRunner])
class TreeSpec extends AbstractWordSpec {
	val singleton: Tree[Int] = Leaf(1)
	val branch: Tree[Int] = Branch(Leaf(1), Leaf(2))
	
	"size" should {
		"work" in {
			Tree.size(singleton) should be (1)
			Tree.size(branch) should be (3)
		}
	}
	"maximum" should {
		"work" in {
			???
		}
	}
	"depth" should {
		"work" in {
			???
		}
	}
	"map" should {
		"work" in {
			???
		}
	}
	"fold" should {
		"work" in {
			???
		}
	}
}