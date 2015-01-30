package fpinscala.datastructures

import fpinscala.AbstractWordSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith


@RunWith(classOf[JUnitRunner])
class ListSpec extends AbstractWordSpec {

	"pattern match" should {
		"be somethin" in {
			List.patternMatch should be (3)
		}
	}

	"tail" should {
		"throw an IAE for an empty list" in {
			val caught = evaluating {
				List.tail(Nil)
			} should produce [IllegalArgumentException]
		}
		"work for a non-empty list" in {
			List.tail(List(1,2)) should be (List(2))
		}
	}
	
	"head" should {
		"throw an IAE for an empty list" in {
			val caught = evaluating {
				List.head(Nil)
			} should produce [IllegalArgumentException]
		}
		"work for a non-empty list" in {
			List.head(List(1,2)) should be (1)
		}
	}
	
	"setHead" should {
		"create a new list for an empty list" in {
			List.setHead(Nil, 1) should be (List(1))
		}
		"work for a non-empty list" in {
			List.setHead(List(2,3), 1) should be (List(1,3))
		}
	}

	"drop" should {
		"return an empty list for an empty list" in {
			List.drop(Nil, 1) should be (Nil)
		}
		"be an identity for a list when n=0" in {
			List.drop(List(1,2), 0) should be (List(1,2))
		}
		"work for a non-empty list" in {
			List.drop(List(1, 2, 10, 20, 15), 2) should be (List(10, 20, 15))
		}
	}
	
	"dropWhile" should {
		"work" in {
			val l: List[Int] = List(1,2,3,4,5,6,7)
			List.dropWhile(l, { a: Int => (a < 4)}) should be (List(4,5,6,7))
		}
	}
	
	"init" should {
		"work" in {
			val l: List[Int] = List(1,2,3,4,5,6,7)
			List.init(l) should be (List(1,2,3,4,5,6))
		}
	}
	
	"length" should {
		"work" in {
			val l: List[Int] = List(1,2,3,4,5,6,7)
			List.length(l) should be (7)
		}
	}
	
	"foldLeft" should {
		"work as an implementation of length" in {
			val l: List[Int] = List(1,2,3,4,5,6,7)
			
			List.lengthLeft(l) should be(7)
		}
		"work as an implementation of sum" in {
			val l: List[Int] = List(1,2,3)
			
			List.sumLeft(l) should be(6)
		}
		"work as an implementation of product" in {
			val l: List[Double] = List(1,2,4)
			List.productLeft(l) should be(8.0)
		}
		"work as a reverse" in {
			val l: List[Int] = List(1,2,3)
			List.reverse(l) should be (List(3,2,1))
		}
	}
	
	"map" should {
		"work" in {
			val l: List[Int] = List(1,2,3)
			List.map(l){ 2 * _ } should be (List(2,4,6))
		}
	}
	
	// 
}

