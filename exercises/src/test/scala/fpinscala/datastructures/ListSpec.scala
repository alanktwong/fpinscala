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
		"fail" in {
			(1 > 0) should be (false)
		}
	}
	
	"init" should {
		"fail" in {
			(1 > 0) should be (false)
		}
	}
	
	"length" should {
		"fail" in {
			(1 > 0) should be (false)
		}
	}
	
	"foldLeft" should {
		"fail" in {
			(1 > 0) should be (false)
		}
	}
	
	
	"map" should {
		"fail" in {
			(1 > 0) should be (false)
		}
	}
}

