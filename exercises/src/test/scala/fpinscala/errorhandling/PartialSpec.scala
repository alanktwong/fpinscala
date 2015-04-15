package fpinscala.errorhandling

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import fpinscala.AbstractWordSpec


@RunWith(classOf[JUnitRunner])
class PartialSpec extends AbstractWordSpec {
	"The map function on the Partial trait" should {
		"apply to left" in {
			val e = Left(1)
			e.map{a => a} should be (e)
		}
		"apply to right" in {
			val e = Right(2)
			e.map{a => a} should be (e)
		}
	}

}