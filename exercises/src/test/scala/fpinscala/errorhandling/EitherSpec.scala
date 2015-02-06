package fpinscala.errorhandling

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import fpinscala.AbstractWordSpec


@RunWith(classOf[JUnitRunner])
class EitherSpec extends AbstractWordSpec {
	"The Either trait" should {
		"map left correctly" in {
			val e = Left(1)
			e.map{a => a} should be (e)
		}
		"map right correctly" in {
			val e = Right(2)
			e.map{a => a} should be (e)
		}
	}
}