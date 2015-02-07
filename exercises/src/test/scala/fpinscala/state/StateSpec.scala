package fpinscala.state

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import fpinscala.AbstractWordSpec

import fpinscala.state.RNG.Simple


@RunWith(classOf[JUnitRunner])
class StateSpec extends AbstractWordSpec {

	"an empty test" should {
		"fail" in {
			(true) should be (false)
		}
	}
}