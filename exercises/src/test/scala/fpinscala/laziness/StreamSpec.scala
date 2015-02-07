package fpinscala.laziness

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import fpinscala.AbstractWordSpec

@RunWith(classOf[JUnitRunner])
class StreamSpec extends AbstractWordSpec {
	"Streams" should {
		"be lazy" in {
			val s = Stream(List(1,2,3))
			s.foldRight(0){ (a,b) =>
				a.head + b
			} should be(1)
		}
	}
}