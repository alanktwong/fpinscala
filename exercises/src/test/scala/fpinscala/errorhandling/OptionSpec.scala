package fpinscala.errorhandling

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import fpinscala.AbstractWordSpec


@RunWith(classOf[JUnitRunner])
class OptionSpec extends AbstractWordSpec {
	val maybeSomething: Option[Int] = Some(1)
	val maybeNothing: Option[Int] = None
	
	"The map function on Option trait" should {
		"apply to something correctly" in {
			maybeSomething.map{_ + 1} should be (Some(2))
		}
		"apply to nothing correctly" in {
			maybeNothing.map{_ + 1} should be (None)
		}
	}
	"The getOrElse function on Option trait" should {
		"apply to something correctly" in {
			maybeSomething.getOrElse(-1) should be (1)
		}
		"apply to nothing correctly" in {
			maybeNothing.getOrElse(-1) should be (-1)
		}
	}
	"The flatMap function on Option trait" should {
		"apply to something correctly" in {
			maybeSomething.flatMap{ a => Some(a + 1) } should be (Some(2))
		}
		"apply to nothing correctly" in {
			maybeNothing.flatMap{ a => Some(a + 1) } should be (None)
		}
		"apply to something (via a pattern match) correctly" in {
			maybeSomething.flatMapViaPatternMatch{ a => Some(a + 1) } should be (Some(2))
		}
		"apply to nothing (via a pattern match) correctly" in {
			maybeNothing.flatMapViaPatternMatch{ a => Some(a + 1) } should be (None)
		}
	}
	"The orElse function on Option trait" should {
		"apply to something correctly" in {
			maybeSomething.orElse( Some(2) ) should be (Some(1))
		}
		"apply to nothing correctly" in {
			maybeNothing.orElse( Some(2) ) should be (Some(2))
		}
		"apply to something (via a pattern match) correctly" in {
			maybeSomething.orElseViaPatternMatch( Some(2) ) should be (Some(1))
		}
		"apply to nothing (via a pattern match) correctly" in {
			maybeNothing.orElseViaPatternMatch( Some(2) ) should be ( Some(2) )
		}
	}
	"The filter function on Option trait" should {
		"apply to something correctly" in {
			maybeSomething.filter( _ > 0 ) should be (maybeSomething)
		}
		"apply to nothing correctly" in {
			maybeNothing.filter( _ > 0 ) should be (maybeNothing)
		}
		"apply to something (via a pattern match) correctly" in {
			maybeSomething.filterViaFlatMap( _ > 0) should be (maybeSomething)
		}
		"apply to nothing (via a pattern match) correctly" in {
			maybeNothing.filterViaFlatMap(_ > 0) should be (maybeNothing)
		}
	}
	"The mean function on Option object" should {
		"apply to nothing correctly" in {
			Option.mean(Nil) should be (None)
		}
		"apply to something correctly" in {
			val ds = List(1.0,2.0,3.0)
			Option.mean(ds) should be (Some(2.0))
		}
	}
}