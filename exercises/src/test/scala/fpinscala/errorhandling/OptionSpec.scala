package fpinscala.errorhandling

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import fpinscala.AbstractWordSpec
import util.Try

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
	"The variance function on Option object" should {
		"apply to nothing correctly" in {
			Option.variance(Nil) should be (None)
		}
		"apply to something correctly" in {
			val ds = List(2.0,2.0,2.0)
			Option.variance(ds) should be (Some(0.0))
		}
	}
	"The map2 function on Option object" should {
		"combine options correctly" in {
			Option.map2(maybeSomething, maybeNothing){ _ + _ } should be (None)
			Option.map2(maybeNothing, maybeSomething){ _ + _ } should be (None)
			Option.map2(maybeSomething, maybeSomething){ _ + _ } should be (Some(2))
		}
		"lift the parsing to calculate insurance rate nicely" in {
			// top secret formula for computing insurance premium from 2 key factors
			def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
				(age * numberOfSpeedingTickets).toDouble
			}
			
			
			def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
				val optAge = Option(Try(age.toInt))
				val optTickets = Option(Try(numberOfSpeedingTickets.toInt))
				Option.map2(optAge, optTickets)(insuranceRateQuote)
			}
		
			parseInsuranceRateQuote("10", "10") should be (Some(100.0))
			parseInsuranceRateQuote("0", "10") should be (Some(0))
			parseInsuranceRateQuote("A", "10") should be (None)
		}
	}
	
	"The sequence function on Option object" should {
		"work nicely" in {
			def parseInts(a: List[String]): Option[List[Int]] = {
				Option.sequence(a map { i => Option(Try(i.toInt)) } )
			}
			parseInts(List("1","2","3")) should be (Some(List(1,2,3)))
		}
	}
	"The traverse function on Option object" should {
		"work more efficiently than a sequence" in {
			def parseInts(a: List[String]): Option[List[Int]] = {
				Option.traverse(a){i => Option(Try(i.toInt))}
			}
			parseInts(List("1","2","3")) should be (Some(List(1,2,3)))
		}
	}
}