package fpinscala.errorhandling

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import fpinscala.AbstractWordSpec


@RunWith(classOf[JUnitRunner])
class EitherSpec extends AbstractWordSpec {
	"The map function on the Either trait" should {
		"apply to left" in {
			val e = Left(1)
			e.map{a => a} should be (e)
		}
		"apply to right" in {
			val e = Right(2)
			e.map{a => a} should be (e)
		}
	}
	"The flatMap function on the Either trait" should {
		"apply to left" in {
			val e = Left(1)
			e.flatMap{a => Right(a)} should be (e)
		}
		"apply to right" in {
			val e = Right(2)
			e.flatMap{a => Right(a)} should be (e)
		}
	}
	"The orElse function on the Either trait" should {
		"apply to left" in {
			val e = Left(1)
			e.orElse(Right(-1)) should be (Right(-1))
		}
		"apply to right" in {
			val e = Right(2)
			e.orElse(Right(-1)) should be (e)
		}
	}
	"The map2 function on the Either trait" should {
		"apply to left" in {
			val e1: Either[String,Int] = Left("String")
			val e2: Either[String,Int] = Right(-1)
			e1.map2(e2){ _ + _ } should be (e1)
		}
		"apply to right" in {
			val e1: Either[String,Int] = Right(2)
			val e2: Either[String,Int] = Right(1)
			e1.map2(e2){ _ + _ } should be (Right(3))
		}
	}
	"The sequence function on the Either object" should {
		"apply to left" in {
			val e1: Either[String,Int] = Left("String")
			val e2: Either[String,Int] = Right(-1)
			
			Either.sequence(List(e2,e1)) should be (e1)
		}
		"apply to right" in {
			val e1: Either[String,Int] = Right(2)
			val e2: Either[String,Int] = Right(1)
			
			Either.sequence(List(e2,e1)) should be (Right(List(1,2)))
		}
	}
	
	def parseInt(str: String): Either[Exception,Int] = {
		try {
			Right(str.toInt)
		} catch {
			case e: Exception => Left(e)
		}
	}
	
	"The traverse function on the Either object" should {
		"apply to left" in {
			val result = Either.traverse(List("1","d"))(parseInt)
			result match {
				case Left(e) => e.getClass should be ( (new java.lang.NumberFormatException).getClass )
				case _ => fail
			}
		}
		"apply to right" in {
			Either.traverse(List("1","2"))(parseInt) should be (Right(List(1,2)))
		}
	}
}