package fpinscala.gettingstarted

import fpinscala.AbstractFlatSpec
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith


@RunWith(classOf[JUnitRunner])
class GettingStartedSpec extends AbstractFlatSpec {
	"factorial" should "work" in {
		MyModule.factorial(0) should be (1)
		MyModule.factorial(1) should be (1)
		MyModule.factorial(2) should be (2)
		MyModule.factorial(3) should be (6)
	}
	
	"fibonacci" should "work" in {
		MyModule.fib(1) should be (1)
		MyModule.fib(2) should be (1)
		MyModule.fib(3) should be (2)
		MyModule.fib(4) should be (3)
		MyModule.fib(5) should be (5)
		MyModule.fib(6) should be (8)
	}
	
	"isSorted" should "work" in {
		val intCompare =  (a:Int,b: Int) => a > b
		
		PolymorphicFunctions.isSorted(Array(), intCompare) should be (true)
		PolymorphicFunctions.isSorted(Array(1), intCompare) should be (true)
		
		PolymorphicFunctions.isSorted(Array(1,2,3), intCompare) should be (true)
		PolymorphicFunctions.isSorted(Array(1,12,30), intCompare) should be (true)
		PolymorphicFunctions.isSorted(Array(100,12,30), intCompare) should be (false)
	}
	
}