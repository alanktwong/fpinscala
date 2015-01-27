package object fpinscala {
	/**
	 *  An alias for the `Nothing` type.
	 *  Denotes that the type should be filled in.
	 */
	type ??? = Nothing

	/**
	 * An alias for the `Any` type.
	 * Denotes that the type should be filled in.
	 */
	type *** = Any

	import java.io.{File => JFile}
	
	import scala.io.Source
	import scala.io.BufferedSource
	
	object Defaults {
		lazy val defaultEncoding = "UTF-8"
	}
}