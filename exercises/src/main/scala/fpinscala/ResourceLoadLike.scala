package fpinscala

import java.io.{File => JFile}

import scala.io.Source
import scala.io.BufferedSource

trait ResourceLoadLike {

	def resourceAsStreamFromSrc(filename: String): Option[Source] = {
		resourceAsStreamFromSrc(filename :: Nil)
	}
	
	def resourceAsStreamFromSrc(resourcePath: List[String]): Option[Source] = {
		ResourceLoader.resourceAsFileFromSrc(resourcePath) match {
			case Some(file) =>
				Some(Source.fromFile(file, "UTF-8"))
			case None =>
				None
		}
	}
	
	def resourceAsString(filename: String): Option[String] = {
		resourceAsString(filename :: Nil)
	}
	
	def resourceAsString(resourcePath: List[String]): Option[String] = {
		resourceAsStreamFromSrc(resourcePath) match {
			case Some(source) =>
				Some(source.mkString)
			case None =>
				None
		}
	}
}

private object ResourceLoader {
	/**
	 * Get a child of a file. For example,
	 * 
	 *   subFile(homeDir, "b", "c")
	 * 
	 * corresponds to ~/b/c
	 */
	private def subFile(file: JFile, children: String*): JFile = {
		children.foldLeft(file)((file, child) => new JFile(file, child))
	}
	
	private def directoriesOfOtherModules(parentDir: JFile, currentModule: JFile): List[JFile] = {
		import collection.immutable.ListSet
		val uris = ListSet("exercises", "chapter-code")
		val files = uris map { new JFile(parentDir, _) }
		val otherModules = files - currentModule
		otherModules.toList
	}
	
	private def getResourceFiles(moduleDir: JFile, resourcePath: List[String]): List[JFile] = {
		def getResourceFileImpl(moduleDir: JFile, dirs: List[String], resourcePath: List[String]): JFile = {
			subFile(moduleDir, (dirs ::: resourcePath): _*)
		}
		val resources = List("src"::"main"::"resources"::Nil, "src"::"test"::"resources"::Nil)
		
		val resourceFiles = for (r <- resources) yield getResourceFileImpl(moduleDir, r, resourcePath)
		resourceFiles
	}
	
	
	/**
	 * Get a resource from the `src/main/resources` directory. Eclipse does not copy
	 * resources to the output directory, then the class loader cannot find them.
	 */
	def resourceAsFileFromSrc(resourcePath: List[String]): Option[JFile] = {
		val uri = getClass.getResource(".").toURI

		val classesDir = new JFile(uri)
		val moduleDir = classesDir.getParentFile.getParentFile.getParentFile.getParentFile

		val parentDir = moduleDir.getParentFile

		val resourceFiles = getResourceFiles(moduleDir, resourcePath)
		resourceFiles.find( _.exists ) match {
			case Some(f) => Some(f)
			case None =>
				val otherModules = directoriesOfOtherModules(parentDir, moduleDir)
				val listOfOtherResouceFiles = for (m <- otherModules) yield getResourceFiles(m, resourcePath)
				val allFiles = for (resourceFiles <- listOfOtherResouceFiles; file <- resourceFiles if file.exists) yield file
				allFiles.headOption
		}
	}
}