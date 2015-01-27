import sbt._
import Keys._

object Resolvers {
  lazy val sunRepo    = "Sun Maven2 Repo" at "http://download.java.net/maven/2"
  lazy val sunRepoGF  = "Sun GF Maven2 Repo" at "http://download.java.net/maven/glassfish"
  lazy val oracleRepo = "Oracle Maven2 Repo" at "http://download.oracle.com/maven"
  lazy val oracleResolvers = Seq (sunRepo, sunRepoGF, oracleRepo)
  
  lazy val sprayIoReleases       = "Spray IO Release Repo" at "http://repo.spray.io"
  lazy val typesafeResolvers     = Seq(sprayIoReleases) ++ Seq("snapshots", "releases").map(Resolver.typesafeRepo) ++Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

  lazy val springReleaseRepo           = "EBR Spring Release Repository" at "http://repository.springsource.com/maven/bundles/release"
  lazy val springExternalReleaseRepo   = "EBR Spring External Release Repository" at "http://repository.springsource.com/maven/bundles/external"
  lazy val springMilestoneRepo         = "Spring Milestone Repository" at "https://repo.springsource.org/libs-milestone"
  lazy val springAppResolvers = Seq(springReleaseRepo, springExternalReleaseRepo)
  
  lazy val jBossRepo = "JBoss Public Maven Repository Group" at "https://repository.jboss.org/nexus/content/groups/public-jboss/"

  lazy val coreResolvers = typesafeResolvers ++ oracleResolvers
}

object Versions {
  lazy val scalaVer       = "2.10.3"
  
  lazy val scalaTestVer   = "2.1.0"
  lazy val junitVer       = "4.11"

  lazy val scalazVer      = "7.1.0"
}


object Dependencies {
  import Versions._
  
  val provided = "provided"
  val test     = "test"
  val runtime  = "runtime"

  lazy val junit            = "junit"           %  "junit"         % junitVer     % test
  lazy val scalaTest        = "org.scalatest"   %% "scalatest"     % scalaTestVer % test
  
  lazy val testDependencies = Seq(junit, scalaTest)

  lazy val scalazFull       = "org.scalaz"      %% "scalaz-full"   % scalazVer

  lazy val coreDependencies   = testDependencies
}


object FPInScalaBuild extends Build {
  import Resolvers._
  import Dependencies._
  import Versions._
  
  val opts = Project.defaultSettings ++ Seq(
    scalaVersion := scalaVer,
    resolvers ++= coreResolvers,
    libraryDependencies ++= coreDependencies
  )

  lazy val root =
    Project(id = "fpinscala",
            base = file("."),
            settings = opts ++ Seq(
              onLoadMessage ~= (_ + nio2check())
            )) aggregate (chapterCode, exercises, answers)
  lazy val chapterCode =
    Project(id = "chapter-code",
            base = file("chaptercode"),
            settings = opts)
  lazy val exercises =
    Project(id = "exercises",
            base = file("exercises"),
            settings = opts)
  lazy val answers =
    Project(id = "answers",
            base = file("answers"),
            settings = opts)

  def nio2check(): String = {
    val cls = "java.nio.channels.AsynchronousFileChannel"
    try {Class.forName(cls); ""}
    catch {case _: ClassNotFoundException =>
      ("\nWARNING: JSR-203 \"NIO.2\" (" + cls + ") not found.\n" +
       "You are probably running Java < 1.7; answers will not compile.\n" +
       "You seem to be running " + System.getProperty("java.version") + ".\n" +
       "Try `project exercises' before compile, or upgrading your JDK.")
    }
  }
}
