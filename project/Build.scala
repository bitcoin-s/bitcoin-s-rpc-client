import sbt._
import Keys._ 
object BitcoinSRPCClientBuild extends Build {

  val appName = "bitcoin-s-rpc-client"
  val appV = "0.0.1" 
  val scalaV = "2.11.4"
  val organization = "org.bitcoins"
  val slf4jV = "1.7.5"
  val akkaV = "10.0.5"
  val logbackV = "1.0.13"
  val appDependencies = Seq(
    "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test",
    "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",

    "com.typesafe.akka" %% "akka-http" % akkaV withSources() withJavadoc(),
    "com.typesafe.akka" %% "akka-http-spray-json" % akkaV withSources() withJavadoc(),

    "org.slf4j" % "slf4j-api" % slf4jV % "provided",
    "ch.qos.logback" % "logback-classic" % logbackV % "test"
  )
  
  val main = Project(appName, file(".")).enablePlugins().settings(
    version := appV,
    scalaVersion := scalaV,
    resolvers += Resolver.sonatypeRepo("releases"),  
    libraryDependencies ++= appDependencies,
    scalacOptions ++= Seq("-unchecked", "-deprecation")  
  )
} 

