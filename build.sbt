
name:="Community-Roster"

scalaVersion:="2.9.2"

version:="1.0-beta"

libraryDependencies ++= Seq(
	"org.scala-lang" % "scala-swing" % "2.9.2",
	"org.drools" % "drools-core" % "5.4.0.Final",
	"org.drools" % "drools-compiler" % "5.4.0.Final",
	"com.sun.xml.bind" % "jaxb-xjc" % "1.0.6",
	"org.scalatest" %% "scalatest" % "1.8" % "test")


resolvers += "JBoss Repository" at "http://repository.jboss.org/maven2/"
