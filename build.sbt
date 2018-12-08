name := "ScalaPluginInternTestAssignment"
version := "1.0"
scalaVersion := "2.12.7"
val ScalatraVersion = "2.6.4"

resolvers += Classpaths.typesafeReleases

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"

libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.5" % "test" 

libraryDependencies ++= Seq(
  "org.scalatra" %% "scalatra" % ScalatraVersion,
  "org.scalatra" %% "scalatra-json" % ScalatraVersion,
  "org.scalatra" %% "scalatra-scalatest" % ScalatraVersion % "test",
  "org.json4s"   %% "json4s-jackson" % "3.5.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3" % "runtime",
  "org.eclipse.jetty" % "jetty-server" % "9.4.8.v20171121",
  "javax.servlet" % "javax.servlet-api" % "3.1.0" % "provided"
)

enablePlugins(SbtTwirl)
enablePlugins(ScalatraPlugin)
