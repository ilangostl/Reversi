scalaVersion := "2.10.1"

libraryDependencies ++=
    "com.typesafe.akka" %% "akka-actor" % "2.1.0" ::
    "com.typesafe.akka" %% "akka-slf4j" % "2.1.0" ::
    Nil

libraryDependencies ++=
    "org.scalatest" %% "scalatest" % "2.0.M5b" % "test" ::
    "com.typesafe.akka" %% "akka-testkit" % "2.1.0" % "test" ::
    Nil
