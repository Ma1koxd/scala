scalaVersion := "3.9.0"

lazy val root = rootProject
  .settings(
    name := "untitled",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "upickle" % "4.4.3"
    )
  )