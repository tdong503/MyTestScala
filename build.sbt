val Http4sVersion = "1.0.0-M40"
val CirceVersion = "0.14.5"

lazy val root = (project in file("."))
  .settings(
    organization := "com.example",
    name := "firstforhttp4",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "3.3.0",
    libraryDependencies ++= Seq(
      "org.http4s"      %% "http4s-ember-server" % Http4sVersion,
      "org.http4s"      %% "http4s-ember-client" % Http4sVersion,
      "org.http4s"      %% "http4s-circe"        % Http4sVersion,
      "org.http4s"      %% "http4s-dsl"          % Http4sVersion,
      "io.circe"        %% "circe-generic"       % CirceVersion,
    )
  )
