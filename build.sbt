val Http4sVersion = "0.21.3"
val CirceVersion = "0.13.0"
val Specs2Version = "4.9.3"
val LogbackVersion = "1.2.3"
val catsRetryVersion = "1.1.0"
val fs2Version = "2.2.2"
val loggingVersion = "3.9.2"
val redis4catsVersion = "0.10.0"
val upickleVersion = "0.9.5"
val fs2MongoVersion = "0.5.0"
val mongoScalaVersion = "4.0.5"

lazy val root = (project in file("."))
  .settings(
    organization := "com.iscs",
    name := "covidbystates",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.13.1",
    libraryDependencies ++= Seq(
      "org.http4s"       %% "http4s-blaze-server" % Http4sVersion,
      "org.http4s"       %% "http4s-blaze-client" % Http4sVersion,
      "org.http4s"       %% "http4s-circe"        % Http4sVersion,
      "org.http4s"       %% "http4s-dsl"          % Http4sVersion,
      "org.http4s"       %% "http4s-async-http-client" % Http4sVersion,
      "io.circe"         %% "circe-generic"       % CirceVersion,
      "io.circe"         %% "circe-parser"        % CirceVersion,
      "dev.profunktor"   %% "redis4cats-streams"  % redis4catsVersion,
      "dev.profunktor"   %% "redis4cats-effects"  % redis4catsVersion,
      "dev.profunktor"   %% "redis4cats-log4cats" % redis4catsVersion,
      "org.mongodb.scala" %% "mongo-scala-driver" % mongoScalaVersion,
      "org.specs2"       %% "specs2-core"         % Specs2Version % "test",
      "ch.qos.logback"   %  "logback-classic"     % LogbackVersion,
      "com.github.cb372" %% "cats-retry"          % catsRetryVersion,
      "co.fs2"           %% "fs2-core"            % fs2Version,
      "co.fs2"           %% "fs2-io"              % fs2Version,
      "co.fs2"           %% "fs2-reactive-streams" % fs2Version,
      "com.typesafe.scala-logging" %% "scala-logging" % loggingVersion
    ),
    addCompilerPlugin("org.typelevel" %% "kind-projector"     % "0.10.3"),
    addCompilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1"),
    Revolver.enableDebugging(5050, true)
  )

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-language:higherKinds",
  "-language:postfixOps",
  "-feature",
  "-Xfatal-warnings",
)

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}
