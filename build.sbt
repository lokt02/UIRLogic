ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = project.in(file(".")).
  aggregate(client, server).
  settings(
    name := "stusys-logic-props",
    publish := {},
    publishLocal := {},
  )

lazy val app = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("app/shared"))
  .settings(
    name := "shared",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-parse" % "0.3.9",
      "com.lihaoyi" %%% "upickle" % "3.1.3",
      "com.lihaoyi" %%% "scalatags" % "0.12.0",
      "org.scalacheck" %%% "scalacheck" % "1.17.0",
      "org.specs2" %%% "specs2-scalacheck" % "4.19.2" % Test,
    ),
  ).
  jvmSettings().
  jsSettings()
  .jsConfigure(_.enablePlugins(ScalaJSWeb))

lazy val server = (project in file("app/jvm")).settings(
    name := "stusys-server",
    scalaJSProjects := Seq(client),
    Assets / pipelineStages := Seq(scalaJSPipeline),
    Compile / compile := ((Compile / compile) dependsOn scalaJSPipeline).value,
    resolvers += "Akka library repository".at("https://repo.akka.io/maven"),
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-http" % "10.5.3",
      "com.typesafe.akka" %% "akka-actor-typed" % "2.8.5",
      "com.typesafe.akka" %% "akka-stream" % "2.8.5",
      "ch.megard" %% "akka-http-cors" % "1.2.0",
      "org.slf4j" % "slf4j-api" % "2.0.9",
      "org.slf4j" % "slf4j-simple" % "2.0.9",
      "org.webjars" % "bootstrap" % "5.2.3",
      "com.vmunier" %% "scalajs-scripts" % "1.3.0",
    ),
    Assets / WebKeys.packagePrefix := "public/",
    Runtime / unmanagedClasspath += (Assets / packageBin).value
).enablePlugins(SbtWeb, SbtTwirl, JavaAppPackaging)
  .dependsOn(app.jvm)

lazy val client = (project in file("app/js")).settings(
    name := "stusys-client",
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.4.0",
      "com.lihaoyi" %%% "utest" % "0.7.4" % Test
    ),
    jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv(),
    testFrameworks += new TestFramework("utest.runner.Framework")
  )
  .enablePlugins(ScalaJSPlugin, ScalaJSWeb)
  .dependsOn(app.js)

