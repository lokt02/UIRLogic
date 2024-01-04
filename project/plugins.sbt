addSbtPlugin("com.vmunier" % "sbt-web-scalajs" % "1.3.0")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.1.5")
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.9.16")
addSbtPlugin("org.playframework.twirl" % "sbt-twirl" % "2.0.2")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.14.0")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.2.0")
addSbtPlugin("io.spray" % "sbt-revolver" % "0.10.0")

libraryDependencies ++= Seq(
  "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.0",
  "org.scala-sbt" %% "scripted-plugin" % sbtVersion.value
)