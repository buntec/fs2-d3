addSbtPlugin("org.typelevel" % "sbt-typelevel" % "0.4.17")
addSbtPlugin("org.typelevel" % "sbt-typelevel-site" % "0.4.17")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.12.0")
libraryDependencies += "org.scala-js" %% "scalajs-env-selenium" % "1.1.1"
libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.1.0"

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "2.0.5")
addSbtPlugin("com.armanbilge" % "sbt-bundlemon" % "0.1.3")
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.3.6")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.10.4")
