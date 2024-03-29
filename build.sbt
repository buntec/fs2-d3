import laika.ast.Styles
import laika.helium.config.IconLink
import laika.helium.config.HeliumIcon
import laika.helium.Helium

Global / onChangedBuildSource := ReloadOnSourceChanges
Global / semanticdbEnabled := true // enable SemanticDB
Global / semanticdbVersion := scalafixSemanticdb.revision // only required for Scala 2.x

ThisBuild / tlBaseVersion := "0.1"

lazy val scala213 = "2.13.10"
lazy val scala3 = "3.3.0-RC1-bin-20221224-6f5bb34-NIGHTLY" // "3.2.1"

ThisBuild / scalaVersion := scala213
ThisBuild / crossScalaVersions := Seq(scala213, scala3)
ThisBuild / organization := "io.github.buntec"
ThisBuild / organizationName := "buntec"
ThisBuild / tlSonatypeUseLegacyHost := false

// publish website from this branch
ThisBuild / tlSitePublishBranch := Some("main")

ThisBuild / developers := List(
  tlGitHubDev("buntec", "Christoph Bunte")
)

ThisBuild / githubWorkflowBuildMatrixAdditions +=
  "browser" -> List("Chrome", "Firefox")
ThisBuild / githubWorkflowBuildSbtStepPreamble += s"set Global / useJSEnv := JSEnv.$${{ matrix.browser }}"

ThisBuild / githubWorkflowBuild +=
  WorkflowStep.Sbt(
    List("bundleMon"),
    name = Some("Monitor artifact size"),
    cond = Some("matrix.project == 'rootJS' && matrix.browser == 'Chrome'")
  )

ThisBuild / githubWorkflowAddedJobs +=
  WorkflowJob(
    id = "coverage",
    name = "Generate coverage report",
    scalas = List(scala213),
    javas = githubWorkflowJavaVersions.value.toList,
    steps = List(WorkflowStep.CheckoutFull)
      ++ WorkflowStep.SetupJava(githubWorkflowJavaVersions.value.toList)
      ++ githubWorkflowGeneratedCacheSteps.value
      ++ List(
        WorkflowStep.Use(
          UseRef.Public("actions", "setup-node", "v3"),
          params = Map("node-version" -> "16", "cache" -> "npm")
        ),
        WorkflowStep.Run(List("npm install")),
        WorkflowStep.Sbt(
          List(
            "set Global / useJSEnv := JSEnv.JSDOM",
            "coverage",
            "test",
            "coverageAggregate"
          )
        ),
        WorkflowStep.Run(List("bash <(curl -s https://codecov.io/bash)"))
      )
  )

lazy val useJSEnv = settingKey[JSEnv]("Browser for running Scala.js tests")
Global / useJSEnv := JSEnv.Chrome
ThisBuild / Test / jsEnv := {
  import org.openqa.selenium.chrome.ChromeOptions
  import org.openqa.selenium.firefox.FirefoxOptions
  import org.scalajs.jsenv.selenium.SeleniumJSEnv
  import org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv
  useJSEnv.value match {
    case JSEnv.Chrome =>
      val options = new ChromeOptions()
      options.setHeadless(true)
      new SeleniumJSEnv(options)
    case JSEnv.Firefox =>
      val options = new FirefoxOptions()
      options.setHeadless(true)
      new SeleniumJSEnv(options)
    case JSEnv.JSDOM =>
      new JSDOMNodeJSEnv()
  }
}

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"

lazy val catsVersion = "2.9.0"
lazy val catsEffectVersion = "3.4.2"
lazy val fs2Version = "3.4.0"
lazy val fs2DomVersion = "0.1.0"
lazy val scalajsDomVersion = "2.3.0"
lazy val domtypesVersion = "0.15.3"
lazy val kindProjectorVersion = "0.13.2"
lazy val betterMonadicForVersion = "0.3.1"
lazy val scalacheckVersion = "1.16.0"
lazy val munitVersion = "1.0.0-M7"

lazy val root = tlCrossRootProject.aggregate(d3, examples)

lazy val commonSettings = List(
  semanticdbEnabled := true, // enable SemanticDB
  semanticdbVersion := scalafixSemanticdb.revision // only required for Scala 2.x
)

lazy val d3 = (project
  .in(file("d3")))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    commonSettings,
    name := "fs2-d3",
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % scalajsDomVersion,
      "org.typelevel" %%% "cats-core" % catsVersion,
      "org.typelevel" %%% "cats-free" % catsVersion,
      "org.typelevel" %%% "cats-effect" % catsEffectVersion,
      "org.typelevel" %%% "cats-effect-kernel" % catsEffectVersion,
      "org.typelevel" %%% "cats-effect-std" % catsEffectVersion,
      "co.fs2" %%% "fs2-core" % fs2Version,
      "com.armanbilge" %%% "fs2-dom" % fs2DomVersion,
      "com.raquo" %%% "domtypes" % domtypesVersion,
      "org.scalameta" %%% "munit" % munitVersion % Test,
      "org.scalacheck" %%% "scalacheck" % scalacheckVersion % Test,
      "org.scala-js" %%% "scala-js-macrotask-executor" % "1.0.0" % Test
    )
  )

lazy val examples = (project in file("examples"))
  .enablePlugins(ScalaJSPlugin, NoPublishPlugin, BundleMonPlugin)
  .settings(
    commonSettings,
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % scalajsDomVersion
    )
  )
  .dependsOn(d3)

lazy val docs = project
  .in(file("site"))
  .enablePlugins(TypelevelSitePlugin)
  .settings(
    tlSiteApiPackage := Some("d3"),
    mdocJS := Some(d3),
    tlSiteRelatedProjects ++= Seq(
      TypelevelProject.CatsEffect,
      TypelevelProject.Fs2
    ),
    laikaConfig ~= { _.withRawContent },
    tlSiteHeliumConfig ~= {
      _.site
        .autoLinkJS() // Actually, this *disables* auto-linking, to avoid duplicates with mdoc
        .site
        .topNavigationBar(navLinks =
          Seq(
            IconLink.external(
              "https://github.com/buntec/fs2-d3",
              HeliumIcon.github,
              options = Styles("svg-link")
            )
          )
        )
    }
  )
