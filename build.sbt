ThisBuild / tlBaseVersion := "0.1"

val scala213 = "2.13.8"
ThisBuild / scalaVersion := scala213
ThisBuild / crossScalaVersions := Seq(scala213, "3.1.2")
ThisBuild / organization := "io.github.buntec"
ThisBuild / organizationName := "buntec"
ThisBuild / tlSonatypeUseLegacyHost := false

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

lazy val scalajsDomVersion = "2.1.0"
lazy val scalacheckVersion = "1.16.0"
lazy val munitVersion = "1.0.0-M5"

lazy val root = tlCrossRootProject.aggregate(d3)

lazy val d3 = (project
  .in(file("d3")))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "scala-js-d3",
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % scalajsDomVersion,
      "org.scalameta" %%% "munit" % munitVersion % Test,
      "org.scalacheck" %%% "scalacheck" % scalacheckVersion % Test,
      "org.scala-js" %%% "scala-js-macrotask-executor" % "1.0.0" % Test
    )
  )
