import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.cross.CrossProject
import sbt.Keys._
import sbt._
import sbtrelease.ReleasePlugin._

object Build extends sbt.Build {
  import ScalaJSPlugin.{autoImport ⇒ sjs}, sjs.toScalaJSGroupID

  object versions {
    def snap(s: String) = s + "-SNAPSHOT"
    val scalaJsReact = "0.10.1"
    val scalaCss     = "0.3.1"
    val components   = snap("0.2.0")
    val unfiltered   = "0.8.4"
    val slick        = "3.0.2"
  }

  val sharedTestDeps = Def.setting(
    "org.scalatest" %%% "scalatest"     % "3.0.0-M7" % Test
  )

  val sharedDeps = Def.setting(Seq(
    "com.lihaoyi"   %%% "upickle"       % "0.3.6",
    "com.lihaoyi"   %%% "autowire"      % "0.2.5"
  ))

  override val settings = super.settings ++ Seq(
    organization         := "com.olvind",
    scalaVersion         := "2.11.7",
    licenses             += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
    scalacOptions       ++= Seq("-encoding", "UTF-8", "-feature", "-language:existentials", "-language:higherKinds", "-language:implicitConversions", "-unchecked", "-Xlint", "-Yno-adapted-args", "-Ywarn-dead-code", "-Ywarn-numeric-widen", "-Ywarn-value-discard", "-Xfuture", "-deprecation") //"-Xlog-implicits"
  )

  lazy val buildSettings = Defaults.coreDefaultSettings ++ releaseSettings ++ Seq(
    updateOptions       := updateOptions.value.withCachedResolution(cachedResoluton = true)
  )

  val crudShared = CrossProject("crud", file("crud"), sjs.CrossType.Full)
    .settings(libraryDependencies ++= sharedDeps.value :+ sharedTestDeps.value)

  val crudJs = crudShared.js.settings(
    libraryDependencies ++= Seq(
      "com.github.japgolly.scalajs-react"              %%% "ext-scalaz71"  % versions.scalaJsReact changing(),
      "com.github.japgolly.scalajs-react"              %%% "extra"         % versions.scalaJsReact changing(),
      "com.github.japgolly.scalacss"                   %%% "core"          % versions.scalaCss,
      "com.github.japgolly.scalacss"                   %%% "ext-react"     % versions.scalaCss,
      "com.github.chandu0101.scalajs-react-components" %%% "core"          % versions.components changing()
    ),
    sjs.emitSourceMaps := true,
    /* create javascript launcher. Searches for an object extends JSApp */
    sjs.persistLauncher := true,
    skip in sjs.packageJSDependencies := true,
    /* same name for generated js file for fastOpt as for fullOpt */
    artifactPath in (Compile, sjs.fastOptJS) :=
      ((crossTarget in (Compile, sjs.fastOptJS)).value / ((moduleName in sjs.fastOptJS).value + "-opt.js")),

    publish := {},
    publishLocal := {}
  )

  /* make `package` depend on fullOptJS, and copy resulting artifacts into crudJVM */
  def copyJsResources(toDirS: SettingKey[File]) = (sjs.fastOptJS in (crudJs, Compile), crossTarget in crudJs, toDirS).map{
    case (_, fromDir, toDir) ⇒
      val files: Array[(File, File)] = IO listFiles fromDir collect {
        case f if f.name endsWith ".js" ⇒ // ← also match «foo.js.map»
          (f, toDir / "classes" / f.name)
      }
      IO.copy(files, overwrite = true)
  }

  lazy val crudJvm: Project = crudShared.jvm.settings(
    testFrameworks            += new TestFramework("com.novocode.junit.JUnitFramework"),
    testOptions               += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a"),
    libraryDependencies      ++= Seq(
      "com.typesafe.scala-logging"  %% "scala-logging"     % "3.1.0",
      "org.scala-lang"               % "scala-reflect"     % scalaVersion.value,
      "org.scalaz"                  %% "scalaz-core"       % "7.1.3",
      "com.typesafe.slick"          %% "slick"             % versions.slick,
      "net.databinder"              %% "unfiltered-filter" % versions.unfiltered,

      "javax.servlet"                % "javax.servlet-api" % "3.1.0"        % Provided,

      "com.typesafe.slick"          %% "slick-testkit"     % versions.slick % Test,
      "com.novocode"                 % "junit-interface"   % "0.10"         % Test,
      "org.slf4j"                    % "slf4j-simple"      % "1.7.7"        % Test,
      "com.h2database"               % "h2"                % "1.4.186"      % Test
    ),
    (packageBin in Compile) <<= (packageBin in Compile) dependsOn copyJsResources(crossTarget)
  )

}
