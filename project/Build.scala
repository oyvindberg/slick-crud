import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.cross.CrossProject
import sbt.Keys._
import sbt._
import sbtrelease.ReleasePlugin._
import wartremover._

object Build extends sbt.Build {
  import ScalaJSPlugin.{autoImport ⇒ sjs}, sjs.toScalaJSGroupID

  object versions {
    def snap(s: String) = s + "-SNAPSHOT"
    val scalaJsReact = "0.11.2"
    val scalaCss     = "0.5.0"
    val components   = "0.5.0"
    val unfiltered   = "0.8.4"
    val slick        = "3.0.2"
    val diode        = "1.0.0"
  }

  val sharedTestDeps: Def.Initialize[ModuleID] =
    Def.setting(
      "org.scalatest" %%% "scalatest"     % "3.0.0-M15" % Test
    )

  val sharedDeps: Def.Initialize[Seq[ModuleID]] =
    Def.setting(Seq(
      "com.lihaoyi"   %%% "upickle"       % "0.3.7",
      "com.lihaoyi"   %%% "autowire"      % "0.2.5",
      "com.olvind"    %%% "stringifiers"  % "0.2"
    ))

  override val settings: Seq[Def.Setting[_]] =
    super.settings ++ Seq(
      organization         := "com.olvind",
      scalaVersion         := "2.11.8",
      licenses             += ("Apache-2.0", url("http://opensource.org/licenses/Apache-2.0")),
      scalacOptions       ++= Seq("-encoding", "UTF-8", "-feature", "-language:existentials", "-language:higherKinds", "-language:implicitConversions", "-unchecked", "-Xlint", "-Yno-adapted-args", "-Ywarn-dead-code", "-Ywarn-numeric-widen", "-Ywarn-value-discard", "-Xfuture", "-deprecation") //"-Xlog-implicits"
    )

  lazy val buildSettings: Seq[Def.Setting[_]] =
    Defaults.coreDefaultSettings ++ releaseSettings ++ Seq(
      updateOptions       := updateOptions.value.withCachedResolution(cachedResoluton = true)
    )

  val crudShared: CrossProject =
    CrossProject("crud", file("crud"), sjs.CrossType.Full)
      .settings(
        libraryDependencies ++= sharedDeps.value :+ sharedTestDeps.value,
        (wartremoverWarnings in Compile) ++= Warts.allBut(Wart.ExplicitImplicitTypes, Wart.Nothing, Wart.Any, Wart.Null)
      )

  val crudJs : Project = crudShared.js.settings(
    libraryDependencies ++= Seq(
      "com.github.japgolly.scalajs-react"              %%% "extra"       % versions.scalaJsReact,
      "com.github.japgolly.scalacss"                   %%% "extreact"    % versions.scalaCss,
      ("com.github.chandu0101.scalajs-react-components" %%% "core"        % versions.components).excludeAll(ExclusionRule(organization = "com.github.japgolly.scalacss")),
      "me.chrons"                                      %%% "diode-react" % versions.diode
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
  def copyJsResources(toDirS: SettingKey[File]): Def.Initialize[Task[Set[File]]] =
    (sjs.fastOptJS in (crudJs, Compile), crossTarget in crudJs, toDirS).map{
      case (_, fromDir, toDir) ⇒
        val files: Array[(File, File)] = IO listFiles fromDir collect {
          case f if f.name contains ".js" ⇒ // ← also match «foo.js.map»
            (f, toDir / "classes" / f.name)
        }
        IO.copy(files, overwrite = true)
    }

  lazy val crudJvm: Project =
    crudShared.jvm.settings(
      testFrameworks            += new TestFramework("com.novocode.junit.JUnitFramework"),
      testOptions               += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a"),
      libraryDependencies      ++= Seq(
        "com.typesafe.scala-logging"  %% "scala-logging"     % "3.1.0",
        "org.scala-lang"               % "scala-reflect"     % scalaVersion.value,
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

  lazy val demo: Project =
    (project in file("demo"))
      .settings(
        name                 := "slick-crud-demo",
        libraryDependencies ++= Seq(
          "net.databinder" %% "unfiltered-jetty" % "0.8.4",
          "org.slf4j"       % "slf4j-simple"     % "1.7.7",
          "com.h2database"  % "h2"               % "1.4.186"
        ),
        (run in Compile) <<= (run in Compile) dependsOn copyJsResources(crossTarget)
      ).dependsOn(crudJvm)
}
