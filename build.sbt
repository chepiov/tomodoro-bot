name := "tomodoro"
version := "0.1"
scalaVersion := "2.12.8"

enablePlugins(JavaAppPackaging)

wartremoverErrors ++= Warts.unsafe
wartremoverExcluded +=
  baseDirectory.value / "src" / "main" / "scala" / "org" / "chepiov" / "tomodoro" / "actors"
wartremoverExcluded += sourceManaged.value / "main" / "org" / "chepiov" / "tomodoro" / "actors"
wartremoverErrors -= Wart.DefaultArguments

PB.targets in Compile := Seq(
  scalapb.gen() -> (sourceManaged in Compile).value
)

libraryDependencies += "com.github.mpilquist"      %% "simulacrum"                   % "0.15.0"
libraryDependencies += "org.typelevel"             %% "cats-effect"                  % "1.2.0"
libraryDependencies += "com.typesafe.akka"         %% "akka-stream"                  % "2.5.22"
libraryDependencies += "com.typesafe.akka"         %% "akka-http"                    % "10.1.8"
libraryDependencies += "com.typesafe.akka"         %% "akka-http-spray-json"         % "10.1.8"
libraryDependencies += "com.typesafe.akka"         %% "akka-persistence"             % "2.5.22"
libraryDependencies += "com.typesafe.akka"         %% "akka-slf4j"                   % "2.5.22"
libraryDependencies += "com.typesafe.akka"         %% "akka-persistence-query"       % "2.5.22"
libraryDependencies += "com.github.scullxbones"    %% "akka-persistence-mongo-scala" % "2.2.5"
libraryDependencies += "org.mongodb.scala"         %% "mongo-scala-driver"           % "2.4.2"
libraryDependencies += "org.mongodb.scala"         % "mongo-scala-bson_2.12"         % "2.4.2"
libraryDependencies += "org.tpolecat"              %% "doobie-core"                  % "0.6.0"
libraryDependencies += "org.tpolecat"              %% "doobie-hikari"                % "0.6.0"
libraryDependencies += "org.tpolecat"              %% "doobie-postgres"              % "0.6.0"
libraryDependencies += "org.flywaydb"              % "flyway-core"                   % "5.2.4"
libraryDependencies += "com.github.pureconfig"     %% "pureconfig"                   % "0.10.2"
libraryDependencies += "com.github.pureconfig"     %% "pureconfig-cats-effect"       % "0.10.2"
libraryDependencies += "io.chrisdavenport"         %% "log4cats-core"                % "0.3.0-M2"
libraryDependencies += "io.chrisdavenport"         %% "log4cats-extras"              % "0.3.0-M2"
libraryDependencies += "io.chrisdavenport"         %% "log4cats-slf4j"               % "0.3.0-M2"
libraryDependencies += "ch.qos.logback"            % "logback-classic"               % "1.2.3"
libraryDependencies += "com.beachape"              %% "enumeratum"                   % "1.5.13"
libraryDependencies += "org.scalactic"             %% "scalactic"                    % "3.0.5"
libraryDependencies += "org.scalatest"             %% "scalatest"                    % "3.0.5" % "test"
libraryDependencies += "org.scalacheck"            %% "scalacheck"                   % "1.14.0" % "test"
libraryDependencies += "com.typesafe.akka"         %% "akka-stream-testkit"          % "2.5.22" % "test"
libraryDependencies += "com.typesafe.akka"         %% "akka-http-testkit"            % "10.1.8" % "test"
libraryDependencies += "org.fusesource.leveldbjni" % "leveldbjni-all"                % "1.8" % "test"
libraryDependencies += "org.iq80.leveldb"          % "leveldb"                       % "0.11" % "test"
libraryDependencies += "org.tpolecat"              %% "doobie-scalatest"             % "0.6.0" % "test"
libraryDependencies += "com.dimafeng"              %% "testcontainers-scala"         % "0.25.0" % "test"
libraryDependencies += "org.testcontainers"        % "postgresql"                    % "1.11.2" % "test"

addCompilerPlugin("org.scalamacros" % "paradise"            % "2.1.0" cross CrossVersion.full)
addCompilerPlugin("org.typelevel"   %% "kind-projector"     % "0.10.0")
addCompilerPlugin("com.olegpy"      %% "better-monadic-for" % "0.3.0-M4")

scalacOptions ++= Seq(
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-encoding",
  "utf-8",                            // Specify character encoding used by source files.
  "-explaintypes",                    // Explain type errors in more detail.
  "-feature",                         // Emit warning and location for usages of features that should be imported explicitly.
  "-language:existentials",           // Existential types (besides wildcard types) can be written and inferred
  "-language:experimental.macros",    // Allow macro definition (besides implementation and application)
  "-language:higherKinds",            // Allow higher-kinded types
  "-language:implicitConversions",    // Allow definition of implicit functions called views
  "-unchecked",                       // Enable additional warnings where generated code depends on assumptions.
  "-Xcheckinit",                      // Wrap field accessors to throw an exception on uninitialized access.
  "-Xfatal-warnings",                 // Fail the compilation if there are any warnings.
  "-Xfuture",                         // Turn on future language features.
  "-Xlint:adapted-args",              // Warn if an argument list is modified to match the receiver.
  "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
  "-Xlint:constant",                  // Evaluation of a constant arithmetic expression results in an error.
  "-Xlint:delayedinit-select",        // Selecting member of DelayedInit.
  "-Xlint:doc-detached",              // A Scaladoc comment appears to be detached from its element.
  "-Xlint:inaccessible",              // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any",                 // Warn when a type argument is inferred to be `Any`.
  "-Xlint:missing-interpolator",      // A string literal appears to be missing an interpolator id.
  "-Xlint:nullary-override",          // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Xlint:nullary-unit",              // Warn when nullary methods return Unit.
  "-Xlint:option-implicit",           // Option.apply used implicit view.
  "-Xlint:package-object-classes",    // Class or object defined in package object.
  "-Xlint:poly-implicit-overload",    // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:private-shadow",            // A private field (or class parameter) shadows a superclass field.
  "-Xlint:stars-align",               // Pattern sequence wildcard must align with sequence component.
  "-Xlint:type-parameter-shadow",     // A local type parameter shadows a type already in scope.
  "-Xlint:unsound-match",             // Pattern match may not be typesafe.
  "-Yno-adapted-args",                // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
  "-Ypartial-unification",            // Enable partial unification in type constructor inference
  "-Ywarn-dead-code",                 // Warn when dead code is identified.
  "-Ywarn-extra-implicit",            // Warn when more than one implicit parameter section is defined.
  "-Ywarn-inaccessible",              // Warn about inaccessible types in method signatures.
  "-Ywarn-infer-any",                 // Warn when a type argument is inferred to be `Any`.
  "-Ywarn-nullary-override",          // Warn when non-nullary `def f()' overrides nullary `def f'.
  "-Ywarn-nullary-unit",              // Warn when nullary methods return Unit.
  "-Ywarn-numeric-widen",             // Warn when numerics are widened.
  "-Ywarn-unused:implicits",          // Warn if an implicit parameter is unused.
  "-Ywarn-unused:imports",            // Warn if an import selector is not referenced.
  "-Ywarn-unused:locals",             // Warn if a local definition is unused.
  "-Ywarn-unused:params",             // Warn if a value parameter is unused.
  "-Ywarn-unused:patvars",            // Warn if a variable bound in a pattern is unused.
  "-Ywarn-unused:privates",           // Warn if a private member is unused.
  "-Ywarn-value-discard"              // Warn when non-Unit expression results are unused.
)

scalacOptions in (Compile, console) --= Seq(
  "-Ywarn-unused:imports",
  "-Xfatal-warnings"
)
