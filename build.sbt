
scalaVersion := "2.12.6"
name := "world3"

resolvers += Resolver.sonatypeRepo("snapshots")
resolvers += Resolver.sonatypeRepo("staging")

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.5.0"

def monadLessVersion = "0.0.13"
libraryDependencies += "io.monadless" %% "monadless-stdlib" % monadLessVersion

