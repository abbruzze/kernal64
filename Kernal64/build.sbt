
name := "kernal64"

version := "1.8.3"

scalaVersion := "2.13.10"

scalacOptions ++= Seq(
  "-language:postfixOps",
  "-encoding","ISO-8859-1",
  "-deprecation",
  "-feature"
)

javacOptions ++= Seq("--release", "11")

libraryDependencies += "com.fifesoft" % "rsyntaxtextarea" % "3.3.1"
libraryDependencies += "org.jsoup" % "jsoup" % "1.15.4"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0"
libraryDependencies += "commons-net" % "commons-net" % "3.3"
libraryDependencies += "com.formdev" % "flatlaf" % "3.0"

Compile / scalaSource := baseDirectory.value / "src"
Compile / resourceDirectory := baseDirectory.value / "resources"
// Generate sources for Version and CPU65816.scala
Compile / sourceGenerators += Def.task {
	import scala.sys.process._
    s"java -cp ${baseDirectory.value / "lib" / "anarres-cpp.jar"} CPP ${baseDirectory.value/"src/ucesoft/cbm/cpu/wd65816/CPU65816.template"} ${baseDirectory.value/"src/ucesoft/cbm/cpu/wd65816/CPU65816.scala"}" !

	val versionFile = baseDirectory.value/"src/ucesoft/cbm/Version.scala"
	val dfBuildDate = new java.text.SimpleDateFormat("MMMM dd yyyy",java.util.Locale.US)
	val dfDate = new java.text.SimpleDateFormat("yyyyMMdd")
	val dfTime = new java.text.SimpleDateFormat("HHmm")
	val now = new java.util.Date
	val buildNumber = if (System.getProperty("kbuild") != null) s" b${System.getProperty("kbuild")}" else ""
    IO.write(versionFile,s"""package ucesoft.cbm
object Version {
    val VERSION = "${version.value}$buildNumber"
	val BUILD_DATE = "${dfBuildDate.format(now)}"
	val DATE = "${dfDate.format(now)}"
	val TIME = "${dfTime.format(now)}"
}
    """)

	Seq(baseDirectory.value/"src/ucesoft/cbm/cpu/wd65816/CPU65816.scala",versionFile)
}

val buildK64Dist = taskKey[Unit]("build distribution zip file for Kernal64")

buildK64Dist := {
	def walk(file:File): List[File] = {
		if (file.isDirectory) {
			val files = for(f <- file.listFiles) yield walk(f)
			files.flatten.toList
		}
		else List(file)
	}
	
	val classDir: File = (Compile / classDirectory).value
	val packDir = classDir.getParentFile / "pack"
	IO.delete(packDir)
	IO.createDirectory(packDir)
	val packLib = packDir / "lib"
	val packRom = packDir / "rom"
	val packKeyb = packDir / "keyboard"
	IO.createDirectories(Seq(packLib,packRom,packKeyb))
	
	// copy roms & images
	val romsDir = (Compile / classDirectory).value / "roms"
	val resDir = (Compile / classDirectory).value / "resources"
	IO.createDirectories(Seq(romsDir,resDir))
	IO.copyDirectory(baseDirectory.value / "roms",romsDir)
	IO.copyDirectory(baseDirectory.value / "resources",resDir)
	
	val files = walk(classDir)
	val jarFiles = files.map { f => 
		(f,f.toString.substring(classDir.toString.length + 1))
	}
	
	val kernal64Jar = packLib / "kernal64.jar"
	IO.jar(jarFiles,kernal64Jar,new java.util.jar.Manifest,None)
	// clean
	//IO.delete(Seq(romsDir,resDir))
	
	val libraries = ((Compile / managedClasspath).value.map(_.data) ++ (Compile / unmanagedClasspath).value.map(_.data)).filterNot(_.getName.startsWith("anarres"))
	val scripts = (baseDirectory.value / "build").listFiles.filter { f => 
		val name = f.getName.toUpperCase
		name.endsWith(".SH") || name.endsWith(".BAT")
	}
	
	// copy libraries
	for (lib <- libraries) {
		IO.copyFile(lib,packLib / lib.getName)
	}
	// copy scripts
	val libJarOnly = (libraries ++ Vector(kernal64Jar)).filter(_.getName.endsWith(".jar")).map(_.getName)
	for(sc <- scripts) {
		val lines = IO.readLines(sc)
		val isLinuxShell = sc.getName.toUpperCase.endsWith(".SH")
		val newLine = if (isLinuxShell) 10.toChar.toString else 13.toChar.toString + 10.toChar
		val pathSep = if (isLinuxShell) ":" else ";"
		val dirSep = if (isLinuxShell) "/" else "\\"
		val libEnv = if (isLinuxShell) "$LIB" else "%LIB%"
		
		val linesWithCP = lines.map { line =>
			val cpLine = if (isLinuxShell) "CP=" else "set CP="
			if (line.startsWith(cpLine)) {
				val cp = libJarOnly.map(jar => s"$libEnv$dirSep$jar").mkString(pathSep)
				s"$cpLine$cp"
			}
			else line
		}
		val content = linesWithCP.mkString(newLine)
		IO.write(packDir / sc.getName,content)
	}
	// copy keyboard config files
	val keybFiles = (baseDirectory.value / "resources").listFiles.filter(_.getName.startsWith("default_keyboard"))
	for(kf <- keybFiles) {
		IO.copyFile(kf,packKeyb / kf.getName)
	}
	
	// zip distribution
	val dist = baseDirectory.value / "dist"
	val zipFile = dist / "kernal64_install.zip"
	val zipFileSet = walk(packDir)
	val zipFiles = zipFileSet.map { f => 
		(f,"kernal64/" + f.toString.substring(packDir.toString.length + 1))
	}
	IO.zip(zipFiles,zipFile,None)
	// set permissions
	val fs = java.nio.file.FileSystems.newFileSystem(zipFile.toPath)
	val root = fs.getPath("/kernal64")
	val perm = java.nio.file.attribute.PosixFilePermissions.fromString("r-xr-xr-x")
	java.nio.file.Files.list(root).filter(p => p.toString.endsWith(".sh")).forEach(p => {
		java.nio.file.Files.setAttribute(p,"zip:permissions",perm)
	})
	fs.close()
	IO.copyFile(kernal64Jar,dist / kernal64Jar.getName)
	
	// clean pack dir
	IO.delete(packDir)
}