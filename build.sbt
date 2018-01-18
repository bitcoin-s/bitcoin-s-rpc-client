testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "3")

coverageExcludedPackages := ".*gen"

coverageMinimum := 75

coverageFailOnMinimum := true

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) => {
    case "logback.xml" => MergeStrategy.discard
    case x => old(x)
  }
}

//test in assembly := {}

assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = false)

//testOptions in Test += Tests.Argument("-oF")
