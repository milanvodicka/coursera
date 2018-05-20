val helloWorld = { name: String =>
  val prefixedName  = s"Greetings from $name!"
  s"Hello World! $prefixedName"
}

helloWorld("Milan")