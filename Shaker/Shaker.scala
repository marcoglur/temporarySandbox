#!/bin/sh
## std-in will be shaked word-wise, line by line

exec scala -Dscript="$0" "$0" "$@"

!#

val settings = loadSettings()

settings.get("test") match {
  case None => //
  case _ =>
    import scala.language.postfixOps
    import sys.process._
    sys.exit("./Test.scala" !)
}

val in = settings.get("file") match {
  case Some(file: String) =>
    import scala.io.Source
    Source.fromFile(file).bufferedReader()
  case _ => Console.in
}

while (in.ready()) {
  println(shake(in.readLine()))
}



def shake(s: String): String = {
  s.split("""[\s]""")
    .collect(
      { case w: String => shakeWord(w) }
    ).mkString(" ")
}

def shakeWord(s: String): String = {
  if (s.split("-").size > 1) {
    return s.split("-").collect(
      { case s: String => shakeWord(s) }
    ).mkString("-")
  }
  if (s.last.toString.matches("""\p{Punct}"""))
    return shakeWord(s.init) + s.last

  if (s(0).toString.matches("""\("'"""))
    return s(0) + shakeWord(s.tail)

  if (s.length < 4) return s
  if (!s.tail.matches("""^[\p{L}&&[^\p{Lu}]]*$""")) return s

  val sb = shuffle(s.init.tail)
  s.head + sb + s.last
}

def shuffle(s: String): String = {
  val `l` = s.length
  if (`l` == 2) return s.reverse.mkString // just switching the two
  if (s.distinct.length == 1) return s // nothing found to shuffle

  val x: Int = (`l` * Math.random).floor.toInt
  val r = x match {
    case 0 | `l` => shuffle(s.tail) + s.head
    case _ => s(x) + shuffle(s.substring(0, x) + s.substring(x + 1))
  }
  if (!(r == s))
    r
  else
    shuffle(s)
}




def loadSettings(): Map[String, Any] = {

  import scala.collection.mutable
  val settings = mutable.Map[String, Any]()
  var curPar = 0

  def takeArg(arg: String) = {
    var a = arg
    if (0 == curPar) {
      a = a.stripPrefix("-")
      if (a.startsWith("t")) {
        settings.update("test", 1)
        a = a.stripPrefix("t")
      }
      if (a.startsWith("f")) {
        a = a.stripPrefix("f")
        curPar = 'f'
      }
    }
    if (curPar == 'f' && a.length > 0) {
      settings update("file", a)
      curPar = 0
    }
  }

  for (arg <- args) {
    takeArg(arg)
  }
  settings.toMap
}
