#!/bin/sh
## to display the usage run this script with the -u argument: ./Fn.scala -u
 exec scala -Dscript="$0" "$0" "$@"

!#


// load properties
var pos: Int = sys.props.getOrElse("Fn.pos", "0").toInt
var rad: Int = sys.props.getOrElse("Fn.rad", "10").toInt
var lineWidth: Int = sys.props.getOrElse("Fn.lineWidth", "0").toInt
var lister = sys.props.getOrElse("Fn.lister", "false").toBoolean
var debug = sys.props.getOrElse("Fn.debug", "false").toBoolean
var quiet = sys.props.getOrElse("Fn.quiet", "false").toBoolean
var usage = sys.props.getOrElse("Fn.usage", "false").toBoolean
var blackDigs: String = sys.props.getOrElse("Fn.blackDigs", "")

val calculate = sys.props.getOrElse("Fn.calculate", "false").toBoolean
val iterate = sys.props.getOrElse("Fn.iterate", (!calculate).toString).toBoolean

var writer = Console.out
var log = Console.out

// defaults
var n: Int = 0
var end: Int = -1



// the current script-name
lazy val cmd = {
  import scala.language.postfixOps
  try {
    List(sys.props("script")) find ((x: String) => {
      x.matches(".*[a-zA-Z]{1}.*")
    }) get
  } catch {
    case _: Throwable => "<Fn.scala>"
  }
}

def readArgs() {

  import scala.util.matching.Regex
  def takeIntArg(a: String): (Int, String) = {
    val p = new Regex("""([0-9]+)(.*)""")
    val p(value: String, rest: String) = a
    (value.toInt, rest)
  }

  var matched: List[Char] = List()

  def read(arg: String): Unit = {
    if (debug) logln("reading arg " + arg + " with rad: " + rad)
    if (arg.length == 0) return

    arg.head match {
      case a if matched contains a => sys.error("duplicated argument: " + a)
      case 'n' => matched ::= 'n'
        n = Integer.parseInt(arg.tail, rad)
      case 'e' => matched ::= 'e'
        end = Integer.parseInt(arg.tail, rad)
      case 'r' if (matched contains 'n') || (matched contains 'e') =>
        sys.error("radix (r) must come before radix is used for start (n) and end (e)!")
      case 'r' => matched ::= 'r'
        takeIntArg(arg.tail) match {
          case (value, rest) => rad = value
            require(37 > rad, "radix is too big")
            if (debug) println("parsed rad " + rad)
            read(rest)
          case _ => sys.error("invalid " + arg)
        }
      case 'w' => matched ::= 'w'
        if (arg.tail.length > 0)
          takeIntArg(arg.tail) match {
            case (value, rest) => lineWidth = value
              if (debug) println("parsed lineWidth " + lineWidth)
              read(rest)
            case _ => sys.error("invalid " + arg)
          }
        else
          lineWidth = rad * (pos - 1)
      case 'p' => matched ::= 'p'
        takeIntArg(arg.tail) match {
          case (value, rest) => pos = value
            read(rest)
          case _ => sys.error("invalid " + arg)
        }
      case 'v' => matched ::= 'v'
        matched ::= 'q'
        if (arg.tail.length > 0) read(arg.tail)
        debug = true
      case 'q' => matched ::= 'q'
        matched ::= 'v'
        if (arg.tail.length > 0) read(arg.tail)
        quiet = true
      case 'u' => matched ::= 'u'
        if (arg.tail.length > 0) read(arg.tail)
        usage = true
      case 'b' => matched ::= 'b'; blackDigs = arg.tail
      case 'l' => matched ::= 'l'
        if (arg.tail.length > 0) read(arg.tail)
        lister = true
      case a => sys.error("wrong argument: " + a)
    }
  }

  def info() = {
    def lbl(str: Any) = {
      "   " + str.toString.padTo(15, " ").mkString + ": "
    }
    logln("")
    logln(" values was:")
    logln(lbl("calculate") + calculate.toString)
    logln(lbl("iterate") + iterate.toString)
    logln(lbl("radix (-r)") + rad.toString)
    logln(lbl("position (-p)") + pos)
    logln(lbl("start (-n)") + Integer.toString(n, rad))
    logln(lbl("end (-e)") + Integer.toString(end, rad))
    logln(lbl("lister (-l)") + lister)
    logln(lbl("lineWidth (-w)") + lineWidth)
    logln(lbl("blackDigs (-b)") + blackDigs)
    logln(lbl("usage (-u)") + usage)
    logln(lbl("debug (-v)") + debug)
    logln(lbl("quiet (-q)") + quiet)
    logln("")
  }

  def print_usage() = {
    println(
      s"""Fn - a simple tool to calculate triangular numbers.
          | Basic usage of Fn:
          |  $cmd [-r<radix:dec>] [-p<position:dec>] [-n<start:int>] \\\\
          |  [-e<end:int>] [-w[<line-width:dec>] [-l<lister:flag>] \\\\
          |  [-u<usage:flag>] [-v<debug:flag>|-q<quiet:flag>]
          | Arguments:
          |   radix, -r:          int decimal; default: 10
          |   position, -p:       int decimal; default: 1
          |   start position, -n: int based on given radix; default: 1
          |   end position, -e:   int based on given radix; default: -1
          |   line width, -w:     int decimal; default: 0 (disabled)
          |   black digit, -b:    int decimal; default: none
          |   list mode, -l:      flag to enable
          |   show usage, -u:     flag to enable
          |   debug/quiet, -v|-q: exclusive flag
          | Examples:
          |  $$ $cmd -l
          |  $$ $cmd -n10
          |  $$ $cmd -p1r17n10g -e11g
          |  $$ $cmd -p12r2e100000000000000000
          |  $$ $cmd -p12r3n100 -e100000000000
          |  $$ $cmd -b13579 -n160010000 -e160013300 -p5
          |  $$ scala -DFn.lister=false -DFn.pos=0 -DFn.rad=10 $cmd -n110 -e100
          |  $$ scala -DFn.pos=1 -DFn.rad=10 $cmd -n1000
          |  $$ scala -DFn.lister=true -DFn.pos=1 -DFn.rad=8 $cmd -n1000
          |  $$ scala -DFn.lister=false -DFn.pos=1 -DFn.rad=8 $cmd -n1000
          |  $$ scala -DFn.debug=true -DFn.pos=7 -DFn.rad=8 $cmd -n100000000
          |  $$ scala -DFn.debug=true -DFn.pos=7 -DFn.rad=8 $cmd -p2n100000000
          |  $$ scala -DFn.calculate=true $cmd -lvp2n12 -e22
          |  $$ scala -DFn.calculate=true  -DFn.iterate=true $cmd -lvp2n12 -e22
		""".stripMargin)

  }


  // main routine
  try {
    args.foreach((x) => {
      if (x.startsWith("-")) read(x.tail)
    })
    if (n < 0) {
      sys.error("start (n=" + Integer.toString(n, rad) + ") must be greater than or equal to 0")
    }
    if (end >= 0 && end < n) {
      sys.error("end (e=" + Integer.toString(end, rad) + ") must be greater than or equal to start (n=" + Integer.toString(n, rad) + ")")
    }
    if (usage) {
      print_usage()
      sys.exit(1)
    }
    if (debug) {
      info()
    }
  } catch {
    case x: RuntimeException => println(x.getClass.getSimpleName + ": " + x.getMessage); if (debug) x.printStackTrace(); info(); println(s"check the usage by running $cmd -u"); sys.exit
  }
}



def printOut(x: Any) = writer.print(x)
def logln(x: Any) = if (!quiet) log.println(x)


readArgs()




lazy val padL = Integer.toString(end, rad).length
lazy val padR = 1
lazy val initLister = "\n n" + " " * (padL - 1) + " | Fn\n" + " " + "-" * padL + "-+-" + "-" * 20

if (lister) {
  logln(initLister)
}


var charPos = 0


// TODO calculating Fn is much too slow
object calculated {

  import java.math.MathContext

  lazy val one = BigDecimal(1, new MathContext(end / 3))
  lazy val two = one + 1
  lazy val five = two + 3
  lazy val sqrt5 = {
    var x0 = BigDecimal(0)
    var x1 = BigDecimal(Math.sqrt(five.doubleValue()))
    while (!x0.equals(x1)) {
      x0 = x1
      x1 = five / x0
      x1 = x1 + x0
      x1 = x1 / two
    }
    x1
  }


  def apply(c: Int): BigInt = {
    def f(n: Int): BigInt = {
      if (n == 1 || n == 2) 1
      else (one / sqrt5 * ((one + sqrt5) / two).pow(n)
        - one / sqrt5 * ((one - sqrt5) / two).pow(n)).toBigInt
    }
    val cur = f(c)
    if (lister) {
      val s = Integer.toString(c, rad)
      val l = s.length
      val pad = " " * (padL - l)
      val x = cur.toString(rad)
      val pad2 = " " * (padR - x.length)
      printOut(" " + pad + s + " | " + pad2 + x + "\n")
    } else {
      if (0 != lineWidth) {
        if (charPos == lineWidth) {
          printOut("\n")
          charPos = 0
        }
        charPos += 1
      }
      import scala.math.pow
      val x = ((cur % pow(rad, pos + 1).toInt) / pow(rad, pos).toInt).toString(rad)
      if (blackDigs.isEmpty) {
        printOut(x)
      } else {
        printOut(if (blackDigs contains x) " " else "\u002E")
      }
    }

    if (c == end) cur
    else calculated(c + 1)
  }
}

object iterated {
  def apply(c: Int = 0, next: BigInt = 1, cur: BigInt = 0): BigInt = {
    if (c >= n) {
      if (lister) {
        val s = Integer.toString(c, rad)
        val l = s.length
        val pad = " " * (padL - l)
        val x = cur.toString(rad)
        val pad2 = " " * (padR - x.length)
        printOut(" " + pad + s + " | " + pad2 + x + "\n")
      } else {
        if (0 != lineWidth) {
          if (charPos == lineWidth) {
            printOut("\n")
            charPos = 0
          }
          charPos += 1
        }
        import scala.math.pow
        val x = ((cur % pow(rad, pos + 1).toInt) / pow(rad, pos).toInt).toString(rad)
        if (blackDigs.isEmpty) {
          printOut(x)
        } else {
          printOut(if (blackDigs contains x) " " else "\u002E")
        }
      }
    }
    if (c == end) next
    else iterated(c + 1, cur + next, next)
  }
}


var start = System.currentTimeMillis()
if (calculate) {
  calculated(n)
  logln("\n Calculated in " + (System.currentTimeMillis() - start) + "ms")
  start = System.currentTimeMillis()
}
if (iterate) {
  iterated()
  logln("\n Iterated in " + (System.currentTimeMillis() - start) + "ms")
}

