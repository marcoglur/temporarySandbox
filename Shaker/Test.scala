#!/bin/sh
## test shaker-script

exec scala -Dscript="$0" "$0" "$@"

!#

import scala.language.postfixOps
import scala.sys.process._


// simple testing
// (input, expected)
testTp(Seq(
  ("tes", "tes"),
  ("test", "tset"),
  ("Test", "Tset"),
  ("tEst", "tEst"),
  ("tesT", "tesT"),
  ("TesT", "TesT"),
  ("TeSt", "TeSt"),
  ("tEst", "tEst")))

// random testing of shakable strings
testSeq((3 to 24).collect({case x =>
  val s = new StringBuffer()
    (0 to x) foreach { _ =>
      s append ('a' + (Math.random() * 24)).toChar
    }
    s.toString
}))

def testTp(tps: Seq[(String,String)]) = {
  val res = shakeSeq(tps.collect({case (s:String,_) =>
     s
  })).iterator

  tps.forall({case (s:String,x:String) =>
    print(s"testing '$s'")
    assert(x == res.next(), s"shake '$s' to get '$x'")
    println(" -> OK")
    true
  })
}
def testSeq(ss: Seq[String]): Unit = {
  assume(ss.forall({ case f => f.length > 3 }))
  val ts = shakeSeq(ss)
  assume(ss.forall({ case s =>
    val t = ts(ss.indexOf(s))
    print(s"testing '$s'")
    assert(!(t == s && s.substring(1, s.length - 2).distinct.length > 1), s"shakable '$s' to get '$t'")
    assert(s(0) == t(0), s"shakable '$s' to get '$t'")
    assert(s.last == t.last, s"shakable '$s' to get '$t'")
    assert(s.init.tail.length == t.init.tail.length, s"shakable '$s' to get '$t'")
    assert(s.init.tail.sorted == t.init.tail.sorted, s"shakable '$s' to get '$t'")
    println(" -> OK")
    true
  }))
}

def shakeSeq(seq: Seq[String]): Seq[String] = {
  ((Seq("echo") ++ seq) #| "./Shaker.scala" !!) split "\\W"
}


