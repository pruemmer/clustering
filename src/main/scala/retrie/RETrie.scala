
package retrie;

import scala.io.Source

object RETrie {

  def main(args : Array[String]) : Unit =
    for (filename <- args) {
      val reader = Source.fromFile(filename)
      val lines = reader.getLines.toVector
      reader.close

      val out = new java.io.FileOutputStream(filename + ".re")
      Console.withOut(out) {
        printTrie(lines, "")
      }
      out.close
    }

  def printTrie(words : Seq[String], prefix : String) : Unit = words match {
    case Seq() =>
      println(prefix + "re.none")
    case Seq(w) =>
      printStr(w, prefix)
    case _ => {
      val groups = words groupBy (_.headOption)
      if (groups.size == 1) {
        (words reduceLeft (commonPrefix _)) match {
          case "" =>
            println(prefix + "re.eps")
          case common => {
            println(prefix + "(re.++")
            printStr(common, prefix + " ")
            printTrie(words.map(_.drop(common.size)), prefix + " ")
            println(prefix + ")")
          }
        }
      } else {
        println(prefix + "(re.union")
        for (c <- groups.keys.toVector.sorted)
          printTrie(groups(c), prefix + " ")
        println(prefix + ")")
      }
    }
  }

  def commonPrefix(w1 : String, w2 : String) : String = {
    var n = 0
    while (n < w1.size && n < w2.size && w1(n) == w2(n))
      n = n + 1
    w1 take n
  }

  def printStr(w : String, prefix : String) = w match {
    case "" => 
      println(prefix + "re.eps")
    case w =>
      println(prefix + "(str.to.re \"" + escapeString(w) + "\")")
  }

  def escapeChar(c: Int): String = c match {
    case 34 =>
      "\"\""
    case c if c >= 32 && c <= 126 && c != 92 =>
      "" + c.toChar
    case c =>
      "\\u{" + Integer.toString(c, 16) + "}"
  }

  def escapeString(str : String) : String =
    for (c <- str; d <- escapeChar(c)) yield d

}
