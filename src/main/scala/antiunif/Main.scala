
package antiunif;

object Main extends App {
  println("HW")

  println(AntiUnification.pp(AntiUnification("abcabcabc",
                                             "abxcaca",
                                             "abcab")))

  val payloads = List(
    "\\x3Cscript>javascript:alert(1)</script>",
    "\\x3Cscript>javascript:alert(1)</script>",
    "'`\"><\\x3Cscript>javascript:alert(1)</script>",
    "'`\"><\\x3Cscript>javascript:alert(1)</script>",
    "'`\"><\\x3Cscript>javascript:alert(1)</script>",
    "'`\"><\\x3Cscript>javascript:alert(1)</script>",
    "'`\"><\\x00script>javascript:alert(1)</script>",
    "'`\"><\\x00script>javascript:alert(1)</script>",
    "'`\"><\\x00script>javascript:alert(1)</script>",
    "'`\"><\\x00script>javascript:alert(1)</script>",
    "<script>javascript:alert(1)<\\x00/script>",
    "<script>javascript:alert(1)<\\x00/script>",
    "<script>javascript:alert(1)</script\\x0D",
    "<script>javascript:alert(1)</script\\x0D",
    "<script>javascript:alert(1)</script\\x0B",
    "<script>javascript:alert(1)</script\\x0B",
    "<script>javascript:alert(1)</script\\x0A",
    "<script>javascript:alert(1)</script\\x0A",
    "<script>javascript:alert(1)</script>",
    "<script>javascript:alert(1)</script>",
    "<script\\x00>javascript:alert(1)</script>",
    "<script\\x00>javascript:alert(1)</script>",
    "<script\\x0A>javascript:alert(1)</script>",
    "<script\\x0A>javascript:alert(1)</script>",
    "<script\\x0C>javascript:alert(1)</script>",
    "<script\\x0C>javascript:alert(1)</script>",
    "<script\\x0D>javascript:alert(1)</script>",
    "<script\\x0D>javascript:alert(1)</script>",
    "<script\\x09>javascript:alert(1)</script>",
    "<script\\x09>javascript:alert(1)</script>",
    "<script\\x2F>javascript:alert(1)</script>",
    "<script\\x2F>javascript:alert(1)</script>",
    "<script\\x20>javascript:alert(1)</script>",
    "<script\\x20>javascript:alert(1)</script>"
  )

  println(AntiUnification.pp(AntiUnification(payloads : _*)))

}
