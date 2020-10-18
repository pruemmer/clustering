
package antiunif;

object AntiUnification {

  abstract class WildcardChar
  case class  ConcChar(code : Int) extends WildcardChar
  case object Wildcard             extends WildcardChar

  type ConcreteWord = Seq[ConcChar]
  type WildcardWord = Seq[WildcardChar]

  def toConcreteWord(str : String) : ConcreteWord =
    (for (c <- str.iterator) yield ConcChar(c)).toVector

  def apply(strings : String*) : WildcardWord = {
    val words : Seq[WildcardWord] = for (s <- strings) yield toConcreteWord(s)
    words reduceLeft (apply(_, _))
  }

  def apply(str1 : WildcardWord, str2 : WildcardWord) : WildcardWord = {
    var specialChar = 1 << 29
    def toConc(w : WildcardWord) =
      for (c <- w) yield c match {
        case Wildcard => {
          specialChar = specialChar + 1
          ConcChar(specialChar)
        }
        case c : ConcChar =>
          c
      }

    val unif = new AntiUnification(toConc(str1), toConc(str2))
    unif.result
  }

  def pp(w : WildcardWord) : String =
    (for (c <- w) yield c match {
       case Wildcard    => '*'
       case ConcChar(c) => c.toChar
     })(collection.breakOut)

}

class AntiUnification(w1 : AntiUnification.ConcreteWord,
                      w2 : AntiUnification.ConcreteWord) {

  import AntiUnification._

  type PartialResult = (List[WildcardChar], Int)

  def addWildcard(r : PartialResult) : PartialResult = r match {
    case (Wildcard :: _, _) => r
    case (w, n)             => (Wildcard :: w, n)
  }

  def addChar(r : PartialResult, c : ConcChar) : PartialResult = {
    val (w, n) = r
    (c :: w, n + 1)
  }

  def best(rs : Seq[PartialResult]) : PartialResult = rs maxBy (_._2)

  val N1 = w1.size
  val N2 = w2.size

  val matrix = Array.ofDim[PartialResult](N1 + 1, N2 + 1)

  matrix(0)(0) = (List(), 0)

  for (x <- 0 to N1;
       y <- 0 to N2;
       if x != 0 || y != 0) {
    matrix(x)(y) = best(
      (for (x1 <- List(x-1); if x1 >= 0) yield addWildcard(matrix(x1)(y))) ++
      (for (y1 <- List(y-1); if y1 >= 0) yield addWildcard(matrix(x)(y1))) ++
      (for (x1 <- List(x-1); y1 <- List(y-1); if x1 >= 0 && y1 >= 0) yield {
         if (w1(x1) == w2(y1))
           addChar(matrix(x1)(y1), w1(x1))
         else
           addWildcard(matrix(x1)(y1))
       })
    )
  }

  val result : WildcardWord = matrix(N1)(N2)._1.reverse

}
