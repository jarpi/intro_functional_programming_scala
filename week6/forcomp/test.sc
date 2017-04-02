import forcomp._

object Anagrams {
  /** A word is simply a `String`. */
  type Word = String
  type Occurrences = List[(Char, Int)]
  type Sentence = List[Word]
  val dictionary: List[Word] = loadDictionaryReduced

  val s: Word = "abbc"

  def wordOccurrences(w: Word): Occurrences =
    w.groupBy(f => f.toLower)
      .map(s => (s._1, s._2.length))
      .toList
      .sorted

  val ss: Sentence = List("abcd", "e")

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences =
    s.flatMap(wordOccurrences)

  val ww: Word = "w"

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(wordOccurrences)

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.get(wordOccurrences(word)) match {
    case w: Some[List[Word]] if w.isDefined => w.get
    case _ => List()
  }

  val abba = List(('a', 2), ('b', 2))

  val upperBound = abba.head._2 + 1
  val upperBound2 = abba.tail.head._2 + 1
  val charBound = abba.head._1
  val charBound2 = abba.tail.head._1

  // abba.map(w => (1 until w._2) map (i => (w._1, i))).toList
  val x = abba.map(w => (1 until w._2).toList.map(j => (w._1, j)))

  val y = abba.toSet[(Char, Int)].subsets().map(_.toList).toList

  x ::: y

  def combinations(occurrences: Occurrences): List[Occurrences] =
    (occurrences foldRight List[Occurrences](Nil)) {
      case ((ch,tm), acc) => {
        acc ++ ( for { comb <- acc; n <- 1 to tm } yield (ch, n) :: comb )
      }
    }

  val uuu = combinations(abba)

  val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
  val r = List(('r', 1))
  val lad = List(('a', 1), ('d', 1), ('l', 1))


  val o = r.foldLeft(lard) {
    (xCh, xI) => if (xCh == 'a') (xCh, 10) else (xCh, 0)
  }


}


