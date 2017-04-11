

import forcomp.loadDictionary



object Anagrams {

  type Word = String

  type Sentence = List[Word]

  type Occurrences = List[(Char, Int)]

  val dictionary: List[Word] = loadDictionary

  val a = ("Almale")

  def wordOccurrences(w: Word): Occurrences = {
    val lowercaseWord = w.toLowerCase
    val chars = lowercaseWord.toList
    val chars_Grouped: Map[Char, List[Char]] = chars.groupBy(c => c)
    val occList: List[(Char, Int)] = chars_Grouped.mapValues(cs => cs.length).toList
    val occListSorted: List[(Char, Int)] = occList.sortBy(cs => cs._1)
    occListSorted
  }

  val b = wordOccurrences(a)

  def sentenceOccurrences(s: Sentence): Occurrences = {
    val sentenceConcat: Word = s.foldLeft(s.head)((w1, w2) => w1.concat(w2))
    val sentenceOcc: Occurrences = wordOccurrences(sentenceConcat)
    sentenceOcc
  }

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    dictionary.groupBy(w => wordOccurrences(w))
  }


  def wordAnagrams(word: Word): List[Word] = {
    val wordOcc: Occurrences = wordOccurrences(word)
    val anags: List[Word] = dictionaryByOccurrences(wordOcc)
    anags
  }


  val c: List[Word] = List("Alma", "le")
  c.mkString("")



  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case Nil => List(List())
    case xs :: ys => {
      val aktualis: List[List[(Char, Int)]] =
        for {
          i <- combinations(ys)
          n <- 1 to xs._2
        }
          yield (xs._1, n) :: i
      aktualis ::: combinations(ys)
    }
  }

  val t = wordOccurrences("lard")

  val tset = combinations(t).toSet
  val t2 = wordOccurrences("r")
val t2set = combinations(t2).toSet

  val diff = tset.diff(t2set)

val sent: List[Word] = List("Yes", "man")
  val sentWord: String = sent.mkString("")
  val words: List[Word] = wordAnagrams(sentWord)


}



