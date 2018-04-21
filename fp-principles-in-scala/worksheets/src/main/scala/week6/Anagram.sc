import scala.io.Source

val in = Source.fromURL("https://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")
val words = in.getLines.toList.filter(_.forall(_.isLetter)).map(_.toLowerCase)

type Word = String
type Sentence = List[Word]
type Sentences = List[Sentence]
type Permutations = List[Sentences]

//def isMeaningful(word: Word): Boolean = words.contains(word.toLowerCase)

def isMeaningful(word: Word): Boolean = true

def splitToWords(sentenceAsString: String): Sentences =
  if (sentenceAsString.isEmpty) List(List())
  else {
    for {
      split <- 1 to sentenceAsString.length
      word = sentenceAsString take split
      rest <- splitToWords(sentenceAsString drop split)
    } yield (word :: rest)
  } toList

splitToWords("abc") == List(
  List("a", "b", "c"),
  List("a", "bc"),
  List("ab", "c"),
  List("abc"),
)

def permutateSentence(sentenceAsString: String): Permutations =
  sentenceAsString.permutations.map(splitToWords).toList

permutateSentence("abc") == List(
  splitToWords("abc"),
  splitToWords("acb"),
  splitToWords("bac"),
  splitToWords("bca"),
  splitToWords("cab"),
  splitToWords("cba"),
)

def naturalSentenceToString(naturalSentence: String): String =
  naturalSentence.filter(_.isLetter).toLowerCase

naturalSentenceToString("A bc") == "abc"

def anagrams(naturalSentence: String): Sentences =
  permutateSentence(naturalSentenceToString(naturalSentence))
    .flatMap(_.filter(_.forall(isMeaningful)))

//anagrams("abcd").map(_.mkString(" ")).mkString(".\n")

naturalSentenceToString("I love my wife and doughter").permutations.toList.length