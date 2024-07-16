package progfun.string_functions

class StringFunctions {}

object StringFunctions {

  def stripPrefix(s: String, prefix: String): String = {
    @scala.annotation.tailrec
    def stripPrefixAux(s: String, prefix: String, sIndex: Int, pIndex: Int): String = {
      if (pIndex == prefix.length) s.drop(sIndex)
      else if (sIndex >= s.length) ""
      else {
        val sChar = s.charAt(sIndex)
        val pChar = prefix.charAt(pIndex)
        if (sChar == pChar) {
          stripPrefixAux(s, prefix, sIndex + 1, pIndex + 1)
        } else {
          s
        }
      }
    }

    stripPrefixAux(s, prefix, 0, 0)
  }

  def stripSuffix(s: String, suffix: String): String = {
    @scala.annotation.tailrec
    def stripSuffixAux(s: String, suffix: String, suffixIndex: Int): String = {
      if (suffixIndex < 0) s
      else if (s.isEmpty) ""
      else {
        val sChar = s.charAt(s.length - 1)
        val suffixChar = suffix.charAt(suffixIndex)
        if (sChar == suffixChar && s.length >= suffix.length) {
          stripSuffixAux(s.dropRight(1), suffix.dropRight(1), suffixIndex - 1)
        } else {
          s
        }
      }
    }

    stripSuffixAux(s, suffix, suffix.length - 1)
  }

  def stringLength(string: String): Int = {
    stringLength_aux(string, 0)
  }

  @annotation.tailrec
  private def stringLength_aux(string: String, count: Int): Int = {
    string match {
      case "" => count
      case _ => stringLength_aux(string.tail, count + 1)
    }
  }

  def customTrim(str: String): String = {
    @scala.annotation.tailrec
    def trimLeft(s: String): String = {
      if (s.nonEmpty && (s.head == ' ' || s.head == '\r')) trimLeft(s.tail) else s
    }

    @scala.annotation.tailrec
    def trimRight(s: String): String = {
      if (s.nonEmpty && (s.last == ' ' || s.last == '\r')) trimRight(s.init) else s
    }

    trimRight(trimLeft(str))
  }

  def trimParts(strings: Array[String]): Array[String] = {
    @scala.annotation.tailrec
    def trimParts_aux(strings: Array[String], index: Int, result: Array[String]): Array[String] = {
      if (index >= strings.length) {
        result
      } else {
        trimParts_aux(strings, index + 1, result :+ customTrim(strings(index)))
      }
    }

    trimParts_aux(strings, 0, Array.empty[String])
  }

  def findFirstSpace(s: String): Int = {
    @annotation.tailrec
    def loop(index: Int): Int = {
      if (index >= s.length) -1
      else if (s.charAt(index) == ' ') index
      else loop(index + 1)
    }

    loop(0)
  }

  def extractSubstring(s: String, start: Int, end: Int): String = {
    @scala.annotation.tailrec
    def loop(currentIndex: Int, acc: String): String = {
      if (currentIndex >= end) acc
      else loop(currentIndex + 1, acc + s.charAt(currentIndex).toString)
    }

    if (start >= end || start < 0 || end > s.length) ""
    else loop(start, "")
  }

  def splitString(line: String, delimiter: Char): Array[String] = {
    @annotation.tailrec
    def splitRecursive(s: String, d: Char, acc: Array[String]): Array[String] = {
      val idx = findFirstSpace(s)
      if (idx < 0) acc :+ s
      else splitRecursive(extractSubstring(s, idx + 1, s.length), d, acc :+ extractSubstring(s, 0, idx))
    }

    splitRecursive(line, delimiter, Array.empty)
  }

  def splitOnDelim(str: String, char: Char): Array[String] = {
    @scala.annotation.tailrec
    def splitOnDelimAux(currentStr: String, delim: Char, acc: List[String]): List[String] = {
      if (currentStr.isEmpty) acc.reverse
      else {
        @scala.annotation.tailrec
        def findDelimiterIndex(s: String, ch: Char, index: Int): Int = {
          if (index >= s.length) -1
          else if (s(index) == ch) index
          else findDelimiterIndex(s, ch, index + 1)
        }

        val delimIndex = findDelimiterIndex(currentStr, delim, 0)

        if (delimIndex == -1) {
          splitOnDelimAux("", delim, currentStr :: acc)
        } else {
          val part = currentStr.take(delimIndex)
          val remaining = currentStr.drop(delimIndex + 1)
          splitOnDelimAux(remaining, delim, part :: acc)
        }
      }
    }

    splitOnDelimAux(str, char, List.empty[String]).toArray
  }

  def arrayLength(arr: Array[String]): Int = {
    @scala.annotation.tailrec
    def lengthAux(index: Int, count: Int): Int = {
      if (index >= arr.length) count
      else lengthAux(index + 1, count + 1)
    }

    lengthAux(0, 0)
  }
}
