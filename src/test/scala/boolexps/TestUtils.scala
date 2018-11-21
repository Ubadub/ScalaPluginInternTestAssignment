package boolexps

object TestUtils {
  def addBrackets(s: String): String = '[' + s + ']'

  def addQuotes(s: String): String = s""""$s""""

  def strip(s: String): String = s.replaceAll("""\s+""", "")

}
