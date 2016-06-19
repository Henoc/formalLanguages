import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

/**
  * Created by heno on 2016/06/16.
  */
class JavaGrammarParsers extends JavaTokenParsers{
  /**
    * 改行(LF)を除く
    */
  override val whiteSpace = """[ \t\x0B\f\r]+""".r
  val nonTerminal = """[A-Z][a-zA-Z]*""".r ^^ NonTerminal
  /**
    * 終端記号とみなすのは小文字から始まる文字列, ダブルクォートで囲った文字列, {}[]":以外の句読文字.
    * そのためJavaの文法定義書から持ってくる場合, 終端記号としての{}[]":はダブルクォートで囲う必要がある.
    */
  val terminal = stringLiteral ^^ Terminal |
    """[a-z][a-zA-Z]*""".r ^^ {case x => Terminal("\"" + x + "\"")} |
    """\p{Punct}""".r >> {
      case x if x == "{" || x == "}" || x == "[" || x == "]" || x == "\"" || x == ":" => failure("This is Punct, but {} or [] or \" or :")
      case x => success(Terminal("\"" + x + "\""))
    }
  val lf : Parser[String] = "\n"
  val comment : Parser[String] = "//" ~> """[^\n]*""".r <~ lf
  val define = ((nonTerminal <~ ":") <~ lf) ~ (tokens <~ lf).+ <~ (lf | comment).* ^^ {case nt ~ choices => Define(nt, choices.to[ArrayBuffer])}
  val tokens : Parser[List[Token]] = (nonTerminal | terminal | (("{" ~> tokens) <~ "}") ^^ Repeat | (("[" ~> tokens) <~ "]") ^^ Maybe).+
  val defines : Parser[ArrayBuffer[Define]] = (lf | comment).* ~> define.+ ^^ (_.to[ArrayBuffer])
}

/**
  * for Scala like method notation.
  */
trait MethodNotation {
  def toMethodNotation : String
  def toArgNotation : String
  def tokensToMethodN(tokens : List[Token], acc : String = "",argAcc : String = "" , isLeftTrowable : Option[Boolean] = None, addCaseExpression : Boolean = false) : String = (tokens,isLeftTrowable) match {
    case ((nont@NonTerminal(_)) :: tail, None) =>        tokensToMethodN(tail,nont.toMethodNotation, nont.toArgNotation, Some(false), addCaseExpression)
    case ((nont@NonTerminal(_)) :: tail, Some(false)) => tokensToMethodN(tail,"(" + acc + " ~ " + nont.toMethodNotation + ")", "(" + argAcc + " ~ " + nont.toArgNotation + ")", Some(false), addCaseExpression)
    case ((nont@NonTerminal(_)) :: tail, Some(true)) =>  tokensToMethodN(tail,"(" + acc + " ~> " + nont.toMethodNotation + ")", nont.toArgNotation, Some(false), addCaseExpression)
    case ((t@Terminal(_)) :: tail, None) =>              tokensToMethodN(tail,"literal(" + t.toMethodNotation + ")", t.toArgNotation, Some(true), addCaseExpression)
    case ((t@Terminal(_)) :: tail, Some(true)) =>        tokensToMethodN(tail,"(" + acc + " ~> " + t.toMethodNotation + ")", t.toArgNotation, Some(true), addCaseExpression)
    case ((t@Terminal(_)) :: tail, Some(false)) =>       tokensToMethodN(tail,"(" + acc + " <~ " + t.toMethodNotation + ")", argAcc, Some(false), addCaseExpression)
    case ((r@Repeat(tokens2)) :: tail, None) =>          tokensToMethodN(tail,r.toMethodNotation, r.toArgNotation,Some(!hasNonTerminal(tokens2)), addCaseExpression)
    case ((r@Repeat(tokens2)) :: tail, Some(true)) =>    tokensToMethodN(tail,"(" + acc + " ~> " + r.toMethodNotation + ")", r.toArgNotation,Some(!hasNonTerminal(tokens2)), addCaseExpression)
    case ((r@Repeat(tokens2)) :: tail, Some(false)) =>   tokensToMethodN(tail,"(" + acc + " ~ " + r.toMethodNotation + ")","(" + argAcc + " ~ " + r.toArgNotation + ")" ,Some(false), addCaseExpression)
    case ((m@Maybe(tokens2)) :: tail, None) =>           tokensToMethodN(tail,m.toMethodNotation,m.toArgNotation,Some(!hasNonTerminal(tokens2)), addCaseExpression)
    case ((m@Maybe(tokens2)) :: tail, Some(true)) =>     tokensToMethodN(tail,"(" + acc + " ~> " + m.toMethodNotation + ")",m.toArgNotation,Some(!hasNonTerminal(tokens2)), addCaseExpression)
    case ((m@Maybe(tokens2)) :: tail, Some(false)) =>    tokensToMethodN(tail,"(" + acc + " ~ " + m.toMethodNotation + ")","(" + argAcc + " ~ " + m.toArgNotation + ")",Some(false), addCaseExpression)
    case (Nil,_) => acc + (if(addCaseExpression) " ^^ {\n  case " + argAcc + " =>\n    ???\n}" else "")
  }

  def hasNonTerminal(tokens : List[Token]) : Boolean = tokens match {
    case Nil => false
    case NonTerminal(_) :: tail => true
    case Terminal(_) :: tail => hasNonTerminal(tail)
    case Repeat(tokens2) :: tail => hasNonTerminal(tokens2) || hasNonTerminal(tail)
    case Maybe(tokens2) :: tail => hasNonTerminal(tokens2) || hasNonTerminal(tail)
  }
}

sealed abstract class Token extends MethodNotation
case class NonTerminal(s : String) extends Token {
  override def toString = s
  val toMethodNotation = s.head.toLower + s.tail
  val toArgNotation = toMethodNotation + "Arg"
}
case class Terminal(s : String) extends Token {
  override def toString = s
  val toMethodNotation = s
  val toArgNotation = s + "Arg"
}
case class Repeat(tokens : List[Token]) extends Token {
  override def toString = "{" + tokens.mkString(" ") +"}"
  val toMethodNotation = "(" + tokensToMethodN(tokens) + ").*"
  val toArgNotation : String = tokens match {
    case hd :: Nil => hd.toArgNotation + "List"
    case hd :: tail => hd.toArgNotation + Repeat(tail).toArgNotation
    case Nil => throw new Exception("No token in tokens of Repeat.")
  }
}
case class Maybe(tokens : List[Token]) extends Token {
  override def toString = "[" + tokens.mkString(" ") + "]"
  val toMethodNotation = "(" + tokensToMethodN(tokens) + ").?"
  val toArgNotation : String = tokens match {
    case hd :: Nil => hd.toArgNotation + "Option"
    case hd :: tail => hd.toArgNotation + Maybe(tail).toArgNotation
    case Nil => throw new Exception("No token in tokens of Maybe.")
  }
}

case class Define(nonTerminal: NonTerminal , choices : ArrayBuffer[List[Token]]) extends MethodNotation{
  override def toString = nonTerminal + ":\n" + choices.map(_.mkString(" ")).mkString("\n")
  def toMethodNotation = "val " + nonTerminal.toMethodNotation + " = " + choices.map(tokensToMethodN(_,addCaseExpression = true)).mkString(" | ")
  def toArgNotation = throw new Exception("toArgNotation method for Define is undefined.")
}