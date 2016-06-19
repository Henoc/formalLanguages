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
  val terminal = stringLiteral ^^ Terminal
  val lf : Parser[String] = "\n"
  val define = ((nonTerminal <~ ":") <~ lf) ~ (tokens <~ lf).+ <~ lf.* ^^ {case nt ~ choices => Define(nt, choices.to[ArrayBuffer])}
  val tokens : Parser[List[Token]] = (nonTerminal | terminal | (("{" ~> tokens) <~ "}") ^^ Repeat | (("[" ~> tokens) <~ "]") ^^ Maybe).+
  val defines : Parser[ArrayBuffer[Define]] = lf.* ~> define.+ ^^ (_.to[ArrayBuffer])
}

trait MethodNotation {
  def toMethodNotation : String
  def tokensToMethodN(tokens : List[Token], acc : String = "", isLeftTrowable : Option[Boolean] = None) : String = (tokens,isLeftTrowable) match {
    case ((nont@NonTerminal(_)) :: tail, None) =>        tokensToMethodN(tail,nont.toMethodNotation, Some(false))
    case ((nont@NonTerminal(_)) :: tail, Some(false)) => tokensToMethodN(tail,"(" + acc + " ~ " + nont.toMethodNotation + ")", Some(false))
    case ((nont@NonTerminal(_)) :: tail, Some(true)) =>  tokensToMethodN(tail,"(" + acc + " ~> " + nont.toMethodNotation + ")", Some(false))
    case ((t@Terminal(_)) :: tail, None) =>              tokensToMethodN(tail,t.toMethodNotation, Some(true))
    case ((t@Terminal(_)) :: tail, Some(true)) =>        tokensToMethodN(tail,"(" + acc + " ~> " + t.toMethodNotation + ")", Some(true))
    case ((t@Terminal(_)) :: tail, Some(false)) =>       tokensToMethodN(tail,"(" + acc + " <~ " + t.toMethodNotation + ")", Some(false))
    case ((r@Repeat(tokens2)) :: tail, None) =>          tokensToMethodN(tail,r.toMethodNotation,Some(!hasNonTerminal(tokens2)))
    case ((r@Repeat(tokens2)) :: tail, Some(true)) =>    tokensToMethodN(tail,"(" + acc + " ~> " + r.toMethodNotation + ")",Some(!hasNonTerminal(tokens2)))
    case ((r@Repeat(tokens2)) :: tail, Some(false)) =>   tokensToMethodN(tail,"(" + acc + " ~ " + r.toMethodNotation + ")",Some(false))
    case ((m@Maybe(tokens2)) :: tail, None) =>           tokensToMethodN(tail,m.toMethodNotation,Some(!hasNonTerminal(tokens2)))
    case ((m@Maybe(tokens2)) :: tail, Some(true)) =>     tokensToMethodN(tail,"(" + acc + " ~> " + m.toMethodNotation + ")",Some(!hasNonTerminal(tokens2)))
    case ((m@Maybe(tokens2)) :: tail, Some(false)) =>    tokensToMethodN(tail,"(" + acc + " ~ " + m.toMethodNotation + ")",Some(false))
    case (Nil,_) => acc
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
}
case class Terminal(s : String) extends Token {
  override def toString = s
  val toMethodNotation = "literal(" + s + ")"
}
case class Repeat(tokens : List[Token]) extends Token {
  override def toString = "{" + tokens.mkString(" ") +"}"
  val toMethodNotation = "(" + tokensToMethodN(tokens) + ").*"
}
case class Maybe(tokens : List[Token]) extends Token {
  override def toString = "[" + tokens.mkString(" ") + "]"
  val toMethodNotation = "(" + tokensToMethodN(tokens) + ").?"
}

case class Define(nonTerminal: NonTerminal , choices : ArrayBuffer[List[Token]]) extends MethodNotation{
  override def toString = nonTerminal + ":\n" + choices.map(_.mkString(" ")).mkString("\n")
  def toMethodNotation = "val " + nonTerminal.toMethodNotation + " = " + choices.map(tokensToMethodN(_)).mkString(" | ")
}