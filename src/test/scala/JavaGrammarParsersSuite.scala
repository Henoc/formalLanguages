import java.nio.file.Paths

import org.scalatest.FunSuite

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.parsing.input.CharSequenceReader

/**
  * Created by heno on 2016/06/16.
  */
class JavaGrammarParsersSuite extends FunSuite{
  import ASTTranslators._
  test("Some grammar parsing"){
    implicit val parser = new JavaGrammarParsers

      """
        |TypeName:
        |Identifier
        |PackageOrTypeName "." Identifier
        |PackageOrTypeName:
        |Identifier
        |PackageOrTypeName "." Identifier
        |ExpressionName:
        |Identifier
        |AmbiguousName "." Identifier
        |MethodName:
        |Identifier
        |PackageName:
        |Identifier
        |PackageName "." Identifier
        |AmbiguousName:
        |Identifier
        |AmbiguousName "." Identifier
        |
      """.stripMargin.parse.map(_.removeIndirectLeftRecursion()).map(_.peekingPrint()).map(_.removeDirectLeftRecursion()).map(_.peekingPrint())
      .map(_.methodNPeekingPrint())
  }

  test("Java grammar parsing"){
    implicit val parser = new JavaGrammarParsers
    Source.fromFile(Paths.get("src","main","resources","javaGrammarExpressions.txt").toFile).mkString.parse
      //.map(_.peekingPrint())
      //.map(_.removeIndirectLeftRecursion())
      .map(_.removeDirectLeftRecursion())
      .map(_.peekingPrint())
      //.map(_.methodNPeekingPrint())
  }

  implicit class Parseable(code : String){
    def parse(implicit parser : JavaGrammarParsers) = parser.defines(new CharSequenceReader(code)) match {
      case parser.Success(result : ArrayBuffer[Define], next) => Some(result)
      case others => println(others); None
    }
  }
}
