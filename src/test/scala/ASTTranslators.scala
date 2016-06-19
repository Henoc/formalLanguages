import scala.collection.mutable.ArrayBuffer

/**
  * Created by heno on 2016/06/16.
  */
object ASTTranslators {
  implicit class AST(defines : ArrayBuffer[Define]){
    def peekingPrint() = {
      println(defines.mkString("\n\n"))
      defines
    }
    def methodNPeekingPrint() = {
      println(defines.map(_.toMethodNotation).mkString("\n\n"))
      defines
    }
    def removeIndirectLeftRecursion() = {
      for (i <- defines.indices) {
        for (j <- 0 until i) {
          var k = 0
          while (k < defines(i).choices.size) {
            if (defines(i).choices(k).head == defines(j).nonTerminal) {
              val alpha = defines(i).choices(k).tail
              defines(i).choices.remove(k)

              for (tokens <- defines(j).choices) {
                defines(i).choices.insert(k, tokens ++ alpha)
                k += 1
              }
            }else{
              k += 1
            }
          }
        }
      }
      defines
    }
    def removeDirectLeftRecursion() = {
      var i = 0
      while(i < defines.size){
        val define = defines(i)
        var j = 0
        val nonTermSuffix = NonTerminal(define.nonTerminal.s + "Suf")
        val suffixChoices = ArrayBuffer.empty[List[Token]]

        if(define.choices.exists(tokens => tokens.head == define.nonTerminal)) {    // A -> Aα is existed
          while (j < define.choices.size) {
            val tokens = define.choices(j)
            if (tokens.head == define.nonTerminal) {
              // A -> Aα
              suffixChoices.+=(tokens.tail :+ Maybe(nonTermSuffix :: Nil)) // A' -> α[A']
              define.choices.remove(j)
            } else {
              // A -> β
              define.choices.update(j,tokens :+ Maybe(nonTermSuffix :: Nil)) // A -> β[A']
              j += 1
            }
          }
          i += 1
          defines.insert(i,Define(nonTermSuffix,suffixChoices))
        }
        i += 1
      }
      defines
    }
  }
}
