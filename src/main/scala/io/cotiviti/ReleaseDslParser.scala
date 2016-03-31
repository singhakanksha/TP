package io.cotiviti

import org.parboiled2._

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

case class Release(str:String)

class ReleaseDslParser(val input: ParserInput) extends Parser {

  protected def whiteSpaceChar = CharPredicate(" \n\r\t\f")
  protected def ws = rule { quiet(zeroOrMore(whiteSpaceChar)) }

 // def Statement:Rule1[String] = rule { str("release") ~> ((releaseName: String) => {("")}) }// ~ str("help") ~> ((s:String,s1:String) => s.toString.concat(s1))}

  def releaseExtractor: Rule1[String] = rule{ ReleaseString ~>
    ((x:String) => x.mkString(",")) }

  def Release: Rule0  = rule("release")
  def ReleaseString: Rule1[String] = rule(capture(Release))

}

object ReleaseDslParser {

  def apply(input: String) = {
    val parser = new ReleaseDslParser(input.toLowerCase())
    val parserResult: Try[String] = parser.releaseExtractor.run()

   println(parserResult.isSuccess)
//   match {
//     case Success(str) => Right("hello")//Right(str)
//     case Failure(e: ParseError) => Left(parser.formatError(e))
//   }

}
}


object Test1 extends App{
  new ReleaseDslParser("release")
}
