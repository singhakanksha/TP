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
    ((x:String) => "here is a list of repos to release") }

  def Release: Rule0  = rule( ws ~ "release" ~ ws)
  def ReleaseString: Rule1[String] = rule(capture(Release))

}

object ReleaseDslParser {

  def apply(input: String) = {
    val inpLower = input.toLowerCase
    println(inpLower)
    val parser = new ReleaseDslParser(inpLower)
    val parserResult: Try[String] = parser.releaseExtractor.run()
//    println("hello")
//   println(parserResult.isSuccess)
    parserResult match {
     case Success(str) => println(str)
     case Failure(e) => throw new RuntimeException("failed" + e.getCause)
   }
//println("hello world")
}
}


object Test1 extends App{
   ReleaseDslParser("  ReleAse   ")
//  parsingResult match {
//    case Success(e) => println(e)
//    case Failure(e) => throw new RuntimeException("failed" + e.getCause)
//  }
}
