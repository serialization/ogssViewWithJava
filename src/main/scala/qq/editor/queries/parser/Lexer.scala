package qq.editor.queries.parser

import scala.util.parsing.combinator._;
import scala.util.parsing.input._;
/** Lexical tokens */
sealed trait Token
case class Ident(val name: String) extends Token with Positional
case class StrLit(val text: String) extends Token with Positional
case class IntLit(val value: Integer) extends Token with Positional
case class FltLit(val value: Double) extends Token with Positional
case class ObjLit(val pool: String, val id: Integer) extends Token with Positional
case class Variable(val name: String) extends Token with Positional
/* punctuation */
case class Bra() extends Token with Positional
case class Ket() extends Token with Positional
case class Braket() extends Token with Positional
case class Squi() extends Token with Positional
case class Ggle() extends Token with Positional
case class Paren() extends Token with Positional
case class Thesis() extends Token with Positional
case class Underscore() extends Token with Positional
case class Dot() extends Token with Positional
case class Comma() extends Token with Positional
case class Semi() extends Token with Positional
case class Equals() extends Token with Positional
case class NEquals() extends Token with Positional
case class LT() extends Token with Positional
case class LTE() extends Token with Positional
case class GT() extends Token with Positional
case class GTE() extends Token with Positional
case class Pound() extends Token with Positional
/* keywords */
case class TypeKwd() extends Token with Positional
case class DirectTypeKwd() extends Token with Positional
case class FilterKwd() extends Token with Positional
case class UnionKwd() extends Token with Positional
case class TrueKwd() extends Token with Positional
case class FalseKwd() extends Token with Positional

/** Lexer for the query language.
 *  
 *  Most tokens types are still unused… */
object Lexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\n\f]+".r

  def bra: Parser[Bra] = "\\[".r ^^ { _ ⇒ new Bra() }
  def ket: Parser[Ket] = "\\]".r ^^ { _ ⇒ new Ket() }
  def braket: Parser[Braket] = "\\[\\]".r ^^ { _ ⇒ new Braket() }
  def squi: Parser[Squi] = "\\{".r ^^ { _ ⇒ new Squi() }
  def ggle: Parser[Ggle] = "\\}".r ^^ { _ ⇒ new Ggle() }
  def paren: Parser[Paren] = "\\(".r ^^ { _ ⇒ new Paren() }
  def thesis: Parser[Thesis] = "\\)".r ^^ { _ ⇒ new Thesis() }
  def underscore: Parser[Underscore] = "_(?![a-zA-Z_\\\\\\P{InBasic_Latin}])".r ^^ { _ ⇒ new Underscore() }
  def dot: Parser[Dot] = "\\.".r ^^ { _ ⇒ new Dot() }
  def comma: Parser[Comma] = ",".r ^^ { _ ⇒ new Comma() }
  def semi: Parser[Semi] = ";".r ^^ { _ ⇒ new Semi() }
  def equals: Parser[Equals] = "=".r ^^ { _ ⇒ new Equals() }
  def nequals: Parser[NEquals] = "/=".r ^^ { _ ⇒ new NEquals() }
  def lt: Parser[LT] = "<".r ^^ { _ ⇒ new LT() }
  def lte: Parser[LTE] = "<=".r ^^ { _ ⇒ new LTE() }
  def gt: Parser[GT] = ">".r ^^ { _ ⇒ new GT() }
  def gte: Parser[GTE] = ">=".r ^^ { _ ⇒ new GTE() }
  def pound: Parser[Pound] = "#".r ^^ { _ ⇒ new Pound() }
  def typeKwd: Parser[TypeKwd] = "type\\b".r ^^ { _ ⇒ new TypeKwd() }
  def directTypeKwd: Parser[DirectTypeKwd] = "(?i)directtype\\b".r ^^ { _ ⇒ new DirectTypeKwd() }
  def filterKwd: Parser[FilterKwd] = "(?i)filter\\b".r ^^ { _ ⇒ new FilterKwd() }
  def unionKwd: Parser[UnionKwd] = "(?i)union\\b".r ^^ { _ ⇒ new UnionKwd() }
  def trueKwd: Parser[TrueKwd] = "(?i)true\\b".r ^^ { _ ⇒ new TrueKwd() }
  def falseKwd: Parser[FalseKwd] = "(?i)false\\b".r ^^ { _ ⇒ new FalseKwd() }

  private def ucnre = "\\\\u[0-9a-fA-F]{4}|\\\\U[0-9a-fA-F]{8}"
  def ident: Parser[Ident] = (
    ("([\\w\\P{InBasic_Latin}&&[\\D]]|" + ucnre + ")(\\w|\\P{InBasic_Latin}|" + ucnre + ")*").r ^^ { x ⇒
      new Ident(ucnre.r.replaceAllIn(x,
        { m ⇒ Integer.parseInt(m.matched.substring(2), 16).toChar.toString() }))
    }) |
    ("'[^']+'".r ^^ { x ⇒ new Ident(x.substring(1, x.length - 1)) })
  def strLit: Parser[StrLit] = "\"([^\"\\\\]|\\\\.)*\"".r ^^ { x ⇒ new StrLit(StringContext.treatEscapes(x.substring(1, x.length - 1))) }
  def intLit: Parser[IntLit] = "[-+]?[0-9]+".r ^^ { x ⇒ new IntLit(x.toInt) }
  def fltLit: Parser[FltLit] = "[-+]?[0-9]+\\.[0-9]+((?i)E[-+]?[0-9]+)?".r ^^ { x ⇒ new FltLit(x.toDouble) }
  def variable: Parser[Variable] = "[?$]".r ~ ident ^^ { case _ ~ x ⇒ new Variable(x.name) }
  def objLit: Parser[ObjLit] = ident ~ pound ~ intLit ^^ { case p ~ _ ~ i ⇒ new ObjLit(p.name, i.value) }

  def tokens: Parser[List[Token]] = {
    phrase(rep1(
      braket | bra | ket | squi | ggle | paren | thesis | underscore | dot | comma | semi
        | equals | nequals | lte | lt | gte | gt | typeKwd | directTypeKwd | filterKwd
        | unionKwd | trueKwd | falseKwd | strLit | fltLit | intLit | variable | objLit
        | ident)) ^^ (x ⇒ x)
  }

  def apply(s: String): List[Token] = {
    parse(tokens, s) match {
      case NoSuccess(msg, next)  ⇒ throw new Exception(msg)
      case Success(result, next) ⇒ result
    }
  }
}
 