package qq.editor.queries.parser
import scala.util.parsing.combinator._
import scala.util.parsing.input._;
import qq.editor.queries._
import qq.editor.queries.parser.Lexer._
import ogss.common.java.api;
import ogss.common.java.internal;

/** Parse for the query language. */
object Parser extends Parsers {
  override type Elem = Token
  
  class TokenReader(val x: Seq[Token]) extends Reader[Token] {
    override def first = x.head
    override def atEnd = x.size == 0
    override def pos = NoPosition
    override def rest = new TokenReader(x.tail)
  }
  
  /** Literal (constant term) */
  def lit(file: qq.editor.File): Parser[Term] = {
    accept("literal", {
      case o: ObjLit => new ConstTerm(file.objOfId(file.s.pool(o.pool), o.id)) 
      case i: IntLit => new ConstTerm(i.value) 
      case f: FltLit => new ConstTerm(f.value) 
      case s: StrLit => new ConstTerm(s.text) 
      case _: TrueKwd => new ConstTerm(true)
      case _: FalseKwd => new ConstTerm(false)
        })
  }
  def objlit(file: qq.editor.File): Parser[internal.Obj] = {
    accept("object literal", {
      case o: ObjLit => file.objOfId(file.s.pool(o.pool), o.id)
    })
  }

  def vari(file: qq.editor.File): Parser[Term] = {
    accept("variable", {
      case v: Variable => new VarTerm(v.name)
        })
  }
  /** Terms */
  def term(file: qq.editor.File): Parser[Term] = lit(file) | vari(file)

  /** Fields/predicates */
  def anyfield(file: qq.editor.File): Parser[Field] = {
    accept("identifier (field)", {
      case i: Ident => new UnspecificField(file, i.name) 
        })
  }
  def certainfield(file: qq.editor.File): Parser[Field] = {
    accept("identifier (type name)", {
      case i: Ident => i.name 
        }) ~
    accept("dot", {case _: Dot => ()}) ~
    accept("identifier (field name)", {
      case i: Ident => i.name
        }) ^^ {case t~_~f => new SpecificTypeField(file, t, f)}
  }
  def field(file: qq.editor.File): Parser[Field] = certainfield(file) | anyfield(file) 
  
  def typeId(file: qq.editor.File): Parser[api.Access[_ <: internal.Obj]] = 
    accept("identifier (type)", {case i: Ident => file.s.pool(i.name)})
  
  /** triples (term fied term) and pseudo triples (term `type` name, term `directType` name) */
  def triple(file: qq.editor.File): Parser[Query] = {
    (term(file) ~ field(file) ~ term(file) ^^ {case s ~ p ~ o => TripleQuery(file, s, p ,o)}) |
    (term(file) ~ accept("`type'", {case _: TypeKwd => ()}) ~ typeId(file)  ^^ {case s ~ _ ~ τ => TypeQuery(file, s, τ)}) | 
    (term(file) ~ accept("`directType'", {case _: DirectTypeKwd => ()}) ~ typeId(file)  ^^ {case s ~ _ ~ τ => DirectTypeQuery(file, s, τ)})
  }

  /** normal queries: joins of triples. */
  def normalQuery(file: qq.editor.File): Parser[Query] = {
    triple(file) ~ rep(accept("dot", {case _: Dot => ()}) ~ triple(file) ^^  {case _ ~ r => r}) ~ opt(accept("dot", {case _: Dot => ()})) ^^
           {case l ~ rs ~ _ => rs.fold(l)(new JoinQuery(file, _, _))}
 
  } 
  
  /** queries: normal queries and some special cases. */
  def query(file: qq.editor.File): Parser[Query] = {
    phrase(
        normalQuery(file) |
    (objlit(file) ^^ {x => new IdQuery(file, "object", x)}) | 
    (typeId(file) ^^ {x => new TypeQuery(file, "instance", x)}))
  } 
  
  /** Parse a string as [[Query]]*/
  def apply(file: qq.editor.File, s: String): Query = {
    val reader = new TokenReader(Lexer(s))
    query(file)(reader) match {
      case NoSuccess(msg, next)  ⇒ throw new Exception(msg)
      case Success(result, next) ⇒ result
    }
  }
  
}