/*
 * qSQL highlighter written in JFlex. I modified the original SQL one
 * simply follow the build instructions on:
 * http://code.google.com/p/jsyntaxpane/wiki/Building
 * replacing the sql.flex (with this) and  config.properties (in this folder)
  * build then use that .jar instead of default.
 */
package jsyntaxpane.lexers;


import jsyntaxpane.Token;
import jsyntaxpane.TokenType;

%%

%public
%class QSqlLexer
%extends DefaultJFlexLexer
%final
%unicode
%char
%type Token
%caseless


%{
    /**
     * Default constructor is needed as we will always call the yyreset
     */
    public QSqlLexer() {
        super();
    }

    @Override
    public int yychar() {
        return yychar;
    }

    private static final byte PARAN     = 1;
    private static final byte BRACKET   = 2;
    private static final byte CURLY     = 3;
%}

/* main character classes */
LineTerminator = \r|\n|\r\n
InputCharacter = [^\r\n]

WhiteSpace = {LineTerminator} | [ \t\f]+


SlashCall = {WhiteSpace}* ("\\d " {InputCharacter}* {LineTerminator}?) | ("system \"d" [^\"]* \") | ("system \"d" [^\"]* \")

/* comments */
Comment = {qMultiLineComment} | {qLineComment}
StartingComment = {qstartMultiLineComment} | {qstartLineComment}
qstartLineComment = "/" [^\[:] {InputCharacter}*
qLineComment = {WhiteSpace}+ {qstartLineComment}
qstartMultiLineComment = "/" {WhiteSpace}* {LineTerminator} [^"\\"]* ("\\")?
qMultiLineComment = {WhiteSpace}+ {LineTerminator} {qstartMultiLineComment}

/* identifiers */
Identifier = [:jletter:][:jletterdigit:]*

/* integer literals */
DecIntegerLiteral = 0 | [1-9][0-9]*

/* floating point literals */
FloatLiteral  = ({FLit1}|{FLit2}|{FLit3}) {Exponent}? [fF]

FLit1    = [0-9]+ \. [0-9]*
FLit2    = \. [0-9]+
FLit3    = [0-9]+
Exponent = [eE] [+-]? [0-9]+

/* string and character literals */
StringCharacter = [^\r\n\"\\]
SingleCharacter = [^\r\n\'\\]

// Create states for Double Quoted and Single Quoted Strings
// %state DQ_STRING, SQ_STRING, YYMAIN     // KDB doesnt use single quote
%state DQ_STRING, YYMAIN

Reserved =
   "ADD"                 |
   "ALL"                 |
   "ALLOW REVERSE SCANS" |
   "ALTER"               |
   "ANALYZE"             |
   "AND"                 |
   "AS"                  |
   "ASC"                 |
   "AUTOMATIC"           |
   "BEGIN"		 |
   "BEFORE"              |
   "BETWEEN"             |
   "BIGINT"              |
   "BINARY"              |
   "BLOB"                |
   "BOTH"                |
   "BUFFERPOOL"		 |
   "BY"                  |
   "CACHE"		 |
   "CALL"                |
   "CASCADE"             |
   "CASE"                |
   "CHANGE"              |
   "CHAR"                |
   "CHARACTER"           |
   "CHECK"               |
   "COLLATE"             |
   "COLUMN"              |
   "COMMIT"		 |
   "CONDITION"           |
   "CONSTANT"		 |
   "CONSTRAINT"          |
   "CONTINUE"            |
   "CONVERT"             |
   "CREATE"              |
   "CROSS"               |
   "CURSOR"              |
   "DATE"		 |
   "DATABASE"            |
   "DATABASES"           |
   "DEC"                 |
   "DECIMAL"             |
   "DECODE"		 |
   "DECLARE"             |
   "DEFAULT"             |
   "DELAYED"             |
   "DELETE"              |
   "DESC"                |
   "DESCRIBE"            |
   "DETERMINISTIC"       |
   "DISTINCT"            |
   "DISTINCTROW"         |
   "DIV"                 |
   "DOUBLE"              |
   "DROP"                |
   "DUAL"                |
   "EACH"                |
   "ELSE"                |
   "ELSEIF"              |
   "ENCLOSED"            |
   "END"		 |
   "ESCAPED"             |
   "EXCEPTION" 		 |
   "EXISTS"              |
   "EXIT"                |
   "EXPLAIN"             |
   "FALSE"               |
   "FETCH"               |
   "FLOAT"               |
   "FLOAT4"              |
   "FLOAT8"              |
   "FOR"                 |
   "FORCE"               |
   "FOREIGN"             |
   "FROM"                |
   "FUNCTION"		 |
   "FULLTEXT"            |
   "GLOBAL TEMPORARY"	 |
   "GRANT"               |
   "GROUP"               |
   "HAVING"              |
   "IF"                  |
   "IGNORE"              |
   "IN"                  |
   "INDEX"               |
   "INFILE"              |
   "INNER"               |
   "INOUT"               |
   "INSENSITIVE"         |
   "INSERT"              |
   "INT"                 |
   "INTEGER"             |
   "INTERVAL"            |
   "INTO"                |
   "IS"                  |
   "IS REF CURSOR"	 |
   "ITERATE"             |
   "JOIN"                |
   "KEY"                 |
   "KEYS"                |
   "KILL"                |
   "LEADING"             |
   "LEAVE"               |
   "LEFT"                |
   "LIKE"                |
   "LIMIT"               |
   "LINES"               |
   "LOAD"                |
   "LOCK"                |
   "LONG"                |
   "LOOP"                |
   "MATCH"               |
   "MERGE"               |
   "MINVALUE"		 |
   "MAXVALUE"		 |
   "MOD"                 |
   "MODIFIES"            |
   "NATURAL"             |
   "NOCYCLE"		 |
   "NOORDER"		 |
   "NOT"                 |
   "NULL"                |
   "NUMERIC"             |
   "NUMBER"              |
   "ON"                  |
   "OPEN"		 |
   "OPTIMIZE"            |
   "OPTION"              |
   "OPTIONALLY"          |
   "OR"                  |
   "ORDER"               |
   "OTHERS"		 |
   "OUT"                 |
   "OUTER"               |
   "OUTFILE"             |
   "PACKAGE"		 |
   "PACKAGE BODY"	 |
   "PAGESIZE"		 |
   "PLS_INTEGER"	 |
   "PRAGMA"		 |
   "PRECISION"           |
   "PRIMARY"             |
   "PROCEDURE"           |
   "PURGE"               |
   "RAISE"		 |
   "READ"                |
   "READS"               |
   "REAL"                |
   "REFERENCES"          |
   "REGEXP"              |
   "RELEASE"             |
   "RENAME"              |
   "REPEAT"              |
   "REPLACE"             |
   "REQUIRE"             |
   "RESTRICT"            |
   "RETURN"              |
   "REVOKE"              |
   "RIGHT"               |
   "RLIKE"               |
   "ROLLBACK"		 |
   "ROWCOUNT"		 |
   "ROWTYPE"		 |
   "SIZE"		 |
   "SCHEMA"              |
   "SCHEMAS"             |
   "SELECT"              |
   "SENSITIVE"           |
   "SEPARATOR"           |
   "SEQUENCE"		 |
   "SET"                 |
   "SHOW"                |
   "SMALLINT"            |
   "SONAME"              |
   "SPATIAL"             |
   "SPECIFIC"            |
   "SQL"                 |
   "SQLEXCEPTION"        |
   "SQLSTATE"            |
   "SQLWARNING"          |
   "STARTING"            |
   "SYSDATE"		 |
   "TABLE"               |
   "TABLESPACE"		 |
   "TERMINATED"          |
   "THEN"                |
   "TO"                  |
   "TO_CHAR"		 |
   "TO_DATE"		 |
   "TRAILING"            |
   "TRIGGER"             |
   "TRUE"                |
   "TRUNCATE"            |
   "TYPE"		 |
   "UNDO"                |
   "UNION"               |
   "UNIQUE"              |
   "UNLOCK"              |
   "UNSIGNED"            |
   "UPDATE"              |
   "USAGE"               |
   "USE"                 |
   "USER"		 |
   "USING"               |
   "VALUES"              |
   "VARBINARY"           |
   "VARCHAR"             |
   "VARCHAR2"            |
   "VARCHARACTER"        |
   "VARYING"             |
   "WHEN"                |
   "WHERE"               |
   "WHILE"               |
   "WITH"                |
   "WRITE"               |
   "XOR"                 |
   "ZEROFILL"
   

dotQWord = ".Q.addmonths" | ".Q.addr" | ".Q.host" | ".Q.chk" | ".Q.cn" | ".Q.pn" | ".Q.D" | ".Q.dd" | ".Q.dpft" | ".Q.dsftg" | ".Q.en" | ".Q.fc" | ".Q.fk" | ".Q.fmt" | ".Q.fs" | ".Q.ft" | ".Q.gc" | ".Q.hdpf" | ".Q.ind" | ".Q.P" | ".Q.par" | ".Q.PD" | ".Q.pd" | ".Q.pf" | ".Q.PV" | ".Q.pv" | ".Q.qp" | ".Q.qt" | ".Q.s" | ".Q.ty" | ".Q.u" | ".Q.v" | ".Q.V" | ".Q.view" | ".Q.def" | ".Q.ff" | ".Q.fsn" | ".Q.fu" | ".Q.id" | ".Q.j10" | ".Q.x10" | ".Q.j12" | ".Q.x12" | ".Q.k" | ".Q.MAP" | ".Q.opt" | ".Q.w" | ".Q.pt" | ".Q.bv" | ".Q.vp" | ".Q.U"

qWord = "xlog" | "xdesc" | "wj1" | "while" | "sums" | "rsave" | "read1" | "read0" | "prior" | "prev" | "prds" | "next" | "mmin" | "mins" | "md5" | "mavg" | "lsq" | "load" | "if" | "hopen" | "hclose" | "get" | "first" | "exit" | "exec" | "do" | "dev" | "deltas" | "cut" | "cov" | "cor" | "binr" | "attr" | "and" | "avg" | "asc" | "all" | "bin" | "cross" | "count" | "differ" | "each" | "eval" | "except" | "exp" | "fby" | "fills" | "fkeys" | "flip" | "getenv" | "group" | "gtime" | "hcount" | "hsym" | "iasc" | "idesc" | "in" | "inter" | "insert" | "inv" | "key" | "keys" | "ltime" | "max" | "maxs" | "mcount" | "mdev" | "med" | "meta" | "mmax" | "mmu" | "mod" | "msum" | "neg" | "not" | "null" | "or" | "over" | "parse" | "peach" | "prd" | "rand" | "rank" | "ratios" | "raze" | "reciprocal" | "reverse" | "rload" | "rotate" | "save" | "scan" | "set" | "setenv" | "show" | "signum" | "ss" | "ssr" | "like" | "string" | "sublist" | "sv" | "system" | "tables" | "til" | "type" | "ungroup" | "union" | "upsert" | "value" | "var" | "view" | "views" | "vs" | "where" | "within" | "wj" | "wsum" | "xasc" | "xbar" | "xcol" | "xcols" | "xexp" | "xgroup" | "xkey" | "xprev" | "xrank" | "0:" | "1:" | "2:" | "1" | "2" | "lj" | "pj" | "ij" | "ej" | "uj" | "aj" | "select" | "update" | "delete" | "lower" | "upper" | "trim" | "rtrim" | "ltrim" | "cols" | "sin" | "asin" | "cos" | "acos" | "tan" | "atan" | "log" | "sqrt" | "abs" | "min" | "sum" | "last" | "wavg" | "hdel" | "enlist" | "ceiling" | "floor" | "any"

dotzWord = ".z.c" | ".z.exit" | ".z.pd" | ".z.q" | ".z.W" | ".z.zd" | ".z.ws" | ".z.bm" | ".z.a" | ".z.ac" | ".z.b" | ".z.d" | ".z.D" | ".z.f" | ".z.h" | ".z.i" | ".z.k" | ".z.K" | ".z.l" | ".z.o" | ".z.pc" | ".z.pg" | ".z.ph" | ".z.pi" | ".z.po" | ".z.pp" | ".z.ps" | ".z.pw" | ".z.1" | ".z.s" | ".z.t" | ".z.T" | ".z.ts" | ".z.u" | ".z.vs" | ".z.w" | ".z.x" | ".z.z" | ".z.Z" | ".z.n" | ".z.N" | ".z.p" | ".z.P"
      
%%

<YYINITIAL> {
  {StartingComment}        { return token(TokenType.COMMENT); }
  .|\n                     { yypushback(1); yybegin(YYMAIN); }
}
  
<YYMAIN> {

  {SlashCall} { return token(TokenType.ERROR); }	
  
  /* keywords */
  {dotzWord}                  { return token(TokenType.KEYWORD2); }
  {dotQWord}                  { return token(TokenType.KEYWORD2); }
  {qWord}                     { return token(TokenType.KEYWORD2); }
  {Reserved}                  { return token(TokenType.KEYWORD); }

  /* operators */

  
  "("                            { return token(TokenType.OPERATOR,  PARAN); }
  ")"                            { return token(TokenType.OPERATOR, -PARAN); }
  "{"                            { return token(TokenType.OPERATOR,  CURLY); }
  "}"                            { return token(TokenType.OPERATOR, -CURLY); }
  "["                            { return token(TokenType.OPERATOR,  BRACKET); }
  "]"                            { return token(TokenType.OPERATOR, -BRACKET); }
  ";"                            | 
  ","                            | 
  "."                            | 
  "="                            | 
  ">"                            | 
  "<"                            |
  "~"                            | 
  "?"                            | 
  ":"                            | 
  "<="                           | 
  ">="                           | 
  "!="                           | 
  "&"                           | 
  "|"                           | 
  "+"                            | 
  "/:"                           | 
  "\:"                            | 
  "-"                            | 
  "*"                            | 
  "/"                            | 
  "\\"                            | 
  "&"                            | 
  "|"                            | 
  "^"                            | 
  "%"                            | 
  "+:"                           | 
  "-:"                           | 
  "*:"                           | 
  "%:"                           | 
  "!:"                           { return token(TokenType.OPERATOR); } 

  /* string literal */
  \"                             {
                                    yybegin(DQ_STRING);
                                    tokenStart = yychar;
                                    tokenLength = 1;
                                 }
//  \'                             {
//                                    yybegin(SQ_STRING);
//                                    tokenStart = yychar;
//                                    tokenLength = 1;
//                                 }

  "boolean" | "byte" | "short" | "int" | "long" | "real" | "float" | 
  "char" | "symbol" | "month" | "date" | "datetime" | "minute" | 
  "second" | "time" | "enum" | "table" | "dictionary"  { return token(TokenType.TYPE); }								 
				

  /* numeric literals */

  {DecIntegerLiteral}            |

  {FloatLiteral}                 { return token(TokenType.NUMBER); }

  /* comments */
  {Comment}                      { return token(TokenType.COMMENT); }

  /* whitespace */
  {WhiteSpace}                 { /* skip */ }

  /* identifiers */
  {Identifier}                   { return token(TokenType.IDENTIFIER); }

}

<DQ_STRING> {
  {StringCharacter}+             { tokenLength += yylength(); }
  \"\"                           { tokenLength += 2; }
  \\.                            { tokenLength += 2; }
  {LineTerminator}               { yybegin(YYMAIN);  }
  \"                             {
                                     yybegin(YYMAIN);
                                     // length also includes the trailing quote
                                     return token(TokenType.STRING, tokenStart, tokenLength + 1);
                                 }
}

// <SQ_STRING> {
  // {SingleCharacter}+             { tokenLength += yylength(); }
  // \'\'                           { tokenLength += 2; }
  // \\.                            { tokenLength += 2; }
  // {LineTerminator}               { yybegin(YYMAIN);  }
  // \'                             {
                                     // yybegin(YYMAIN);
                                     //length also includes the trailing quote
                                     // return token(TokenType.STRING, tokenStart, tokenLength + 1);
                                 // }
// }

/* error fallback */
.|\n                             {  }
<<EOF>>                          { return null; }

