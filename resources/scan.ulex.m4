ifdef(`PROGLR_TOKEN_STR', , `define(`PROGLR_TOKEN_STR', `Token')')

%defs (
  open PROGLR_TOKEN_STR
  type lex_result = PROGLR_TOKEN_STR.token
  val eof = fn () => PROGLR_TOKEN_STR.EOF
  fun unescape s = case (String.fromCString s) of NONE => s | SOME s' => s'
);

%name Lexer;
%states INITIAL IN_CHAR IN_STRING IN_COMMENT;

%let digit = [0-9];
%let char = [\u0020-\u007e];
%let letter = [a-zA-Z\u00c0-\u00ff] & [^\u00d7\u00f7];
%let space = [ \t\r\n];
%let apos = ['];
%let quot = ["];
%let backslash = [\\];

ifdef(`PROGLR_USE_INTEGER', `
(* Integer of integers, defined digit+ *)
<INITIAL> {digit}+ => (Integer (Option.valOf (Int.fromString (yytext))));
')

ifdef(`PROGLR_USE_DOUBLE', `
(* Double of floating point numbers, defined digit+ ’.’ digit+ (’e’ ’-’? digit+)? *)
<INITIAL> {digit}+ "." {digit}+ ("e" "-"? {digit}+)? => (Double (Option.valOf (Real.fromString (yytext))));
')

ifdef(`PROGLR_USE_CHAR', `
(* Char of characters (in single quotes), defined ’\’’ ((char - ["’\\"]) | (’\\’ ["’\\nt"])) ’\’’ *)
<INITIAL> {apos} => (YYBEGIN IN_CHAR; continue ());
<IN_CHAR> ( ({char} & [^\u0027\\]) | ({backslash}[\u0027\\nt]) ) => (Char (Option.valOf (Char.fromCString yytext)));
<IN_CHAR> {apos} => (YYBEGIN INITIAL; continue ());
')

ifdef(`PROGLR_USE_STRING', `
(* String of strings (in double quotes), defined ’"’ ((char - ["\"\\"]) | (’\\’ ["\"\\nt"]))* ’"’ *)
<INITIAL> {quot}{2} => (String "");
<INITIAL> {quot} => (YYBEGIN IN_STRING; continue ());
<IN_STRING> ( ({char} & [^"\\]) | ({backslash}["\\nt]) )+ => (String (unescape yytext)); 
<IN_STRING> {quot} => (YYBEGIN INITIAL; continue ());
')

ifdef(`PROGLR_USE_IDENT', `
(* Ident of identifiers, defined letter (letter | digit | ’_’ | ’\’’)* *)
<INITIAL> {letter} ({letter} | {digit} | "_" | "\u0027")* => (Ident yytext);
')

ifdef(`PROGLR_BLOCK_COMMENT_OPEN', `
<INITIAL> PROGLR_BLOCK_COMMENT_OPEN => (YYBEGIN IN_COMMENT; continue ());
<IN_COMMENT> PROGLR_BLOCK_COMMENT_CLOSE => (YYBEGIN INITIAL; continue ());
<IN_COMMENT> . => (continue ());
')

ifdef(`PROGLR_LINE_COMMENT', `
<INITIAL> PROGLR_LINE_COMMENT [^\n]* [\n] => (continue ());
')

define(`keywords',`ifelse($2,,,`<INITIAL> "$2" => ($1);
keywords(shift(shift($@)))')')

keywords(PROGLR_KEYWORDS)

<INITIAL> {space}+ => (continue ());
