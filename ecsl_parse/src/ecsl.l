%%

// "FSFS"

\; "SEMI"
: "COLON"
, "COMMA"
\.\. "DOTDOT"
\.\.= "DOTDOTEQ"
\. "DOT"
& "AMPERSAND"

use "USE"
:: "PATH"
super "SUPER"

let "LET"
mut "MUT"
imm "IMM"
= "ASSIGN"
as "AS"

struct "STRUCT"
enum "ENUM"
comp "COMP"
fn "FN"
sys "SYS"
return "RETURN"
impl "IMPL"

while "WHILE"
for "FOR"
if "IF"
else "ELSE"
match "MATCH"
in "IN"
break "BREAK"
continue "CONTINUE"

\( "LBRACKET"
\) "RBRACKET"
\{ "LCURLY"
\} "RCURLY"
\[ "LSQUARE"
\] "RSQUARE"

\+ "PLUS"
\- "MINUS"
\* "STAR"
/ "FSLASH"

&& "AND"
|| "OR"
! "NOT"

\< "LT"
> "GT"
\<= "LE"
>= "GE"
== "EQEQ"
!= "NOTEQ"

\| "PIPE"
# "HASH"
\-> "ARROW"

(false|true) "BOOLEAN"
[a-zA-Z_][a-zA-Z0-9_]* "IDENT"
[0-9]+\.[0-9]* "FLOAT"
[0-9]+ "INT"
"(?:\\\\|\\"|[^"\n])*" "STRING"
'([^\t\n\r\f\\'\\]|\\[tnfr'"\\])' "CHAR"
[\t\n\r ]+ ;