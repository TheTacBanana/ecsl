%%

// "FSFS"

\; "SEMICOLON"
: "COLON"
, "COMMA"
\. "DOT"
& "AMPERSAND"

use "USE"
let "LET"
= "EQ"
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

[a-zA-Z_][a-zA-Z0-9_]* "IDENT"
[0-9]+ "INT"
"(?:\\\\|\\"|[^"\n])*" "STRING"
(false|true) "BOOLEAN"
'([^\t\n\r\f\\'\\]|\\[tnfr'"\\])' "CHARACTER"
[\t ]+ ;