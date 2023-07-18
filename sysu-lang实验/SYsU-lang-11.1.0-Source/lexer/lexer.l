%{
#include <cctype>
#include <cstdio>
#include <string>
#define YYEOF 0
int yylex();

int flag1=1;
int flag2=0;

std::string mess1 = "\t";
std::string mess2 = "\t";

int main() {
  do {

  } while (yylex() != YYEOF);
}
std::string yyloc = "<stdin>";

void fun(){
  mess1 = "\t";
  mess2 = "\t";
  if(flag1==1) {
    mess1="[StartOfLine] ";
    flag1=0;
  }
  if(flag2==1) {
    mess2="[LeadingSpace] ";
    flag2=0;
  }
}

int yyrow = 1, yycolumn = 1, yycolpre = 1;
#define YY_USER_ACTION                                                         \
  do {                                                                         \
    yycolumn += yyleng;                                                        \
  } while (0);
%}
%option noyywrap
%%
#.* {
  std::string s(yytext);
  auto l = s.find("\""), r = s.rfind("\"");
  yyloc = s.substr(l + 1, r - l - 1);
  for (int i = 0; i < s.size(); ++i)
    if (std::isdigit(s[i])) {
      for (yyrow = 0; i < s.size() && std::isdigit(s[i]); ++i)
        yyrow = yyrow * 10 + s[i] - '0';
      --yyrow;
      break;
    }
}
\n {
  ++yyrow;
  yycolpre = yycolumn;
  yycolumn = 1;
  flag1=1;
}
[\t ]+ {
  flag2=1;
}
[\f\r\t\v\ ] {
  flag2=1;
}
int {
  fun();
  std::fprintf(yyout, "int '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
               yyrow, yycolumn - yyleng);

  return ~YYEOF;
}

float {
  fun();
  std::fprintf(yyout, "float '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
char {
  fun();
  std::fprintf(yyout, "char '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
long {
  fun();
  std::fprintf(yyout, "long '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}

return {
  fun();
  std::fprintf(yyout, "return '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\( {
  fun();
  std::fprintf(yyout, "l_paren '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\) {
  fun();
  std::fprintf(yyout, "r_paren '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\{ {
  fun();
  std::fprintf(yyout, "l_brace '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\} {
  fun();
  std::fprintf(yyout, "r_brace '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
; {
  fun();
  std::fprintf(yyout, "semi '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}

0[xX][0-9a-fA-F]+ {
  fun();
  std::fprintf(yyout, "numeric_constant '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),
               yyloc.c_str(), yyrow, yycolumn - yyleng);
  return ~YYEOF;
}

\.[0-9]+ {
  fun();
  std::fprintf(yyout, "numeric_constant '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),
               yyloc.c_str(), yyrow, yycolumn - yyleng);
  return ~YYEOF;
}



0[xX]([0-9a-fA-F]+)?(\.[0-9a-fA-F]+)?([pP][+-]?[0-9]+)? {
  fun();
  std::fprintf(yyout, "numeric_constant '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),
               yyloc.c_str(), yyrow, yycolumn - yyleng);
  return ~YYEOF;
}

[0-9]+(\.?[0-9]+)? {
  fun();
  std::fprintf(yyout, "numeric_constant '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),
               yyloc.c_str(), yyrow, yycolumn - yyleng);
  return ~YYEOF;
}

[0-9]+(?:\.[0-9]+)?[eE][+-]?[0-9]+ {
  fun();
  std::fprintf(yyout, "numeric_constant '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),
               yyloc.c_str(), yyrow, yycolumn - yyleng);
  return ~YYEOF;
}

[0-9]+\. {
  fun();
  std::fprintf(yyout, "numeric_constant '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),
               yyloc.c_str(), yyrow, yycolumn - yyleng);
  return ~YYEOF;
}


\.[0-9]+[eE][+-]?[0-9]+ {
  fun();
  std::fprintf(yyout, "numeric_constant '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),
               yyloc.c_str(), yyrow, yycolumn - yyleng);
  return ~YYEOF;
}




const {
  fun();
  std::fprintf(yyout, "const '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
if {
  fun();
  std::fprintf(yyout, "if '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
void {
  fun();
  std::fprintf(yyout, "void '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
while {
  fun();
  std::fprintf(yyout, "while '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
break {
  fun();
  std::fprintf(yyout, "break '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
else {
  fun();
  std::fprintf(yyout, "else '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
continue {
  fun();
  std::fprintf(yyout, "continue '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
do {
  fun();
  std::fprintf(yyout, "do '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}

[a-zA-Z_][a-zA-Z_0-9]* {
  fun();
  std::fprintf(yyout, "identifier '%s' %s Loc=<%s:%d:%d>\n", yytext,(mess1+mess2).c_str(),
               yyloc.c_str(), yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
<<EOF>> {
  std::fprintf(yyout, "eof ''\t\tLoc=<%s:%d:%d>\n", yyloc.c_str(), yyrow - 1,
               yycolpre - yyleng);
  return YYEOF;
}



\/ {
  fun();
  std::fprintf(yyout, "slash '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}


\[ {
  fun();
  std::fprintf(yyout, "l_square '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\] {
  fun();
  std::fprintf(yyout, "r_square '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}

\=\= {
  fun();
  std::fprintf(yyout, "equalequal '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}

\!\= {
  fun();
  std::fprintf(yyout, "exclaimequal '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}

\! {
  fun();
  std::fprintf(yyout, "exclaim '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}

\= {
  fun();
  std::fprintf(yyout, "equal '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}


\>\= {
  fun();
  std::fprintf(yyout, "greaterequal '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\<\= {
  fun();
  std::fprintf(yyout, "lessequal '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\> {
  fun();
  std::fprintf(yyout, "greater '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\< {
  fun();
  std::fprintf(yyout, "less '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
&& {
  fun();
  std::fprintf(yyout, "ampamp '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\|\| {
  fun();
  std::fprintf(yyout, "pipepipe '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}

, {
  fun();
  std::fprintf(yyout, "comma '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}

\+ {
  fun();
  std::fprintf(yyout, "plus '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\* {
  fun();
  std::fprintf(yyout, "star '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\- {
  fun();
  std::fprintf(yyout, "minus '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}
\% {
  fun();
  std::fprintf(yyout, "percent '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
       yyrow, yycolumn - yyleng);
  return ~YYEOF;
}

\.\.\. {
  fun();
  std::fprintf(yyout, "ellipsis '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
  yyrow, yycolumn - yyleng);
  return ~YYEOF;
}


\"([^\"\\\n]|(\\.))*\" {
  fun();
  std::fprintf(yyout, "string_literal '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
  yyrow, yycolumn - yyleng);
  return ~YYEOF;
}



. {
  fun();
  std::fprintf(yyout, "unknown '%s' %s Loc=<%s:%d:%d>\n", yytext, (mess1+mess2).c_str(),yyloc.c_str(),
  yyrow, yycolumn - yyleng);
  return ~YYEOF;
}




%%

