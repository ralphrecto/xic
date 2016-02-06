package mjw297;

import java_cup.runtime.*;

%%

%cupsym Sym
%cup

%class Lexer
%unicode
%line
%column

%{
%}

%%

[^] { throw new Error("Illegal character <"+ yytext()+">"); }
