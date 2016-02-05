import java_cup.runtime.*;
import mjw297.Sym;

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
