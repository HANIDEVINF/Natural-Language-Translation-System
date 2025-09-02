%{
    #include <stdio.h>
    #include <string.h>
    #include <stdbool.h>
    #include "syn.tab.h"
    #include "ts.h"
    #include "ts_quad.h"
    extern int N ;
    extern int C ;

%}
chiffre [0-9]
lettre [a-zA-Z]
IDF {lettre}({lettre}|{chiffre}|[_]?)*
CST_INTEGER [-+]*{chiffre}*
CST_FLOAT [-+]*{chiffre}*"."{chiffre}*
CHAINE \'(\\.|[^"^\n\\]*)*\'
com_1 "//".*
com_p "/*"([^*]|\*+[^*/])*\*+"/"


%%
"PROGRAMME" {   rechercher(yytext,"Mot Cle"," ",0,1) ;
                printf("L'entite lexicale reconnue est MOT CLE : %s \n", yytext);
                C = C + strlen(yytext);
                return mc_prog ;
            }
"VAR"   {       rechercher(yytext,"Mot Cle"," ",0,1) ;
                printf("L'entite lexicale reconnue est MOT CLE : %s \n", yytext);
                C = C + strlen(yytext);
                return mc_var;
        }
"CONST"     {   rechercher(yytext,"Mot Cle"," ",0,1) ;
                printf("L'entite lexicale reconnue est MOT CLE : %s \n", yytext);
                C = C + strlen(yytext); 
                return mc_cst;
            }
"BEGIN"     {   rechercher(yytext,"Mot Cle"," ",0,1) ;
                printf("L'entite lexicale reconnue est MOT CLE : %s \n", yytext);
                C = C + strlen(yytext);
                return mc_begin;
                }
"END."      {   rechercher(yytext,"Mot Cle"," ",0,1) ;
                printf("L'entite lexicale reconnue est MOT CLE : %s \n", yytext);
                C = C + strlen(yytext);
                return mc_endp;
            }
"END"       {   rechercher(yytext,"Mot Cle"," ",0,1) ;
                printf("L'entite lexicale reconnue est MOT CLE : s \n", yytext);
                C = C + strlen(yytext);
                return mc_end;
            }
"INTEGER"  {   rechercher(yytext,"Mot Cle"," ",0,1) ;
                printf("L'entite lexicale reconnue est MOT CLE : %s \n", yytext);
                C = C + strlen(yytext);
                return mc_integer;
            }
"REAL"     {   rechercher(yytext,"Mot Cle"," ",0,1) ;
                printf("L'entite lexicale reconnue est MOT CLE : %s \n", yytext);
                C = C + strlen(yytext);
                return mc_float;
            }
"STRING"    {   rechercher(yytext,"Mot Cle"," ",0,1) ;
                printf("L'entite lexicale reconnue est MOT CLE : %s \n", yytext);
                C = C + strlen(yytext);
                return mc_string;
            }
"IF"        {   rechercher(yytext,"Mot Cle"," ",0,1) ;
                printf("L'entite lexicale reconnue est MOT CLE : %s \n", yytext);
                C = C + strlen(yytext);
                return mc_if;
            }
"ELSE"      {   rechercher(yytext,"Mot Cle"," ",0,1) ;
                printf("L'entite lexicale reconnue est MOT CLE : %s \n", yytext);
                C = C + strlen(yytext);
                return mc_else;
                }
"WHILE"     {   rechercher(yytext,"Mot Cle"," ",0,1) ;
                printf("L'entite lexicale reconnue est MOT CLE : %s \n", yytext);
                C = C + strlen(yytext);
                return mc_while;
            }
"FOR"       {   rechercher(yytext,"Mot Cle"," ",0,1) ;
                printf("L'entite lexicale reconnue est MOT CLE : %s \n", yytext);
                C = C + strlen(yytext);
                return mc_for;
            }
"DO"        {   rechercher(yytext,"Mot Cle"," ",0,1) ;
                printf("L'entite lexicale reconnue est MOT CLE : %s \n", yytext);
                C = C + strlen(yytext);
                return mc_do;
            }
"writeln"   {   rechercher(yytext,"Mot Cle"," ",0,1) ;
                printf("L'entite lexicale reconnue est MOT CLE : %s \n", yytext);
                C = C + strlen(yytext);
                return mc_writeln;
            }
"readln"    {   rechercher(yytext,"Mot Cle"," ",0,1) ;
                printf("L'entite lexicale reconnue est MOT CLE : %s \n", yytext);
                C = C + strlen(yytext);
                return mc_readln;
            }
"<"         {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return inf;
            }
"<="        {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return inf_eg;
            }
">"         {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return sup;
            }
">="        {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return sup_eg;
            }
"=="        {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return eg;
            }
"!="        {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return diff;
            }
"+"         {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return plus;
            }
"-"         {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return moin;
            }
"*"         {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return foi;
            }
"/"         {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return division;
            }
","         {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return vg;
            }
";"         {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return pvg;
            }
"="         {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return aff;
            }
"{"         {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return ao;
            }
"}"         {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return af;
            }
"("         {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return po;
            }
")"         {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return pf;
            }
":"         {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return deuxp;
            }
"&&"        {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return et_log;
            }
"||"        {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return ou_log;
            }
"!"         {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return non_log;
            }
"["         {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return co;
            }
"]"         {   rechercher(yytext,"separateur"," ",0,2) ;
                printf("L'entite lexicale reconnue est SEPARATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                return cf;
            }
{IDF}       {   rechercher(yytext,"identificateur"," ",0,0) ;
                printf("L'entite lexicale reconnue est IDENTIFICATEUR : %s \n", yytext);
                C = C + strlen(yytext);
                yylval.string=strdup(yytext);
                return idf;
            }
{CST_INTEGER}   {   rechercher(yytext,"constante","INTEGER",atoi(yytext),0) ;
                printf("L'entite lexicale reconnue est ENTIER : %s \n", yytext);
                C = C + strlen(yytext);
                yylval.INTEGER=atoi(yytext);
                return cst_e;
            }
{CST_FLOAT}      {   rechercher(yytext,"constante","REAL",atof(yytext),0) ;
                printf("L'entite lexicale reconnue est REEL : %s \n", yytext);
                C = C + strlen(yytext);
                yylval.REAL=atof(yytext);
                return cst_r;
            }
{CHAINE}    {   rechercher(yytext,"constante","STRING",atof(yytext),0) ;
                printf("L'entite lexicale reconnue est CHAINE DE CARACTERE : %s \n", yytext);
                C = C + strlen(yytext);
                yylval.string=strdup(yytext);
                return chaine;
            }
{com_1}     {   rechercher(yytext,"commentaire"," ",atof(yytext),0) ;
                printf("L'entite lexicale reconnue est une commentaire d'une ligne  : %s \n", yytext);
                C = C + strlen(yytext);
                yylval.string=strdup(yytext);
                return c1;
            }
{com_p}     {   rechercher(yytext,"commentaire"," ",atof(yytext),0) ;
                printf("L'entite lexicale reconnue est un commentaire de plusieur ligne : %s \n", yytext);
                C = C + strlen(yytext);
                yylval.string=strdup(yytext);
                return cp;
            }

\n      {N ++; C = 1 ;}
[ \t]       C =  C + strlen(yytext);

.  {printf("****caractere ivalide a la ligne %d et colonne %d ***** \n", N , C); return 1 ;} 
