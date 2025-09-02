%{
    #include <stdio.h>
    #include <string.h>
    #include <stdbool.h>
    int cpt = 0 ;
    bool n1 = false , n2 = false ;
    int t1 , t2;
    char i1 [20] , i2 [20] ;
    float f1 , f2 ;
    int N = 1 ;
    int C = 1 ;
    int Fin_if=0,deb_else=0;
    int qc=0 , tqc , tqcidf ,fin_while , fin_for;
    char tmp [20] , tmpidf [20] , tmp2 [20] , pas [20] , tmp3 [20] , tmp_for [20];
    char sauvType [20];
    int yylex();
    int yyerror(const char *s) {
        fprintf(stderr, "**** Erreur de syntaxe a la ligne %d : %s **** \n",N , s);
        return 0; 
    }
    
    void initialisation();
    void afficher();
    void rechercher(const char* entite, const char* code, const char* type, float val, int categorie);
    void insererType(const char* name, const char* type);
    int rechercheNonDeclare(const char* name);
    int CompType(const char* name, const char* type);
    int VerifIdfConst(char entite[]);
    char* retournType(char entite[]);
    void CodeCst (char entite []);
    void quadr(char opr[],char op1[],char op2[],char res[]);
    void ajour_quad(int num_quad, int colon_quad, char val []);
    void afficher_qdr();
%}


%union {
    int INTEGER;
    float REAL ;
    char* string ;
    bool boleen ;
}


%token  <string>mc_prog      <string>mc_var     <string>mc_cst    <string>mc_begin      <string>mc_endp     <string>mc_end      <string>mc_if     <string>mc_else    <string>mc_for 
        <string>mc_do        <string>mc_while   <string>idf       inf                   inf_eg              sup              sup_eg 
        eg           diff      aff       pvg        plus     moin 
        foi          division   deuxp     ao            af         po        pf         <INTEGER>cst_e    <INTEGER>cst_r 
        <string>chaine       <REAL>mc_float   <INTEGER>mc_integer   <string>mc_string            vg               <string>mc_writeln          
        <string>mc_readln        et_log    ou_log  non_log                    c1           cp     co     cf

%left foi division 
%left plus moin 
%left inf inf_eg sup sup_eg eg diff

%type <string>EXP

%start S
%%

S : mc_prog idf mc_var ao VARIABLES af mc_begin INSTRUCTIONS mc_endp  { printf("\n Le programme est correcte . \n"); YYACCEPT; }
  | mc_prog idf mc_begin INSTRUCTIONS mc_endp  { printf("\n Le programme est correcte . \n"); YYACCEPT; }

;

VARIABLES :  COMMENTAIRE TYPES LISTVAR pvg COMMENTAIRE VARIABLES
          | COMMENTAIRE mc_cst LISTCONST pvg COMMENTAIRE VARIABLES
          |

;
TYPES : mc_integer {strcpy(sauvType,"INTEGER");}
     | mc_float {strcpy(sauvType,"REAL");}
     | mc_string {strcpy(sauvType,"STRING");}

;
LISTVAR : idf aff cst_e vg LISTVAR
        {
			if (rechercheNonDeclare($1)==0) {insererType($1,sauvType);}
			else {printf("****Erreur semantique 'double declaration' a la ligne %d et la colonne %d,la variable %s est deja declaree ****\n", N,C, $1); return 1 ;}
            if (CompType($1,"INTEGER")==0) {printf("****Erreur semantique a la ligne %d et la colonne %d : ICOMPATIBILITE DE TYPE de la variable %s ****\n", N,C, $1); return 1 ;}
		}
        | idf aff cst_e 
        {
            if (rechercheNonDeclare($1)==0) {insererType($1,sauvType);}
			else {printf("****Erreur semantique 'double declaration' a la ligne %d et la colonne %d,la variable %s est deja declaree ****\n", N,C, $1); return 1 ;}
            if (CompType($1,"INTEGER")==0) {printf("****Erreur semantique a la ligne %d et la colonne %d : ICOMPATIBILITE DE TYPE de la variable %s ****\n", N,C, $1); return 1 ;}
        }     
        | idf vg LISTVAR
        {
			if (rechercheNonDeclare($1)==0) {insererType($1,sauvType);}
			else {printf("****Erreur semantique 'double declaration' a la ligne %d et la colonne %d,la variable %s est deja declaree ****\n", N,C, $1); return 1 ;}
            if(CompType($1,sauvType) == 0){printf("****Erreur semantique a la ligne %d et la colonne %d : ICOMPATIBILITE DE TYPE de la variable %s ****\n", N,C, $1); return 1 ;}
		}
        | idf
        {
			if (rechercheNonDeclare($1)==0) {insererType($1,sauvType);}
			else {printf("****Erreur semantique 'double declaration' a la ligne %d et la colonne %d,la variable %s est deja declaree ****\n", N,C, $1); return 1 ;}
            if(CompType($1,sauvType) == 0){printf("****Erreur semantique a la ligne %d et la colonne %d : ICOMPATIBILITE DE TYPE de la variable %s ****\n", N,C, $1); return 1 ;}
		}
        | idf aff cst_r vg LISTVAR
        {
			if (rechercheNonDeclare($1)==0) {insererType($1,sauvType);}
			else {printf("****Erreur semantique 'double declaration' a la ligne %d et la colonne %d,la variable %s est deja declaree ****\n", N,C, $1); return 1 ;}
            if (CompType($1,"REAL")==0) {printf("****Erreur semantique a la ligne %d et la colonne %d : ICOMPATIBILITE DE TYPE de la variable %s ****\n", N,C, $1); return 1 ;}
		}
        | idf aff cst_r 
        {
			if (rechercheNonDeclare($1)==0) {insererType($1,sauvType);}
			else {printf("****Erreur semantique 'double declaration' a la ligne %d et la colonne %d,la variable %s est deja declaree ****\n", N,C, $1); return 1 ;}
            if (CompType($1,"REAL")==0) {printf("****Erreur semantique a la ligne %d et la colonne %d : ICOMPATIBILITE DE TYPE de la variable %s ****\n", N,C, $1); return 1 ;}
		}
        | idf aff chaine vg LISTVAR
        {
			if (rechercheNonDeclare($1)==0) {insererType($1,sauvType);}
			else {printf("****Erreur semantique 'double declaration' a la ligne %d et la colonne %d,la variable %s est deja declaree ****\n", N,C, $1); return 1 ;}
            if (CompType($1,"STRING")==0) {printf("****Erreur semantique a la ligne %d et la colonne %d : ICOMPATIBILITE DE TYPE de la variable %s ****\n", N,C, $1); return 1 ;}
		}
        | idf aff chaine   
        {
			if (rechercheNonDeclare($1)==0) {insererType($1,sauvType);}
			else {printf("****Erreur semantique 'double declaration' a la ligne %d et la colonne %d,la variable %s est deja declaree ****\n", N,C, $1); return 1 ;}
            if (CompType($1,sauvType)==0) {printf("****Erreur semantique a la ligne %d et la colonne %d : ICOMPATIBILITE DE TYPE de la variable %s ****\n", N,C, $1); return 1 ;}
		}
        | idf co cst_e cf vg LISTVAR 
        {
			if (rechercheNonDeclare($1)==0) {insererType($1,sauvType);}
			else {printf("****Erreur semantique 'double declaration' a la ligne %d et la colonne %d,la variable %s est deja declaree ****\n", N,C, $1); return 1 ;}
            if($3 < 0){printf("****Erreur semantique a la ligne %d et la colonne %d,la taille de tableau ne peut pas etre negative  ****\n", N,C, $1); return 1 ;}
            if (CompType($1,sauvType)==0) {printf("****Erreur semantique a la ligne %d et la colonne %d : ICOMPATIBILITE DE TYPE de la variable %s ****\n", N,C, $1); return 1 ;}

		} 
        | idf co cst_e cf 
        {
			if (rechercheNonDeclare($1)==0) {insererType($1,sauvType);}
			else {printf("****Erreur semantique 'double declaration' a la ligne %d et la colonne %d,la variable %s est deja declaree ****\n", N,C, $1); return 1 ;}
            if($3 < 0){printf("****Erreur semantique a la ligne %d et la colonne %d,la taille de tableau ne peut pas etre negative  ****\n", N,C, $1); return 1 ;}
            if (CompType($1,sauvType)==0) {printf("****Erreur semantique a la ligne %d et la colonne %d : ICOMPATIBILITE DE TYPE de la variable %s ****\n", N,C, $1); return 1 ;}
		} 

;
LISTCONST : idf aff cst_e vg { 
            if (rechercheNonDeclare($1)==0) {insererType($1,sauvType); CodeCst($1);}
			else {printf("****Erreur semantique 'double declaration' a la ligne %d et la colonne %d,la variable %s est deja declaree ****\n", N,C, $1); return 1 ;} }
            LISTCONST 
         | idf aff cst_e {
            if (rechercheNonDeclare($1)==0) {insererType($1,sauvType); CodeCst($1);}
			else {printf("****Erreur semantique 'double declaration' a la ligne %d et la colonne %d,la variable %s est deja declaree ****\n", N,C, $1); return 1 ;}}
         | idf aff cst_r vg { 
            if (rechercheNonDeclare($1)==0) {insererType($1,sauvType); CodeCst($1);}
			else {printf("****Erreur semantique 'double declaration' a la ligne %d et la colonne %d,la variable %s est deja declaree ****\n", N,C, $1); return 1 ;} }
         LISTCONST
         | idf aff cst_r { 
            if (rechercheNonDeclare($1)==0) {insererType($1,sauvType); CodeCst($1);}
			else {printf("****Erreur semantique 'double declaration' a la ligne %d et la colonne %d,la variable %s est deja declaree ****\n", N,C, $1); return 1 ;} }
         | idf aff chaine vg { 
            if (rechercheNonDeclare($1)==0) {insererType($1,sauvType); CodeCst($1);}
			else {printf("****Erreur semantique 'double declaration' a la ligne %d et la colonne %d,la variable %s est deja declaree ****\n", N,C, $1); return 1 ;} } 
         LISTCONST
         | idf aff chaine { 
            if (rechercheNonDeclare($1)==0) {insererType($1,sauvType); CodeCst($1);}
			else {printf("****Erreur semantique 'double declaration' a la ligne %d et la colonne %d,la variable %s est deja declaree ****\n", N,C, $1); return 1 ;} }

;
INSTRUCTIONS : COMMENTAIRE idf {sprintf(tmpidf,"%s",$2) ;} aff EXP pvg COMMENTAIRE 
             {
                if (rechercheNonDeclare($2) == 0)
                {
                    printf("****Erreur semantique a la ligne %d et la colonne %d , variable %s non declaree **** ", N , C , $2);
                    return 1 ;
                }

                if(VerifIdfConst($2) == 1){
                    printf("****Erreur semantique a la ligne %d et la colonne %d , identificateur %s est constant **** ", N , C , $2);
                    return 1;
                }

                if((strcmp($2,"INTEGER") == 0 && strcmp($4,"INTEGER") == 0) || (strcmp($2,"REAL") == 0 && strcmp($4,"REAL") == 0) || (strcmp($2,"string") == 0 && strcmp($4,"string") == 0)){
                   
                }else { printf("****Erreur semantique a la ligne %d et la colonne %d : ICOMPATIBILITE DE TYPE de la variable %s ****\n", N,C, $2); return 1 ; }
                    tqcidf = qc ;
                    quadr("=" ,"","",tmpidf);
                    sprintf(tmp,"t%d",cpt-1);
                    ajour_quad(tqcidf,1,tmp);

             }
             INSTRUCTIONS 
             |  COMMENTAIRE INST_IF  COMMENTAIRE INSTRUCTIONS
             |  COMMENTAIRE INST_FOR COMMENTAIRE INSTRUCTIONS
             |  COMMENTAIRE INST_WHILE  COMMENTAIRE INSTRUCTIONS 
             |  COMMENTAIRE INST_DO COMMENTAIRE INSTRUCTIONS
             | COMMENTAIRE mc_writeln po chaine pf pvg  COMMENTAIRE INSTRUCTIONS
             |  COMMENTAIRE mc_writeln po idf pf pvg COMMENTAIRE INSTRUCTIONS
             |  COMMENTAIRE mc_readln po chaine pf pvg COMMENTAIRE INSTRUCTIONS
             |  COMMENTAIRE mc_readln po idf pf pvg COMMENTAIRE INSTRUCTIONS
             | 

;
INST_IF : B mc_else ao INSTRUCTIONS af {
                                           sprintf(tmp,"%d",qc);  
                                           ajour_quad(Fin_if,1,tmp);
                                         }

;
B: A ao INSTRUCTIONS af {   
                         Fin_if=qc;
                         quadr("BR", "","vide", "vide"); 
                         sprintf(tmp,"%d",qc); 
                         ajour_quad(deb_else,1,tmp);
                        }

;
A: mc_if P CONDITION P { 
                         deb_else=qc; 
                         sprintf(tmp,"t%d",cpt-1);
                         quadr("BZ", "" ,tmp, "vide"); 
                       }

;
INST_FOR: mc_for P idf  
        { if (rechercheNonDeclare($3) == 0)
            {
                printf("****Erreur semantique a la ligne %d et la colonne %d , variable %s non declaree **** ", N , C , $3);
                return 1 ;
            }
            sprintf(tmp_for,"%s",$3);
        } 
          deuxp cst_e { sprintf(tmp2,"%d",$6); 
                        quadr("=",tmp2,"",tmp_for); } 
          deuxp cst_e deuxp idf {   if (rechercheNonDeclare($3) == 0)
                                    {
                                        printf("****Erreur semantique a la ligne %d et la colonne %d , variable %s non declaree **** ", N , C , $3);
                                        return 1 ;
                                    }
                                    sprintf(pas,"%d",$9);
                                    sprintf(tmp2,"%s",$11);
                                    sprintf(tmp3,"t%d",cpt);
                                    quadr("<",tmp_for,tmp2,tmp3);
                                    fin_for = qc ;
                                    quadr("BZ","",tmp3,"");

        }
          P ao INSTRUCTIONS af { quadr("+",tmp_for,pas,tmp_for);
                                 sprintf(tmp,"%d",qc);
                                 ajour_quad(fin_for,1,tmp);
                               }

;
INST_WHILE: K ao INSTRUCTIONS af {  sprintf(tmp,"%d",qc);
                                    ajour_quad(fin_while,1,tmp); }

;

K: mc_while P CONDITION P {  fin_while = qc ;
                             sprintf(tmp,"t%d",cpt-1);
                             quadr("BZ","",tmp,"");
                          }

;
INST_DO: mc_do mc_begin INSTRUCTIONS mc_end mc_while po CONDITION pf pvg

;
CONDITION: P EXP inf EXP P{    
                        if(strcmp($2,"INTEGER") == 0 && strcmp($4,"INTEGER") == 0 || strcmp($2,"REAL") == 0 && strcmp($4,"REAL") == 0 || strcmp($2,"string") == 0 && strcmp($4,"string") == 0){
                        }else {
                           printf("****Erreur semantique a la ligne %d et la colonne %d : ICOMPATIBILITE DE TYPE ****\n", N,C); return 1 ;
                        }
                    }
         | P EXP inf_eg EXP P {    
                        if(strcmp($2,"INTEGER") == 0 && strcmp($4,"INTEGER") == 0 || strcmp($2,"REAL") == 0 && strcmp($4,"REAL") == 0 || strcmp($2,"string") == 0 && strcmp($4,"string") == 0){
                            
                        }else {
                            printf("****Erreur semantique a la ligne %d et la colonne %d : ICOMPATIBILITE DE TYPE ****\n", N,C); return 1 ;
                        }
                    }
         | P EXP sup EXP P  {    
                        if(((strcmp($2,"INTEGER") == 0) && (strcmp($4,"INTEGER") == 0)) || ((strcmp($2,"REAL") == 0) && (strcmp($4,"REAL") == 0)) || ((strcmp($2,"string") == 0) && (strcmp($4,"string") == 0))){
                            
                        }else {
                            printf("****Erreur semantique a la ligne %d et la colonne %d : ICOMPATIBILITE DE TYPE ****\n", N,C); return 1 ;
                        }
                        
                    }
         | P EXP sup_eg EXP P {    
                        if(strcmp($2,"INTEGER") == 0 && strcmp($4,"INTEGER") == 0 || strcmp($2,"REAL") == 0 && strcmp($4,"REAL") == 0 || strcmp($2,"string") == 0 && strcmp($4,"string") == 0){
                          
                        }else {
                            printf("****Erreur semantique a la ligne %d et la colonne %d : ICOMPATIBILITE DE TYPE ****\n", N,C); return 1 ;
                        }
                    }
         | P EXP eg EXP P{    
                        if(strcmp($2,"INTEGER") == 0 && strcmp($4,"INTEGER") == 0 || strcmp($2,"REAL") == 0 && strcmp($4,"REAL") == 0 || strcmp($2,"string") == 0 && strcmp($4,"string") == 0){
                           
                        }else {
                            printf("****Erreur semantique a la ligne %d et la colonne %d : ICOMPATIBILITE DE TYPE ****\n", N,C); return 1 ;
                        }
                    }
         | P EXP diff EXP P {    
                        if(strcmp($2,"INTEGER") == 0 && strcmp($4,"INTEGER") == 0 || strcmp($2,"REAL") == 0 && strcmp($4,"REAL") == 0 || strcmp($2,"string") == 0 && strcmp($4,"string") == 0){
                            
                        }else {
                            printf("****Erreur semantique a la ligne %d et la colonne %d : ICOMPATIBILITE DE TYPE ****\n", N,C); return 1 ;
                        }
                    }
         | P EXP et_log EXP P {
            ajour_quad(tqc,0,"&&");
            sprintf(tmp,"t%d",cpt);
            cpt ++;
            ajour_quad(tqc,3,tmp);
            n1 = false ;
            n2 = false ;
         }
         | P EXP ou_log EXP P {
            ajour_quad(tqc,0,"||");
            sprintf(tmp,"t%d",cpt);
            cpt ++;
            ajour_quad(tqc,3,tmp);
            n1 = false ;
            n2 = false ;
         }
         | P non_log EXP P {    
                        if(strcmp($3,"booleen") != 0){
                            printf("****Erreur semantique a la ligne %d et la colonne %d : ICOMPATIBILITE DE TYPE , la variable n'est pas booleen****\n", N,C); return 1 ;
                        }
                    }  
         | EXP sup EXP {
            ajour_quad(tqc,0,">");
            sprintf(tmp,"t%d",cpt);
            cpt ++;
            ajour_quad(tqc,3,tmp);
            n1 = false ;
            n2 = false ;
         }
         | EXP sup_eg EXP {
            ajour_quad(tqc,0,">=");
            sprintf(tmp,"t%d",cpt);
            cpt ++;
            ajour_quad(tqc,3,tmp);
            n1 = false ;
            n2 = false ;
         }
         | EXP inf EXP {
            ajour_quad(tqc,0,"<");
            sprintf(tmp,"t%d",cpt);
            cpt ++;
            ajour_quad(tqc,3,tmp);
            n1 = false ;
            n2 = false ;
         }
         | EXP inf_eg EXP {
            ajour_quad(tqc,0,"<=");
            sprintf(tmp,"t%d",cpt);
            cpt ++;
            ajour_quad(tqc,3,tmp);
            n1 = false ;
            n2 = false ;
         }
         | EXP eg EXP {
            ajour_quad(tqc,0,"==");
            sprintf(tmp,"t%d",cpt);
            cpt ++;
            ajour_quad(tqc,3,tmp);
            n1 = false ;
            n2 = false ;
         }
         | EXP diff EXP {
            ajour_quad(tqc,0,"!=");
            sprintf(tmp,"t%d",cpt);
            cpt ++;
            ajour_quad(tqc,3,tmp);
            n1 = false ;
            n2 = false ;
         }

;
EXP :  P EXP plus EXP P  {    
                        if((strcmp($2,"INTEGER") == 0 && strcmp($4,"INTEGER") == 0) || (strcmp($2,"REAL") == 0 && strcmp($4,"REAL") == 0) || (strcmp($2,"string") == 0 && strcmp($4,"string") == 0)){
                            strcpy($$,$2);
                            ajour_quad(tqc,0,"+");
                            sprintf(tmp,"t%d",cpt);
                            cpt ++;
                            ajour_quad(tqc,3,tmp);
                            n1 = false ;
                            n2 = false ;
                        }else {
                            printf("****Erreur semantique a la ligne %d et la colonne %d : ICOMPATIBILITE DE TYPE ****\n", N,C); return 1 ;
                        }
                    }
    | P EXP moin EXP P {    
                       if(strcmp($2,"INTEGER") == 0 && strcmp($4,"INTEGER") == 0 || strcmp($2,"REAL") == 0 && strcmp($4,"REAL") == 0 || strcmp($2,"string") == 0 && strcmp($4,"string") == 0){
                           strcpy($$,$2);
                           ajour_quad(tqc,0,"-");
                           sprintf(tmp,"t%d",cpt);
                           cpt ++;
                           ajour_quad(tqc,3,tmp);
                            n1 = false ;
                            n2 = false ;
                        }else {
                            printf("****Erreur semantique a la ligne %d et la colonne %d : ICOMPATIBILITE DE TYPE ****\n", N,C); return 1 ;
                        }
    }
    | P EXP foi EXP P {    
                        if(strcmp($2,"INTEGER") == 0 && strcmp($4,"INTEGER") == 0 || strcmp($2,"REAL") == 0 && strcmp($4,"REAL") == 0 || strcmp($2,"string") == 0 && strcmp($4,"string") == 0) {
                           strcpy($$,$2);
                           ajour_quad(tqc,0,"*");
                           sprintf(tmp,"t%d",cpt);
                           cpt ++;
                           ajour_quad(tqc,3,tmp);
                            n1 = false ;
                            n2 = false ;
                        }else {
                            printf("****Erreur semantique a la ligne %d et la colonne %d : ICOMPATIBILITE DE TYPE ****\n", N,C); return 1 ;
                        }
    }
    | P EXP division EXP P {
                        if(strcmp($2,"INTEGER") == 0 && strcmp($4,"INTEGER") == 0 || strcmp($2,"REAL") == 0 && strcmp($4,"REAL") == 0 || strcmp($2,"string") == 0 && strcmp($4,"string") == 0) {
                            strcpy($$,$2);
                            ajour_quad(tqc,0,"/");
                            sprintf(tmp,"t%d",cpt);
                            cpt ++;
                            ajour_quad(tqc,3,tmp);
                            n1 = false ;
                            n2 = false ;
                        }else {
                            printf("****Erreur semantique a la ligne %d et la colonne %d : ICOMPATIBILITE DE TYPE ****\n", N,C); return 1 ;
                        }
    }
    | P EXP et_log EXP P {
        ajour_quad(tqc,0,"&&");
        sprintf(tmp,"t%d",cpt);
        cpt ++;
        ajour_quad(tqc,3,tmp);
            n1 = false ;
            n2 = false ;
    }
    | P EXP ou_log EXP P {
        ajour_quad(tqc,0,"||");
        sprintf(tmp,"t%d",cpt);
        cpt ++;
        ajour_quad(tqc,3,tmp);
            n1 = false ;
            n2 = false ;
    }
    | P non_log EXP P 
    | chaine {$$ = retournType($1) ;}
    | idf  { if (rechercheNonDeclare($1) == 0)
                {
                    printf("****Erreur semantique a la ligne %d et la colonne %d , variable %s non declaree **** ", N , C , $1);
                    return 1 ;
                }else {$$ = retournType($1) ;
                        if(n1 == false){
                            sprintf(tmp,"%s",$1); 
                            tqc = qc ;
                            quadr("", tmp ,"","");
                            n1 = true ;
                        }else if(n2 == false){
                            sprintf(tmp,"%s",$1); 
                            ajour_quad(tqc,2,tmp);
                            n2 = true ;
                        }  
                      } 

             
            } 
    | cst_e {   $$ = "INTEGER" ;

                if(n1 == false){
                    sprintf(tmp,"%d",$1);
                    tqc = qc ;
                    quadr("", tmp ,"","");
             }else if(n2 == false){
                        sprintf(tmp,"%d",$1);
                        ajour_quad(tqc,2,tmp);
                        n2 = true ;
             }
            }
    | cst_r {   $$ = "REAL" ;

                if(n1 == false){
                sprintf(tmp,"%f",$1);
                tqc = qc;
                quadr("", tmp ,"","");
                n1 = true ;
             }else if(n2 == false){
                        sprintf(tmp,"%f",$1);
                        ajour_quad(tqc,2,tmp);
                        n2 = true ;
             }
            }
;
P : po
  | pf
  |

;
COMMENTAIRE : c1 COMMENTAIRE
            | cp COMMENTAIRE
            | c1
            | cp 
            |

;




%%
int main ()
{
   initialisation();
   yyparse(); 
   printf("\n");
   afficher();
   afficher_qdr();
 }
int yywrap ()
 {}