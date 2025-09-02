%{ 
    
    #include <stddef.h> 
    #include <stdio.h>
    #include <stdlib.h>
    #include "quad.h"

	extern FILE* yyin; 

	int nb_ligne=1, Col=1;

    char SAVE_IDF_FN[20];
    char SAVE_IDF_FN_f_louwel[20];
	int double_declaration_variable = 1;
	
	
    int dontchange=0;
    int sauv_fin_if=0;
    int Fin_if,Fin_while,deb_else,deb_while,Fin ;
    
    typedef struct valeurs {
        int integer;
        char string[20];
        char caractere;
        float real;
        char boole[20];
    }valeurs;
	
	valeurs v ;
    valeurs sauvcst ;
	char *input_filename = NULL;
   
	int sauvType ;
	int sauvTypecst;
	char sauvchar[20];
	char tmp[20];
	int valtemp = 0;
	int ntemp =0;
   
    int program_OU_routine_positioning=1;
	
	
    char nomRoutine[20];
  
    
void set_input_filename(char *filename) {
    input_filename = strdup(filename); // Save the input filename
}


%}



%union {
    int entier;
    float real;	
    char character; 
    char* string;
    struct {
        int integer;
        char string[59];
        float real;
        int boole;
        int type; 
        char valtemp[50];
    }valeur;
}

%type<valeur>EXPRESSION_ARTHMTQ
%type<valeur>OPERANDE
%type<valeur>idff

 %token  mc_programme mc_var mc_begin mc_end mc_const mc_enddo <string>mc_real <entier>cst_int_pos <entier>cst_int_neg <string> mc_char <string> mc_integer <string> mc_douxpoint mc_for mc_read mc_write mc_while mc_or mc_and mc_gt mc_ge mc_eq mc_ne mc_le mc_lt mc_if mc_else <string> idf <entier>cst_int cst_char <real>cst_real <real>cst_real_neg <real>cst_real_pos plus moins egal mult   parnths_ovr parnths_frm   vergule acc_frm acc_ovr crch_ovr crch_frm division mc_false mc_true pvr mc_routine <string>cst_string 


%left                   egal  
%left                   mc_gt mc_ge  mc_ne mc_le mc_eq mc_lt
%left                   plus         moins       
%left                   mult           division 
%left                   mc_and  mc_or        
%right                  parnths_ovr        parnths_frm        acc_ovr        acc_frm        crch_ovr        crch_frm
%start S
%%

S: PROGRAM { printf("\n\n Le programme est correcte syntaxiquement. \n"); YYACCEPT;}
;

PROGRAM: mc_programme idf mc_var acc_ovr DEC acc_frm mc_begin LIST_INSTRUCTION mc_end | mc_programme idf mc_var acc_ovr DEC acc_frm mc_begin mc_end

INST: INST_CONDITION  
                    | aff 
                    | INST_READ 
                    | INST_WRITE
                    | INST_LOOP
;



INST_CONDITION:IF|IF_ELSE 
;
A:mc_if parnths_ovr EXPRESSION_ARTHMTQ {    deb_else=qc;
                                           
                                            generer(SAVE_IDF_FN_f_louwel,"BZ","",$3.valtemp," ",program_OU_routine_positioning);
                                            
                                            sprintf(tmp,"%d",qc); 
                                            ajour_quad(Fin,1,tmp);
                                            

                               }
;

B:A parnths_frm acc_ovr LIST_INSTRUCTION {  
                                 Fin_if=qc;
                                 generer(SAVE_IDF_FN_f_louwel,"BR", ""," ", " ",program_OU_routine_positioning); 
				                 sprintf(tmp,"%d",qc); 
                                 ajour_quad(deb_else,1,tmp);}
  |A parnths_frm acc_ovr {  
                                 Fin_if=qc;
                                 generer(SAVE_IDF_FN_f_louwel,"BR", ""," ", " ",program_OU_routine_positioning); 
				                 sprintf(tmp,"%d",qc); 
                                 ajour_quad(deb_else,1,tmp);}
;

IF:B acc_frm 
;

C:IF mc_else acc_ovr LIST_INSTRUCTION {        sprintf(tmp,"%d",qc);  
                                     ajour_quad(Fin_if,1,tmp);}
  |IF mc_else acc_ovr {        sprintf(tmp,"%d",qc);  
                                     ajour_quad(Fin_if,1,tmp);}
;


IF_ELSE:C acc_frm 
;


D:mc_while parnths_ovr {deb_while=qc;}
;


E:D EXPRESSION_ARTHMTQ {Fin_while=qc;
                        generer(SAVE_IDF_FN_f_louwel,"BZ","",$2.valtemp," ",program_OU_routine_positioning);
                        sprintf(tmp,"%d",qc); 
                        ajour_quad(Fin,1,tmp);
                        }
;

F:E parnths_frm acc_ovr LIST_INSTRUCTION { sprintf(tmp,"%d",deb_while);
                        generer(SAVE_IDF_FN_f_louwel,"BR",tmp," "," ",program_OU_routine_positioning);
                        sprintf(tmp,"%d",qc); 
                        ajour_quad(Fin_while,1,tmp);}
  |E parnths_frm acc_ovr { sprintf(tmp,"%d",deb_while);
                        generer(SAVE_IDF_FN_f_louwel,"BR",tmp," "," ",program_OU_routine_positioning);
                        sprintf(tmp,"%d",qc); 
                        ajour_quad(Fin_while,1,tmp);}
;

G:mc_for parnths_ovr idff mc_douxpoint idff mc_douxpoint idff mc_douxpoint idff parnths_frm acc_ovr LIST_INSTRUCTION
 |mc_for parnths_ovr idff mc_douxpoint idff mc_douxpoint idff mc_douxpoint idff parnths_frm acc_ovr
;

INST_LOOP:F acc_frm | G acc_frm
;

INST_WRITE : mc_write parnths_ovr LIST_STRING_WRITE parnths_frm pvr
;
LIST_STRING_WRITE : writeidfwlastring
                    |writeidfwlastring vergule  LIST_STRING_WRITE
;
writeidfwlastring: cst_string
                    |idf

;
INST_READ : mc_read parnths_ovr idf parnths_frm pvr  
;




LIST_INSTRUCTION:INST LIST_INSTRUCTION  
                |INST
;               

TYPEs: mc_integer 	{sauvType = 1;}
      |mc_real		{sauvType = 2;}


;

DEC :  TYPEs ListIDFF pvr DEC 
    |  TYPEs tab DEC
    |  mc_const idf egal cst pvr DEC { sauvType = 5;
            if(dontchange==0){
			    if (declared($2,program_OU_routine_positioning)==0) {
                inserertype($2,sauvType,program_OU_routine_positioning);
                insererval($2,sauvcst);
                dontchange=1;
                }
            }
			else {printf("\nErreur semantique 'CONST CANT BE CHANGE' a la ligne %d,la variable %s est deja declaree \n", nb_ligne, $2);}
		}
    |
 ;

tab: idf crch_ovr cst_int crch_frm pvr {
			
            if (declared($1,program_OU_routine_positioning)==0) { 
                inserertype($1,sauvType,program_OU_routine_positioning);
                inserertaille($1,$3,-1);
                 if($3<0){
                    printf("\nErreur semantique a la ligne  %d : la taille d'un tablau ne doit pas etre < 0\n",nb_ligne);

                    
                }else{
                    sprintf(tmp,"%d",$3); 
                    generer(SAVE_IDF_FN,"BOUNDS","O",tmp, "vide",program_OU_routine_positioning);
                    generer(SAVE_IDF_FN,"ADEC",$1,"vide", "vide",program_OU_routine_positioning);

                }
               }
			else {printf("\nErreur semantique 'double declaration' a la ligne %d ,la variable %s est deja declaree \n", nb_ligne, $2);}
		}

    |idf crch_ovr parnths_ovr cst_int_neg parnths_frm crch_frm pvr {
			
            if (declared($1,program_OU_routine_positioning)==0) { 
                inserertype($1,sauvType,program_OU_routine_positioning);
                inserertaille($1,$4,-1);
                 if($4<0){
                    printf("\nErreur semantique a la ligne  %d : la taille d'un tablau ne doit pas etre < 0\n",nb_ligne);

                    
                }else{
                    sprintf(tmp,"%d",$4); 
                    generer(SAVE_IDF_FN,"BOUNDS","O",tmp, "vide",program_OU_routine_positioning);
                    generer(SAVE_IDF_FN,"ADEC",$1,"vide", "vide",program_OU_routine_positioning);

                }
               }
			else {printf("\nErreur semantique 'double declaration' a la ligne %d ,la variable %s est deja declaree \n", nb_ligne, $2);}
		}

    |idf crch_ovr parnths_ovr cst_int_pos parnths_frm crch_frm pvr {
			
            if (declared($1,program_OU_routine_positioning)==0) { 
                inserertype($1,sauvType,program_OU_routine_positioning);
                inserertaille($1,$4,-1);
                 if($4<0){
                    printf("\nErreur semantique a la ligne  %d : la taille d'un tablau ne doit pas etre < 0\n",nb_ligne);

                    
                }else{
                    sprintf(tmp,"%d",$4); 
                    generer(SAVE_IDF_FN,"BOUNDS","O",tmp, "vide",program_OU_routine_positioning);
                    generer(SAVE_IDF_FN,"ADEC",$1,"vide", "vide",program_OU_routine_positioning);

                }
               }
			else {printf("\nErreur semantique 'double declaration' a la ligne %d ,la variable %s est deja declaree \n", nb_ligne, $2);}
		}

    |idf crch_ovr cst_int vergule cst_int crch_frm pvr {
            inserertaille($1,$3,$5);
            
			if (declared($1,program_OU_routine_positioning)==0) {
                inserertype($1,sauvType,program_OU_routine_positioning);
                if($3<0){
                    printf("\nErreur semantique a la ligne  %d : la taille d'un tablau ne doit pas etre < 0\n",nb_ligne);
                }else{
                 if($5<0){
                    printf("\nErreur semantique a la ligne  %d : la taille d'un tablau ne doit pas etre < 0\n",nb_ligne);


                }else{
                    sprintf(tmp,"%d",$3);           
                    generer(SAVE_IDF_FN,"BOUNDS"," O",tmp, "vide",program_OU_routine_positioning);
                    sprintf(tmp,"%d",$5); 
                    generer(SAVE_IDF_FN,"BOUNDS"," O",tmp, "vide",program_OU_routine_positioning);
                    generer(SAVE_IDF_FN,"ADEC",$1,"vide", "vide",program_OU_routine_positioning);

                }
                }
            }
			else {printf("\nErreur semantique 'double declaration' a la ligne %d  ,la variable %s est deja declaree \n", nb_ligne, $2);}
		}

    |idf crch_ovr parnths_ovr cst_int_pos parnths_frm parnths_frm vergule parnths_ovr cst_int_pos parnths_frm crch_frm pvr {
            inserertaille($1,$4,$8);
            
			if (declared($1,program_OU_routine_positioning)==0) {
                inserertype($1,sauvType,program_OU_routine_positioning);
                if($4<0){
                    printf("\nErreur semantique a la ligne  %d : la taille d'un tablau ne doit pas etre < 0\n",nb_ligne);
                }else{
                 if($8<0){
                    printf("\nErreur semantique a la ligne  %d : la taille d'un tablau ne doit pas etre < 0\n",nb_ligne);


                }else{
                    sprintf(tmp,"%d",$4);           
                    generer(SAVE_IDF_FN,"BOUNDS"," O",tmp, "vide",program_OU_routine_positioning);
                    sprintf(tmp,"%d",$8); 
                    generer(SAVE_IDF_FN,"BOUNDS"," O",tmp, "vide",program_OU_routine_positioning);
                    generer(SAVE_IDF_FN,"ADEC",$1,"vide", "vide",program_OU_routine_positioning);

                }
                }
            }
			else {printf("\nErreur semantique 'double declaration' a la ligne %d  ,la variable %s est deja declaree \n", nb_ligne, $2);}
		}

    |idf crch_ovr parnths_ovr cst_int_neg parnths_frm parnths_frm vergule parnths_ovr cst_int_neg parnths_frm crch_frm pvr {
            inserertaille($1,$4,$8);
            
			if (declared($1,program_OU_routine_positioning)==0) {
                inserertype($1,sauvType,program_OU_routine_positioning);
                if($4<0){
                    printf("\nErreur semantique a la ligne  %d : la taille d'un tablau ne doit pas etre < 0\n",nb_ligne);
                }else{
                 if($8<0){
                    printf("\nErreur semantique a la ligne  %d : la taille d'un tablau ne doit pas etre < 0\n",nb_ligne);


                }else{
                    sprintf(tmp,"%d",$4);           
                    generer(SAVE_IDF_FN,"BOUNDS"," O",tmp, "vide",program_OU_routine_positioning);
                    sprintf(tmp,"%d",$8); 
                    generer(SAVE_IDF_FN,"BOUNDS"," O",tmp, "vide",program_OU_routine_positioning);
                    generer(SAVE_IDF_FN,"ADEC",$1,"vide", "vide",program_OU_routine_positioning);

                }
                }
            }
			else {printf("\nErreur semantique 'double declaration' a la ligne %d  ,la variable %s est deja declaree \n", nb_ligne, $2);}
		}
;


ListIDFF : idf vergule ListIDFF {
			if (declared($1,program_OU_routine_positioning)==0) {inserertype($1,sauvType,program_OU_routine_positioning);}
			else {printf("\nErreur semantique 'double declaration' a la ligne %d,la variable %s est deja declaree \n", nb_ligne, $1);}
		}
            | idf {
			if (declared($1,program_OU_routine_positioning)==0) {inserertype($1,sauvType,program_OU_routine_positioning);}
			else {printf("\nErreur semantique 'double declaration' a la ligne %d,la variable %s est deja declaree \n", nb_ligne, $1);}
		}
            | idf egal cst vergule ListIDFF  {
			if (declared($1,program_OU_routine_positioning)==0) {
                inserertype($1,sauvType,program_OU_routine_positioning);
                if(sauvType == sauvTypecst){
                    insererval($1,sauvcst);
                }else{
                    printf("\nErreur semantique a la ligne %d : ICOMPATIBILITE DE TYPE de la variable %s \n", nb_ligne, $1);
                }
            }
			else {printf("\nErreur semantique 'double declaration' a la ligne %d,la variable %s est deja declaree \n", nb_ligne, $1);}
		}
            | idf egal cst {
			if (declared($1,program_OU_routine_positioning)==0) {
                inserertype($1,sauvType,program_OU_routine_positioning);
                if(sauvType == sauvTypecst){
                    insererval($1,sauvcst);
                }else{
                    printf("\nErreur semantique a la ligne %d : ICOMPATIBILITE DE TYPE de la variable %s \n", nb_ligne, $1);
                }
            }
			else {printf("\nErreur semantique 'double declaration' a la ligne %d,la variable %s est deja declaree \n", nb_ligne, $1);}
		}
;

cst : cst_char {sauvTypecst=3;}
        | cst_int {sauvcst.integer = $1 ;sauvTypecst=1;}
        | parnths_ovr cst_int_pos parnths_frm {sauvcst.integer = $2 ;sauvTypecst=1;}
        | parnths_ovr cst_int_neg parnths_frm {sauvcst.integer = $2 ;sauvTypecst=1;}
        | cst_real {sauvcst.real = $1 ;sauvTypecst=2;}
        | parnths_ovr cst_real_pos parnths_frm {sauvcst.real = $2 ;sauvTypecst=2;}
        | parnths_ovr cst_real_neg parnths_frm {sauvcst.real = $2 ;sauvTypecst=2;}
        | cst_string {strcpy(sauvcst.string, strdup($1)); sauvTypecst=3;}
;
                
aff:     idf egal EXPRESSION_ARTHMTQ pvr { 
    if(rechercheNonDec($1,program_OU_routine_positioning)==1 ){printf("\nErreur semantique a la ligne  %d : la variable %s n'est pas declaree !!\n",nb_ligne,$1);
    }else{
        if(gettype($1) < 3 && $3.type < 3){
            sauvcst.real = $3.real ;
            sauvcst.integer = (int)$3.real ;
        }else if(gettype($1) == 3 && $3.type == 3){
            if(depastaille($1,strlen($3.valtemp),0) == 1){
                printf("\nErreur semantique a la ligne  %d : la taille de la chaine est supperier a la taille de la variable !!\n",nb_ligne,$1);
                strcpy(sauvcst.string," ") ;
            }else{
                strcpy(sauvcst.string, $3.valtemp) ;
            }

        }else if(gettype($1)==4 && $3.type != 3) {
            if($3.real != 0){
                strcpy(sauvcst.boole,"VRAI");
            }else{
                strcpy(sauvcst.boole,"VRAI");
            }
        }else{
            printf("\nErreur semantique a la ligne  %d : INCOMPATIBILIE DE TYPE !!\n",nb_ligne);
        }
        insererval($1,sauvcst);
}
    generer(SAVE_IDF_FN_f_louwel,"=",$3.valtemp," ",$1,program_OU_routine_positioning);
}
         | idf crch_ovr idff crch_frm egal EXPRESSION_ARTHMTQ pvr { 
		    if($3.integer > getTailleTab($1)) printf("erreur semantique  depassement de la taille d'un tableau\n");
		 
		 if (rechercheNonDec($1,program_OU_routine_positioning)==1 ){printf("\nErreur semantique a la ligne  %d : la variable %s n'est pas declaree !!\n",nb_ligne,$1);}      
         }
         | idf crch_ovr idff vergule idff crch_frm egal EXPRESSION_ARTHMTQ pvr { 
		 
		 if($3.integer > getTailleTab($1)) printf("erreur semantique  depassement de la taille d'un tableau\n");
		 
		 if (rechercheNonDec($1,program_OU_routine_positioning)==1 ){printf("\nErreur semantique a la ligne  %d : la variable %s n'est pas declaree !!\n",nb_ligne,$1);}    
         } 
;


EXPRESSION_ARTHMTQ:  OPERANDE { $$.real = $1.real ; 
                                strcpy($$.valtemp,$1.valtemp);
                               
} 
                    |EXPRESSION_ARTHMTQ moins EXPRESSION_ARTHMTQ{if(($1.type==3)||($3.type==3)){printf("\nErreur semantique a la ligne  %d : INCOMPATIBILIE DE TYPE charecter dans une exepression arithmitique !!\n",nb_ligne);
                                                                }else{
                                                                
                                                                $$.type=max($1.type,$3.type);
                                                                strcpy($1.valtemp,$$.valtemp);
                                                                sprintf($$.valtemp, "T%d", ntemp);
                                                                $$.real = $1.real - $3.real;
                                                                
                                                                generer(SAVE_IDF_FN_f_louwel,"-",$1.valtemp,$3.valtemp,$$.valtemp,program_OU_routine_positioning);
                                                                ntemp++;}}

                    |EXPRESSION_ARTHMTQ mult EXPRESSION_ARTHMTQ {if(($1.type==3)||($3.type==3)){printf("\nErreur semantique a la ligne  %d : INCOMPATIBILIE DE TYPE charecter dans une exepression arithmitique !!\n",nb_ligne);
                                                                }else{
                                                                $$.type=max($1.type,$3.type);
                                                                $$.real = $1.real * $3.real;
                                                                strcpy($1.valtemp,$$.valtemp);
                                                                sprintf($$.valtemp, "T%d", ntemp);
                                                                generer(SAVE_IDF_FN_f_louwel,"*",$1.valtemp,$3.valtemp,$$.valtemp,program_OU_routine_positioning);
                                                                ntemp++;}}

                    |EXPRESSION_ARTHMTQ plus EXPRESSION_ARTHMTQ {if(($1.type==3)||($3.type==3)){printf("\nErreur semantique a la ligne  %d : INCOMPATIBILIE DE TYPE charecter dans une exepression arithmitique !!\n",nb_ligne);
                                                                }else{
                                                                    $$.type=max($1.type,$3.type);
                                                                    $$.real = $1.real + $3.real;
                                                                    strcpy($1.valtemp,$$.valtemp);
                                                                    sprintf($$.valtemp, "T%d", ntemp);
                                                                    generer(SAVE_IDF_FN_f_louwel,"+",$1.valtemp,$3.valtemp,$$.valtemp,program_OU_routine_positioning);
                                                                    ntemp++;
                                                                    }
                                                                }

                    |EXPRESSION_ARTHMTQ division EXPRESSION_ARTHMTQ {if(($1.type==3)||($3.type==3)){printf("\nErreur semantique a la ligne  %d : INCOMPATIBILIE DE TYPE charecter dans une exepression arithmitique !!\n",nb_ligne);
                                                                }else{
                                                                $$.type=max($1.type,$3.type);
                                                               
                                                                if($3.real==0){
                                                                    printf("\nErreur semantique DIVISION SUR ZERO a la ligne %d .. \n", nb_ligne);
                                                                    
                                                                }else{
                                                                     $$.real = $1.real / $3.real;
                                                                }
                                                                strcpy($1.valtemp,$$.valtemp);

                                                                sprintf($$.valtemp, "T%d", ntemp);
                                                                
                                                                generer(SAVE_IDF_FN_f_louwel,"/",$1.valtemp,$3.valtemp,$$.valtemp,program_OU_routine_positioning);
                                                                ntemp++;}}

                    |EXPRESSION_ARTHMTQ mc_and EXPRESSION_ARTHMTQ {strcpy($1.valtemp,$$.valtemp);sprintf($$.valtemp,"T%d",ntemp);quadL(SAVE_IDF_FN_f_louwel,3,$1.valtemp,$3.valtemp,$$.valtemp,program_OU_routine_positioning);ntemp++;}

                    |EXPRESSION_ARTHMTQ mc_or EXPRESSION_ARTHMTQ{strcpy($1.valtemp,$$.valtemp);sprintf($$.valtemp,"T%d",ntemp);quadL(SAVE_IDF_FN_f_louwel,2,$1.valtemp,$3.valtemp,$$.valtemp,program_OU_routine_positioning);ntemp++;}

                    |EXPRESSION_ARTHMTQ mc_ge EXPRESSION_ARTHMTQ {strcpy($1.valtemp,$$.valtemp);sprintf($$.valtemp,"T%d",ntemp);quadC(SAVE_IDF_FN_f_louwel,2,$1.valtemp,$3.valtemp,$$.valtemp,program_OU_routine_positioning);ntemp++;}

                    |EXPRESSION_ARTHMTQ mc_gt EXPRESSION_ARTHMTQ {
                                                                    
                                                                    strcpy($1.valtemp,$$.valtemp);
                                                                    sprintf($$.valtemp,"T%d",ntemp);
                                                                    quadC(SAVE_IDF_FN_f_louwel,1,$1.valtemp,$3.valtemp,$$.valtemp,program_OU_routine_positioning);
                                                                    ntemp++;}

                    |EXPRESSION_ARTHMTQ mc_le EXPRESSION_ARTHMTQ{strcpy($1.valtemp,$$.valtemp);sprintf($$.valtemp,"T%d",ntemp);quadC(SAVE_IDF_FN_f_louwel,4,$1.valtemp,$3.valtemp,$$.valtemp,program_OU_routine_positioning);ntemp++;}

                    |EXPRESSION_ARTHMTQ mc_lt EXPRESSION_ARTHMTQ {strcpy($1.valtemp,$$.valtemp);sprintf($$.valtemp,"T%d",ntemp);quadC(SAVE_IDF_FN_f_louwel,3,$1.valtemp,$3.valtemp,$$.valtemp,program_OU_routine_positioning);ntemp++;}

                    |EXPRESSION_ARTHMTQ mc_ne EXPRESSION_ARTHMTQ {strcpy($1.valtemp,$$.valtemp);sprintf($$.valtemp,"T%d",ntemp);quadC(SAVE_IDF_FN_f_louwel,6,$1.valtemp,$3.valtemp,$$.valtemp,program_OU_routine_positioning);ntemp++;}
                    
                    |EXPRESSION_ARTHMTQ mc_eq EXPRESSION_ARTHMTQ {strcpy($1.valtemp,$$.valtemp);sprintf($$.valtemp,"T%d",ntemp);quadC(SAVE_IDF_FN_f_louwel,5,$1.valtemp,$3.valtemp,$$.valtemp,program_OU_routine_positioning);ntemp++;}

                    |parnths_ovr EXPRESSION_ARTHMTQ parnths_frm { strcpy($$.valtemp,$2.valtemp);$$ = $2 ;
                    }
 ;

OPERANDE : idf { if (rechercheNonDec($1,program_OU_routine_positioning)==1 ){printf("\nErreur semantique a la ligne  %d : la variable %s n'est pas declaree !!\n",nb_ligne,$1);
                }else{strcpy($$.valtemp,$1); 
                 $$.type=gettype($1); 
                 if(gettype($1)==3){
                    strcpy($$.string, recupstring($1));
                 }else{
                    $$.real = recupval($1);
                    
                 }
  }

 }

         | idf crch_ovr idff crch_frm { if (rechercheNonDec($1,program_OU_routine_positioning)==1 )
         {printf("\nErreur semantique a la ligne  %d : la variable %s n'est pas declaree !!\n",nb_ligne,$1);}
         sprintf($$.valtemp, "%s(%s)", $1,$3.valtemp);
          }

         | idf crch_ovr idff vergule idff crch_frm { if (rechercheNonDec($1,program_OU_routine_positioning)==1 ){printf("\nErreur semantique a la ligne  %d : la variable %s n'est pas declaree !!\n",nb_ligne,$1);
             sprintf($$.valtemp, "%s[%s,%s]", $1, $3.valtemp, $5.valtemp);
         } 
         }
         | cst_int {$$.type=1; $$.real=$1;
            sprintf($$.valtemp, "%d", $1);
         }
         | parnths_ovr cst_int_neg parnths_frm {$$.type=1; $$.real=$2;
            sprintf($$.valtemp, "%d", $2);
         }
         | parnths_ovr cst_int_pos parnths_frm {$$.type=1; $$.real=$2;
            sprintf($$.valtemp, "%d", $2);
         }
         | cst_real {$$.type=2;sprintf($$.valtemp, "%f", $1);$$.real=$1;
         }
         | parnths_ovr cst_real_pos parnths_frm {$$.type=2;sprintf($$.valtemp, "%f", $2);$$.real=$2;
         }
         | parnths_ovr cst_real_neg parnths_frm {$$.type=2;sprintf($$.valtemp, "%f", $2);$$.real=$2;
         }
         | cst_string {$$.type=3;strcpy($$.valtemp,strdup($1));
         }
         | mc_true {$$.type=4; sprintf($$.valtemp, "%d", 1);
         }
         | mc_false {$$.type=4;sprintf($$.valtemp, "%d", 1);
         }
;

idff: cst_int{ 
               sprintf($$.valtemp, "%d", $1); $$.integer = $1;}
     | parnths_ovr cst_int_pos parnths_frm{ sprintf($$.valtemp, "%d", $2); $$.integer = $2;}
     | parnths_ovr cst_int_neg parnths_frm{ sprintf($$.valtemp, "%d", $2); $$.integer = $2;}
     |idf 
     { if (rechercheNonDec($1,program_OU_routine_positioning)==1 ){printf("\nErreur semantique a la ligne  %d : la variable %s n'est pas declaree !!\n",nb_ligne,$1);}
         strcpy($$.valtemp,$1); 
         $$.integer = (int) recupval($1) ;
     }
;

%%

int main(int argc, char *argv[]) 
{
    if (argc < 2) {
        fprintf(stderr, "Usage: %s input_filename\n", argv[0]);
        return 1;
    }
   
      set_input_filename(argv[1]);

 yyin = fopen(argv[1], "r");
 if (!yyin) {
    fprintf(stderr, "Could not open input_file.txt\n");
    exit(1);
    }
   initialisation();
   yyparse(); 
   printf("\n\n");
   afficher();
   afficher_qdr();
   afficherroutine();
   afficher_qdrR();
}
int yyerror ( char*  msg )  
{
    printf ("File %s , line %d , character %d:syntax error\n",input_filename,nb_ligne,Col);  // Pass the filename to the function
    // Rest of your lexer code
    // Invoke the lexer File "Test", line 4, character 56: syntax error
    yylex();
} 

int yywrap () {}


