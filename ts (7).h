#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

typedef struct valeurs
{
    int integer;
    char string[20];
    char* caractere;
    float real;
    char boole[20];
} valeurs;

typedef struct valeur
{
    int integer;
    char string[59];
    float real;
    int boole;
    int type;
    char valtemp[50];
} valeur;

typedef struct Element
{
    int state;
    char name[20];
    char code[20];
    char type[20];
    valeurs val;
    int taille;
    int taille2;
    int locale_global;
    struct Element *next;
} Element;

typedef struct Elt
{
    int state;
    char name[20];
    char type[20];
    struct Elt *next;
} Elt;

typedef struct Parameter
{
    int n;
    char nomp[60];
    char typep[20];
    struct Parameter *next;
} Parameter;

typedef struct Routine
{
    int ultiliser;
    int nbp;
    char nomr[60];
    char typer[20];
    char valDEretourne[20];
    Parameter *parametersr; // Head of the linked list for parameters
    struct Routine *next;
} Routine;

Routine *parametersRoutine = NULL;



void inserer_type_parametre(char entite[], int i, int j)
{
    if (j == 0)
    {
        Routine *currentRoutine = parametersRoutine;
        while (currentRoutine != NULL)
        {
            Parameter *currentParameter = currentRoutine->parametersr;
            while (currentParameter != NULL)
            {
                if (strcmp(currentParameter->nomp, entite) == 0)
                {
                    switch (i)
                    {
                    case 1:
                        strcpy(currentParameter->typep, "INTEGER");
                        break;
                    case 2:
                        strcpy(currentParameter->typep, "REAL");
                        break;
                    case 3:
                        strcpy(currentParameter->typep, "CHAR");
                        break;
                    case 4:
                        strcpy(currentParameter->typep, "BOOL");
                        break;
                    default:
                        break;
                    }
                    break;
                }
                currentParameter = currentParameter->next;
            }
            currentRoutine = currentRoutine->next;
        }
    }
}

void afficherroutine()
{
    Routine *current = parametersRoutine;
    while (current != NULL)
    {
        Parameter *param = current->parametersr;
        printf("Routine Name: %s , Type: %s\n", current->nomr, current->typer);
        printf("Parameters:\n");
        while (param != NULL)
        {
            printf("           - Name: %s  type parametre %s\n", param->nomp, param->typep);
            param = param->next;
        }
        current = current->next;
    }
}

Element *elementList = NULL;
Elt *tabsList = NULL, *tabmList = NULL;
void initialisation()
{
    // This function remains the same as it initializes linked lists.
    elementList = NULL;
    tabsList = NULL;
    tabmList = NULL;
    Routine *parametersRoutine = NULL;
}
void inserer(char entite[], char code[], char type[20], valeurs val, int y)
{

    Elt *newElt = (Elt *)malloc(sizeof(Elt));
    Element *newElement = (Element *)malloc(sizeof(Element));

    switch (y)
    {
    case 0: // Insertion in the table des IDF et CONST
    {

        if (newElement == NULL)
        {
            printf("Memory allocation failed.\n");
            return;
        }

        newElement->state = 1;
        strcpy(newElement->name, entite);
        strcpy(newElement->code, code);
        strcpy(newElement->type, type);
        newElement->taille2 = 0;
        newElement->taille = 0;

        newElement->val = val;
        newElement->next = elementList;
        elementList = newElement;
        break;
    }
    case 1: // Insertion in the table des mots clés
    {
        if (newElt == NULL)
        {
            printf("Memory allocation failed.\n");
            return;
        }

        newElt->state = 1;
        strcpy(newElt->name, entite);
        strcpy(newElt->type, code);
        newElt->next = tabmList;
        tabmList = newElt;
        break;
    }
    case 2: // Insertion in the table des séparateurs
    {

        if (newElt == NULL)
        {
            printf("Memory allocation failed.\n");
            return;
        }

        newElt->state = 1;
        strcpy(newElt->name, entite);
        strcpy(newElt->type, code);
        newElt->next = tabsList;
        tabsList = newElt;
        break;
    }
    }
}

void afficher()
{
    printf("/*********************************Table des symboles IDF***************************/\n");
    printf("____________________________________________________________________________________\n");
    printf("\t| Nom_Entite |  Code_Entite | Type_Entite | Val_Entite     | taille        |taille2\n");
    printf("_____________________________________________________________________________________\n");

    Element *temp = elementList;
    while (temp != NULL)
    {
        int x = gettype(temp->name);
        switch (x)
        {
        case 1:
            printf("\t|%12s  |%15s | %12s | %12d |%12d   |%12d\n", temp->name, temp->code, temp->type, temp->val.integer, temp->taille, temp->taille2);
            break;
        case 2:
            printf("\t|%12s  |%15s | %12s | %12f |%12d   |%12d\n", temp->name, temp->code, temp->type, temp->val.real, temp->taille, temp->taille2);
            break;
        case 3:
            printf("\t|%12s  |%15s | %12s | %12s |%12d   |%12d\n", temp->name, temp->code, temp->type, temp->val.string, temp->taille, temp->taille2);
            break;
        case 4:
            printf("\t|%12s  |%15s | %12s | %12s |%12d   |%12d\n", temp->name, temp->code, temp->type, temp->val.boole, temp->taille, temp->taille2);
            break;
        default:

            break;
        }
        temp = temp->next;
    }
    temp = elementList;
    while (temp != NULL)
    {
        int x = gettype(temp->name);
        switch (x)
        {
        case 1:
            break;
        case 2:
            break;
        case 3:
            break;
        case 4:
            break;
        default:
            printf("\t|%12s  |%15s | %12s | %12s |%12d   |%12d\n", temp->name, temp->code, temp->type, temp->val.string, temp->taille, temp->taille2);

            break;
        }
        temp = temp->next;
    }
    printf("\n/***************Table des symboles mots clés*************/\n");
    printf("_____________________________________\n");
    printf("\t| NomEntite |  CodeEntite | \n");
    printf("_____________________________________\n");

    Elt *tempTabm = tabmList;
    while (tempTabm != NULL)
    {
        if (tempTabm->state == 1)
        {
            printf("\t|%10s |%12s | \n", tempTabm->name, tempTabm->type);
        }
        tempTabm = tempTabm->next;
    }

    printf("\n/***************Table des symboles séparateurs*************/\n");
    printf("_____________________________________\n");
    printf("\t| NomEntite |  CodeEntite | \n");
    printf("_____________________________________\n");

    Elt *tempTabs = tabsList;
    while (tempTabs != NULL)
    {
        if (tempTabs->state == 1)
        {
            printf("\t|%10s |%12s | \n", tempTabs->name, tempTabs->type);
        }
        tempTabs = tempTabs->next;
    }
}

void rechercher(char entite[], char code[], char type[20], valeurs val, int y)
{

    switch (y)
    {

    case 0: // Search in table des IDF et CONST
    {
        Element *temp = elementList;

        while (temp != NULL && strcmp(entite, temp->name) != 0)
        {
            temp = temp->next;
        }

        if (temp == NULL)
        {
            inserer(entite, code, type, val, y);
        }
        break;
    }
    case 1: // Search in table des mots clés
    {
        Elt *temp = tabmList;

        while (temp != NULL && strcmp(entite, temp->name) != 0)
        {
            temp = temp->next;
        }

        if (temp == NULL)
        {
            inserer(entite, code, type, val, y);
        }
        break;
    }
    case 2: // Search in table des séparateurs
    {
    }
        Elt *temp = tabsList;

        while (temp != NULL && strcmp(entite, temp->name) != 0)
        {
            temp = temp->next;
        }

        if (temp == NULL)
        {
            inserer(entite, code, type, val, y);
        }
        break;

    default:
        printf("Invalid option\n");
        break;
    }
}
Element *rechercheridf(char entite[])
{

    Element *temp = elementList;

    while (temp != NULL && strcmp(entite, temp->name) != 0)
    {
        temp = temp->next;
    }
    return temp;
}
int declared(char entite[], int j)
{

    Element *temp = elementList;

    while (temp != NULL && strcmp(entite, temp->name) != 0)
    {
        temp = temp->next;
    }
    if (strcmp(temp->type, "aa") == 0 || temp->locale_global != j)
    {
        return 0;
    }
    else
    {
        return 1;
    }
}
void inserertype(char entite[], int i, int j)
{
    Element *e = rechercheridf(entite);

    if (e != NULL)
    {
        switch (i)
        {
        case 1:
            strcpy(e->type, "INTEGER");
            break;
        case 2:
            strcpy(e->type, "REAL");
            break;
        case 3:
            strcpy(e->type, "CHAR");
            break;
        case 4:
            strcpy(e->type, "BOOL");
            break;
	case 5:
            strcpy(e->type, "CONST");
            break;
        default:
            break;
        }
        e->locale_global = j;
        if (j == 0)
            inserer_type_parametre(entite, i, j);
    }
    else
    {
        printf("Error: Entity '%s' not found!\n", entite);
    }
}

int getTailleTab (char entite[])
{
  Element *e = rechercheridf(entite);
  return e->taille;}



int gettype(char entite[])
{
    Element *e = rechercheridf(entite);
    char type[][20] = {"INTEGER", "REAL", "CHAR", "BOOL"};
    if (e != NULL)
    {
        if (strcmp(type[0], e->type) == 0)
        {
            return 1;
        }
        else if (strcmp(type[1], e->type) == 0)
        {
            return 2;
        }
        else if (strcmp(type[2], e->type) == 0)
        {
            return 3;
        }
        else if (strcmp(type[3], e->type) == 0)
        {
            return 4;
        }
        else
        {
            return 0;
        }
    }
    return -1;
}

void inserertaille(char entite[], int taille1, int taille2)
{
    Element *e = rechercheridf(entite);

    if (e != NULL)
    {
        e->taille = taille1;
        e->taille2 = taille2;
    }
    else
    {
        printf("Error: Entity '%s' not found!\n", entite);
    }
}

void insererval(char entite[], valeurs v)
{
    Element *e = rechercheridf(entite);

    if (e != NULL)
    {
        e->val = v;
    }
    else
    {
        printf("Error: Entity '%s' not found!\n", entite);
    }
}

int rechercheNonDec(char entite[], int j)
{

    Element *temp = elementList;

    while (temp != NULL && strcmp(entite, temp->name) != 0)
    {
        temp = temp->next;
    }
    if (temp != NULL && strcmp(temp->type, "aa") == 0 && strcmp(temp->code, "CONSTANT") != 0 || temp->locale_global != j)
    {
        return 1;
    }
    else
    {
        return 0;
    }
}

int max(int a, int b)
{
    if (a > b)
        return a;
    return b;
}

char *recupstring(char entite[])
{
    Element *e = rechercheridf(entite);

    return strdup(e->val.string);
}

float recupval(char entite[])
{
    Element *e = rechercheridf(entite);

    if (strcmp(e->type, "REAL") == 0)
    {
        return e->val.real;
    }
    else
    {
        int i;
        i = e->val.integer;
        return i;
    }
}

int depastaille(char entite[], int t1, int t2)
{
    Element *e = rechercheridf(entite);

    if (e->taille >= t1 && e->taille2 >= t2)
    {
        return 0;
    }
    else
    {
        return 1;
    }
}

