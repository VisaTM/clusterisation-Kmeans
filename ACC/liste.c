
/*******************************************************************
 *
 *  Fichier créé par VALMIR Christophe (23 Sep 2004)
 *    * Allocation dynamique
 *    * Stucture revue
 *
 ******************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <libgen.h>
#include <float.h>
#include "liste.h"


/*****************************************
  Imprime les valeurs en entree standard
  pour l'applet
 
  fonction: _ printParametre

  parametre: _ liste=     un tableau de liste des valeurs
             _ nbreListe= nombre de liste dans le tableau
  sortie:    _ l'entrée strandard-> parametre de l'applet
 
 
*****************************************/
void
printParametre (listeValeur **liste, unsigned int nbreListe) {
  register unsigned int cpt;
  register listeValeur *element;
  register float valeur = -1;

  for ( cpt = 0; cpt < nbreListe ; cpt++ ) {
    element = liste[cpt];
    while (element != NULL) {
          /*  Modification D. BESAGNI (25/02/2009)
                   remplacement des caracteres separateurs
                   par des caracteres ASCII non-ambigus.
             */
      printf("%06d,%06d,%.4f", element->y, element->x, element->valeur);
      element = element->suivant;
      if ( element != NULL)
                putchar(':');
    }
         putchar('\n');
 }
}


/*****************************

  fonction: _ inserer_chaine_classee
    insere dans la liste un nouvelle element Triée

  parametre: _ listeValeur -> adresse du premier element de la liste
             _ x,y -> coordonnée dans la matrice
             _ valeur -> valeur de la coordonnée
  sortie:    _ L'adresse de la nouvelle liste

*******************************/
listeValeur *
inserer_chaine_classee (listeValeur *liste, unsigned int x, unsigned int y, float valeur) {
  register listeValeur *index = 0;
  register listeValeur *element=emalloc(sizeof(listeValeur));
  
  element->x = x;
  element->y = y;
  element->valeur = valeur;

  index = liste;
  /*recherche de sa place dans la liste*/
  while ( index != NULL ) {
    if ( index->valeur >= valeur )  {
       if( !index->suivant || index->suivant->valeur < valeur )
       {
        element->suivant = index->suivant;
        index->suivant = element;
        return liste;
      }
    }
    index=index->suivant;
  }

  /*J'ai pas trouve de place je prends la premiere */
  element->suivant = liste;
  liste = element;
  return liste;
}

/*****************************

  fonction: _ inserer_chaine
    insere dans la liste un nouvelle element Non Triée
    Cette fonction est tres rapide 

  parametre: _ listeValeur -> adresse du premier element de la liste
             _ x,y -> coordonnée dans la matrice
             _ valeur -> valeur de la coordonnée
  sortie:    _ L'adresse de la nouvelle liste

*******************************/
listeValeur *
inserer_chaine (listeValeur *liste,unsigned int x,unsigned int y,float valeur) {
  register listeValeur *element=emalloc(sizeof(listeValeur));
  element->x = x;
  element->y = y;
  element->valeur = valeur;
  element->suivant = liste;
  liste = element;
  return liste;
}



/********************************************
 
  fonction: _ visu_chaine

  Utiliser au moment du developement

  parametre: _ une liste de valeurs
  sortie:    _ sortie strandard -> affiche la chaine

********************************************/
void
visu_chaine (listeValeur *liste) {
  while ( liste != NULL ) {
    printf("[%d,%d]=%f\n", liste->x, liste->y, liste->valeur);
    liste = liste->suivant;
  }
   printf("\n");
}



/********************************************
 
  fonction: _ structurerMatrice

    Converti la matrice en tableau de liste de valeur
    listeValeur[0] contient la liste de valeur du level 1

  parametre: _ mat -> matrice
             _ nbreLevel -> nombre de tableau (ou le nombre de level)
  sortie:    _ listeValeur -> un tableau de liste de valeur de hauteur nbreLevel
 
 ********************************************/
listeValeur **
structurerMatrice(matrice *mat, int nbreLevel) {

  register listeValeur **retour;
  register float debutPartie = mat->valmin;
  register float pas = (mat->valmax - mat->valmin) / (float)nbreLevel;
  register unsigned int niveau;
  register unsigned int x,y;
  
  retour=emalloc(sizeof(listeValeur*)*nbreLevel);
  for ( y = 0 ; y < mat->hauteur ; y++ )
    for ( x = 0 ; x < y ; x++ ) {
      /*recherche a quel niveau le nombre appartient*/
      niveau = nbreLevel - ((mat->grille[x][y] - debutPartie) / pas);
      /*L'ajouter dans la liste a la quel il coorespond*/
      if ( mat->grille[x][y] != 0 ) {
        /*fprintf(stderr,"niveau %d J insere la coor (%d,%d)=%f\n",niveau,x,y,mat->grille[x][y]);*/
        retour[niveau] = inserer_chaine_classee (retour[niveau], x, y, mat->grille[x][y]);
       }
    }

  return retour;
}
