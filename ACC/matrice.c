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
#include "matrice.h"

/********************************************
  
  Lis le fichier ent
  pour connaitre les dimensions du tableau

 ********************************************/
void
lecture_du_fichier_ent(char *fichier_ent,int *nbreClasse,int *nbre_Mc) {
  FILE *file;

  file = fopen(fichier_ent, "r"); 
  if (file == NULL) {
    fprintf(stderr,"Impossible de trouver le fichier de parametres : %s\n",fichier_ent);
    exit(1);
  }
  fscanf(file, " %d\n", nbreClasse);
  fscanf(file, " %d\n", nbre_Mc);
  fclose(file);
}



/******************************
 
  Lis le fichier de poids
  et initialise la matrice en memoire
 
   parametre: _ nomFichier
   sortie:    _ matrice -> la matrice lu dans le fichier
 
 *******************************/
matrice *
lire_matrice(int nbreclass, int nbreMotCle, char *nomFichier) {
  FILE *file;

  matrice *original = emalloc(sizeof(matrice));
  unsigned int x,y;
  char ligne[50];
  float tmp;

  original->hauteur = nbreclass;
  original->largeur = nbreMotCle;

  /* Initialisation de la ligne de Y */
  original->grille = emalloc(sizeof(float*) *original->hauteur + 1);


  file = fopen(nomFichier,"r");
  if (file == NULL) {
    fprintf(stderr,"impossible de trouver le fichier : %s\n", nomFichier);
    exit(1);
  }
  x = 0; y = -1;
  original->grille[0] = emalloc(sizeof(float) * (original->largeur) * (original->hauteur));

/*fprintf(stderr,"%d %d\n",nbreclass, nbreMotCle);
fprintf(stderr,"Zone [%ld,%ld]\n", original->grille[0] ,original->grille[0]+ sizeof(float) * (original->largeur) * (original->hauteur));*/
  while(!feof(file)) {
    y++;
    for (x = 0 ; x < original->largeur ; x++ ) {
      fscanf(file," %f\n",&tmp);
      original->grille[y][x] = tmp;
    }
    original->grille[y+1] = &(original->grille[y][x]);
  } 
  fclose(file);
  return original;
}


/*
  original->hauteur=nbreclass;
  original->largeur=nbreMotCle;

  Initialisation de la ligne de Y
  original->grille=emalloc(sizeof(float*)*original->hauteur);


  file=fopen(nomFichier,"r"); 
  x=0;y=-1;
  while(!feof(file)) {
    y++;
    original->grille[y]=emalloc(sizeof(float)*original->largeur);
    for (x=0;x<original->largeur;x++) {
    fscanf(file,"%f ",&original->grille[y][x]);
    }
  }  
  fclose(file);

*/


/**********************************
   Realise le produit scalaire   
   sur une coordonnée            

  parametre: _ mat1 -> ala matrice
             _ x,y  -> coordonée du point à calculer

  sortie:    _ un float -> a la valeur du produit scalaire à
               cette coordonée
 **********************************/
float
prod_scal_coord(matrice *mat1, unsigned int x, unsigned int y) {
  register unsigned int cpt;
  register float res = 0;
  register int rest;
  register unsigned int cote_min;

  for (cpt = 0 ; cpt < mat1->largeur ; cpt++ ) 
    res += (mat1->grille[x][cpt]) * (mat1->grille[y][cpt]);
  return res;
}



/*****************************
 
    Renvoie le produit scalaire 
    de la matrice

  parametre: _ l'addresse de la matrice
  sortie:    _ adresse de la nouvelle matrice calculer

  
 ******************************/
matrice *
prod_scal (matrice *mat1) {
  register unsigned int x,y;
  register float temp;
  matrice *resultat = emalloc(sizeof(matrice));

  /*initialiser la matrice resultat*/
  resultat->hauteur = mat1->hauteur;
  resultat->largeur = mat1->hauteur;
  resultat->grille = emalloc(sizeof(float *)*resultat->hauteur);
  resultat->valmin = FLT_MAX;
  resultat->valmax = FLT_MIN;


  for ( y = 0 ; y < resultat->hauteur ; y++ )
    resultat->grille[y] = emalloc(sizeof(float *)*resultat->largeur);

  for ( y = 0 ; y < mat1->hauteur ; y++ )
    for ( x = 0 ; x <= y ; x++ ) {
      if ( x != y ) {
        temp = prod_scal_coord(mat1,y,x);
	resultat->grille[x][y] = temp;
	resultat->grille[y][x] = temp;

	/*Mets le min et le max de la matrice*/
	if ( temp != 0 ) {
	  if (resultat->valmin>temp)  resultat->valmin = temp-0.000001;
	  if (resultat->valmax<temp)  resultat->valmax = temp+0.000001;
	}
      }
      else
	resultat->grille[y][x] = 1;
    }
  return resultat;
}


/**********************************
 
  Affiche la matrice en memoire
  sur l'entrée standard

  Utiliser pour le dévelopement

 *********************************/
void affiche_matrice(matrice *mat) {
  int x, y;

  for( y = 0 ; y < mat->hauteur ; y++ ) {
    for( x = 0 ; x < mat->largeur ; x++ ) {
      printf("%.3f ",mat->grille[y][x]);
    }
    putchar ('\n');
  }
}



/***********************************
 
 fonction: emalloc
 
   Allocation de la memoire avec 
  la gestion des erreurs
 
 ***********************************/
void *
emalloc(unsigned int taille){
  void *pointeur = calloc((size_t) taille,1);
  if (pointeur == NULL) {
    fprintf(stderr, "Memoire pleine\n");
    exit(1);
  }
  return pointeur;
}
