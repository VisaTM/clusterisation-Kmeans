#include "matrice.h"

#ifndef _LISTE_H
#define	_LISTE_H

typedef struct ListeValeur {
  unsigned int x;
  unsigned int y;
  float valeur;
  struct ListeValeur *suivant;
}listeValeur;

void printParametre     (listeValeur **liste,unsigned int nbreListe);
listeValeur **structurerMatrice(matrice *mat,int nbreLevel);
listeValeur *inserer_chaine_classee (listeValeur *liste,unsigned int x,unsigned int y,float valeur);
#endif	/* _LISTE_H */
