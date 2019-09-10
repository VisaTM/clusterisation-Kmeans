
/*******************************************************************
 *
 *  Fichier créé par VALMIR Christophe (23 Sep 2004)
 *    
 *    * Stucture de la matrice
 *
 ******************************************************************/

#ifndef _MATRICE_H
#define	_MATRICE_H
typedef struct Matrice {
  float **grille;
  unsigned int largeur;
  unsigned int hauteur;
  float valmin;
  float valmax;
}matrice;


void lecture_du_fichier_ent(char *fichier_pds,int *nbreClasse,int *nbre_Mc);
matrice *lire_matrice   (int nbreClass,int nbreMotcle,char *nomFichier);
matrice *calcul_cos (matrice *mat1);
matrice *prod_scal (matrice *mat1);
void affiche_matrice(matrice *mat);
void *emalloc       (unsigned int taille);
#endif	/* _MATRICE_H */
