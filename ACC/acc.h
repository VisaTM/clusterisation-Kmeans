/*******************************************************************
 *
 *  acc.h
 *
 *        Ce fichier contient les structuree utilisees pour      
 *     faire la cartographie avec l'Analyse en Composantes
 *     Connexes (ACC).
 *
 *
 *  Fichier créé par VALMIR Christophe (23 Sep 2004)
 *
 ******************************************************************/

typedef struct ListeValeur {
  unsigned int x;
  unsigned int y;
  float valeur;
  struct ListeValeur *suivant;
}listeValeur;

extern void lecture_du_fichier_ent(char *fichier_pds,int *nbreClasse,int *nbre_Mc);
extern matrice *lire_matrice   (int nbreClass,int nbreMotcle,char *nomFichier);
extern matrice *calcul_cos (matrice *mat1);
extern matrice *prod_scal (matrice *mat1);
extern void affiche_matrice(matrice *mat);
extern void *emalloc       (unsigned int taille);