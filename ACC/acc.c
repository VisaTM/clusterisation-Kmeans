/**********************************************************************/
/*                                                                    */
/*  Ce fichier contient le programme principal utilise pour faire la  */
/*  cartographie avec les relations d'equivalences.                   */
/*                                                                    */
/*  Les parametres d'entree/sortie :                                  */
/*    - Le nombre des prototype                                       */
/*    - Le nombre des mot-clefs.                                      */
/*    - Le non du fichier des prototypes                              */
/*    - Le nombre de niveaux en sortie                                */
/*                                                                    */
/**********************************************************************/

/*******************************************************************
 *
 *  Fichier créé par VALMIR Christophe (23 Sep 2004)
 *    * Allocation dynamique
 *    * Stucture revue
 *
 ******************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <math.h>
#include <time.h>
#include <libgen.h>
#include <float.h>
#include "liste.h"
#include "matrice.h"

    extern char *optarg;
    char *programme;

/****************************************
 * fonction:  usage
 *
 * parametre : valeur du code de sortie
 * sortie :    sortie erreur
 ****************************************/
int 
usage (int code){
    fprintf(stderr, "Usage: %s -c nbre_classes -k nb_mots-cles -f fichier_entree [ -l nb_niveaux ] \n", programme);
    fprintf(stderr, "nombre de niveaux = 10 par defaut\n");
    exit(code);
}

/****************************************
 * fonction:  main
 *
 * paramètres : options
 * sortie :     sortie strandard
 ****************************************/
 int 
main (int argc,char *argv []) 
{
    matrice *poids;
    matrice *cos;
    int  nbre_niveaux = 10;
    int  nbre_classes = 0;
    int  nbre_mots_cles = 0;
    int  c;
    char *fichier_angles = NULL;
    char *fichier_ent = NULL;
    listeValeur **tableauListeValeur;

    programme = argv[0];

while ( index(programme, '/') != (void *) NULL ) 
    programme ++;

while ( ( c = getopt(argc,argv,"c:e:f:k:l:") ) != EOF ) {
    switch( c ) 
        {
        case 'c' : 
               nbre_classes = atoi(optarg);
               break;

        case 'e' : 
               fichier_ent = optarg;
               break;

        case 'f' : 
               fichier_angles = optarg;
               break;

        case 'k' : 
               nbre_mots_cles = atoi(optarg);
               break;

        case 'l' : 
               nbre_niveaux = atoi(optarg);
               break;

        default  : usage(1);
               break;

        }    /* end switch */
    }    /* end while */ ;

  /*Verification des valeurs*/
  if ( fichier_angles == NULL )
      usage(2);
  if ( fichier_ent )
    lecture_du_fichier_ent(fichier_ent, &nbre_classes, &nbre_mots_cles);
  if ( nbre_classes <= 0 ) 
      usage(3);
  if ( nbre_mots_cles <= 0 )
      usage(3);
  if ( nbre_niveaux <= 0 ) 
      usage(3);

  /* Debut du traitement*/
  poids = lire_matrice(nbre_classes, nbre_mots_cles, fichier_angles);
  /*affiche_matrice(poid);*/

  cos = prod_scal  (poids);
  /*affiche_matrice(cos);*/
    
  tableauListeValeur = structurerMatrice(cos, nbre_niveaux);
  printParametre(tableauListeValeur, nbre_niveaux);
  
  exit(0);
}
