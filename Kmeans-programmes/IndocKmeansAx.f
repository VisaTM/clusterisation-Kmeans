c ****************************************************************
c *		      IndocKmeansAx.f 
c *
c * - Fonction de classification : methode des K-MEANS axiales
c *   Permet n pasages adaptatifs suivis de passages non
c *   adaptatifs
c *      - Utilise le module IndocKmeansTools.f.
c *      - Les numerotations des documents,
c *         		     des descripteurs,
c *		             et des neurones commencent par zero.
c *      - Passage batch arret NB_CHGT = 0, puis NB_LEC_MAX = 50.
c *      - Lit deux fichiers :
c *	           a.data : donnees
c *	           a.init : matrice constituee par IndocInitMat, 
c *                         la lexure est effectuee par LIRE_POIDS
c *                         de IndocKmeansTools.f.
c *      - Genere 7 fichiers :
c *	           a.pds.tmp : matrice definitive, 
c *		           sauvegarde par ECRIRE_POIDS de 
c *                        IndocKmeansTools.f;
c *	           a.doc.tmp : triplets 
c *		           (no_ neurones \t 
c *                         coordonnee_sur_le_neurone : 
c *                         no_de_document);
c *	           a.des.tmp : triplets
c *		           (no_neurones \t 
c *                         coordonnee_sur_le_neurone : 
c *                         no_de_descripteur);
c *	           a.iner.tmp : inertie des neurones
c *	           a.card.tmp : cardinal des neurones
c *	           a.res.tmp :
c *         		- deroulement de l'algorithme : evolution
c *                       de NB_CHGT, somtau et somtauvrai
c *       		- classe des documents (NO_DOC lu dans 
c *                       a.lst : T(NO_DOC)
c *       		- somme totale et sommes en colonnes.
c *	           a.entete.tmp : entete de la matrice des poids;
c *
c * - Parametres :
c *    -d fichier de donnees (defaut : a.data)
c *    -i fichier sauvegarde des poids (defaut : a.init)
c *    -op output prefixe des fichiers generees (defaut : a)
c *    -os output suffixe des fichiers generees (defaut : a)
c *    -sd seuil des coordonnees des documents (defaut : 0.3)
c *    -sk seuil des coordonnees des descripteurs (defaut : 1)
c *    -a nombre de passages adaptatifs (defaut : 0)
c *    -pk (valeurs des mots cles ponderees ; defaut : non)
c *
c * Auteur : C. FRANCOIS
c * Date : 19/01/91
c * Modification : 19/02/92 (C. FRANCOIS)
c *
c *		   C. FRANCOIS 12/08/93
c *		   - Addition de l'option a : nombre de passages 
c *                  adaptatifs suivis de passages adaptatifs : le
c *                  nombre maximun de passages reste 50
c *		   - Modification du choix du theme qui apprend 
c *                  =pb exe-quos
c *		   - Affichage du nombre de documents non affectes
c *		   - Traitement des erreurs 
c *		   - Taille des tableaux dans IndocKmeansAx.h	
c *
c *		   C. FRANCOIS 09/11/93
c *		   - Appel de ECRIRE_POIDS
c *		   - Mise en commentaire de la creation de donz.donz 
c *			= fichier des poids lisible par SPADN
c *		   - traitement des erreurs STOP -> CALL EXIT(200)
c *		   - fin du programme END -> CALL EXIT(0)
c *		   - creation a.card.tmp et a.iner.tmp
c *		   - suppression de nb max documents et descripteurs
c *		      car inutiles ici
c *
c *		   C. FRANCOIS 11/09/95
c *		   - Consevation de tous les documents :
c *		     - Modification de la fonction : AFFICHE_DOC
c *		     - Impression systematique de la projection max 
c *		     pour un document, utilisation des euils pour 
c *		     les projections suivantes 
c *
c *		   C. FRANCOIS 26/03/96
c *		   - addition du parametre os , o devient op
c *		     op = prefixe des fichiers de sortie,
c *		     os = suffixe des fichiers de sortie.
c *		   C. FRANCOIS 20/04/98
c *		   - modification du format des données en entrée
c *		     FORMAT(I6,5X,I2)	!entete d'un document (no doc, nb des)
c *		     devient 
c *		     FORMAT(I6,5X,I7)	!entete d'un document (no doc, nb des)
c *		   C. FRANCOIS 28/11/2000
c *		   - modification de la taille des chaines de caracteres 
c *                  de stockage du nom + chemin des fichiers 40 -> 100
c *
c *		   C. FRANCOIS 31/10/2003
c *		   - modification appel de ECRIRE_POIDS, ecriture avec entete
c *		   - Suppression de la creation de donz.donz 
c *			= fichier des poids lisible par SPADN
c *
c *		   C. FRANCOIS 10/05/2010
c *		   - rajout d'un tableau de valeurs pour TF-IDF.
c *
c *		   D. BESAGNI 19/07/2013
c *		   - ecriture de la ponderation des documents et des 
c *                  descripteurs avec 3 decimales au lieu de 2.
c *
c ****************************************************************

        PROGRAM IndocKmeansAx 
        
        INCLUDE "IndocKmeansAx.h"


c --------------------------------------------------------------
c
c	declarations
c
c --------------------------------------------------------------

c ...	parametres de la methode
        CHARACTER*100	ARGV
        INTEGER		I,IARGC,M
        INTEGER		NB_ADAPTATIF	! nb passages adaptatifs avant 
        				! le traitement en non adaptatif
        REAL		SEUIL_DOC	! seuil min de coordonnees : doc.
        REAL		SEUIL_DES	! seuil min de coordonnees : des.
        CHARACTER*70	RACINE		! prefixe des noms de fichiers
        INTEGER		LONG_RACINE	! longueur reelle de RACINE
        CHARACTER*30	SUFFIXE		! suffixe des noms de fichiers
        INTEGER		LONG_SUFFIXE	! longueur reelle de SUFFIXE
        CHARACTER	BLANC
        LOGICAL		PONDERATION	! descripteurs pondérés ?

c ...	fichiers a ouvrir
        CHARACTER*100 	DONNEES 	! donnees a lire
        CHARACTER*100 	INITIAL		! matrice a lire
        CHARACTER*100 	RESULTATS 	! varaibles stat. pour interpretation
        CHARACTER*100 	POIDS		! sauvegarde de MATRICE
        CHARACTER*100 	ENTETE		! sauvegarde de l'entete
        CHARACTER*100	TRIDOC		! sauvegarde des coord. des doc.
        CHARACTER*100	TRIDES		! saugarde des coord. des des.
        CHARACTER*100	INER_FIC	! saugarde de l'inertie des classes
        CHARACTER*100	CARD_FIC	! saugarde du cardinal des classes

c ... 	parametres  generaux lus dans le fichier DONNEES
        INTEGER 	DMAX	! nombre total de descripteurs -1 (0 -> DMAX)
        INTEGER		NB_DOC 	! nombre total de documents  -1 (0 -> NB_DOC)
        CHARACTER*40 	TITRE 	! chaine de titre du fichier a lire
        INTEGER		NO_DOC	! numero du document courant
        INTEGER		NB_DES 	! nombre de descripteurs indexant ce doc.
        INTEGER 	DES(0:IDESCRIPTEURS)	! tableau de ces descripteurs
c ...	rajout d'un tableau de valeurs pour TF-IDF (C. François 10/05/2010)
        REAL		VALDES(0:IDESCRIPTEURS)	! tableau des valeurs de ces descripteurs

c ... 	parametres  generaux lus dans le fichier INITIAL
        INTEGER 	NMAX	! nombre de neurones demandes (0 -> NMAX)
        REAL		MATRICE(0:NEURONES,0:IDESCRIPTEURS) !matrice des neurones

c ...	variables definissant l arret de la classification
        INTEGER		NB_CHGT 	! nb changement de classe
        INTEGER		NB_LEC    	! nb lectures effectuees

c ...	variables utilisees pour l apprentissage
        REAL		X(0:IDESCRIPTEURS)	! DES normalisee
        REAL		Y(0:NEURONES)		! MATRICE * X
        INTEGER		KMAX			! indice du neurone qui apprend
        INTEGER 	V(0:IDOCUMENTS)		! marque des documents
        REAL		ACCUMUL(0:NEURONES,0:IDESCRIPTEURS) !copie de la matrice 
        				!des neurones

c ...	variables utilisees pour evaluer la qualite des resultats 
        REAL		TAU(0:NEURONES) ! tableau des sommes de proj. 
        				! des neurones v. adaptative
        REAL		TAUVRAI(0:NEURONES) 	! tableau des sommes de proj. 
        					! des neurones v. non adaptative
        INTEGER 	SOMCOL(0:IDESCRIPTEURS) ! sommes des colonnes de la matrice
        					! des documents non normalisee !!!!!!
        INTEGER		CARD_CLASS(0:NEURONES)	! cardinal des classes
        INTEGER		SOMTOT 			! somme de SOMCOL
        REAL         	SOMTAU		! somme TAU 
        				! critere  geometrique (lelu p.97)
        REAL         	SOMTAUVRAI	! somme TAUVRAI
        				! critere  inertiel (lelu p.97)
        REAL		POND		! valeur ponderee des descripteurs
        INTEGER		JJ		! compteur des documents
c	INTEGER 	NON_TRAITE	! nombre de documents non traites
        INTEGER 	NON_CLASSE	! nombre de documents non classes
        INTEGER 	NBCLASS		! nombre reel de classes obtenues

c ...	variables logiques
        LOGICAL		ADAPTATIF	! vrai si algorithme adaptatif

c ...	message d'erreur
        CHARACTER*100 	ERREUR 		! message imprime sur erreur standard
        CHARACTER*1	TAB		! caractere tabulation
c --------------------------------------------------------------
c
c	declaration des formats
c
c --------------------------------------------------------------

c ...	formats de lecture dans DONNEES

10      FORMAT(I7, I7, A40) 	!parametres  generaux dans le fichier DONNEES
11      FORMAT(I6,5X,I7)	!entete d'un document (no doc, nb des)
12      FORMAT(I6)		!no de descripteur
13      FORMAT(10X) 		!premiere ligne du fichier DONNEES


c ...	formats d'ecriture dans RESULTATS

c ...	ecriture en cours de classification des variables
14      FORMAT(I6,A,I4,A,I4,A,I6,A,F10.2,A,F10.2) 

c ...	ecriture en fin de classification
15      FORMAT(A40) 		!titre lu dans le fichier DONNEES
16      FORMAT('liste des TAU des classes :' /, (10F7.2)) 
18      FORMAT( 'classe des documemts(no doc : no classe)')
141     FORMAT(I7, ' documemts traites')
181     FORMAT(I7, ' documemts non traites')
182     FORMAT(I7, ' documemts non classes')
19      FORMAT(I4, ' : ', I6)	!no doc : classe de ce document
103     FORMAT(' somme totale :',I9, ' sommes en colonne :'/,(10I7)) 

c ...	formats d'ecriture dans INER_FIC

161     FORMAT(I6.6, A, F7.2)	!no neurone : tauvrai du neurone

c ...	formats d'ecriture dans CARD_FIC

17      FORMAT(I6.6, A, I7)	!no neurone : cardinal du neurone

c ...	formats d'ecriture dans TRIDOC et TRIDES

108     FORMAT(I6, A, F6.3, ':', I6)	!no classe \t coordonnee : no doc ou des



c --------------------------------------------------------------
c
c      initialisations des parametres de la methode
c
c --------------------------------------------------------------

        NB_ADAPTATIF = 1
        DONNEES = 'a.data'  
        INITIAL = 'a.init' 
        RACINE = ''
        BLANC = ' '
        TAB = CHAR(9)
        PONDERATION = .FALSE.
        RESULTATS = 'a.res.tmp'  
        POIDS = 'a.pds.tmp'
        ENTETE = 'a.entete.tmp'
c     	POIDS_SPADN = 'a.don.tmp' 
        TRIDOC = 'a.doc.tmp' 
        TRIDES = 'a.des.tmp'
        INER_FIC = 'a.iner.tmp'
        CARD_FIC = 'a.card.tmp' 
        SEUIL_DOC = 0.3
        SEUIL_DES = 1.0

        M = IARGC()
        I = 1
        DO WHILE (I .LE. M)
        	CALL GETARG(I,ARGV)

        	IF ( ARGV .EQ. '-a') THEN
        		I = I + 1
        		CALL GETARG(I,ARGV)
        		NB_ADAPTATIF = IATOI(ARGV)
        	ELSE IF ( ARGV .EQ. '-d') THEN
        		I = I + 1
        		CALL GETARG(I,ARGV)
        		DONNEES = ARGV
        	ELSE IF ( ARGV .EQ. '-i') THEN
        		I = I + 1
        		CALL GETARG(I,ARGV)
        		INITIAL = ARGV
        	ELSE IF ( ARGV .EQ. '-op') THEN
        		I = I + 1
        		CALL GETARG(I,ARGV)
        		RACINE = ARGV
        	ELSE IF ( ARGV .EQ. '-os') THEN
        		I = I + 1
        		CALL GETARG(I,ARGV)
        		SUFFIXE = ARGV
        	ELSE IF ( ARGV .EQ. '-sd') THEN
        		I = I + 1
        		CALL GETARG(I,ARGV)
        		SEUIL_DOC = FATOF(ARGV)
        	ELSE IF ( ARGV .EQ. '-sk') THEN
        		I = I + 1
        		CALL GETARG(I,ARGV)
        		SEUIL_DES = FATOF(ARGV)
        	ELSE IF ( ARGV .EQ. '-pk') THEN
        		I = I + 1
        		CALL GETARG(I,ARGV)
        		PONDERATION = .TRUE.
        	ELSE
        		WRITE(0,*) 'usage: IndocKmeansAx 
     1  		-a number of adapatif treatement
     1  		-d data file -i matrix file -op root of ouput files 
     1  		-os suffix of ouput files
     1  		-sd documents seuil -sk keywords seuil [ -pk ]'
        		CALL EXIT(200)
        	ENDIF
        	I = I + 1
        ENDDO

c ...	mise a jour du nom des fichiers crees
        IF (RACINE .NE. '') THEN
        	IF(INDEX(RACINE,BLANC) .NE. 0) THEN
        		LONG_RACINE = INDEX(RACINE,BLANC) - 1
        	ELSE
        		LONG_RACINE =  LEN(RACINE) + 1
        	ENDIF
        ENDIF
        IF (SUFFIXE .NE. '') THEN
        	IF(INDEX(SUFFIXE,BLANC) .NE. 0) THEN
        		LONG_SUFFIXE = INDEX(SUFFIXE,BLANC) - 1
        	ELSE
        		LONG_SUFFIXE =  LEN(SUFFIXE) + 1
        	ENDIF
        ENDIF

        IF (RACINE .NE. '') THEN
        	IF (SUFFIXE .NE. '')  THEN
        		RESULTATS = RACINE(1:LONG_RACINE)//'res.'
     1  					//SUFFIXE(1:LONG_SUFFIXE)
        		POIDS = RACINE(1:LONG_RACINE)//'pds.'
     1  					//SUFFIXE(1:LONG_SUFFIXE)
        		ENTETE = RACINE(1:LONG_RACINE)//'entete.'
     1  					//SUFFIXE(1:LONG_SUFFIXE)
c			POIDS_SPADN = RACINE(1:LONG_RACINE)//'donz.'//SUFFIXE(1:LONG_SUFFIXE)
        		TRIDOC = RACINE(1:LONG_RACINE)//'doc.'
     1  					//SUFFIXE(1:LONG_SUFFIXE)
        		TRIDES = RACINE(1:LONG_RACINE)//'des.'
     1  					//SUFFIXE(1:LONG_SUFFIXE)
        		INER_FIC = RACINE(1:LONG_RACINE)//'iner.'
     1  					//SUFFIXE(1:LONG_SUFFIXE)
        		CARD_FIC = RACINE(1:LONG_RACINE)//'card.'
     1  					//SUFFIXE(1:LONG_SUFFIXE)
        	ELSE
        		RESULTATS = RACINE(1:LONG_RACINE)//'res.'
        		POIDS = RACINE(1:LONG_RACINE)//'pds.'
        		ENTETE = RACINE(1:LONG_RACINE)//'entete.'
c			POIDS_SPADN = RACINE(1:LONG_RACINE)//'donz.'
        		TRIDOC = RACINE(1:LONG_RACINE)//'doc.'
        		TRIDES = RACINE(1:LONG_RACINE)//'des.'
        		INER_FIC = RACINE(1:LONG_RACINE)//'iner.'
        		CARD_FIC = RACINE(1:LONG_RACINE)//'card.'
        	ENDIF
        ENDIF

c --------------------------------------------------------------
c
c	ouverture des fichiers
c
c --------------------------------------------------------------

c ... 	ouverture du fichier de donnees  
        CALL NOUVR(1, DONNEES, .TRUE., .TRUE., 0) 
        REWIND 1 

c ... 	ouverture du fichier de resultats
        CALL NOUVR(2, RESULTATS, .TRUE., .TRUE., 0) 
        REWIND 2



c --------------------------------------------------------------
c
c      initialisations generales
c
c --------------------------------------------------------------

c ... 	lecture des parametres  generaux dans le fichier DONNEES
        READ(1,10) DMAX, NB_DOC, TITRE 

c ...	NB_DOC = le nombre de documents total et comme la numerotation
c ...	commence a 0, on va jusqu'a NB_DOC-1. 	
        NB_DOC = NB_DOC -1

c ...	blindage si NB_DOC trop important !!!!
        IF(NB_DOC .GT. IDOCUMENTS) THEN
        	WRITE(0,*) 'IndocKmeansAx :'
        	WRITE(0,*) 'nombre de documents trop important'
        	CLOSE(1)
        	CALL EXIT(200)
        ENDIF

c ...	initialisation de MATRICE par lecture de INITIAL
        CALL LIRE_POIDS(INITIAL,NEURONES,NMAX,DMAX,MATRICE,.TRUE.) 
 
c ...	initialisation de V(NB_DOC) 
        DO I = 0, IDOCUMENTS 
        	 V(I)= -2
        ENDDO 
c ...	initialisation de SOMCOL
        DO  I = 0,DMAX 
        	SOMCOL(I) = 0. 
        ENDDO 

c ...	initialisations divers
        KMAX = -1
        SOMTOT = 0 
        NB_CHGT = 1
        JJ = 0
        NB_LEC = 0
        ERREUR = ''

c --------------------------------------------------------------
c
c	ecriture du deroulement de la classification dans RESULTATS
c
c --------------------------------------------------------------

c	WRITE(2,*) ' '
c ...	impression des parametres generaux lus dans DONNEES
c	WRITE(2,*) 'Fichier de donnes:', DONNEES
c      	WRITE(2,*) NB_DOC+1, ' documents dans le fichier direct', TITRE
c      	WRITE(2,*) DMAX+1, ' mots-cles dans l index affine' 
c ...	impression des parametres generaux lus dans INITIAL
c	WRITE(2,*) 'Fichier contenant la matrice initialisee:', INITIAL
c ...	impression des parametres generaux de la methode
c	WRITE(2,*) 'Fichier de sauvegarde de la matrice:', POIDS
c	WRITE(2,*) 'Fichier de sauvegarde de la matrice lisible par SPADN:',POIDS_SPADN
c	WRITE(2,*) 'Fichier des coordonnees des documents:', TRIDOC
c	WRITE(2,*) 'Fichier des coordonnees des descripteurs:', TRIDES

        WRITE(2,*) '- Deroulement de la classification'
        WRITE(2,*) '--------------'
        WRITE(2,*) ' '
        WRITE(2,*) 'Entete des indicateurs'
        WRITE(2,*) ' '
        WRITE(2,*) 'Numero de passage : Nb Pas'
        WRITE(2,*) 'Nombre de classes obtenues : Cl'
        WRITE(2,*) 'Nombre de classes unitaires: ClU'
        WRITE(2,*) 'Nombre de changements : Chgt' 
        WRITE(2,*) 'Critere geometrique : sTAU' 
        WRITE(2,*) 'Critere inertiel : sTV'
        WRITE(2,*) '--------------'
c       WRITE(2,*) 'Nb Pas \t  Cl \t ClU \t  Chgt \t     sTAU \tsTV'
        WRITE(2,*) 'Nb Pas'//TAB//'Cl'//TAB//'ClU'//TAB//'Chgt'//TAB
     1  					//'sTAU'//TAB//'sTV'

 
c --------------------------------------------------------------
c
c     lecture du fichier de donnees
c
c --------------------------------------------------------------

        DO WHILE ((NB_CHGT .NE. 0)  .AND. (NB_LEC .LT. NB_LEC_MAX))

c ............. gestion du nombre de passage adaptatifs avant non adapatifs 
        	IF ( NB_LEC .LT. NB_ADAPTATIF) THEN
        		ADAPTATIF = .TRUE.
        	ELSE
        		ADAPTATIF = .FALSE.
        	ENDIF

c ............. reinitialisations en debut de fichier
        	NB_LEC = NB_LEC + 1
        	NB_CHGT = 0 
        	JJ = 0
        	DO K = 0,NMAX 
        		TAU(K) = 0. 
        		TAUVRAI(K) = 0.
        		CARD_CLASS(K) = 0 
        	ENDDO 
c ...		initialisation de ACCUMUL
        	DO  K = 0,NMAX
        		DO  J = 0,DMAX 
        			ACCUMUL(K,J) = 0.
        		ENDDO 
        	ENDDO

c ............. lecture du fichier avec apprentissage
c ............. 	lecture d un document , calcul de Y et de l indice KMAX
c .............		mise ajour du TAU de la classe KMAX et de NB_CHGT
c .............		mise a jour du cardinal de la classe KMAX
242     		CALL TRAITE_DOC(IDOCUMENTS,DMAX,NMAX,NO_DOC,NB_DES,
     1  		DES,JJ,X,VALDES,NEURONES,Y,MATRICE,KMAX,NB_DOC,V,
     1  		NB_CHGT,CARD_CLASS,TAU,PONDERATION,ERREUR,*243,*290)

c ..................... si le document est affecte,
c ..................... apprentissage  et normalisation du neurone KMAX
        		if (KMAX .GE. O) THEN
        			if (ADAPTATIF) THEN
        				CALL APPRENTISSAGE(NEURONES,NMAX,DMAX,MATRICE,
     1  				KMAX,NB_DES,DES,TAU,X,Y)
        			ELSE
        				CALL APPRENTISSAGE(NEURONES,NMAX,DMAX,ACCUMUL,
     1  				KMAX,NB_DES,DES,TAU,X,Y)
        			ENDIF
        		ENDIF

c.............  	lecture du document suivant 
        		GO TO 242 
243     	CONTINUE 
c ............. fin de lecture du fichier avec apprentissage

c ............. le contenu de ACCUMUL est verse dans Matrice
        	IF (.NOT. ADAPTATIF) THEN
        		DO K = 0, NMAX
        			DO II = 0, DMAX
        				MATRICE(K, II) = ACCUMUL(K, II)
        			ENDDO
        		ENDDO
        	ENDIF
 
c .............	lecture du fichier sans apprentissage
c .............	positionnement en debut de fichier 
        	REWIND 1 
        	READ(1,13) 
c ............. 	lecture d un document , calcul de Y 
c ............. 	mise ajour du TAUVRAI de la classe V(NO_DOC)
244     	CALL VARIANCE(DMAX,NMAX,NO_DOC,NB_DES,DES,VALDES,X,
     1  	NEURONES,Y,MATRICE,NB_DOC,V,TAUVRAI,PONDERATION,
     1  	ERREUR,*245,*290)

c ............. 	lecture du document suivant 
        	GO TO 244 
245     	CONTINUE
c ............. fin de lecture du fichier sans apprentissage

c ............. ecriture des resultats intermediaires dans RESULTAT 
        	SOMTAU = 0. 
        	SOMTAUVRAI   = 0. 
        	NBMONO = 0 
        	NBCLASS = 0 
        	DO K = 0, NMAX 
        		SOMTAU = SOMTAU + TAU(K) 
        		SOMTAUVRAI   = SOMTAUVRAI   + TAUVRAI(K) 
        		IF(CARD_CLASS(K) .NE. 0) NBCLASS = NBCLASS + 1 
        		IF(CARD_CLASS(K) .EQ. 1) NBMONO = NBMONO + 1 
        	ENDDO  
        	WRITE(2,14) NB_LEC, TAB, NBCLASS, TAB, NBMONO, TAB, NB_CHGT, 
     1  	TAB, SOMTAU, TAB, SOMTAUVRAI

c ............ repositionnemt en debut de fichier 
        	REWIND 1 
        	READ(1,13) 
        ENDDO	!fin du while

 
c --------------------------------------------------------------
c
c	place des documents 
c
c --------------------------------------------------------------

c ...	ouverture du fichier des coordonnees des documents a trier
c ...	sur les neurones : TRIDOC 
        CALL NOUVR(4, TRIDOC, .TRUE.,.TRUE.,0)
        REWIND(4)

c ...	dernier passage sans apprentissage 
c ...	positionnement en debut de fichier deja fait en fin d'apprentissage
 
c ............. lecture d un document et m. a j. des sommes en colonne,  
c .............	calcul de Y
c .............	ecriture de TRIDOC:triplets(K,val,NO_DOC) en acces sequentiel
c ............. impression des classes de chaque documents dans RESULTAT
246     	CALL AFFICHE_DOC(DMAX,NMAX,NO_DOC,NB_DES,
     1  	DES,VALDES,X,NEURONES,Y,MATRICE,NB_DOC,V,CARD_CLASS,
     1  	SOMCOL,SOMTOT,SEUIL_DOC,PONDERATION,ERREUR,TAB,*247,*290)

c .............	lecture du document suivant 
        	GO TO 246
247     CONTINUE
c ...	fin de lecture du fichier sans apprentissage

c ...	fermeture du fichier : TRIDOC
        CLOSE(4)

c --------------------------------------------------------------
c 
c	place des descripteurs
c
c --------------------------------------------------------------

c ...	ouverture du fichier des coordonnees des descripteurs a trier
c 	sur les neurones : TRIDES
        CALL NOUVR(4, TRIDES, .TRUE.,.TRUE.,0)
        REWIND(4)
c ...	pour chaque neurone
        DO   K = 0, NMAX 
c ............. ecriture d une ligne de matrice ponderee
c ............. sous forme d une serie de triplets(K,val,J) en acces sequentiel
        	IF(CARD_CLASS(K).NE.0) THEN		
        		DO J = 0, DMAX
        			IF(SOMCOL(J) .EQ. 0) THEN 
        				POND = 0 
        			ELSE 
        				POND = MATRICE(K, J) * 
     1                   		SQRT(REAL(SOMTOT) / REAL(SOMCOL(J))) 
        			ENDIF
        			IF((POND .GE. SEUIL_DES)) THEN
        				WRITE(4,108) K, TAB, POND, J
        			ENDIF
        		ENDDO
        	ENDIF
        ENDDO

c ...	fermeture du fichier : TRIDES
        CLOSE(4)	

c --------------------------------------------------------------
c
c 	ecriture des fichiers : 
c		poids, resultats, card_fic et var_fic 
c
c --------------------------------------------------------------
c                    derniere modif : 31/10/2003
c ... 	sauvegarde de matrice dans le fichier POIDS
        CALL ECRIRE_POIDS(POIDS,ENTETE,NEURONES,NMAX,DMAX,MATRICE,
     1  .FALSE.,NBCLASS,CARD_CLASS)

c ... 	ecriture des donnees TAU et CARDCLASS dans le fichier RESULTAT
c	WRITE(2,15) TITRE 
c	WRITE(2,16) (TAU(K), K = O,NMAX)  


c ...	calcule du nombre de documents non traites et non classes
c	NON_TRAITE = 0
        NON_CLASSE = O
        DO J = 0, NB_DOC
c		IF (V(J) .EQ. -2 ) NON_TRAITE = NON_TRAITE + 1
        	IF (V(J) .EQ. -1 ) NON_CLASSE = NON_CLASSE + 1
        ENDDO

c ...	impression des documents traites, non traites et non classes dans RESULTAT
        WRITE(2,*) ' '
        WRITE(2,141) JJ
c	IF (NON_TRAITE .GT. 0 ) WRITE(2,181) NON_TRAITE
        IF (NON_CLASSE .GT. 0 ) WRITE(2,182) NON_CLASSE

c ...	impression des classes de chaque documents dans RESULTAT
c	WRITE(2,18)
c	DO J = 0, NB_DOC
c		WRITE(2,19) J, V(J)
c	ENDDO

c ...	impression des SOMCOL et SOMTOT
c     	WRITE (2, 103) SOMTOT, (SOMCOL(J), J = 0, DMAX) 

        CLOSE(2) 

c ...	ouverture du fichier de la variance des neurones : INER_FIC 
        CALL NOUVR(4, INER_FIC, .TRUE.,.TRUE.,0)
        REWIND(4)
c ...	pour chaque neurone
        DO   K = 0, NMAX
c ............. ecriture d un couple (K,tauvrai)
        	IF(CARD_CLASS(K).NE.0) THEN		
        		WRITE(4,161) K,TAB,TAUVRAI(K)
        	ENDIF
        ENDDO
c ...	fermeture du fichier : VART_FIC
        CLOSE(4)

c ...	ouverture du fichier du cardinal des neurones : CARD_FIC 
        CALL NOUVR(4, CARD_FIC, .TRUE.,.TRUE.,0)
        REWIND(4)
c ...	pour chaque neurone
        DO   K = 0, NMAX
c ............. ecriture d un couple (K,card_class)
        	IF(CARD_CLASS(K).NE.0) THEN		
        		WRITE(4,17) K,TAB,CARD_CLASS(K)
        	ENDIF
        ENDDO
c ...	fermeture du fichier : CARD_FIC
        CLOSE(4)

c --------------------------------------------------------------
c
c	fin ou erreur de lecture, arret du programme
c
c --------------------------------------------------------------
        GO TO 291

290     WRITE(0,*) 'IndocKmeansAx :'
        WRITE(0,*) ERREUR
        CLOSE(1) 
        CLOSE(2)
        CLOSE(4)
        CALL EXIT(200) 

291     CLOSE(1) 
        CALL EXIT(0)
        END

c ****************************************************************
c	FIN PROGRAM IndocKmeansAx
c ***************************************************************

c ****************************************************************
c		SUBROUTINE CALCULE_Y
c
c calcule le vecteur Y(NEURONES) = MATRICE * X
c 
c auteur : C. FRANCOIS
c date : 19/11/91
c Modification : C. FRANCOIS 23/07/93
c		 - Simplification du sous-programme
c
c *************************************************************** 

        SUBROUTINE CALCULE_Y(NB_DES,NMAX,DMAX,DES,VALDES,X,NEURONES,
     1  			Y,MATRICE)

        INTEGER		NB_DES 				! nombre de descripteurs indexant ce doc.
        INTEGER 	NMAX				! nombre de neurones demandes
        INTEGER 	DMAX				! nombre total de descripteurs
        INTEGER 	DES(0:NB_DES)			! tableau de ces descripteurs
        REAL		VALDES(0:NB_DES)		! tableau des valeurs de ces descripteurs
        REAL		X(0:NB_DES)			! DES normalisee
        INTEGER		NEURONES			! nombre maximum de neurones
        REAL		Y(0:NMAX)			! MATRICE * X
        REAL		MATRICE(0:NEURONES,0:DMAX)	! matrice des neurones


c ...	variables internes a la procedure 
c       REAL		NBDES
        REAL		NORMDES

c ...	definition de X  : normalisation des DES
c       NBDES = real(NB_DES)  
c       X = SQRT(1/NBDES) 
        NORMDES = 0. 
        DO I = 0, NB_DES 
        	IF (VALDES(I) .GT. 1.0E-18) THEN
        		NORMDES = NORMDES + VALDES(I) * VALDES(I) 
        	ENDIF
        ENDDO  
        IF (NORMDES .EQ. 0.) NORMDES = .00000001 
        NORMDES = SQRT(NORMDES)  
        DO I = 0, NB_DES 
        	IF (VALDES(I) .LE. 1.0E-33) THEN 
        		X(I) = 0.0 
        	ELSE 
        		X(I) = VALDES(I)/NORMDES 
        	ENDIF 
        ENDDO 
 

c ...	calcul de  Y =  MATRICE * X 
        DO K = 0,NMAX 
        	Y(K) = 0. 
        	DO I = 0,NB_DES 
        		Y(K) = Y(K) + (MATRICE(K,DES(I)) * X(I)) 
        	ENDDO
        ENDDO 

        RETURN 
        END 

c ****************************************************************
c		SUBROUTINE APPRENTISSAGE
c
c apprentissage, normalisation du neurone MATRICE(KMAX)
c mise a jour du cardinal de la classe KMAX
c 
c auteur : C. FRANCOIS
c date : 23/07/93
c
c *************************************************************** 

        SUBROUTINE APPRENTISSAGE(NEURONES,NMAX,DMAX,MATRICE,KMAX,NB_DES,
     1                       DES,TAU,X,Y)

        INTEGER		NEURONES	! nombre maximum de neurones
        INTEGER 	NMAX		! nombre de neurones demandes
        INTEGER 	DMAX		! nombre total de descripteurs
        REAL		MATRICE(0:NEURONES, 0:DMAX) ! matrice des neurones
        INTEGER		KMAX		! indice du neurone qui apprend
        INTEGER		NB_DES 		! nombre de descripteurs indexant ce doc.
        INTEGER 	DES(0:NB_DES)	! tableau de ces descripteurs
        REAL		TAU(0:NMAX) 	! tableau des sommes de proj. des neurones
        REAL		X(0:NB_DES)	! DES normalisee
        REAL		Y(0:NMAX)	! MATRICE * X



c ...	variables internes a la procedure 
        REAL		SOMCAR 		! somme des carres des elements 
        				! du neurone KMAX

c ...	appentissage du neurone KMAX
        DO I = 1,NB_DES 
                MATRICE(KMAX,DES(I)) = MATRICE(KMAX,DES(I))+
     1  	(Y(KMAX) * X(I) / TAU(KMAX)) 
        ENDDO 

c ...	normalisation du neurone KMAX
        SOMCAR = 0. 
        DO II = 0, DMAX 
        	IF (MATRICE(KMAX, II) .GT. 1.0E-18) THEN
        		SOMCAR = SOMCAR +  MATRICE(KMAX,II) * 
     1  		MATRICE(KMAX,II) 
        	ENDIF
        ENDDO  
        IF (SOMCAR .EQ. 0.) SOMCAR = .00000001 
        SOMCAR = SQRT(SOMCAR)  
        DO II = 0, DMAX 
        	IF (MATRICE(KMAX, II) .LE. 1.0E-33) THEN 
        		MATRICE(KMAX, II) = 0.0 
        	ELSE 
        		MATRICE(KMAX, II)=MATRICE(KMAX, II)/SOMCAR 
        	ENDIF 
        ENDDO 

        RETURN
        END
c ****************************************************************
c		SUBROUTINE TRAITE_DOC
c
c lecture d'un document dans le fichier 1
c mise a jour de DES et JJ
c calcul de Y = MATRICE * X (= DES normalise)
c recherche du neurone qui apprend : KMAX
c mise a jour du vecteur V(NB_DOC) et de NB_CHGT
c mise ajour du TAU de la classe KMAX
c
c auteur : C. FRANCOIS
c date : 23/07/93
c Modification : C. FRANCOIS 27/08/1993
c	 	 Recheche du neurone qui apprend
c		 IF (Y(K) .GE. YMAX) -> IF (Y(K) .GT. YMAX)
c
c **************************************************************** 

        SUBROUTINE TRAITE_DOC(IDOCUMENTS,DMAX,NMAX,NO_DOC,NB_DES,DES,JJ,
     1  		X,VALDES,NEURONES,Y,MATRICE,KMAX,NB_DOC,V,NB_CHGT,
     1  		CARD_CLASS,TAU,PONDERATION,ERREUR,*,*)

        INTEGER 	IDOCUMENTS	! nombre max de colonnes de la matrice des classes
        INTEGER 	DMAX		! nombre total de descripteurs
        INTEGER 	NMAX		! nombre de neurones demandes
        INTEGER		NO_DOC		! numero du document courant
        INTEGER		NB_DES 		! nombre de descripteurs indexant ce doc.
        INTEGER 	DES(0:NB_DES)	! tableau de ces descripteurs
        INTEGER		JJ		! compteur des documents
        REAL		X(0:NB_DES)	! DES normalisee
        REAL		VALDES(0:NB_DES)	! tableau des valeurs de ces descripteurs
        INTEGER		NEURONES	! nombre maximum de neurones
        REAL		Y(0:NMAX)	! MATRICE * X
        REAL		MATRICE(0:NEURONES, 0:DMAX) ! matrice des neurones
        INTEGER		KMAX		! indice du neurone qui apprend
        INTEGER		NB_DOC 		! nombre total de documents 
        INTEGER 	V(0:NB_DOC)	! marque des documents
        INTEGER 	NB_CHGT		! nombre de doc. ayant changer de classe
        INTEGER		CARD_CLASS(0:NMAX)	! cardinal des classes
        REAL		TAU(0:NMAX) 	! tableau des sommes de proj. des neurones
        LOGICAL		PONDERATION	! descripteurs pondérés ?
        CHARACTER*100 	ERREUR 		! message imprime sur erreur standard

c ...	variables internes a la procedure 
        REAL		YMAX 		! intermediaire de selection 
        				! du neurone KMAX
 
c ...	formats de lecture dans DONNEES

11      FORMAT(I6,5X,I7)	!entete d'un document (no doc, nb des)
12      FORMAT(I6)		!no de descripteur
13      FORMAT(I6,F11.8)	!no de descripteur, poids descripteur


        YMAX = 0. 
        KMAX = -1 

c ...	lecture d un document  
        READ(1,11,END =30)NO_DOC, NB_DES 
        	IF (NB_DES .EQ. 0) THEN
        		ERREUR = 'document ayant 0 descripteurs'
        		GO TO 31
        	ENDIF 
        	IF(JJ .GT. IDOCUMENTS) THEN
        		ERREUR = 'nombre de documents trop important ligne 868'
        		GO TO 31
        	ENDIF
        	JJ = JJ + 1 
        	IF (PONDERATION) THEN
        		DO  I = 1,NB_DES 
        			READ(1,13) DES(I),VALDES(I) 
        			IF(DES(I) .GT. DMAX) THEN
        				ERREUR = 'numero de descripteur trop important ligne 876'
        				GO TO 31
        			ENDIF
        		ENDDO 
        	ELSE
        		DO  I = 1,NB_DES 
        			READ(1,12)DES(I) 
        			IF(DES(I) .GT. DMAX) THEN
        				ERREUR = 'numero de descripteur trop important ligne 884'
        				GO TO 31
        			ENDIF
        			VALDES(I) = 1.0
        		ENDDO 
        	ENDIF
c ...		calcul de Y 
        	CALL CALCULE_Y(NB_DES,NMAX,DMAX,DES,VALDES,X,NEURONES,
     1  				Y,MATRICE)
c ...	 	recherche du neurone qui apprend
        	DO K = 0,NMAX  
        		IF (Y(K) .LE. 1.0E-33) Y(K) = 0.0 
        		IF (Y(K) .GT. YMAX) THEN
        			YMAX = Y(K) 
        			KMAX = K 
        		ENDIF 
        	ENDDO 
c ...		remplir le vecteur V(NB_DOC) et mise a jour de NB_CHGT
        	IF (V(NO_DOC) .NE. KMAX) THEN
        		NB_CHGT = NB_CHGT + 1 
        		V(NO_DOC) = KMAX 
        	ENDIF

        	IF (KMAX .GE. 0) THEN
c ...			mise a jour du cardinal de la classe KMAX 
        		 CARD_CLASS(KMAX) = CARD_CLASS(KMAX) + 1

c ...			mise a jour du TAU de la classe KMAX
        		IF(Y(KMAX) .GT. 1.0E-18) TAU(KMAX) = TAU(KMAX) + 
     1                                          (Y(KMAX)*Y(KMAX)) 
        		IF (TAU(KMAX) .EQ. 0.) TAU(KMAX) = .00000001 
        	ENDIF

        RETURN
30      RETURN 1
31      RETURN 2
        END


c ****************************************************************
c		SUBROUTINE VARIANCE
c
c lecture d'un document dans le fichier 1
c calcul de Y = MATRICE * X (= DES normalise)
c mise ajour du TAUVRAI de la classe V(NO_DOC) 
c TAUVRAI = variance intra-classe
c
c auteur : C. FRANCOIS
c date : 23/07/93
c
c *************************************************************** 

        SUBROUTINE VARIANCE(DMAX,NMAX,NO_DOC,NB_DES,DES,VALDES,X,
     1           	NEURONES,Y,MATRICE,NB_DOC,V,TAUVRAI,PONDERATION,
     2           	ERREUR,*,*)

        INTEGER 	DMAX			! nombre total de descripteurs
        INTEGER 	NMAX			! nombre de neurones demandes
        INTEGER		NO_DOC			! numero du document courant
        INTEGER		NB_DES 			! nombre de descripteurs indexant ce doc.
        INTEGER 	DES(0:NB_DES)		! tableau de ces descripteurs
        REAL		VALDES(0:NB_DES)	! tableau des valeurs de ces descripteurs
        REAL		X(0:NB_DES)		! DES normalisee
        INTEGER		NEURONES		! nombre maximum de neurones
        REAL		Y(0:NMAX)		! MATRICE * X
        REAL		MATRICE(0:NEURONES,0:DMAX) ! matrice des neurones
        INTEGER		NB_DOC 			! nombre total de documents 
        INTEGER 	V(0:NB_DOC)		! marque des documents
        REAL		TAUVRAI(0:NMAX)		! tableau des sommes de proj. des neurones
        LOGICAL		PONDERATION		! descripteurs pondérés ?
        CHARACTER*100 	ERREUR 			! message imprime sur erreur standard

c ...	formats de lecture dans DONNEES

11      FORMAT(I6,5X,I7)	!entete d'un document (no doc, nb des)
12      FORMAT(I6)		!no de descripteur
13      FORMAT(I6,F11.8)	!no de descripteur, poids descripteur

c ...	lecture d un document  
        READ(1,11,END =30)NO_DOC, NB_DES 
        	IF (NB_DES .EQ. 0) THEN
        		ERREUR = 'document ayant 0 descripteurs'
        		GO TO 31
        	ENDIF 
        	IF (PONDERATION) THEN
        		DO  I = 1,NB_DES 
        			READ(1,13) DES(I),VALDES(I) 
        			IF(DES(I) .GT. DMAX) THEN
        				ERREUR = 'numero de descripteur trop important'
        				GO TO 31
        			ENDIF
        		ENDDO 
        	ELSE
        		DO  I = 1,NB_DES 
        			READ(1,12)DES(I) 
        			IF(DES(I) .GT. DMAX) THEN
        				ERREUR = 'numero de descripteur trop important'
        				GO TO 31
        			ENDIF
        			VALDES(I) = 1.0
        		ENDDO 
        	ENDIF

c ...		calcul de Y 
        	CALL CALCULE_Y(NB_DES,NMAX,DMAX,DES,VALDES,X,NEURONES,
     1  				Y,MATRICE)

c ...		mise ajour du TAUVRAI de la classe V(NO_DOC)
        	IF (V(NO_DOC) .GE. 0) THEN
        		IF(Y(V(NO_DOC)) .GT. 1.0E-18) THEN
        			TAUVRAI(V(NO_DOC)) = TAUVRAI(V(NO_DOC)) + 
     1  			( Y(V(NO_DOC)) * Y(V(NO_DOC)) ) 
        		ENDIF
        		IF (TAUVRAI(V(NO_DOC)) .EQ. 0.) 
     1  			TAUVRAI(V(NO_DOC)) = .00000001 
        	ENDIF

        RETURN
30      RETURN 1
31      RETURN 2
        END


c ****************************************************************
c		SUBROUTINE AFFICHE_DOC
c
c lecture d'un document dans le fichier 1
c mise a jour des sommes en colonnes
c calcul de Y = MATRICE * X (= DES normalise)
c ecriture des projections du document :
c 	- sur KMAX
c	- sur les autres axes K si Y(K) >= seuil
c
c auteur : C. FRANCOIS
c date : 23/07/93
c Modification : C. FRANCOIS 27/08/1993
c	 	 ecriture systematique de la projection 
c		 du document sur KMAX
c
c **************************************************************** 

        SUBROUTINE AFFICHE_DOC(DMAX,NMAX,NO_DOC,NB_DES,DES,VALDES,X,
     1           	NEURONES,Y,MATRICE,NB_DOC,V,CARD_CLASS,
     1  		SOMCOL,SOMTOT,SEUIL_DOC,PONDERATION,ERREUR,TAB,*,*)

        INTEGER 	DMAX		! nombre total de descripteurs
        INTEGER 	NMAX		! nombre de neurones demandes
        INTEGER		NO_DOC		! numero du document courant
        INTEGER		NB_DES 		! nombre de descripteurs indexant ce doc.
        INTEGER 	DES(0:NB_DES)	! tableau de ces descripteurs
        REAL		VALDES(0:NB_DES)	! tableau des valeurs de ces descripteurs
        REAL		X(0:NB_DES)	! DES normalisee
        INTEGER		NEURONES	! nombre maximum de neurones
        REAL		Y(0:NMAX)	! MATRICE * X
        REAL		MATRICE(0:NEURONES, 0:DMAX) ! matrice des neurones
        INTEGER		NB_DOC 		! nombre total de documents 
        INTEGER 	V(0:NB_DOC)	! marque des documents
        INTEGER		CARD_CLASS(0:NMAX)	! cardinal des classes
        INTEGER 	SOMCOL(0:DMAX)	! sommes des colonnes de la matrice
        				! des documents non normalisee !!!!!!
        INTEGER		SOMTOT 		! somme de SOMCOL
        REAL		SEUIL_DOC	! seuil min de coordonnees : doc.
        LOGICAL		PONDERATION	! descripteurs pondérés ?
        CHARACTER*100 	ERREUR 		! message imprime sur erreur standard
        CHARACTER*1	TAB		! caractere tabulation


c ...	formats de lecture dans DONNEES

11      FORMAT(I6,5X,I7)	!entete d'un document (no doc, nb des)
12      FORMAT(I6)		!no de descripteur
13      FORMAT(I6,F11.8)	!no de descripteur, poids descripteur

c ...	formats d'ecriture dans RESULTAT
19      FORMAT(I7, ' : ', I6 /, 'Y:', (20F7.2))	
c				!no doc : classe de ce document

c ...	formats d'ecriture dans TRIDOC et TRIDES

108     FORMAT(I6, A, F6.3, ':', I6)	!no classe \t coordonnee : no doc ou des

c ...	lecture d un document  et m. a j. des sommes en colonne
        READ(1,11,END =30)NO_DOC, NB_DES 
        	IF (NB_DES .EQ. 0) THEN
        		ERREUR = 'document ayant 0 descripteurs'
        		GO TO 31
        	ENDIF 
        	IF (PONDERATION) THEN
        		DO  I = 1,NB_DES 
        			READ(1,13) DES(I),VALDES(I)
        			IF(DES(I) .GT. DMAX) THEN
        				ERREUR = 'numero de descripteur trop important'
        				GO TO 31
        			ENDIF
        			SOMCOL(DES(I)) = SOMCOL(DES(I)) + VALDES(I) 
        			SOMTOT = SOMTOT + VALDES(I) 
        		ENDDO 
        	ELSE
        		DO  I = 1,NB_DES 
        			READ(1,12)DES(I) 
        			IF(DES(I) .GT. DMAX) THEN
        				ERREUR = 'numero de descripteur trop important'
        				GO TO 31
        			ENDIF
        			SOMCOL(DES(I)) = SOMCOL(DES(I)) + 1 
        			SOMTOT = SOMTOT + 1 
        		ENDDO 
        	ENDIF

c ...		calcul de Y 
        	CALL CALCULE_Y(NB_DES,NMAX,DMAX,DES,VALDES,X,NEURONES,
     1  				Y,MATRICE)
c		IF (V(NO_DOC) .EQ. -1 ) THEN
c			WRITE(2,19) NO_DOC, V(NO_DOC), (Y(K), K = 0,NMAX)
c		ENDIF

c ...		ecriture de TRIDOC:triplets(K,val,NO_DOC) en acces sequentiel
            	DO K =0, NMAX
        		IF(CARD_CLASS(K).NE.0) THEN
        			IF((K.EQ.V(NO_DOC)).OR.(Y(K).GE.SEUIL_DOC))
     1  				THEN		
        				WRITE(4,108) K, TAB, Y(K), NO_DOC
        			ENDIF
        		ENDIF

            	ENDDO

        RETURN
30      RETURN 1
31      RETURN 2
        END
c ******************************************************************************
