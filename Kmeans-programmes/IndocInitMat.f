c ****************************************************************
c *			IndocInitMat.f
c *
c * - Fonctionn d'initialisation de la matrice des poids au hasard
c *   ou avec les donnees
c *   utilise le module IndocKmeansTools.f (ecrire_poids)
c *
c * - Les parametres :
c *		-d fichier de donnees (defaut : a.data)
c *		-i fichier sauvegarde des poids (defaut : a.init)
c *     	-n nombre de neurones (defaut : 30)
c *		-h initialisation au hasard (defaut : faux)
c *		-g graine d initialisation (defaut : 1)
c *     -pk (valeurs des mots cles ponderees ; defaut : non)
c *
c * Auteur : C. FRANCOIS
c * Date : 19/01/91
c * Modification : C. FRANCOIS 23/07/93
c *		   - Traitement des erreurs 
c *		   - Taille des tableaux dans IndocKmeansAx.h
c *		   C. FRANCOIS 29/10/93
c *		   - traitement des erreurs STOP -> CALL EXIT(200)
c *		   - fin du programme END -> CALL EXIT(0)
c *
c *		  C. FRANCOIS 20/05/2010
c *		  - rajout d'un tableau de valeurs pour TF-IDF.
c *
c ***************************************************************

        PROGRAM IndocInitMat

        INCLUDE "IndocKmeansAx.h"

c --------------------------------------------------------------
c
c	declarations
c
c --------------------------------------------------------------

c ...	parametres de la methode
        CHARACTER*100	ARGV
        INTEGER		I,IARGC,M
      	INTEGER 	NMAX	! nombre de neurones demandes
        LOGICAL		HASARD	! initialisation par Hasard ou par donnees ?
        INTEGER 	IGRAINE ! graine d initilisation par hazard
        LOGICAL		PONDERATION	! descripteurs pondérés ?

c ...	fichiers a ouvrir
        CHARACTER*100 	DONNEES 	! donnees a lire
        CHARACTER*100 	POIDS		! sauvegarde de MATRICE

c ... 	parametres  generaux lus dans le fichier DONNEES
        INTEGER 	DMAX	! nombre total de descripteurs
        INTEGER		NB_DOC 	! nombre total de documents  
        CHARACTER*40	TITRE	! chaine de titre du fichier a lire
      	INTEGER 	DES(IDESCRIPTEURS) ! tableau des descripteurs 1 doc
c ...	rajout d'un tableau de valeurs pour TF-IDF (C. François 20/05/2010)
        REAL		VALDES(IDESCRIPTEURS)	! tableau des valeurs de ces descripteurs

c ...	matrice des neurones a creer
        REAL		MATRICE(0:NEURONES,0:IDESCRIPTEURS) 

c ...	message d'erreur
        CHARACTER*100 	ERREUR 	! message imprime sur erreur standard

c --------------------------------------------------------------
c
c      initialisations des parametres de la methode
c
c --------------------------------------------------------------
        DONNEES = 'a.data'  
        POIDS = 'a.init' 
        NMAX = 30
        HASARD = .FALSE.
        IGRAINE = 1
        ERREUR = ' '
        PONDERATION = .FALSE.

        M = IARGC()
        I = 1
        DO WHILE (I .LE. M)
        	CALL GETARG(I,ARGV)
        	IF ( ARGV .EQ. '-d') THEN
        		I = I + 1
        		CALL GETARG(I,ARGV)
        		DONNEES = ARGV
        	ELSE IF ( ARGV .EQ. '-i') THEN
        		I = I + 1
        		CALL GETARG(I,ARGV)
        		POIDS = ARGV
        	ELSE IF ( ARGV .EQ. '-n') THEN
        		I = I + 1
        		CALL GETARG(I,ARGV)
        		NMAX = IATOI(ARGV)
        	ELSE IF ( ARGV .EQ. '-h') THEN
        		HASARD = .TRUE.
        	ELSE IF ( ARGV .EQ. '-g') THEN
        		I = I + 1
        		CALL GETARG(I,ARGV)
        		IGRAINE = IATOI(ARGV)
        	ELSE IF ( ARGV .EQ. '-pk') THEN
        		I = I + 1
        		CALL GETARG(I,ARGV)
        		PONDERATION = .TRUE.
        	ELSE
        		WRITE(0,*) 'usage: IndocInitMat 
     1  		-n number_of_clusters -d data_file 
     1  		-i initialized_matrix_file -h -g initialisation_seed 
     1          [ -pk ]'
        		CALL EXIT(200)
        	ENDIF
        	I = I + 1
        ENDDO
  
c --------------------------------------------------------------
c
c      initialisations generales
c
c --------------------------------------------------------------

c ... 	ouverture du fichier de donnees
        CALL NOUVR(1, DONNEES, .TRUE.,.TRUE.,0) 
        REWIND 1

c ... 	lecture des parametres  generaux dans le fichier DONNEES
        READ(1,10) DMAX, NB_DOC, TITRE 
10      FORMAT(I7, I7, A40) 
 
c ...	DMAX = le nombre de descripteurs total et comme la matrice
c ...	commence a la colonne 0, on va jusqu'a DMAX-1 descripteurs.
        DMAX = DMAX-1
c ...	NMAX = le nombre de neurones total et comme la matrice
c ...	commence a la ligne 0, on va jusqu'a NMAX-1 descripteurs.
        NMAX = NMAX-1


c ...	blindage si DMAX et NMAX trop importants !!!!
        IF(DMAX .GT. IDESCRIPTEURS) THEN
        	WRITE(0,*) 'IndocInitMat :'
        	WRITE(0,*) 'nombre de descripteurs trop important'
        	CLOSE(1)
        	CALL EXIT(200)
        ENDIF

        IF(NMAX .GT. NEURONES) THEN
        	WRITE(0,*) 'IndocInitMat :'
        	WRITE(0,*) 'nombre de classes trop important'
        	CLOSE(1)
        	CALL EXIT(200)
        ENDIF 

c ...	initialisation de MATRICE a 0
        DO K = 0, NMAX
        	DO J = 0, DMAX
        		MATRICE(K,J) = 0
        	ENDDO
        ENDDO
c --------------------------------------------------------------
c
c      mise a jour de la matrice 
c
c --------------------------------------------------------------
        IF (HASARD) THEN
        	CALL INIT_HASARD(NEURONES, NMAX, DMAX, MATRICE, IGRAINE)
        ELSE
        	CALL INIT_DONNEES(1, NEURONES, NMAX, DMAX, MATRICE,DES,
     1  		VALDES, PONDERATION, NB_DOC, ERREUR, *30)


        ENDIF

        		
c ...   ecriture du fichier init = matrice initiale
        CALL ECRIRE_MATRICE(POIDS,NEURONES,NMAX,DMAX,MATRICE,.TRUE.)

c --------------------------------------------------------------
c
c	fin ou erreur de lecture, arret du programme
c
c --------------------------------------------------------------
        GO TO 31
        
30      WRITE(0,*) 'IndocInitMat :'
        WRITE(0,*) ERREUR
        CLOSE(1) 
        CALL EXIT(200)

31      CLOSE(1)
        CALL EXIT(0)
        END
 
c ****************************************************************
c	FIN PROGRAM IndocInitMat
c ****************************************************************


c ****************************************************************
c		SUBROUTINE INIT_HASARD
c
c  initialise la matrice des poids au hasard 
c
c  auteur : C. FRANCOIS
c  date : 19/01/91 
c
c ****************************************************************

        SUBROUTINE INIT_HASARD (NEURONES,NMAX, DMAX, MATRICE,IGRAINE) 

        INTEGER 	NMAX		! nombre de neurones demandes
        INTEGER 	DMAX		! nombre total de descripteurs
        REAL    	MATRICE(0:NEURONES,0:DMAX) ! matrice des neurones
        INTEGER 	IGRAINE 	! graine d initilisation
 
c ...	variables internes a la procedure
        REAL    	RGRAINE ! graine d initilisation
        REAL    	SOM 	! somme de la ligne courante de MATRICE

c ...	mise a jour de la graine d initialisation         
        RGRAINE = RAND(IGRAINE)
 
c ...	constitution de la matrice  
        DO K = 0, NMAX 
        	SOM = 0.        
        	DO J = 0, DMAX 
c .............		 initialisation de (K,J) au hasard
        		MATRICE(K, J) = RAND(0)  
c .............		calcul de la somme de la ligne K
        		IF(MATRICE(K,J) .GT. 1.0E-18) SOM = SOM 
     1                        + MATRICE(K,J) * MATRICE(K,J) 
        	ENDDO
        	IF (SOM .EQ. 0.)  SOM = .00000001 
        	SOM = SQRT(SOM) 
c ............. normalisation de la ligne K
        	DO J = 0,DMAX 
        		MATRICE(K,J) =  MATRICE(K,J) / SOM 
        	ENDDO 
        ENDDO 
 
        RETURN 
        END 

c ****************************************************************
c		SUBROUTINE INIT_DONNEES
c
c initialise la matrice des poids par les donnees une donnee 
c toutes les NB_DOC/NMAX 
c
c auteur : C. FRANCOIS
c date : 19/01/91
c Modification : C. FRANCOIS 23/07/93
c		 - Traitement des erreurs
c
c ****************************************************************

        SUBROUTINE INIT_DONNEES (NO,NEURONES,NMAX, DMAX, MATRICE,
     1  			DES,VALDES,PONDERATION,NB_DOC,ERREUR,*) 

c ....	INTEGER 	NO ! no d unite logique du fichier DONNEES
        INTEGER 	NMAX		! nombre de neurones demandes
        INTEGER 	DMAX		! nombre total de descripteurs
        REAL    	MATRICE(0:NEURONES,0:DMAX) ! matrice des neurones
        INTEGER 	DES(DMAX)	! tableau de ces descripteurs
        REAL		VALDES(DMAX)	! tableau des valeurs de ces descripteurs
        LOGICAL		PONDERATION	! descripteurs pondérés ?
        INTEGER		NB_DOC		! nombre total de documents 
        CHARACTER*100 	ERREUR 		! message imprime sur erreur standard

c ...	variables internes a la procedure
        INTEGER		ISAUT	! nb documents a passer entre 2 lectures
        INTEGER		NO_DOC	! numero du document courant
        INTEGER		NB_DES 	! nombre de descripteurs indexant ce doc
        REAL		SOM	! somme de la ligne courante de MATRICE


c ... 	calcul de ISAUT : nb de documents a passer 
c ...	entre deux lectures
        ISAUT = ( NB_DOC / (NMAX + 1) ) - 1

c ...	ecriture de la matrice, neurone par neurone
        DO K = 0, NMAX
        	SOM = 0.
c ............. saut des ISAUT documents non lus
        	DO JJ = 1, ISAUT
        		READ(NO,11,END = 20)NO_DOC,  NB_DES 
11           		FORMAT(I6,5X,I7) 
        		IF (NB_DES .EQ. 0) THEN
        			ERREUR = 'document ayant 0 descripteurs'
        			GO TO 21
        		ENDIF 
        		IF (PONDERATION) THEN
        			DO   I = 1,NB_DES 
                          READ(NO,13)DES(I),VALDES(I) 
13                        FORMAT(I6,F11.8)	!no de descripteur, poids descripteur 
        			ENDDO
        		ELSE
        			DO   I = 1,NB_DES 
                          READ(NO,12)DES(I) 
12                        FORMAT(I6) 
        			ENDDO
         		ENDIF
        	ENDDO
c ............. lecture d un document
        	READ(NO,11,END = 20) NO_DOC,  NB_DES 
        	IF (NB_DES .EQ. 0) THEN
        		ERREUR = 'document ayant 0 descripteurs'
        		GO TO 21
        	ENDIF 
        	IF (PONDERATION) THEN
        		DO   I = 1,NB_DES 
                     READ(NO,13)DES(I),VALDES(I) 
        		IF(DES(I) .GT. DMAX) THEN
        			ERREUR = 'numero de descripteur trop important'
        			GO TO 21
        		ENDIF
        		ENDDO
        	ELSE
        		DO   I = 1,NB_DES 
                    READ(NO,12)DES(I) 
        			IF(DES(I) .GT. DMAX) THEN
        				ERREUR = 'numero de descripteur trop important'
        				GO TO 21
        			ENDIF
        			VALDES(I) = 1.0
        		ENDDO
         	ENDIF
c ............. mise a jour de la ligne	K par "copie"			
        	DO I = 1,  NB_DES
        		MATRICE(K,DES(I)) = VALDES(I)
        	ENDDO
c ............. normalisation de la ligne K
        	DO J = 0, DMAX
        		IF(MATRICE(K,J) .GT. 1.0E-18) THEN
        			SOM = SOM + MATRICE(K,J) * MATRICE(K,J) 
        		ENDIF
        	ENDDO
        	IF (SOM .EQ. 0.)  SOM = .00000001 
        	SOM = SQRT(SOM) 
         	DO  J = 0, DMAX 
                   	MATRICE(K,J) =  MATRICE(K,J) / SOM 
        	ENDDO
        ENDDO
c ... 	fin de lecture du fichier
20      CONTINUE

       	RETURN 
21      RETURN 1
       END 

c ******************************************************************************
