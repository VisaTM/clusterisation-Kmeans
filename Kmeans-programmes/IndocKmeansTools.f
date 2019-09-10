c ******************************************************************************
c *		IndocKmeansTools.f
c *
c * - Fonctions utilisees par les modules :
c *		- IndocInitMat.f
c *		- IndocKmeansAx.f
c *
c * Auteur : C. FRANCOIS
c * Date : 19/01/91
c * Modification : 30/10/91 (C. FRANCOIS)
c *  	           ajouter parametre a la fonction ECRIRE_POIDS pour 
c *		   avoir ou non la premiere ligne NMAX, DMAX
c *		   derniere modification 18/02/92
c *		   27/09/93 (C. FRANCOIS)
c *		   ECRIRE_MATRICE, ECRIRE_POIDS , LIRE_POIDS
c *		   31/10/2003 (C. FRANCOIS)
c *		   ECRIRE_MATRICE, ECRIRE_POIDS , LIRE_POIDS
c *                pb de compatibilite avec Gnu Fortran 77
c *                suppression de ECRIRE_DONZ
c ******************************************************************************


c ******************************************************************************
c		SUBROUTINE NOUVR
c
c	ouverture d un fichier 
c	- si existe :  ecrasement
c	-selon la valeur logique de FORMA, fichier formate ou pas 
c	-selon la valeur logique de SEQ, acces sequentiel ou direct
c	-TAILLE = taille en mots d un enregistrement
c
c	auteur : A. LELU
c	modification : 19/01/91 (C. FRANCOIS)
c
c ******************************************************************************

      	SUBROUTINE NOUVR(NO, SUNIT, FORMA,SEQ,TAILLE) 
  
c ... 	parametres
        INTEGER		NO	! numero d unite logique associee au fichier
        CHARACTER*100 	SUNIT 	! nom du fichier 
        LOGICAL		FORMA	! fichier formate ?
        LOGICAL		SEQ	! acces sequentiel ?
        INTEGER		TAILLE	! taille en mots d un enregistrement

c ...	variables internes a la procedure
        LOGICAL		EX	! fichier existe ?
        CHARACTER*40 	ETAT	! si oui ="old" sinon = "new"
        CHARACTER*40 	FORME	! "formatted" ou "unformatted"

c ...	definition des parametres d ouverture 

        INQUIRE (FILE = SUNIT, EXIST = EX) 
        IF(EX) THEN
        	ETAT = 'OLD'
        ELSE	
        	ETAT = 'NEW'
        ENDIF
        IF(FORMA) THEN
        	FORME = 'FORMATTED'
        ELSE      
        	FORME = 'UNFORMATTED'
        ENDIF

c ...	ouverture de SUNIT
        IF (SEQ) THEN
        	OPEN (NO, FILE = SUNIT, STATUS = ETAT,
     1                      FORM = FORME) 
        ELSE
        	OPEN (NO, FILE = SUNIT, STATUS = ETAT,
     1         ACCESS = 'DIRECT', RECL = TAILLE, FORM = FORME)
        ENDIF
        RETURN 
        END 
 
c ******************************************************************************
c		SUBROUTINE ECRIRE_MATRICE
c
c	ecrit la matrice non ponderee en format (<DMAX+1> E15.8) dans 
c	le fichier POIDS precedee du nombre de neurones
c
c	auteur : C. FRANCOIS
c	date : 19/01/91 
c	modification : 30/10/91 (C. FRANCOIS)
c		       ajouter parametre LIGNE1
c		       si LIGNE1 = TRUE 
c			 la premiere ligne NMAX, DMAX est imprimee
c		       la matrice est imprimee sous la forme d un vecteur
c		       27/09/93 (C. FRANCOIS)
c		       changement du format d'ecriture : ecriture en matrice
c		       31/10/2003 (C. FRANCOIS)
c		       changement du format d'ecriture : ecriture en vecteur
c
c ******************************************************************************

        SUBROUTINE ECRIRE_MATRICE(POIDS,NEURONES,NMAX,DMAX,MATRICE,
     1  		LIGNE1)

c ...	parametres
        CHARACTER*100	POIDS	! fichier de la matrice a ecrire
        INTEGER		NEURONES	!nombre max de neurones
        INTEGER		NMAX		!nombre de neurones demandes
        INTEGER		DMAX		! nombre total de descripteurs
        REAL		MATRICE(0:NEURONES,0:DMAX) ! matrice a creer
        LOGICAL		LIGNE1		! sauvegarder l entete ?

c ... 	ouverture du fichier des poids
      	CALL NOUVR(3, POIDS, .TRUE.,.TRUE.,0) 
      	REWIND 3 
        IF (LIGNE1) THEN
        	WRITE(3,*) NMAX + 1
        	WRITE(3,*) DMAX + 1
        ENDIF

c ... 	ecriture de la matrice des poids
        DO K = 0, NMAX
        	DO J = 0, DMAX
        		WRITE(3,11) MATRICE(K,J)
11                      FORMAT (E15.8)
        	ENDDO
        ENDDO
        CLOSE(3)
        RETURN
        END

c ******************************************************************************
c		SUBROUTINE ECRIRE_POIDS
c
c	ecrit la matrice non ponderee en format (<DMAX+1> E15.8) dans 
c	le fichier POIDS precedee du nombre de neurones
c
c	auteur : C. FRANCOIS
c	date : 19/01/91 
c	modification : 30/10/91 (C. FRANCOIS)
c		       ajouter parametre LIGNE1
c		       si LIGNE1 = TRUE 
c			  la premiere ligne NMAX, DMAX est imprimee
c		       la matrice est imprimee sous la forme d un vecteur
c		       19/05/92 (C. FRANCOIS)
c		       sauvegarde des lignes correspondant aux seules 
c		       classes non vides
c		       27/09/93 (C. FRANCOIS)
c		       changement du format d'ecriture : ecriture en matrice
c		       25/09/95 (C. FRANCOIS)
c		       ecriture de l'entete dans un fichier particulier
c		       31/10/2003 (C. FRANCOIS)
c		       changement du format d'ecriture : ecriture en vecteur
c
c ******************************************************************************

        SUBROUTINE ECRIRE_POIDS(POIDS,ENTETE,NEURONES,NMAX,DMAX,MATRICE,
     1  		LIGNE1,NBCLASS,CARD_CLASS)

c ...	parametres
        CHARACTER*100	POIDS	! fichier de la matrice a ecrire
        CHARACTER*100	ENTETE	! fichier de l'entete
        INTEGER		NEURONES	!nombre max de neurones
        INTEGER		NMAX		!nombre de neurones demandes
        INTEGER		DMAX		! nombre total de descripteurs
        REAL		MATRICE(0:NEURONES,0:DMAX) ! matrice a creer
        LOGICAL		LIGNE1		! sauvegarder l entete ?
        INTEGER		NBCLASS		! nombre de classes non vides
        				! si classes non calculees = -1
        INTEGER		CARD_CLASS(0:NMAX)	!cardinal des classes


c ... 	ouverture du fichier des poids

        CALL NOUVR(3, POIDS, .TRUE.,.TRUE.,0) 
        
        REWIND 3 
        IF (LIGNE1) THEN
        	WRITE(3,*) NBCLASS
        	WRITE(3,*) DMAX + 1
        ELSE
c ... 		ouverture du fichier des entete
        	CALL NOUVR(4, ENTETE, .TRUE.,.TRUE.,0) 
        	REWIND 4 
        	WRITE(4,*) NBCLASS
        	WRITE(4,*) DMAX + 1
               	CLOSE(4)
        ENDIF


c ... 	ecriture de la matrice des poids
        DO K = 0, NMAX
        	IF (CARD_CLASS(K) .NE. 0) THEN 
        		DO J = 0, DMAX
        			WRITE(3,11) MATRICE(K,J)
11                      	FORMAT (E15.8)
        		ENDDO
        	ENDIF
        ENDDO
        CLOSE(3)
        RETURN
        END

c ******************************************************************************
c		SUBROUTINE LIRE_POIDS
c
c	lit la matrice non ponderee en format (<DMAX+1> E15.8) dans 
c	le fichier POIDS precedee du nombre de neurones
c
c 	- Parametres :
c		LIGNE1 
c		si LIGNE1 = TRUE 
c			la premiere ligne NMAX, DMAX est lue
c		la matrice est lue sous la forme d un vecteur
c
c	auteur : C. FRANCOIS
c	date : 05/11/91 
c	modification : 27/09/93 (C. FRANCOIS)
c		       changement du format de lecture : lecture en matrice
c		       31/10/2003 (C. FRANCOIS)
c		       changement du format d'ecriture : lecture en vecteur
c ******************************************************************************

        SUBROUTINE LIRE_POIDS(POIDS,NEURONES,NMAX,DMAX,MATRICE,
     1  		LIGNE1)


c ...	parametres
        CHARACTER*100	POIDS	! fichier de la matrice a lire
        INTEGER		NEURONES	!nombre max de neurones
        INTEGER		NMAX		!nombre de neurones demandes
        INTEGER		DMAX		! nombre total de descripteurs
        REAL		MATRICE(0:NEURONES,0:DMAX) ! matrice a creer
        LOGICAL		LIGNE1

c ... 	ouverture du fichier des poids
        CALL NOUVR(3, POIDS, .TRUE.,.TRUE.,0) 
        REWIND 3 
        IF (LIGNE1) THEN
        	READ(3,*) NMAX
        	READ(3,*) DMAX 
        ENDIF
        NMAX = NMAX - 1
        DMAX = DMAX - 1
c ... 	lecture de la matrice des poids
        DO K = 0, NMAX
        	DO J = 0, DMAX
        		READ(3,11) MATRICE(K,J)
11                      FORMAT (E15.8)
        	ENDDO
        ENDDO

        CLOSE(3)
        RETURN
        END

c ******************************************************************************
c		INTEGER FUNCTION ATOI
c
c	tranforme une chaine de caracteres en l entier correspondant
c	!Attention chaine de 10 caracteres max
c
c	auteur : C. FRANCOIS
c	date : 18/02/92
c
c ******************************************************************************
 
        INTEGER FUNCTION IATOI(CHAINE) 

c ...	parametres
        CHARACTER	CHAINE*20
 
c ...	variables internes a la procedure
        INTEGER		IMAX	!longueur de CHAINE
        INTEGER		I	! compteur
        INTEGER		ENTIER	! entier calcule
        INTEGER		ZERO	! code decimal du caractere 0
        CHARACTER	BLANC

        ZERO = 48
        ENTIER = 0
        BLANC = ' '
        IF(INDEX(CHAINE,BLANC) .NE. 0) THEN
        	IMAX = INDEX(CHAINE,BLANC) - 1
        ELSE
        	IMAX =  LEN(CHAINE)
        ENDIF
        I = 1
        DO WHILE (I .LE. IMAX)
        	ENTIER  = (10 * ENTIER) + (ICHAR(CHAINE(I:I)) - ZERO)
        	I = I + 1
        ENDDO

        IATOI = ENTIER  
        RETURN   
        END

c ******************************************************************************
c		REAL FUNCTION RFATOF
c
c	tranforme une chaine de caracteres dans le reel correspondant
c	!Attention chaine de 10 caracteres max
c
c	auteur : C. FRANCOIS
c	date : 19/02/92
c
c ******************************************************************************

        REAL FUNCTION FATOF(CHAINE)

c ...	parametres
        CHARACTER	CHAINE*20
 
c ...	variables internes a la procedure
        INTEGER		IMAX	!longueur de CHAINE
        INTEGER		I	! compteur de CHAINE
        INTEGER		II	! compteur des decimales
        REAL		RNOMBRE	! reel calcule
        INTEGER		ZERO	! code decimal du caractere 0
        CHARACTER	BLANC

        ZERO = 48
        RNOMBRE = 0.0
c ...	calcul de la longueur de la chaine
        BLANC = ' '
        IF(INDEX(CHAINE,BLANC) .NE. 0) THEN
        	IMAX = INDEX(CHAINE,BLANC) - 1
        ELSE
        	IMAX =  LEN(CHAINE)
        ENDIF
        I = 1
c ...	calcul de la partie entiere
        DO WHILE ( (CHAINE(I:I) .NE. '.') .AND. (I .LE. IMAX))
        	RNOMBRE  = (10 * RNOMBRE) + (ICHAR(CHAINE(I:I)) - ZERO)
        	I = I + 1
        ENDDO
c ...	calcul de la partie decimale
        I = I + 1
        II = 1
        DO WHILE (I .LE. IMAX)
        	RNOMBRE = RNOMBRE + (REAL(ICHAR(CHAINE(I:I)) - ZERO)/(10**II))
        	I = I + 1
        	II = II + 1
        ENDDO

        FATOF = RNOMBRE
        RETURN  
        END 
 
c ******************************************************************************

