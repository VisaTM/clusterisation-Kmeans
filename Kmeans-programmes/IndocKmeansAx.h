c ******************************************************************************
c *		      IndocKmeansAx.h 
c *
c * - Parametres de la methode des K-MEANS axiales
c * - Utilise dans IndocInitMat.f et IndocKmeansAx.f
c *
c * Auteur : C. FRANCOIS
c * Date : 22/07/93
c * Modification : 30/10/2003 (C. FRANCOIS)
c *		   mise a jour des parametres
c ******************************************************************************

        INTEGER		NEURONES
        INTEGER		IDESCRIPTEURS
        INTEGER		IDOCUMENTS
        INTEGER		NB_LEC_MAX
        PARAMETER	(
     1  		NEURONES = 200, 
     2  		IDESCRIPTEURS = 200000, 
     3  		IDOCUMENTS = 300000, 
     4  		NB_LEC_MAX = 50
     5  		) 


c ...	NEURONES :  nombre max de lignes de la matrice des classes
c ...	IDESCRIPTEURS : nombre max de colonnes de la matrice des classes
c ...	IDOCUMENTS : nombre max de documents = taille max de V
c ... 	NB_LEC_MAX : nombre max de lectures du fichier
