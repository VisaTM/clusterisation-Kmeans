%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 			IndocMatlabAcp.m  
%
% Analyse en Composantes Principales non! normee, centree.
%
% [po2,vp] = IndocMatlabAcp(vecteur)
% vecteur: Matrice (n*p) a analyser lue sous forme d evecteur,    
%			 n << p!!!!!!!.
% po2	: Matrice (n.2) des projections des n observations sur 
% 	  les 2 premiers axes factoriels
%	  (colonne i : projections des observations sur l'axe i).
% vp	: Vecteur des n valeurs propres par ordre de grandeur 
%	  decroissant.
%
% Auteur : G. BLOCH & M. GABSI
% Date : 27/07/91
% Modification : C. FRANCOIS (26/08/1993)
%                Changement des variables rendues par la fonction
%		 C. FRANCOIS (27/09/1993)
%                Lecture direct de la matrice x
%		 C. FRANCOIS (29/05/1995)
%                optimisations selon A. LELU
%		 suppression des boucles de centrage de x et
%		  de calcul de
%		  u : matrice des vecteurs propres de x'*x
%		 C. FRANCOIS (31/10/2003)
% 		 lecture de la matrice sous forme de vecteur
%		 appel de IndocVecMat
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [po2,vp] = IndocMatlabAcp(vecteur)
%
x=IndocVecMat(vecteur);
[n,p] = size(x);
dmin  = min(size(x));
disp('ACP');
%
% centrage de x
% mx    : moyenne en colonne de x
mx=mean(x);
x=x-ones(n,1)*mx;
clear mx;
disp('fin centrage');
%
% diagonalisation de x
% v     : matrice des vecteurs propres de x*x'
% d     : matrice diagonale des valeurs propres de x*x'
[v,d] = eig(x*x');
disp('fin diagonalisation de x*xT');
%
% lambda : vecteur des valeurs propres de x*x' par ordre decroissant
% v      : matrice des vecteurs propres de x*x'tries
%          selon l'ordre decroissant des valeurs propres
[lambda,t] = sort(diag(d));
clear d;
t          = t(n:-1:1);
lambda     = lambda([n:-1:n-dmin+1]);
v          = v(:,t);
clear t;
disp('fin tri valeurs et vecteurs propres de x*xT');
%
% u : matrice des vecteurs propres de x'*x
u=x'*v*diag(ones(size(lambda)) ./sqrt(lambda));
clear v;
disp('fin calcul vecteurs propres de xT*x');
%
% po : projections des observations sur les axes
po = x*u;
%
% po2      : Matrice (n.2) des projections des n observations sur les 2
%          premiers axes factoriels
po2 = po(:,[1 2]);
% attention syntaxe de selection de colonnes
% 2 axes 1 et 3 : po2 = po(:,[1 3]); 
% 3 axes 1 a 3 : po2 = po(:,1:3); 
clear po;
disp('fin projections des observations sur les axes');
%
% vp   : Vecteur des n valeurs propres par ordre de grandeur decroissant.
SLambda = sum(lambda);
vp = lambda * 100 / SLambda;
disp('fin transformation des valeurs propres en %');
whos

