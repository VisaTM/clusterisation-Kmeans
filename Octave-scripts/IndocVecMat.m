%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 			IndocVecMat.m  
%
% Transforme un vecteur en une matrice de 
% vecteur(1) lignes et vecteur(2) colonnes
%
%   x=IndocVecMat(vecteur)
%   x : matrice a creer
%   vecteur: vecteur a transformer
%   le premier élément de vecteur est le nombre de lignes de x
%   et le deuxième élément de vecteur le nombre de colennes de x.
%
% Auteur : C. FRANCOIS
% Date : 06/11/91 - 31/10/2003
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function x=IndocVecMat(vecteur)
%
disp('debut transformation vecteur en matrice');
l = length(vecteur);
nbneurones = vecteur(1);
nbdes = vecteur(2);
cptneurones = 1;
x=zeros(nbneurones,nbdes);
j = 3;
while j < l
     cptdes = 1;
     while cptdes <= nbdes
          x(cptneurones,cptdes)=vecteur(j);
          j = j + 1;
          cptdes = cptdes + 1;
     end
     cptneurones = cptneurones + 1;
end
disp('fin transformation vecteur en matrice');