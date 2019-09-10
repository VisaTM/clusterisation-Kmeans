Programmes de K-means axiales
=============================

Programmes FORTRAN pour exécuter une clusterisation par K-means axiales 

### Installation

Copiez les différents fichiers de ce répertoire dans un répertoire temporaire et lancez la commande : 

```
    make
```

Copiez les programmes nouvellement compilés “IndocInitMat” et “IndocKmeansAx” dans un répertoire 
présent dans la variable $*PATH*, comme “~/bin” or “/usr/local/bin”. Sous Cygwin, ces programmes 
s'appellent “IndocInitMat.exe” et “IndocKmeansAx.exe”. 

### Limitation

Actuellement, les limites de cette application sont : 

 - 200 clusters
 - 200 000 termes
 - 300 000 documents

Ces limites sont définies dans le fichier “*header*” `IndocKmeansAx.h` et peuvent être modifiées 
en changeant respectivement les valeurs des paramètres `NEURONES`, `IDESCRIPTEURS` et `IDOCUMENTS` avant 
de recompiler les programmes FORTRAN avec `make`. 


