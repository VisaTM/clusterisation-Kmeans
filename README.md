Neurodoc
========

Un outil de clusterisation appliquant l'algorithme des K-means axiales sur un fichier de données “*document × terme*” 

Cette application utilise deux programmes FORTRAN mettant en œuvre une variante des K-means 
appelée K-means axiales et ensuite, exécute une Analyse en Composantes Principales (ACP) 
à l'aide de GNU Octave, un clone du logiciel de calcul numérique MATLAB. Il est encapsulé 
dans un script Perl qui gère le reformattage des données et génère le fichier résultat.

### Installation

#### 1 - Prérequis

Le script “**neurodoc.pl**” fonctione avec **Perl 5** (toute version supérieure à 5.8.3). Vous 
avez aussi besoin de :

 - [**gcc**](https://gcc.gnu.org) pour compiler le programme ACC 
 - [**gfortran**](https://gcc.gnu.org/fortran/) pour compiler les programmes K-means 
 - [**GNU Octave**](https://www.gnu.org/software/octave/) pour exécuter l’ACP 

#### 2 - K-means axiales

Copiez les différents fichiers du répertoire “Kmeans-programmes” dans un répertoire temporaire 
et lancez la commande : 

```
    make
```

Copiez les programmes nouvellement compilés “IndocInitMat” et “IndocKmeansAx” dans un répertoire 
présent dans la variable $*PATH*, comme “~/bin” or “/usr/local/bin”. Sous Cygwin, ces programmes 
s'appellent “IndocInitMat.exe” et “IndocKmeansAx.exe”. 

#### 3 - Scripts Octave 

Vous pouvez copier les scripts du répertoire “Octave-scripts” dans un autre répertoire ou non. 
De toute façon, indiquez le chemin du répertoire contenant ces scripts, soit en déclarant la 
variable globale $*MATLABPATH* 

```
    export MATLABPATH=/path/to/scripts
```

soit en utilisant l’option “-s” (voir la section the “[Options](https://github.com/VisaTM/clusterisation-Kmeans#options)” ci-dessous). 

#### 4 - ACC

Copiez les différents fichiers du répertoire “ACC” dans un répertoire temporaire et lancez la commande : 

```
    make
```

Copiez le fichier nouvellement compilé “acc” dans un répertoire présent dans la variable $*PATH*, 
comme “~/bin” or “/usr/local/bin”. Sous Cygwin, ce programme s'appellent “acc.exe”. 

### Usage
```
    neurodoc.pl  -i input_file -o output_file.(json|xml) -c cluster_number
                  [ -d document_threshold ] [ -t term_threshold ] [ -m metadata ]
                  [ -s script_path ] [ -f frequency ] [ -jk ]
    neurodoc.pl  -h
```


### Options
```
    -c  indique le nombre de clusters (de 2 à 200) 
    -d  donne la valeur de seuil pour qu’un document soit inclus dans un cluster 
        (valeur par défaut : 0,3) 
    -f  donne la fréquence minimum (nombre de documents) d’un terme (valeur 
        par défaut : 2) 
    -h  affiche cette aide et quitte 
    -i  donne le nom du fichier d’entrée  
    -j  définit le format du fichier de sortie comme étant JSON (même si 
        l’extension du susdit fichier indique autre chose)
    -k  garde les fichiers résultats de K-means (supprimés par défaut)
    -m  donne le nom du fichier de métadonnées (au format TSV)
    -o  donne le nom du fichier de sortie avec l’extension du fichier indiquant 
        le format du fichier : JSON ou XML (XML par défautpour toute autre 
        extension) 
    -s  indique le chemin du répertoire avec les scripts Matlab/Octave 
    -t  give the threshold value for a term to be included in a cluster 
        (default value: 1.0)
    -t  donne la valeur de seuil pour qu’un terme soit inclus dans un cluster 
        (valeur par défaut : 1,0)
```

### Fichier d’entrée

Le fichier d’entrée est un fichier de données “document × terme” où on a sur 
chaque ligne l’identifiant d’un document (généralement, un nom de fichier avec 
ou sans extension), une tabulation et un terme. Et il y a autant de lignes que 
de termes pour chaque document du corpus. 

```text
GS2_0000067	abrupt transition
GS2_0000067	apparent contrast
GS2_0000067	arc collision
   ...
GS2_0000067	wide variability
GS2_0000592	anomalous change
GS2_0000592	atomic oxygen
   ...
```

### Fichier de métadonnées

Vous pouvez aussi utiliser en entrée un fichier TSV contenant les métadonnées 
associées aux documents telles que titre, identifiants, nom de revue, date de 
publication et auteurs. La première ligne est réservée aux noms de champ. Seuls 
les champs dont le nom apparait dans la liste suivante seront utilisés : 

 * __Filename__
 * __IstexId__ ou __Istex Id__
 * __ARK__
 * __DOI__
 * __Title__
 * __Source__
 * __PublicationDate__ ou __Publication date__
 * __Author__ ou __Authors__

L’ordre des champs n’a pas d’importance et les noms de champs ne sont pas sensibles 
à la casse. D’autres champs peuvent être présents dans le fichier, ils ne seront 
simplement pas pris en compte. 

### Docker

Pour construire une image Docker, faire&nbsp;:

```bash
   docker build -t visatm/neurodoc .
```

À noter que les variables `http_proxy`, `https_proxy` et `no_proxy` ne sont pas définies dans le Dockerfile. Il est cependant possible de leur affecter une valeur lors de la création de l’image Docker. Par exemple, pour l’INIST, cela donne&nbsp;:

```bash
   docker build --build-arg http_proxy="http://proxyout.inist.fr:8080/" \
                --build-arg https_proxy="http://proxyout.inist.fr:8080/" \
                --build-arg no_proxy="localhost, 127.0.0.1, .inist.fr" \
                -t visatm/neurodoc .
```

Il est également possible depuis la version 17.07 de Docker d’obtenir le même résultat en configurant le client Docker. Pour cela, il faut modifer le fichier `~/.docker/config.json` pour ajouter ces informations sous la forme suivante&nbsp;:

```json
    "proxies": {
        "default": {
            "httpProxy": "http://proxyout.inist.fr:8080",
            "httpsProxy": "http://proxyout.inist.fr:8080",
            "noProxy": "localhost,127.0.0.1,.inist.fr"
        }
    }
```

Dans l’exemple suivant, on utilise `neurodoc.pl` à partir de son image Docker dans le cas où on veut télécharger des métadonnées ainsi que créer un fichier “doc × terme” en supposant que&nbsp;:

 * l’utilisateur à l’identifiant (ou [UID](https://fr.wikipedia.org/wiki/User_identifier)) 1002 
 * l’utilisateur à l’identifiant de groupe (ou [GID](https://fr.wikipedia.org/wiki/Groupe_%28Unix%29)) 400 
 * le fichier d’entrée “doc × terme” s’appelle “**exemple.txt**” et se trouve dans le répertoire courant 
 * le fichier de métadonnées s’appelle “**metadata.tsv**” et se trouve dans le répertoire courant 
 * le fichier de sortie devra s’appeler “**cluster.json**” et être dans le répertoire courant 
 * on veut obtenir 20 clusters 


```bash
   docker run --rm -u 1002:400 -v `pwd`:/tmp visatm/neurodoc neurodoc.pl -i exemple.txt -m metadata.tsv -o cluster.json -c 20
```

### Galaxy : fichiers de configuration

On a 2 fichiers de configuration pour `neurodoc.pl` sous Galaxy, 1 en anglais, 1 en français : 

 * neurodoc_en.xml 
 * neurodoc_fr.xml 

On peut installer l’une ou l’autre langue au choix sous Galaxy, de préférence avec le nom `neurodoc.xml`, et on doit indiquer son nom et son chemin dans le fichier `config/tool_conf.xml`. Si l’on suppose que ce fichier a été placé dans le répertoire `tools/clustering` de Galaxy, l’entrée dans le fichier `config/tool_conf.xml` est :

```xml
    <section id="clustering" name="Clustering">
      <tool file="clustering/neurodoc.xml" />
    </section>
```


