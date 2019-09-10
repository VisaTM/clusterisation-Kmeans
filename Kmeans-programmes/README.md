Axial K-means programmes
========================

FORTRAN programmes to perform clustering by axial K-means 

### Installation

Copy the different files of this directory in a temporary directory and run the command 

```
    make
```

Copy the newly compiled programmes `IndocInitMat` and `IndocKmeansAx` in a directory 
present in the $*PATH* variable, as `~/bin` or `/usr/local/bin`. On Cygwin, the programmes are 
named `IndocInitMat.exe` and `IndocKmeansAx.exe`. 

### Limitation

Currently, the limits of the application are:

 - 200 clusters
 - 200 000 terms
 - 300 000 documents

These limits are set in the header file `IndocKmeansAx.h` and can be modified by changing 
respectively the values of the parameters NEURONES, IDESCRIPTEURS and IDOCUMENTS before 
recompiling the FORTRAN programmes with `make`.
