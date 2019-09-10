
CFLAGS= -O
F77= gfortran



All: IndocInitMat IndocKmeansAx 

IndocInitMat: IndocInitMat.f IndocKmeansAx.h IndocKmeansTools.f
	$(F77) IndocInitMat.f IndocKmeansTools.f $(CFLAGS) -o $@

IndocKmeansAx: IndocKmeansAx.f IndocKmeansAx.h IndocKmeansTools.f
	$(F77) IndocKmeansAx.f IndocKmeansTools.f $(CFLAGS) -o $@


