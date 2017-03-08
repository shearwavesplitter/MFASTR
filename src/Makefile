#make file
#LINUX
#  setting up to optimize for quicker running 5 March 2010
# uncomment for IFC compiler
#FC = ifc -w90 -w95 -cm -O3 -tpp7 -axW -Vaxlib
# uncomment for f77 compiler
#FC = g77
# uncomment for gfortran compiler
FC = gfortran -std=legacy
#SOLARIS
# uncomment for f77 compiler
#FC = f77

OBJECTS = \
	fftable.o \
	fstrcat.o \
	zabs_max.o \
	zagglomerative_cluster.o \
	zass_mfm.o \
	zass_ini.o \
	zbreadsac.o \
	zc_74calhar.o \
	zc_73dudhar.o \
	zcluster.o \
	zcovariance.o \
	zdetrend.o \
	zeigen2x2.o \
	zerror95.o \
	zerror_interp.o \
	zerror_min.o \
	zfft.o \
	zgrade_mfm.o \
	zgrid_lambda2.o \
	zlag.o \
	zlinint.o \
	zmultiply.o \
	zndf.o \
	zpackresults.o \
	zreaddataSAC_mfm.o \
	zresample.o \
	zrotabc2enz.o \
	zrotate2d.o \
	zselect_cluster_mfm.o \
	zselect_measurement.o \
	zsourcepol.o \
	zsplint.o \
	zsplit.o \
	zsplit_ini.o \
	zwindow.o \
	zwrite1.o \
	zwrite2.o \
	zwrite_logfile.o \
	zwrite_logmfm.o \
	zwrite_outfiles.o

ass_mfm: ass.o $(OBJECTS)
	$(FC) -O3 -o ../exec/ass_mfm ass.f $(OBJECTS)
	chmod g=rx,o=rx ../exec/ass_mfm


psplit: psplit.o $(OBJECTS)
	$(FC) -O3 -o ../exec/psplit psplit.f $(OBJECTS)
	chmod g=rx,o=rx ../exec/psplit

ssplit: split.o $(OBJECTS)
	$(FC) -O3 -o ../exec/ssplit split.f $(OBJECTS)
	chmod g=rx,o=rx ../exec/ssplit

sac2xy: sac2xy.o $(OBJECTS)
	$(FC) -O3 -o ../exec/sac2xy sac2xy.f $(OBJECTS)
	chmod g=rx,o=rx ../exec/sac2xy

create_ass_ini: create_ass_ini.o 
	$(FC) -O3 -o ../exec/create_ass_ini create_ass_ini.f
	chmod g=rx,o=rx ../exec/create_ass_ini

protate:	protate.f
	$(FC) -O3 -o ../exec/protate protate.f zprotate.f zreadsac.f zwritesac.f zgethdr.f zselectp.f zreadp.f zmedfit.f zrotate.f zchnhdr_dble.f zrobfit.f
	chmod g=rx,o=rx ../exec/protate


all: ass_mfm ssplit psplit sac2xy create_ass_ini protate


