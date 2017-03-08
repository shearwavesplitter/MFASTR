c-----------------------------------------------------------------------
	subroutine zprotate(infile_e,infile_n,infile_z,ftol,
     >	outfile_a,outfile_b,outfile_c,phi,theta,sd_phi,sd_theta)
c-----------------------------------------------------------------------
c
c	summary
c	-------
c	rotates the east, north, and up components by phi and theta to
c	give the compont parallel and 2 components perpendicular to the
c	direction of p-wave propagation.
c
c	all files are in SAC ASCII format
c
c	the beginning and end of the p-wave must be defined in the
c	SAC files headers in T0 and T1 (stops if undefined)
c
c   after rotation:
c	phi and dphi are stored in header variable RESP0 and RESP1
c	dtheta and dtheta are stored in header variable RESP2 and RESP3
c	'pwave' is stored in KUSER0 to indicate rotation method
c
c	CMPAZ and CMPINC are not changed in the header
c
c	algorithm
c	---------
c	-read in e,n,z comps and e,n,z comps of pwave
c	-fit pwave azimuthal dirn using robust estimation (L1 norm - NR p699)
c	on e-n comp
c	-calc phi
c	-project e-n comp onto line of best fit
c	-fit pwave vertical dirn  using robust estimation on horiz-n comp
c	-calc theta
c	-change relevant header varaibles
c	-rotate e,n,z comps by phi and theta to give a,b,c
c	-write output files
c
c	variables:
c	---------
c input: infile_e		char*50	east component
c	   infile_n		char*50	north component
c	   infile_z		char*50	up component
c	   ftol		dble		required tolernace on variance estimate for
c						phi and theta
c output:outfile_a	char*50	rotated horizontal component
c						(perp to pwave dirn and horiz)
c	   outfile_b	char*50	rotated vertical component
c						(perp to pwave dirn and a)
c	   outfile_c	char*50	rotated radial component
c						(parallel to pwave dirn)
c	   phi		dble		angle between pwave dirn and north
c						(clockwise from north)
c	   theta		dble		angle between c dirn and up (z)
c	   sd_phi		dble		standard deviation of phi (via monte carlo)
c	   sd_theta		dble		standard deviation of theta(via monte carlo)
c
c	local variables
c	---------------
c	   np		int			max file size
c	   pi		dble			pi (3.141...)
c
c	   h1		dble(14,5)		header part1
c	   h2		int(8,5)		header part2
c	   h3a	char*8		header part3a
c	   h3b	char*16		header part3b
c	   h4		char*8 (7,3)	header part4
c
c	   d1_e	dble(np)		east component of data
c	   npts_e	int			number of data points for east comp
c	   npts	int			number of data points (=npts_e=npts_n=npts_z)
c	   a_e	dble			start of p-wave on east comp
c	   f_e	dble			end of p-wave on east comp
c	   delta_e	dble			time spacing of east comp
c	   a,f,delta dble			as above (should be same for e, n, z comps)
c
c	   pwave_e	dble(np)		east pwave component
c	   npwave_e	int			number of points in east pwave comp
c	   npwave	int			= npwave_e = npwave_n = npave_z
c
c	   a,b,c	dble(np)		rotated comps (see above)
c	   mh		dble			grad of best fit to e-n pwave comps
c	   ch		dble			intercept
c	   mv,cv	dble			as above but for horiz-z comp
c						where horiz = projection of e-n comp onto
c						plane containing the best fitting line
c						of e-n data
c	   phi	dble			angle between north and pwave dirn
c	   theta	dble			angle between up and normal to 
c						pwave dirn in vertical plane
c
c	   FMTdbl	char*12		format '(g15.7)'
c	   FMTint	char*12		format '(i10)'
c	   FMTc08	char*12		format '(a8)'
c	   FMTc16	char*12		format '(a16)'
c	   [ used to convert variable types using internal files]
c
c	   c16	char*16		dummy variable used to pass arguments to 
c						zgethdr and zchnhdr as a character varaible
c
c	   pwave_horiz  dble(np)	e-n comp of pwave projected onto
c						of line of best fit through e-n data
c
c	notes
c	  all angles in DEGREES
c
c-----------------------------------------------------------------------
c  modifications:
c	16-07-01	N. Teanby	Original code
c	26-07-01	N. Teanby	Monte Carlo calculation of errors included
c-----------------------------------------------------------------------

	implicit none
	integer np
	double precision pi
	parameter (np=1000000, pi=3.141592654)
c  ** header **
	double precision h1(14,5)
	integer h2(8,5)
	character h3a*8, h3b*16
	character*8 h4(7,3)
c  ** data (unrotated) **
	double precision d1_e(np),d1_n(np),d1_z(np)
	integer npts_e,npts_n,npts_z,npts
	double precision a_e,f_e,delta_e
	double precision a_n,f_n,delta_n
	double precision a_z,f_z,delta_z
	double precision delta
c  ** p-wave components only **
	double precision pwave_e(np),pwave_n(np),pwave_z(np)
	integer npwave_e,npwave_n,npwave_z,npwave
c  ** rotated components **
	double precision a(np),b(np),c(np)
c  ** rotation varaibles **
	double precision ftol
	double precision mh,ch,var_mh,var_ch
	double precision mv,cv,var_mv,var_cv
	double precision phi,theta,sd_phi,sd_theta,var_phi,var_theta
c  ** formats **
	character*12 FMTdbl, FMTint, FMTc08, FMTc16
c  ** misc **
	character c16*16
	double precision pwave_horiz(np)
	integer i
	character*50 infile_e,infile_n,infile_z
	character*50 outfile_a,outfile_b,outfile_c
	character*55 temp20,temp21,temp22,temp23
	double precision minpe,maxpe,minph,maxph

c  ** format descriptors **	
	FMTdbl='(g15.7)'
	FMTint='(i10)'
	FMTc08='(a8)'
	FMTc16='(a16)'

c  ** select p wave data from 3 components [e,n,z] **	
	call zreadp(infile_e,10,np,
     >	d1_e,npts_e,pwave_e,npwave_e,a_e,f_e,delta_e)
	call zreadp(infile_n,11,np,
     >	d1_n,npts_n,pwave_n,npwave_n,a_n,f_n,delta_n)
	call zreadp(infile_z,12,np,
     >	d1_z,npts_z,pwave_z,npwave_z,a_z,f_z,delta_z)

c  ** check that npwave, a, f, and delta are the same for all comps **
	if ((a_e.ne.a_n).or.(a_e.ne.a_z)) then
	   print*,'WARNING:zdriver'
	   print*,'WARNING:  a_e = a_n = a_z : not true'
	endif
	if ((f_e.ne.f_n).or.(f_e.ne.f_z)) then
	   print*,'WARNING:zdriver'
	   print*,'WARNING:  f_e = f_n = f_z : not true'
	endif
	if ((delta_e.ne.delta_n).or.(delta_e.ne.delta_z)) then
	   print*,'WARNING:zdriver'
	   print*,'WARNING:  delta_e = delta_n = delta_z : not true'
	else
	   delta=delta_e
	endif
	if ((npwave_e.ne.npwave_n).or.(npwave_e.ne.npwave_z)) then
	   print*,'WARNING:zdriver'
	   print*,'WARNING:  npwave_e = npwave_n = npwave_z : not true'
	else
	   npwave=npwave_e
	endif

c  ** find line of best fit for e-n using robust estimation (L1 norm) **
c  ** phi output in degrees **
	call zrobfit(pwave_e,pwave_n,np,npwave,ftol,ch,mh,var_ch,var_mh,
     >	phi,var_phi)
	sd_phi=sqrt(var_phi)
	
c  ** project horiz comp of data onto e-n line of best fit **
	do 1 i=1,npwave
	   pwave_horiz(i) = pwave_n(i)*cos(phi*pi/180.)
     >			+ pwave_e(i)*sin(phi*pi/180.)
1	continue
c  ** find line of best fit for h-z using robust estimation (L1 norm) **
c  ** theta output in degrees **
	call zrobfit(pwave_horiz,pwave_z,np,npwave,ftol,
     >		cv,mv,var_cv,var_mv,theta,var_theta)
	sd_theta=sqrt(var_theta)

c  ** rotate e,n,z comps to a,b,c **
	call zreadsac(infile_e,50,np,h1,h2,h3a,h3b,h4,d1_e,npts)
	call zreadsac(infile_n,51,np,h1,h2,h3a,h3b,h4,d1_n,npts)
	call zreadsac(infile_z,52,np,h1,h2,h3a,h3b,h4,d1_z,npts)
c
	call zrotate(d1_e,d1_n,d1_z,npts,np,theta,phi,a,b,c)

c  ** read in file, change header, write header and rotated comp **
c  ** CMPAZ/CMPINC NOT CHANGED TO CORRECT VALUE BECAUSE SPLIT.M ASSUMES THAT
C  ** TWO COMPONENTS ARE HORIZONTAL AND COMPLAINS IF THEY AREN'T
c  ** instead phi and theta are stored in RESP0 and RESP2 **
c  ** a **
	call zreadsac(infile_e,70,np,h1,h2,h3a,h3b,h4,d1_e,npts)
	write(c16,FMTc16) 'a               '
	call zchnhdr_dble('kcmpnm ',c16,h1,h2,h3a,h3b,h4)
	write(c16,FMTdbl) phi
	call zchnhdr_dble('resp0 ',c16,h1,h2,h3a,h3b,h4)
	write(c16,FMTdbl) sd_phi
	call zchnhdr_dble('resp1 ',c16,h1,h2,h3a,h3b,h4)
	write(c16,FMTdbl) theta
	call zchnhdr_dble('resp2 ',c16,h1,h2,h3a,h3b,h4)
	write(c16,FMTdbl) sd_theta
	call zchnhdr_dble('resp3 ',c16,h1,h2,h3a,h3b,h4)
	write(c16,FMTc08) 'pwave   '
	call zchnhdr_dble('kuser0 ',c16,h1,h2,h3a,h3b,h4)
	call zwritesac(outfile_a,60,np,h1,h2,h3a,h3b,h4,a,npts)
c  ** b **
	call zreadsac(infile_n,71,np,h1,h2,h3a,h3b,h4,d1_n,npts)
	write(c16,FMTc16) 'b               '
	call zchnhdr_dble('kcmpnm ',c16,h1,h2,h3a,h3b,h4)
	write(c16,FMTdbl) phi
	call zchnhdr_dble('resp0 ',c16,h1,h2,h3a,h3b,h4)
	write(c16,FMTdbl) sd_phi
	call zchnhdr_dble('resp1 ',c16,h1,h2,h3a,h3b,h4)
	write(c16,FMTdbl) theta
	call zchnhdr_dble('resp2 ',c16,h1,h2,h3a,h3b,h4)
	write(c16,FMTdbl) sd_theta
	call zchnhdr_dble('resp3 ',c16,h1,h2,h3a,h3b,h4)
	write(c16,FMTc08) 'pwave   '
	call zchnhdr_dble('kuser0 ',c16,h1,h2,h3a,h3b,h4)
	call zwritesac(outfile_b,61,np,h1,h2,h3a,h3b,h4,b,npts)
c  ** c **
	call zreadsac(infile_z,72,np,h1,h2,h3a,h3b,h4,d1_z,npts)
	write(c16,FMTc16) 'c               '
	call zchnhdr_dble('kcmpnm ',c16,h1,h2,h3a,h3b,h4)
	write(c16,FMTdbl) phi
	call zchnhdr_dble('resp0 ',c16,h1,h2,h3a,h3b,h4)
	write(c16,FMTdbl) sd_phi
	call zchnhdr_dble('resp1 ',c16,h1,h2,h3a,h3b,h4)
	write(c16,FMTdbl) theta
	call zchnhdr_dble('resp2 ',c16,h1,h2,h3a,h3b,h4)
	write(c16,FMTdbl) sd_theta
	call zchnhdr_dble('resp3 ',c16,h1,h2,h3a,h3b,h4)
	write(c16,FMTc08) 'pwave   '
	call zchnhdr_dble('kuser0 ',c16,h1,h2,h3a,h3b,h4)
	call zwritesac(outfile_c,62,np,h1,h2,h3a,h3b,h4,c,npts)

c  ** write files for plotting partical motion **
c  ** initial values of max/min of pwave motion (used to plot sensible line)
	maxpe=pwave_e(1)
	minpe=pwave_e(1)
	maxph=pwave_horiz(1)
	minph=pwave_horiz(1)
c  ** name and open files required **	
	temp20=infile_e(1:index(infile_e,'.e')-1)//'.en'	
	temp21=infile_e(1:index(infile_e,'.e')-1)//'.hz'	
	temp22=infile_e(1:index(infile_e,'.e')-1)//'.en.fit'	
	temp23=infile_e(1:index(infile_e,'.e')-1)//'.hz.fit'
	open(20,file=temp20)
	open(21,file=temp21)
	open(22,file=temp22)
	open(23,file=temp23)
c  ** write p-wave partical motions and find min/max of 'x' axis **
	do 20 i=1,npwave
		write(20,1020) pwave_e(i),pwave_n(i)
		write(21,1020) pwave_horiz(i),pwave_z(i)
		maxpe=max(pwave_e(i),maxpe)
		minpe=min(pwave_e(i),minpe)
		maxph=max(pwave_horiz(i),maxph)
		minph=min(pwave_horiz(i),minph)
20	continue
c  ** write 2 points defining lines of best fit **
	write(22,1020) minpe, mh*minpe + ch
	write(22,1020) maxpe, mh*maxpe + ch
	write(23,1020) minph, mv*minph + cv
	write(23,1020) maxph, mv*maxph + cv
c  ** close files **
	close(20)
	close(21)
	close(22)
	close(23)
	
	return

1020	format(e15.5,2x,e15.5)

	end

