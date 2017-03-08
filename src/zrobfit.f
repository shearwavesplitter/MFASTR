c-----------------------------------------------------------------------
	subroutine zrobfit(x,y,np,n,ftol,c,m,var_c,var_m,
     >	angle,var_angle)
c-----------------------------------------------------------------------
c
c	robust (L1 norm) fit x,y data to linear model y=mx+c
c	monte carlo estimate of errors
c	repeat:
c		s=s+1
c		random selection of n data (with replacement)
c		fit again
c		calc variance of m and c from s simulations
c		if fractional channge in var_m and var_c < ftol
c	end
c	m and c are original values calulated on entire data set
c	var_m/c are variance of the s simulations
c
c	variables:
c input: x		dble(np)	input x data
c	   y		dble(np)	input y data
c	   n		int		number of data
c	   np		int		array dimension
c	   ftol	dble		required tolernace of variance estimate OF ANGLE
c output:c		dble		intercept of robust fit
c	   m		dble		grad of robust fit
c	   var_m	dble		monte carlo estimate of variance of m
c	   var_c	dble		monte carlo estimate of variance of c
c	   angle	dble		angle of line to horizontal
c	   var_angle	dble	variance of angle
c
c-----------------------------------------------------------------------
c  modifications:
c	26-07-01	N. Teanby	Original code
c-----------------------------------------------------------------------
	
	implicit none
	integer n,np,s,sp,np1,iseed
	double precision pi
	parameter (sp=50,np1=1000000,pi=3.141592654)
	double precision x(np),y(np),ftol,c,m,var_c,var_m
	double precision x1(np1),y1(np1)
	double precision abdev
	double precision c1(sp),m1(sp),abdev1(sp)
	double precision var_c_old,var_m_old,dummy_ave,fc,fm
	double precision angle,var_angle,var_angle_old,fa
	double precision diff0,diff1,diff2,sum
	
	sum=0.

c  ** negative integer seed for random number generator **	
	iseed = -783084

c  ** check array dimensions are same **	
	if (np.ne.np1) then
	   stop 'ERROR:- zrobfit. np .ne. np1'
	endif
	
c  ** robust estimation of m and c using entire dataset **
	call zmedfit(x,y,np,n,c,m,abdev)
c  ** angle is between 0 and 180 **
	angle = atan2(1.0d0,m)*180./pi
cccc	print*,'----------- angle = ',angle

c  ** find variance of parameters **
	do 1 s=1,sp
c	** take a random selection of data, with replacement **
	   call zrsel(x,y,n,np,iseed,x1,y1)
c	** robust estimation of m1(s) and c1(s) **
	   call zmedfit(x1,y1,np,n,c1(s),m1(s),abdev1(s))
c	** find min angle between lines defined by m and m1 using dot product **
	   diff1=acos((1 + m*m1(s))/sqrt((1. + m**2)*(1. + m1(s)**2)))
	   diff2=acos(-(1 + m*m1(s))/sqrt((1. + m**2)*(1. + m1(s)**2)))
	   diff0=min(diff1,diff2)*180./pi
c	** calc sum of differences squared, using angle as mean value **
	   sum = sum + diff0**2
c	** if change in variance estimate < ftol exit loop **
c	** (do at least 10 iterations) **
	   if (s .eq. 9) then
c	   ** calc variance of m1, s1, and angle **
	      call avevar(c1,s,sp,dummy_ave,var_c_old)
	      call avevar(m1,s,sp,dummy_ave,var_m_old)
		var_angle=sum/dble(s-1)
	   elseif (s.ge.10) then
c	   ** calc variance of m1, s1, and angle **
	      call avevar(c1,s,sp,dummy_ave,var_c)
	      call avevar(m1,s,sp,dummy_ave,var_m)
		var_angle=sum/dble(s-1)
c	   ** calc tolerances and exit if meets criteria **
		if (real(var_c).ne.0.) then
		   fc = abs(var_c - var_c_old)/var_c
		else
		   fc = 0.
		endif
		if (real(var_m).ne.0.) then
		   fm = abs(var_m - var_m_old)/var_m
		else
		   fm = 0.
		endif
		if (real(var_angle).ne.0.) then
		   fa = abs(var_angle - var_angle_old)/var_angle
		else
		   fa = 0.
		endif
		if (fa.le.ftol) then
		   goto 11
		endif
	      var_c_old=var_c
	      var_m_old=var_m
	      var_angle_old=var_angle
	   endif
c 	** check ftol condition is met on last pass **
	   if ((s.eq.sp).and.(fa.gt.ftol)) then
	      print*,'WARNING:- zrobfit'
	      print*,'WARNING:-  max number of iterations reached'
	      print*,'WARNING:- ftol=',real(fa),'( required=',ftol,' )'
	   endif
1	continue
11	continue

	return
	end

c-----------------------------------------------------------------------
	subroutine zrsel(x,y,n,np,iseed,x1,y1)
c-----------------------------------------------------------------------
c	randomly select n elements from x/y, with replacement
c
c-----------------------------------------------------------------------
c  modifications:
c	26-07-01	N. Teanby	Original code
c-----------------------------------------------------------------------
	implicit none
	integer n,np,i,iseed,rn
	double precision x(np),y(np),x1(np),y1(np)
	real ran2,r
	
	do 1 i=1,n
c     ** find a random number **
	   r = ran2(iseed)
c     ** scale up to be in range 1-n and round to nearest integer **
	   rn=nint(1. + dble(n-1)*dble(r))
c     ** select this element in x/y to be ith element in x1/y1 **
	   x1(i)=x(rn)
	   y1(i)=y(rn)
1	continue

	return
	end

c-----------------------------------------------------------------------
      FUNCTION ran2(idum)
      INTEGER idum,IM1,IM2,IMM1,IA1,IA2,IQ1,IQ2,IR1,IR2,NTAB,NDIV
      REAL ran2,AM,EPS,RNMX
      PARAMETER (IM1=2147483563,IM2=2147483399,AM=1./IM1,IMM1=IM1-1,
     *IA1=40014,IA2=40692,IQ1=53668,IQ2=52774,IR1=12211,IR2=3791,
     *NTAB=32,NDIV=1+IMM1/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
      INTEGER idum2,j,k,iv(NTAB),iy
      SAVE iv,iy,idum2
      DATA idum2/123456789/, iv/NTAB*0/, iy/0/
      if (idum.le.0) then
        idum=max(-idum,1)
        idum2=idum
        do 11 j=NTAB+8,1,-1
          k=idum/IQ1
          idum=IA1*(idum-k*IQ1)-k*IR1
          if (idum.lt.0) idum=idum+IM1
          if (j.le.NTAB) iv(j)=idum
11      continue
        iy=iv(1)
      endif
      k=idum/IQ1
      idum=IA1*(idum-k*IQ1)-k*IR1
      if (idum.lt.0) idum=idum+IM1
      k=idum2/IQ2
      idum2=IA2*(idum2-k*IQ2)-k*IR2
      if (idum2.lt.0) idum2=idum2+IM2
      j=1+iy/NDIV
      iy=iv(j)-idum2
      iv(j)=idum
      if(iy.lt.1)iy=iy+IMM1
      ran2=min(AM*iy,RNMX)
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software *%&&,1{.

c-----------------------------------------------------------------------
      SUBROUTINE avevar(dat,n,np,ave,var)
      INTEGER n,np
      double precision ave,var,dat(np)
      INTEGER j
      double precision s
c  ** average **
      ave=0.0
      do 11 j=1,n
        ave = ave + dat(j)
11    continue
      ave = ave / dble(n)

c  ** variance **
      var=0.0
      do 12 j=1,n
        s   = dat(j)-ave
	  var = var + s*s
12    continue
	var=var/dble(n - 1)

	return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software *%&&,1{.
