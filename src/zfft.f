c-----------------------------------------------------------------------
      SUBROUTINE zfft(y,nn,np,isign,yfft)
c-----------------------------------------------------------------------
c
c	routine to calc fft of of a real time series
c
c	INPUT VARIABLES
c	y(np)		real		time seris
c	nn		int		number of data points
c	np		int		size of array
c	isign		int		set =  1 for fft
c					set = -1 for inverse fft
c	OUTPUT VARIABLES
c	yfft(2*np)	real		fft of y
c		-odd and even indices are real and imag parts, respectively
c		-contains nn pairs of points
c
c	NOTES: nn must be a power of 2 (not checked for in routine)
c
c-----------------------------------------------------------------------
c	n. teanby	5-7-02	modified from Num Rec four1.f
c-----------------------------------------------------------------------
      INTEGER isign,nn,np
      REAL y(np),yfft(2*np)
      INTEGER i,istep,j,m,mmax,n
      REAL tempi,tempr
      DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp

c  -- this bit added --
c  ** copy y to yfft, because NR routine replaces y with it's fft **
c  ** yfft: odd and even indices are real and imag parts, respectively **
      do 100 i=1,nn
c	** real part **
	   yfft(2*i-1)    = y(i)
c	** imaginary part (=0 because real data) **
	   yfft(2*i)  = 0.0
100   continue

c  -- this bit unchanged from NR routine --
      n=2*nn
      j=1
      do 11 i=1,n,2
        if(j.gt.i)then
          tempr=yfft(j)
          tempi=yfft(j+1)
          yfft(j)=yfft(i)
          yfft(j+1)=yfft(i+1)
          yfft(i)=tempr
          yfft(i+1)=tempi
        endif
        m=n/2
1       if ((m.ge.2).and.(j.gt.m)) then
          j=j-m
          m=m/2
        goto 1
        endif
        j=j+m
11    continue
      mmax=2
2     if (n.gt.mmax) then
        istep=2*mmax
        theta=6.28318530717959d0/(isign*mmax)
        wpr=-2.d0*sin(0.5d0*theta)**2
        wpi=sin(theta)
        wr=1.d0
        wi=0.d0
        do 13 m=1,mmax,2
          do 12 i=m,n,istep
            j=i+mmax
            tempr=sngl(wr)*yfft(j)-sngl(wi)*yfft(j+1)
            tempi=sngl(wr)*yfft(j+1)+sngl(wi)*yfft(j)
            yfft(j)=yfft(i)-tempr
            yfft(j+1)=yfft(i+1)-tempi
            yfft(i)=yfft(i)+tempr
            yfft(i+1)=yfft(i+1)+tempi
12        continue
          wtemp=wr
          wr=wr*wpr-wi*wpi+wr
          wi=wi*wpr+wtemp*wpi+wi
13      continue
        mmax=istep
      goto 2
      endif


      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software *%&&,1{.
