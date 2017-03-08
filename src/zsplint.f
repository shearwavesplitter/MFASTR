c-----------------------------------------------------------------------
	subroutine zsplint(y,n,f,yinterp,ninterp)
c-----------------------------------------------------------------------
c	interpolate y by integer factor of f using NR cubic B-spline routines
c
c	variables
c    in:
c	y(np)		real	data series to interpolate
c	np		int	array dimension (read from SIZE_np.h at compile time)
c	n		int	number of data points
c	f		int	interpolation factor
c    out:
c	yinterp(np)	real	interpolated y
c	ninterp	int	number of interpolated points = f(n-1)+1
c    local:
c	y2(np)	int	second derivatives of splines
c
c-----------------------------------------------------------------------
c	N. Teanby	21-8-02	Original code
c-----------------------------------------------------------------------

	implicit none
	integer n,np,f,i,ninterp
	include "SIZE_np.h"
	real y(np),yinterp(np),y2(np),x

c  ** calc number of interpolated datapoints **
c        print *," In program zsplint new version"
	ninterp=f*(n-1)+1
	if (ninterp.gt.np) then
	   print *,'np, ninterp=',np,ninterp
	   pause 'ERROR: zsplint: resampling creates too many datapoints.
     1       Try recompiling after editing SIZE_np.h' 
	endif

c  ** find second derivatives of interpolating splines **
	call spline(y,n,y2)

c  ** do interpolation **
	do 1,i=1,ninterp
	   x=1+real(i-1)/real(f)
	   call splint(y,y2,n,np,x,yinterp(i))
1	continue

	return
	end

c-----------------------------------------------------------------------
c  ** modified for natural spline only and unit x spacing **
c	y = data to interpolate
c	n = number of data points
c	np = array dimensions
c	y2 = 2nd derivatives of splines
      SUBROUTINE spline(y,n,y2)
      implicit none
	INTEGER n,np
	include "SIZE_np.h"
      REAL y(np),y2(np)
      INTEGER i,k
      REAL p,qn,sig,un,u(np)

      y2(1)=0.
      u(1)=0.

      do 11 i=2,n-1
        sig=1./2.
        p=sig*y2(i-1)+2.
        y2(i)=(sig-1.)/p
        u(i)=(6.*((y(i+1)-y(i))-(y(i)-y(i-1)))/2.-sig*u(i-1))/p
11    continue
      qn=0.
      un=0.
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
      do 12 k=n-1,1,-1
        y2(k)=y2(k)*y2(k+1)+u(k)
12    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software *%&&,1{.
	
c-----------------------------------------------------------------------
c  ** modified for unit x spacing **
c	ya = data to interpolate
c	n = number of data points
c	np = array dimensions
c	y2a = 2nd derivatives of splines (from spline)
c	x = x value to interpolate at
c	y = interpolated value
      SUBROUTINE splint(ya,y2a,n,np,x,y)
      implicit none
	INTEGER n,np
      REAL x,y,y2a(np),ya(np)
      INTEGER k,khi,klo
      REAL a,b,h
      klo=1
      khi=n
1     if (khi-klo.gt.1) then
        k=(khi+klo)/2
        if(real(k).gt.x)then
          khi=k
        else
          klo=k
        endif
      goto 1
      endif
      h=real(khi-klo)
      if (h.eq.0.) pause 'bad x input in splint'
      a=(khi-x)/h
      b=(x-real(klo))/h
      y=a*ya(klo)+b*ya(khi)+((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**
     *2)/6.
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software *%&&,1{.

