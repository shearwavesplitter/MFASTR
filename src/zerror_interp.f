c-----------------------------------------------------------------------
	subroutine zerror_interp(error,np1,np2,np2int,error_int)
c-----------------------------------------------------------------------
c
c	interpolate the error surface in the tlag direction.
c	the interpolation is done one row at a time
c
c	variables
c    in:
c	error(np1,np2)	real		error surface (i.e. lambda2)
c	np1/2			int		array dimensions
c	np2int		int		np2 after interpolation
c    out:
c	error_int(np1,np2int)	real	interpolated error surface
c    local:
c	np			int		array dimension
c					(read from SIZE_np.h at compile time)
c
c
c-----------------------------------------------------------------------
c	N. Teanby	4-8-02	Original code
c-----------------------------------------------------------------------

	implicit none
	integer np,np1,np2,np2int
	include "SIZE_np.h"
	real error(np1,np2),error_int(np1,np2int)
	real error_row(np),error_row_int(np)
	integer f,i,j,n_int
	
c  ** check that np is big enough **
	if (np.lt.np2int) then
	   pause 'ERROR: zerror_interp: np not big enough'
	endif

c  ** calc the interpolation factor based on np2 and np2int **
	f = (np2int-1)/(np2-1)
c  ** f needs to be an integer for zsplint to work **
	if (mod(np2int-1,np2-1).ne.0) then
	   pause 'ERROR: zerror_interp: f not a whole number'
	endif

c  ** interpolate error surface in tlag direction **
c  ** do interpolation one row at a time **
	do 1 i=1,np1
c	** copy ech row to a dummy array **
	   do 11 j=1,np2
	      error_row(j)=error(i,j)
11	   continue
c	** interpolate the row data **
	   call zsplint(error_row,np2,f,error_row_int,n_int)
c     ** check that n_int = np2int 
c	   (this should not be possible but check anyway)**
	   if (np2int.ne.n_int) then
	      pause 'ERROR: zerror_interp: np2int.ne.n_int'
	   endif
c	** copy interpolated row to output array **
	   do 22 j=1,n_int
	      error_int(i,j)=error_row_int(j)
22	   continue
1	continue

	return
	end
