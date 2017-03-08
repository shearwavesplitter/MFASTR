c-----------------------------------------------------------------------
	subroutine zsourcepol(fast,lambda1,lambda2,vec1,vec2,spol,dspol)
c-----------------------------------------------------------------------
c
c	-calculate source polarisation and error
c	-error is the MAD (max angular deviation) and is the angle
c	subtended by the min and max eigenvalues (see kirschvink 1980, PCA)
c
c	variables
c	fast			real	fast direction (deg clock from N)
c	lambda1/2		real	largest/smallest eignevalue
c	vec1/2		real	largest/smallest eignevector
c	spol			real	polarisation direction (deg clock from N)
c	dspol			real	error (MAD angle)
c
c-----------------------------------------------------------------------
c	N. Teanby	31-7-02	Original code
c-----------------------------------------------------------------------

	implicit none
	real fast,lambda1,lambda2,vec1(2),vec2(2),spol,dspol,pi
	parameter (pi=3.141592654)

c  ** polarization in azimuth degrees(clockwise from north)
c	note, eigenvectors are in rotated coord frame and must add
c	fast to the angle. **
	spol = fast + atan2(vec1(1),vec1(2))*180./pi

c  ** calc MAD angle as a measure of the error **
	dspol = atan(lambda2/lambda1)*180./pi

	return
	end
