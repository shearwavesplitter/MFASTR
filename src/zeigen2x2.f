c-----------------------------------------------------------------------
	subroutine zeigen2x2(matrix,lambda1,lambda2,vec1,vec2)
c-----------------------------------------------------------------------
c
c	calculate eigenvalues and eigenvectors of a 2x2 matrix
c	variables
c    in:
c	matrix(2,2)			real	matrix to find eigenvalues of
c    out:
c	lambda1/2			real	eignevalues (lambda1 is largest)
c	vec1/2			real	eignevectors (vec1 corresponds to lambda1)
c
c	see Boas p413 for maths
c
c-----------------------------------------------------------------------
c	N. Teanby	16-7-02	Original code
c-----------------------------------------------------------------------

	implicit none
	real matrix(2,2),lambda1,lambda2,a,b,c,temp,norm,vec1(2),vec2(2)

c  ** EIGENVALUES **
c  ** eigenvalue are the solution of a quadratic eqn with c.f.s **
	a = 1.
	b = - matrix(1,1) - matrix(2,2)
	c = matrix(1,1)*matrix(2,2) - matrix(1,2)*matrix(2,1)

	lambda1 = 0.5*( -b + sqrt(b**2 - 4*a*c))
	lambda2 = 0.5*( -b - sqrt(b**2 - 4*a*c))
	
c  ** order the eigenvalues so that lambda2 is the smallest **
	if (lambda2.gt.lambda1) then
	   temp    = lambda1
	   lambda1 = lambda2
	   lambda2 = temp
	endif

C  ** EIGENVECTORS (normalised) **
	vec1(1)=1.
	vec1(2)=(lambda1-matrix(1,1))/matrix(1,2)
	norm = sqrt(vec1(1)**2 + vec1(2)**2)
	vec1(1)=vec1(1)/norm
	vec1(2)=vec1(2)/norm
	
	vec2(1)=1.
	vec2(2)=(lambda2-matrix(1,1))/matrix(1,2)
	norm = sqrt(vec2(1)**2 + vec2(2)**2)
	vec2(1)=vec2(1)/norm
	vec2(2)=vec2(2)/norm
	
	return
	end
