c-----------------------------------------------------------------------
	function fftable(ndf)
c-----------------------------------------------------------------------
c
c	function to return value of f statistic
c
c	specialised for k=2 and alpha=0.05 (2 params and 95% confidence)
c
c	table downloaded from:
c	http://www.itl.nist.gov/div898/handbook/eda/section3/eda3673.htm
c
c	ndf 	int	number of degrees of freedom
c
c-----------------------------------------------------------------------
c	N. Teanby	1-8-02	Original code
c-----------------------------------------------------------------------

	implicit none
	real fftable
	integer ndf
	real ftable_data(100)
	data ftable_data / 199.500, 19.000, 9.552, 6.944, 5.786, 
     >5.143, 4.737, 4.459, 4.256, 4.103, 
     >3.982, 3.885, 3.806, 3.739, 3.682, 
     >3.634, 3.592, 3.555, 3.522, 3.493, 
     >3.467, 3.443, 3.422, 3.403, 3.385, 
     >3.369, 3.354, 3.340, 3.328, 3.316, 
     >3.305, 3.295, 3.285, 3.276, 3.267, 
     >3.259, 3.252, 3.245, 3.238, 3.232, 
     >3.226, 3.220, 3.214, 3.209, 3.204, 
     >3.200, 3.195, 3.191, 3.187, 3.183, 
     >3.179, 3.175, 3.172, 3.168, 3.165, 
     >3.162, 3.159, 3.156, 3.153, 3.150, 
     >3.148, 3.145, 3.143, 3.140, 3.138, 
     >3.136, 3.134, 3.132, 3.130, 3.128, 
     >3.126, 3.124, 3.122, 3.120, 3.119, 
     >3.117, 3.115, 3.114, 3.112, 3.111, 
     >3.109, 3.108, 3.107, 3.105, 3.104, 
     >3.103, 3.101, 3.100, 3.099, 3.098, 
     >3.097, 3.095, 3.094, 3.093, 3.092, 
     >3.091, 3.090, 3.089, 3.088, 3.087 /

	if (ndf.le.0) then
c	** if ndf is below range 1-100 error **
	   pause 'ERROR: fftable: ndf.le.0'
	else if (ndf.le.100) then
c	** if ndf is in range 1-100 take directly from tabulated values **
	   fftable=ftable_data(ndf)
	else if (ndf.le.999) then
c	** if ndf is in range 100-999 use linear interpolation **
c	** f=3.087 at ndf = 100, and f=3.000 at ndf=999 **
	   fftable = 3.087 - 0.087*real(ndf-100)/899.
	else if (ndf.gt.999) then
c	** if ndf is over range return value at 999 **
	   fftable = 3.0
	   print*,'WARNING: ndf.gt.999, f statistic at ndf=999 returned'
	endif

	return
	end
