c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
	subroutine zrotabc2enz(phi,theta,angle_abc,dangle_abc,
     >angle_strike_enz,dangle_strike_enz,angle_dip_enz,dangle_dip_enz)
c-----------------------------------------------------------------------
c
c	-find strike and dip of a plane 
c
c	-the original e n z coords have been rotated into a b c (in frame of ray)
c	-the angles az and polaz are measured in the a b c frame
c	-a b c is rotated by phi and theta relative to e n z
c	-this prog asumes that angle abc defines a plane parralell to c axis
c	-the normal to this plane rotated into the e n z frame and used 
c	 to define the plane in the enz frame (plane goes through origin in each
c	 case)
c	-angle enz is the angle, clockwise from north, of the intersection of the 
c	 plane with the en horizontal plane
c
c-----------------------------------------------------------------------
c	n. teanby	31-08-01	original code
c-----------------------------------------------------------------------
	
	implicit none
	real deg2rad,fdstr,fddip
	parameter (deg2rad=0.0174533)
	real phi,theta,angle_abc,dangle_abc
	real angle_strike_enz,dangle_strike_enz
	real angle_dip_enz,dangle_dip_enz
	real phi_rad,theta_rad,angle_abc_rad,nx,ny,nz

c  ** convert angles to radians **	
	phi_rad       = phi       * deg2rad
	theta_rad     = theta     * deg2rad
	angle_abc_rad = angle_abc * deg2rad

c  ** calculate the normal to the plane in enz coords **	
	nx =   cos(phi_rad)*cos(angle_abc_rad)
     >     - sin(phi_rad)*cos(theta_rad)*sin(angle_abc_rad)
	ny = - sin(phi_rad)*cos(angle_abc_rad)
     >     - cos(phi_rad)*cos(theta_rad)*sin(angle_abc_rad)
	nz =   sin(theta_rad)*sin(angle_abc_rad)

c  ** calc strike and dip **
	call zstrikedip(nx,ny,nz,angle_strike_enz,angle_dip_enz)	

c  ** get strike in range 0-180 degrees (initially -180 to 180)**
	if (angle_strike_enz.lt.0.) then
	   angle_strike_enz = angle_strike_enz + 180.
	endif

c  ** error on strike and dip of angle in enz **
	dangle_strike_enz = 
     >( fdstr(theta,angle_abc, dangle_abc) +
     >  fdstr(theta,angle_abc,-dangle_abc) )/2.
	dangle_dip_enz   =
     >( fddip(theta,angle_abc, dangle_abc) +
     >  fddip(theta,angle_abc,-dangle_abc) )/2.

	return
	end

c-----------------------------------------------------------------------
	subroutine zstrikedip(nx,ny,nz,strike,dip)
c-----------------------------------------------------------------------
c
c	-find strike and dip of a plane defined by the plane normal
c	(nx,ny,nz)
c
c	output strike and dip in degrees
c-----------------------------------------------------------------------
c	n. teanby	18-09-01	original code
c-----------------------------------------------------------------------
	
	implicit none
	real pi
	parameter (pi=3.141592654)
	real nx,ny,nz,strike,dip

c  ** find dip (rem vector has magnitude = 1, use cos = base/hypot **
	dip = asin( sqrt(nx**2 + ny**2))

c  ** find strike **
	if (nz.ge.0.) then
	   strike = atan2(-ny,nx)
	else
	   strike = atan2(ny,-nx)
	endif
	
c  ** convert to degrees **
	dip    = dip    * 180./pi
	strike = strike * 180./pi

	return
	end

c-----------------------------------------------------------------------
	function fdstr(theta,az,daz) 
c-----------------------------------------------------------------------
c
c	consider a plane in abc frame defined by angle az
c	consider a second plane in abc frame defined by az+daz
c
c	this function find the angle between the strikes of the two
c	planes in the enz frame.
c
c	expression calculated by defining the plane normals in abc
c	rotating these normals to enz and finding the angle between
c	the horizontal components using the dot product
c
c in:	theta		real	rotation angle theta in degrees
c	az		real	azimuth of plane 1 in abc frame in degrees
c	daz		real	difference between azimuths of plane 1 and 2
c				in abc frame in degrees
c	RETURN	real	difference between strike of two planes in
c				enz frame in degrees
c
c-----------------------------------------------------------------------
c	n. teanby	12-12-01	original code
c-----------------------------------------------------------------------
	implicit none

	real pi
	parameter (pi=3.141592654)
	real theta,az,daz,fdstr
	real theta_rad, az_rad, daz_rad
	real dotprod,mod
	
	
	theta_rad	= theta	*pi/180.
	az_rad	= az		*pi/180.
	daz_rad	= daz		*pi/180.
	
	dotprod = 	cos(az_rad)*cos(az_rad+daz_rad) + 
     >		cos(theta_rad)**2 * sin(az_rad)*sin(az_rad+daz_rad)

	mod = sqrt((cos(az_rad)**2 + cos(theta_rad)**2 * sin(az_rad)**2)*
     >(cos(az_rad+daz_rad)**2+cos(theta_rad)**2*sin(az_rad+daz_rad)**2))

	fdstr    =	acos(dotprod/mod)*180./pi
	
	return
	end

c-----------------------------------------------------------------------
	function fddip(theta,az,daz) 
c-----------------------------------------------------------------------
c
c	consider a plane in abc frame defined by angle az
c	consider a second plane in abc frame defined by az+daz
c
c	this function find the angle between the dips of the two
c	planes in the enz frame.
c
c	expression calculated by defining the vertical comp of the 
c	plane normals in abcthis  to enz and finding the angle between
c	the vertical components
c
c in:	theta		real	rotation angle theta in degrees
c	az		real	azimuth of plane 1 in abc frame in degrees
c	daz		real	difference between azimuths of plane 1 and 2
c				in abc frame in degrees
c	RETURN	real	difference between dips of two planes in
c				enz frame in degrees
c
c-----------------------------------------------------------------------
c	n. teanby	12-12-01	original code
c-----------------------------------------------------------------------
	implicit none

	real pi
	parameter (pi=3.141592654)
	real theta,az,daz,fddip
	real theta_rad, az_rad, daz_rad
	real nz,nz0
	
	
	theta_rad	= theta	*pi/180.
	az_rad	= az		*pi/180.
	daz_rad	= daz		*pi/180.
	
c  ** calc vertical comp of plane normal in enz frame **
	nz0 =   sin(theta_rad)*sin(az_rad)
	nz  =   sin(theta_rad)*sin(az_rad + az_rad)

	fddip    =	abs(acos(nz0)-acos(nz))*180./pi
	
	return
	end
