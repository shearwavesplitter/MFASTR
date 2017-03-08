c-----------------------------------------------------------------------
	subroutine zrotate(x,y,z,n,np,theta,phi,a,b,c)
c-----------------------------------------------------------------------
c
c	rotate the coordinate axes of xyz data to new axes basis abc
c	 axes have been rotated in horiz (x-y) plane by phi (clockwise)
c	 z and c axis are separated by angle theta
c
c	for sesimic data:	x = east
c				y = north
c				z = up
c			c = in direction of wave propagation
c			a = horiz comp
c			b = perp to a and c in vertical plane
c				phi   = angle between east and a comps (clockwise)
c				theta = angle between c and z
c
c	relations between axes are:
c	a = x cos(phi) - y sin(phi)
c	b = x sin(phi) cos(theta) + y cos(phi) cos(theta) - z sin(theta)
c	c = x sin(phi) sin(theta) + y cos(phi) sin(theta) + z cos(theta)
c
c	variables:
c input: x		dble(np)		x comp of data
c	   y		dble(np)		y comp of data
c	   z		dble(np)		z comp of data
c	   n		int			number of data
c	   np		int			array dimension
c	   theta	dble			angle between r and z
c	   phi	dble			angle between x and h comps
c						(clockwise from x)
c output:a		dble(np)		rotated x axis
c	   b		dble(np)		rotated y axis
c	   c		dble(np)		rotated z axis
c
c  ** NB all angle in DEGREES **
c
c-----------------------------------------------------------------------
c  modifications:
c	12-07-01	N. Teanby	Original code
c-----------------------------------------------------------------------
	
	implicit none
	integer n,np
	double precision pi
	parameter (pi=3.141592654)
	double precision x(np),y(np),z(np),theta,phi,theta_rad,phi_rad
	double precision a(np),b(np),c(np)
	integer i
	
c  ** convert angles to radians **
	phi_rad   = phi * pi/180.
	theta_rad = theta * pi/180.
	
	do 1 i=1,np
	   a(i)=0.
	   b(i)=0.
	   c(i)=0.
1	continue

	do 2 i=1,n
	   a(i) =  (x(i) * cos(phi_rad)) 
     >         - (y(i) * sin(phi_rad))

	   b(i) =  (x(i) * sin(phi_rad) * cos(theta_rad))
     >         + (y(i) * cos(phi_rad) * cos(theta_rad))
     >         - (z(i) * sin(theta_rad))

	   c(i) =  (x(i) * sin(phi_rad) * sin(theta_rad)) 
     >	   + (y(i) * cos(phi_rad) * sin(theta_rad))
     >         + (z(i) * cos(theta_rad))
2	continue

	return
	end

