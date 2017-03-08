c-----------------------------------------------------------------------
	function fda(angle1,angle2)
c-----------------------------------------------------------------------
c	find differnce between two angles which have 180 deg ambiguity
c	angle1/2	angles in degrees
c	fda		difference in degrees
c-----------------------------------------------------------------------
c	N. Teanby	30-8-02	Original code
c-----------------------------------------------------------------------
	implicit none
	real angle1,angle2,pi,fda
	parameter (pi=3.141592654)
	
	fda = acos( abs(cos( (angle1-angle2)*pi/180.) ) )*180./pi
	
	return
	end



