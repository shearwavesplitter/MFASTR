c-----------------------------------------------------------------------
	function fangle90(angle)
c-----------------------------------------------------------------------
c
c	function to return angle in the range -90 to 90 degrees
c	ASSUMES A 180 DEGREEE AMBIGUITY (true for both polarisation angle
c	and fast direction)
c
c	angle		real	angle to convert
c
c-----------------------------------------------------------------------
c	N. Teanby	4-8-02	Original code
c-----------------------------------------------------------------------

	implicit none
	real angle,fangle90

c  ** keep looping over angle until it's in the correct range **
1	continue
	if (angle.gt.90.) then
	   angle = angle - 180.
	else if (angle.lt.-90) then
	   angle = angle + 180.
	else
c	** angle is in correct range already so don't change **
	   goto 999
	endif
	goto 1

999	continue

	fangle90 = angle
	
	return
	end
