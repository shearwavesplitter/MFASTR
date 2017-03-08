c-----------------------------------------------------------------------
	subroutine zreaddataSAC(file1,file2,lu,force_spick,
     >x,y,n,np,b,delta,as,fs,ppick,spick,phi,theta,
     >ev_x,ev_y,ev_z,ev_time)
c-----------------------------------------------------------------------
c	read in two binary sac files containing two components of a seismogram
c
c	force_spick	logical	.true. ERROR if spick not defined
c				(use in zass.f, where window is defined using spick)
c					.false. no error if spick not defined
c				(use in split.f, where window is defined)
c
c	x/y(np)	real	data in files 1 and 2
c	n		int	number of data points
c	np		int	array dimensions
c	b		real	start of time series
c	delta		real	sampling interval
c	as		real	beginning of s-wave window (hand pick)
c	fs		real	end of s-wave window (hand pick)
c	ppick		real	p-wave onset (seconds) read from T4 header variable
c	spick		real	s-wave onset (seconds) read from T5 header variable
c	phi		real	rotation angle of frame (deg clock from N)
c	theta		real	rotation angle of frame (angle between z and z')
c	ev_x/y/z	real	x/y/z position of event read from sac header in metres
c				(stored in RESP4/5/6, not ideal but no x y z loc in
c				SAC header)
c	ev_time	real	time of event in days 
c	 
c-----------------------------------------------------------------------
c	N. Teanby	20-8-02	original code
c-----------------------------------------------------------------------

	implicit none
	integer n,np,lu
	real x(np),y(np),b,delta,as,fs,ppick,spick,phi,theta
	real ev_x,ev_y,ev_z,ev_time
	logical force_spick
c  ** header for sac file
	real h1(14,5)
	integer h2(8,5)
	character h3a*8, h3b*16
	character*8 h4(7,3)
c  ** 
	character*50 file1,file2

c  ** read in data (SAC binary file) **
	call zbreadsac(file1,lu,np,h1,h2,h3a,h3b,h4,x,n)
	call zbreadsac(file2,lu,np,h1,h2,h3a,h3b,h4,y,n)

c  ** extract header info **
	delta  = h1(1,1)
	b      = h1(2,1)
	as     = h1(3,3)
	fs     = h1(3,4)
	ppick  = h1(3,5)
	spick  = h1(4,1)
	phi	 = h1(5,2)
	theta	 = h1(5,4)
	ev_x   = h1(6,1)
	ev_y   = h1(6,2)
	ev_z   = h1(6,3)
	ev_time= real(h2(1,2)) + real(h2(1,3))/24. + real(h2(1,4))/1440. +
     > real(h2(1,5))/86400. + real(h2(2,1))/86400000.
	print*,'spick: ',spick
c  ** make sure the essential quantities are defined **
	if (delta.lt.0.) pause 'ERROR: zreaddataSAC: delta= -ve'	
	if (b    .lt.0.) pause 'ERROR: zreaddataSAC: b    = -ve'
	if (force_spick) then	
	  if (spick.lt.0.) pause 'ERROR: zreaddataSAC: S-Pick missing '
	endif
	
c  ** check other non-essential quantities are defined **
	if (nint(phi).eq.-12345) then
	   phi=0.
	   print*,'WARNING: zreaddataSAC: phi undefined - set to 0'
	endif
	if (nint(theta).eq.-12345) then
	   theta=0.
	   print*,'WARNING: zreaddataSAC: theta undefined - set to 0'
	endif
	if (nint(ppick).eq.-12345) then
	   print*,'WARNING: zreaddataSAC: ppick = undefined'
	endif
	if (nint(as).eq.-12345) then
	   print*,'WARNING: zreaddataSAC: as = undefined'
	endif
	if (nint(fs).eq.-12345) then
	   print*,'WARNING: zreaddataSAC: fs = undefined'
	endif
	if (h2(2,1).eq.-12345) then
	   ev_time =0.
	   print*,'WARNING: zreaddataSAC: ev_time undefined, set to 0'
	endif
	if ((nint(ev_x).eq.-12345).or.(nint(ev_y).eq.-12345).or.
     >	(nint(ev_z).eq.-12345)) then
	   ev_x=0.
	   ev_y=0.
	   ev_z=0.
	   print*,'WARNING: zreaddataSAC: ev_x/y/z undefined, set to 0'
	endif
	   
	return
	end

