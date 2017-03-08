	parameter (np1=181,np2=21,np2int=161)
c  this sets the number of points but also be default sets the maximum
c  delay time since the delay time is reset to an integer multiple of np2*delta
c  previously np2 was 41 so that for 100 sample/sec data we had to go no
c  less than 0.4 s
c  np2int-1 must be a multiple of np2-1.  It is the number of interpolated points.
