	program sac2xy
	
c  ** convert sac binary format to xy 2 column data (time,disp)**
	
	implicit none
	integer np,j
	parameter (np=1000000)
c  ** header **
	real h1(14,5)
	integer h2(8,5)
	character h3a*8, h3b*16
	character*8 h4(7,3)
	real d1(np)
	integer npts
c  ** other **
	character*50 infile,outfile
	real b,delta
	
	print*,'enter infile'
	read (*,'(a)'),infile
	print*,'enter outfile'
	read (*,'(a)'),outfile
		
	call zbreadsac(infile,20,np,h1,h2,h3a,h3b,h4,d1,npts)
c  ** find beginning of time series (b) and spacing (delta)**
	delta  = h1(1,1)
	b      = h1(2,1)
c  ** output file **
	open(30,file=outfile)
	do 5 j=1,npts
	   write(30,'(e15.5,2x,e15.5)') b+real(j-1)*delta,d1(j)
5	continue
	close(30)

	end


