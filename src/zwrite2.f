c-----------------------------------------------------------------------
	subroutine zwrite2(file,lu,fmt,x,y,n,np)
c-----------------------------------------------------------------------
c	write out a 2 column datafile
c-----------------------------------------------------------------------
c	n. teanby	22/10/01	original code
c-----------------------------------------------------------------------
	
	implicit none
	integer n,np,i,lu
	real x(np),y(np)
	character*50 file
	character*50 fmt

	open(lu,file=file,status='unknown')
	do i = 1,n
	   write(lu,fmt) x(i),y(i)
	enddo
	close(lu)
	
	return
	end
