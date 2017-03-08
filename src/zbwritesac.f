c-----------------------------------------------------------------------
	subroutine zbwritesac(file,lu,np,h1,h2,h3a,h3b,h4,d1,npts)
c-----------------------------------------------------------------------
c
c	write a sac binary data file
c	  (may need to add a data part2 if unevenly spaced data is ever used)
c
c	variables:
c input: file	char*50		file to write
c	   lu		int			logical unit to write file to
c	   np		int			dimension of d1 array
c output:h1		real(14,5)		header part1
c	   h2		int(8,5)		header part2
c	   h3a	char*8		header part3a
c	   h3b	char*16		header part3b
c	   h4		char*8 (7,3)	header part4
c	   d1		real(np)		data points
c	   npts	int			number of data points (=h2(2,5))
c
c-----------------------------------------------------------------------
c  modifications:
c	5-11-02	N. Teanby	Original code (modified from zbreadsac.f)
c-----------------------------------------------------------------------

	implicit none

	integer lu,np,npts
	real h1(14,5)
	integer h2(8,5)
	character h3a*8, h3b*16
	character*8 h4(7,3)
	real d1(np)
	character*4 char_tmp1,char_tmp2,char_tmp3,char_tmp4
	character*8 char_tmp8
	character*50 file
	integer i,j

c  ** open file as direct access **		
	open(unit=lu,file=file,form='unformatted',access='direct',recl=4)
	print*,'file opened'
c  ** h1 **
	do i=1,14
	   do j=1,5
	      write(lu,rec=5*(i-1)+j) h1(i,j)
	   enddo
	enddo
c  ** h2 **
	do i=1,8
	   do j=1,5
	      write(lu,rec=70+5*(i-1)+j) h2(i,j)
	   enddo
	enddo
c  ** h3 **
	char_tmp1=h3a(1:4)
	char_tmp2=h3a(5:8)
	write(lu,rec=111) char_tmp1
	write(lu,rec=112) char_tmp2
	char_tmp1=h3b(1:4)
	char_tmp2=h3b(5:8)
	char_tmp3=h3b(9:12)
	char_tmp4=h3b(13:16)
	write(lu,rec=113) char_tmp1
	write(lu,rec=114) char_tmp2
	write(lu,rec=115) char_tmp3
	write(lu,rec=116) char_tmp4
c  ** h4 **
	do i=1,7
	   do j=1,3
	      char_tmp8=h4(i,j)
		char_tmp1=char_tmp8(1:4)
		char_tmp2=char_tmp8(5:8)
		write(lu,rec=116+6*(i-1)+2*j-1) char_tmp1
	      write(lu,rec=116+6*(i-1)+2*j) char_tmp2
	   enddo
	enddo
c  ** d1 **
c  ** check npts same as h2(2,5) from header **
	if (npts.ne.h2(2,5)) then
	   print*,'WARNING:zbwritesac'
	   print*,'WARNING:  npts.ne.h2(2,5) in header'
	endif
c  ** check npts does not exceed array dimension **
	if (npts.gt.np) then
	   print*,'WARNING:zbwritesac'
	   print*,'WARNING:  npts.gt.np, data missed out'
	endif
c  ** write data **
	do i=1,npts
	   write(lu,rec=158+i),d1(i)
	enddo

	close(lu)
	end
