c-----------------------------------------------------------------------
	subroutine zchnhdr_dble(hdr,c16,h1,h2,h3a,h3b,h4)
c-----------------------------------------------------------------------
c
c	change varaibles in sac header
c
c	variables:
c input: hdr	char*6		name of header varaible to read
c	   c16	char*16		the header varaible in a *16 character
c	   h1		dble(14,5)		header part1
c	   h2		int(8,5)		header part2
c	   h3a	char*8		header part3a
c	   h3b	char*16		header part3b
c	   h4		char*8 (7,3)	header part4
c output:h1		dble(14,5)		header part1 after changes
c	   h2		int(8,5)		header part2 after changes
c	   h3a	char*8		header part3a after changes
c	   h3b	char*16		header part3b after changes
c	   h4		char*8 (7,3)	header part4 after changes
c
c-----------------------------------------------------------------------
c  modifications:
c	12-07-01	N. Teanby	Original code
c-----------------------------------------------------------------------
	
	implicit none
	integer l
	character*6 hdr
	double precision h1(14,5)
	integer h2(8,5)
	character h3a*8, h3b*16
	character*8 h4(7,3)
	character c16*16
	character*12 FMTdbl, FMTint, FMTc08, FMTc16

c  ** get length of hdr **
	l=index(hdr//' ',' ')-1

c  ** format descriptors **	
	FMTdbl='(g15.7)'
	FMTint='(i10)'
	FMTc08='(a8)'
	FMTc16='(a16)'

c  ** locate varaible in sac header and write to c16 using internal files **
c  **h1**
	if ((hdr(1:l).eq.'delta').or.(hdr(1:l).eq.'DELTA')) then
	   read(c16,FMTdbl) h1(1,1)
	else if ((hdr(1:l).eq.'a').or.(hdr(1:l).eq.'A')) then
	   read(c16,FMTdbl) h1(2,4)
	else if ((hdr(1:l).eq.'t0').or.(hdr(1:l).eq.'T0')) then
	   read(c16,FMTdbl) h1(3,1)
	else if ((hdr(1:l).eq.'t1').or.(hdr(1:l).eq.'T1')) then
	   read(c16,FMTdbl) h1(3,2)
	else if ((hdr(1:l).eq.'t2').or.(hdr(1:l).eq.'T2')) then
	   read(c16,FMTdbl) h1(3,3)
	else if ((hdr(1:l).eq.'t3').or.(hdr(1:l).eq.'T3')) then
	   read(c16,FMTdbl) h1(3,4)
	else if ((hdr(1:l).eq.'t4').or.(hdr(1:l).eq.'T4')) then
	   read(c16,FMTdbl) h1(3,5)
	else if ((hdr(1:l).eq.'t5').or.(hdr(1:l).eq.'T5')) then
	   read(c16,FMTdbl) h1(4,1)
	else if ((hdr(1:l).eq.'f').or.(hdr(1:l).eq.'F')) then
	   read(c16,FMTdbl) h1(5,1)
	else if ((hdr(1:l).eq.'resp0').or.(hdr(1:l).eq.'RESP0')) then
	   read(c16,FMTdbl) h1(5,2)
	else if ((hdr(1:l).eq.'resp1').or.(hdr(1:l).eq.'RESP1')) then
	   read(c16,FMTdbl) h1(5,3)
	else if ((hdr(1:l).eq.'resp2').or.(hdr(1:l).eq.'RESP2')) then
	   read(c16,FMTdbl) h1(5,4)
	else if ((hdr(1:l).eq.'resp3').or.(hdr(1:l).eq.'RESP3')) then
	   read(c16,FMTdbl) h1(5,5)
	else if ((hdr(1:l).eq.'resp4').or.(hdr(1:l).eq.'RESP4')) then
	   read(c16,FMTdbl) h1(6,1)
	else if ((hdr(1:l).eq.'resp5').or.(hdr(1:l).eq.'RESP5')) then
	   read(c16,FMTdbl) h1(6,2)
	else if ((hdr(1:l).eq.'resp6').or.(hdr(1:l).eq.'RESP6')) then
	   read(c16,FMTdbl) h1(6,3)
	else if ((hdr(1:l).eq.'cmpaz').or.(hdr(1:l).eq.'CMPAZ')) then
	   read(c16,FMTdbl) h1(12,3)
	else if ((hdr(1:l).eq.'cmpinc').or.(hdr(1:l).eq.'CMPINC')) then
	   read(c16,FMTdbl) h1(12,4)
c  **h2**
	else if ((hdr(1:l).eq.'npts').or.(hdr(1:l).eq.'NPTS')) then
	   read(c16,FMTint) h2(2,5)
c  **h3**
c  **h4**
	else if ((hdr(1:l).eq.'kt0').or.(hdr(1:l).eq.'KT0')) then
	   read(c16,FMTc08) h4(2,1)
	else if ((hdr(1:l).eq.'kt1').or.(hdr(1:l).eq.'KT1')) then
	   read(c16,FMTc08) h4(2,2)
	else if ((hdr(1:l).eq.'kt2').or.(hdr(1:l).eq.'KT2')) then
	   read(c16,FMTc08) h4(2,3)
	else if ((hdr(1:l).eq.'kt3').or.(hdr(1:l).eq.'KT3')) then
	   read(c16,FMTc08) h4(3,1)
	else if ((hdr(1:l).eq.'kt4').or.(hdr(1:l).eq.'KT4')) then
	   read(c16,FMTc08) h4(3,2)
	else if ((hdr(1:l).eq.'kt5').or.(hdr(1:l).eq.'KT5')) then
	   read(c16,FMTc08) h4(3,3)
	else if ((hdr(1:l).eq.'kuser0').or.(hdr(1:l).eq.'KUSER0')) then
	   read(c16,FMTc08) h4(5,3)
	else if ((hdr(1:l).eq.'kcmpnm').or.(hdr(1:l).eq.'KCMPNM')) then
	   read(c16,FMTc08) h4(6,3)
c  **error message**
	else
	 print*,'WARNING:zchnhdr'
	 print*,'WARNING: _',hdr,'_ not defined, variable not changed'
	 print*,'WARNING: remember to put a blank space after variable'
	endif

	return
	end


