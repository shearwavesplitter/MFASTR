c-----------------------------------------------------------------------
	subroutine zsplit_ini(inifile,lu,
     >	ext1,ext2,fast_scale,tlag_scale,OPT_outfiles)
c-----------------------------------------------------------------------
c	read in fast_scale and tlag_scale from the split.ini file
c
c	in:
c	 inifile	char*50	initialisation file
c	out:
c	 ext1/2	char*50	extention of the two components to do analysis on
c	 fast_scale	real		grid search scale for fast direction
c	 tlag_scale	real		grid search scale for tlag
c	 OPT_outfiles logical	true if write outfiles for gmt plots
c
c-----------------------------------------------------------------------
c	N. Teanby	12-5-03	Original code
c-----------------------------------------------------------------------

	implicit none
	integer lu
	real fast_scale,tlag_scale
	logical OPT_outfiles
	character*50 inifile,ext1,ext2
		
	open(lu,file=inifile,status='old')
	read(lu,*) ext1
        print *,'using ext1 as ',ext1
	read(lu,*) ext2
        print *,'using ext2 as ',ext2
	read(lu,*) fast_scale
        print *,'using fast_scale as ',fast_scale
	read(lu,*) tlag_scale
        print *,'using tlag_scale as ',tlag_scale
	read(lu,*) OPT_outfiles
        print *,'using OPT_outfiles as ',OPT_outfiles
	close(lu)

	return
	end
