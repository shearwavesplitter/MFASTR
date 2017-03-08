c-----------------------------------------------------------------------
	subroutine zass_ini(inifile,lu,ext1,ext2,nwbeg,nwend,
     >dt_beg,dt_end,dtlag_max,dfast_max,t_off_beg,t_off_end,
     >tlag_scale,fast_scale,max_no_clusters,nmin,
     >OPT_verbose,OPT_outfiles)
c-----------------------------------------------------------------------
c
c	read in fast_scale and tlag_scale from the ass.ini file
c
c	variables
c	---------
c    in:
c	inifile	char*50	name of parameters file
c	lu		int		logical unit to open files on
c    out:
c	ext1/2	char*50	extention of the two components to do analysis on
c	nwbeg		int		number of start positions for S-wave window
c	nwend		int		number of end positions for S-wave window
c	dt_beg	real		increment for start of window (in seconds)
c	dt_end	real		increment for end of window (in seconds)
c	t_off_beg	real		maximum window beginning (relative to spick)
c	t_off_end	real		minimum window end (relative to spick)
c	dtlag_max	real		max allowable error in lag time for inclusion 
c					in clustering
c	dfast_max	real		" fast direction "
c	tlag_scale	real		range of tlag scale in seconds
c	fast_scale	real		range of fast direction scale in degrees
c	max_no_clusters	int	max. number of clusters
c	nmin		int		minimum number of points in an acceptable cluster
c	OPT_verbose	logical	true for verbose output
c	OPT_outfiles logical	true if write outfiles for gmt plots
c
c-----------------------------------------------------------------------
c	n.teanby	12-5-03	original code
c-----------------------------------------------------------------------
	
	implicit none
	integer nwbeg,nwend,lu
	integer nmin,max_no_clusters
	real dtlag_max,dfast_max,tlag_scale,fast_scale,dt_beg,dt_end
	real t_off_beg,t_off_end
	logical OPT_verbose,OPT_outfiles
	character*50 inifile,ext1,ext2

	open(lu,file=inifile,status='old')
	read(lu,*) ext1
	read(lu,*) ext2
	read(lu,*) nwbeg 
	read(lu,*) nwend
	read(lu,*) dt_beg
	read(lu,*) dt_end
	read(lu,*) dtlag_max
	read(lu,*) dfast_max
	read(lu,*) t_off_beg
	read(lu,*) t_off_end
	read(lu,*) tlag_scale
	read(lu,*) fast_scale
	read(lu,*) max_no_clusters
	read(lu,*) nmin
	read(lu,*) OPT_verbose
	read(lu,*) OPT_outfiles
	close(lu)

	return
	end
