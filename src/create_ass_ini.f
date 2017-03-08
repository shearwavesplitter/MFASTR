c-----------------------------------------------------------------------
	program create_ass_ini
c-----------------------------------------------------------------------
c
c	program to create an 'ass.ini' file for use with ASS automated
c	shearwave splitting code
c
c	prompt user for input
c
c	example 'ass.ini' aoutput file
c
c
c	'.a'            ext1
c	'.b'            ext2
c	4               nwbeg  (was 2)
c	15              nwend (was 25)
c	0.016           dt_beg
c	0.008           dt_end
c	0.8             dtlag_max
c	30.             dfast_max
c	0.02            t_off_beg
c	0.04            t_off_end
c	0.04            tlag_scale
c	180.            fast_scale
c	20              max_no_clusters
c	2               nmin
c	.true.  OPT_verbose
c	.true.  OPT_outfiles
c
c
c-----------------------------------------------------------------------
c	n.teanby	19/6/03	original code
c-----------------------------------------------------------------------
	implicit none
	character*1 ext1,ext2
	character*2 ext1full,ext2full
	integer nwbeg,nwend,max_no_clusters,nmin,n
	real dt_beg,dt_end,dtlag_max,dfast_max
	real t_off_beg,t_off_end,tlag_scale,fast_scale
	character*6 OPT_verbose,OPT_outfiles
	
	character*50 inifile,field
	character*1 ans
	real tbeg0,tbeg1,tend0,tend1
	
	integer ierror
	
	inifile='ass.ini'

c  ** set some default values **
	open(17,file=inifile,status='old',iostat=ierror)
	close(17)
	if (ierror.eq.0) then
c  	** get these from ass.ini if already exists **
	   print*,'reading from: ',inifile
	   open(17,file=inifile,status='old')
         read(17,*) ext1full
         read(17,*) ext2full
	 ext1=ext1full(2:2)
	 ext2=ext2full(2:2)
         read(17,*) nwbeg
         read(17,*) nwend	
         read(17,*) dt_beg
         read(17,*) dt_end
         read(17,*) dtlag_max
         read(17,*) dfast_max
         read(17,*) t_off_beg
         read(17,*) t_off_end
         read(17,*) tlag_scale
         read(17,*) fast_scale
         read(17,*) max_no_clusters
         read(17,*) nmin
         read(17,*) OPT_verbose
         read(17,*) OPT_outfiles
	   close(17)
	else
c 	** otherwise some starting values which are sensible for **
c	** local data **
c	** copycatted from MKS' and RAJA's files... **
	   print*,'using defaults:'
	   ext1		='e'
	   ext2		='n'
	   nwbeg		=4
	   nwend		=15
	   dt_beg	=0.016
	   dt_end	=0.008
	   dtlag_max	=0.8
	   dfast_max	=45.
	   t_off_beg	=0.02
	   t_off_end	=0.04
	   tlag_scale	=0.8
	   fast_scale	=180.
	   max_no_clusters	=20
	   nmin		=3
	   OPT_verbose ='.true.'
	   OPT_outfiles='.true.'
	endif

c  ** update values from user input **
1000	continue
	print*,'ENTER FIELD LABEL TO CHANGE: l=list values: q=quit+save'
	print*,' enter field, then return, then new value'
1	continue
	   read (*,'(a)'),field
	   if ((field.eq.'exit').or.(field.eq.'q')) then
	      goto 2
	   else if (field.eq.'l') then
            print*,' ext1             ',ext1
            print*,' ext2             ',ext2
            print*,' nwbeg            ',nwbeg
            print*,' nwend            ',nwend    
            print*,' dt_beg          ',dt_beg
            print*,' dt_end          ',dt_end
            print*,' dtlag_max       ',dtlag_max
            print*,' dfast_max       ',dfast_max
            print*,' t_off_beg       ',t_off_beg
            print*,' t_off_end       ',t_off_end
            print*,' tlag_scale      ',tlag_scale
            print*,'*fast_scale      ',fast_scale
            print*,'*max_no_clusters  ',max_no_clusters
            print*,' nmin             ',nmin
            print*,'*OPT_verbose      ',OPT_verbose
            print*,'*OPT_outfiles     ',OPT_outfiles
		print*,'(* = value fixed)'
	   else if (field.eq.'ext1') then
		read*,ext1
	   else if (field.eq.'ext2') then
		read*,ext2
	   else if (field.eq.'nwbeg') then
		read*,nwbeg
	   else if (field.eq.'nwend    ') then
		read*,nwend
	   else if (field.eq.'dt_beg') then
		read*,dt_beg
	   else if (field.eq.'dt_end') then
		read*,dt_end
	   else if (field.eq.'dtlag_max') then
		read*,dtlag_max
	   else if (field.eq.'dfast_max') then
		read*,dfast_max
	   else if (field.eq.'t_off_beg') then
		read*,t_off_beg
	   else if (field.eq.'t_off_end') then
		read*,t_off_end
	   else if (field.eq.'tlag_scale') then
		read*,tlag_scale
c	   else if (field.eq.'fast_scale') then
c		read*fast_scale
c	   else if (field.eq.'max_no_clusters') then
c		read*,max_no_clusters
	   else if (field.eq.'nmin') then
		read*,nmin
c	   else if (field.eq.'OPT_verbose') then
c		read*,OPT_verbose
c	   else if (field.eq.'OPT_outfiles') then
c		read*,OPT_outfiles
	   else if (field.eq.'wind') then
	   	print*,'min window'
		read*,tbeg1,tend0
		print*,'max window'
		read*,tbeg0,tend1
		dt_beg = (tbeg0-tbeg1)/real(nwbeg)
		dt_end = (tend1-tend0)/real(nwend)
		t_off_beg = tbeg1
		t_off_end = tend0
	   else 
	      print*,'UNSUPPORTED OPTION - default used'
	   endif
	goto 1
2	continue
c  ** check nmin is sensible **
	n=nwbeg*nwend
	if (nmin.gt.n/max_no_clusters) then
	   print*,'nmin = ',nmin
	   print*,'recommended nmin = ',n/max_no_clusters,' or less'
	   print *,' nwbeg, nwend are ',nwbeg,nwend
	   print *,' n, max_no_clusters ',n,max_no_clusters
	   print*,'REDEFINE? (y/n)'
	   read*,ans
	   if (ans.ne.'n') goto 1000
	endif
		
c  ** write 'ass.ini' file **
	open(17,file=inifile,status='unknown')
	write(17,1001) ext1
	write(17,1002) ext2
	write(17,1003) nwbeg
	write(17,1004) nwend	
	write(17,1005) dt_beg
	write(17,1006) dt_end
	write(17,1007) dtlag_max
	write(17,1008) dfast_max
	write(17,1009) t_off_beg
	write(17,1010) t_off_end
	write(17,1011) tlag_scale
	write(17,1012) fast_scale
	write(17,1013) max_no_clusters
	write(17,1014) nmin
	write(17,1015) OPT_verbose
	write(17,1016) OPT_outfiles
	close(17)
c1001	format('\'.',a1,'\'             ext1')
1001	format(".",a1,"             ext1")
c1002	format('\'.',a1,'\'             ext2')
1002	format('.',a1,'             ext2')
1003	format(i15,2x,'nwbeg')
1004	format(i15,2x,'nwend')
1005	format(e15.5,2x,'dt_beg')
1006	format(e15.5,2x,'dt_end')
1007	format(e15.5,2x,'dtlag_max')
1008	format(e15.5,2x,'dfast_max')
1009	format(e15.5,2x,'t_off_beg')
1010	format(e15.5,2x,'t_off_end')
1011	format(e15.5,2x,'tlag_scale')
1012	format(e15.5,2x,'fast_scale')
1013	format(i15,2x,'max_no_clusters')
1014	format(i15,2x,'nmin')
1015	format(a15,2x,'OPT_verbose')
1016	format(a50,2x,'OPT_outfile')

c  ** print a brief output message **
	print*,'----------------------------------------------------'
	print*,'------------',inifile
	print*,'TOTAL no. windows = ',n
	print*,'Using components: ',ext1,' and ',ext2
	print*,'window start: ',nwbeg, ' windows. spaced at ',dt_beg,
     >	's offset ',t_off_beg,'s before S-pick'
	print*,'window end  : ',nwend, ' windows. spaced at ',dt_end,
     >	's offset ',t_off_end,'s after S-pick'
	print*,'------------'
	print*,'min no. points for a cluster to be acceptable: ',nmin
	print*,'max err for inclusion in cluster: ',dtlag_max,' s : ',
     >	dfast_max,' degrees'
	print*,'----------------------------------------------------'
	end




	
