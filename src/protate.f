	program protate
	implicit none
	double precision phi,theta,sd_phi,sd_theta,ftol
	character*50 infile_e,infile_n,infile_z
	character*50 outfile_a,outfile_b,outfile_c
	
c  ** set required tolernace of variance estimate on phi and theta **
	ftol=0.05

	read(*,'(a)') infile_e
	read(*,'(a)') infile_n
	read(*,'(a)') infile_z
	read(*,'(a)') outfile_a
	read(*,'(a)') outfile_b
	read(*,'(a)') outfile_c
	
	call zprotate(infile_e,infile_n,infile_z,ftol,
     >	outfile_a,outfile_b,outfile_c,phi,theta,sd_phi,sd_theta)
	
	print*,'phi   (angle clockwise from n) =',real(phi),
     >	' +/- ',real(sd_phi)
	print*,'theta (angle from z)           =',real(theta),
     >	' +/- ',real(sd_theta)

	end
	
