	function fstrcat(str1,str2)
c	concatenate two strings, str1 and str2
	implicit none
	character*50 str1,str2,fstrcat
	integer len1,len2

      len1=index(str1,' ')-1
      len2=index(str2,' ')-1
	if (len1+len2.gt.50) then
	   print*,'string1 = ',str1
	   print*,'string2 = ',str2
	   pause 'ERROR: fstrcat: combined string length exceeds 50'
	else
         fstrcat=str1(1:len1)//str2(1:len2)
	endif
	return
	end
