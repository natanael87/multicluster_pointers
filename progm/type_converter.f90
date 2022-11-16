MODULE converter
	USE init
	IMPLICIT NONE

	CONTAINS

! Convierte enteros a caracter 
	character(len=20) function str(k)
		implicit none
	    !"Convert an integer to string."
	    integer, intent(in) :: k
	    write (str, *) k
	    str = adjustl(str)
	end function str

! Convierte reales a caracter
	character(6) function real2str(real_)
	implicit none
	real, intent(in) :: real_
	character(6) :: string_	
		write(string_,"(f6.4)") real_
		real2str = string_
	end function real2str

! Convierte double a caracter
	character(8) function dble2str(real_)
	implicit none
	real(8), intent(in) :: real_
	character(8) :: string_	
		write(string_,"(f8.6)") real_
		dble2str = string_
	end function dble2str

END MODULE