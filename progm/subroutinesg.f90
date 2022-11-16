MODULE functions
	USE global
	IMPLICIT NONE
	
	REAL(4), DIMENSION(:), ALLOCATABLE :: betas, deltas
	REAL(4) ::beta, delta

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
	character(8) function real2str(real_)
	implicit none
	real, intent(in) :: real_
	character(8) :: string_	
		write(string_,"(f8.6)") real_
		real2str = string_
	end function real2str

! Subrutina para arrays dinámicos con elementos reales(4)
	SUBROUTINE AddToList(list, element)
	integer :: i, isize
	REAL(4), intent(in) :: element
	REAL(4), dimension(:), allocatable, intent(inout) :: list
	REAL(4), dimension(:), allocatable :: clist

	if(allocated(list)) then
	  isize = size(list)
	  allocate(clist(isize+1))
	  do i=1,isize          
	  clist(i) = list(i)
	  end do
	  clist(isize+1) = element

	  deallocate(list)
	  call move_alloc(clist, list)

	else
	  allocate(list(1))
	  list(1) = element
	end if

	END SUBROUTINE AddToList

! Lee el archivo de los valores de beta o delta
	SUBROUTINE read_bd()
	USE global
	IMPLICIT NONE
	
	INTEGER :: i, stat
	REAL(4) :: buf

	IF (the_action == standard_action) THEN
		OPEN(UNIT=unit1, FILE=folder_main_pc//'betas/betas.dat', STATUS='old')
		DO
			read(unit1, *, iostat=stat) buf
			if (stat /= 0) exit
			call AddToList(betas, buf)
			! process buf
		ENDDO
		CLOSE(unit1)
	END IF

	IF (the_action == delta_constraint) THEN
		OPEN(UNIT=unit2, FILE=folder_main_pc//'deltas/deltas.dat', STATUS='old')
		DO
			read(unit2, *, iostat=stat) buf
			if (stat /= 0) exit
			call AddToList(deltas, buf)
			! process buf
		ENDDO
		CLOSE(unit2)
	END IF

	END SUBROUTINE read_bd

	SUBROUTINE write_bd()
		USE global
		IMPLICIT NONE

		INTEGER :: i,j,d, k, ll

		!WRITE(*,*) SIZE(betas)

		!WRITE(*,*) d

		IF (the_action == standard_action) THEN
			d = SIZE(betas)/nbd
			DO i=1,d
				k = i*nbd-nbd+1
				ll = i*nbd
				!WRITE(*,*) k, ll
				OPEN(UNIT=unit3, file=folder_main_pc//'betas/b'//trim(real2str(betas(k)))//'and'//trim(str(nbd-1))//'.dat')
				DO j=k,ll
					!WRITE(*,*) j
					WRITE(unit3,'(F8.6)') betas(j)
				ENDDO
				CLOSE(unit3)
			ENDDO 
		END IF

		IF (the_action == delta_constraint) THEN
			d = SIZE(deltas)/nbd
			DO i=1,d
				k = i*nbd-nbd+1
				ll = i*nbd
				OPEN(UNIT=unit3, file=folder_main_pc//'deltas/d'//trim(real2str(deltas(k)))//'and'//trim(str(nbd-1))//'.dat')
				DO j=k,ll
					WRITE(unit3,'(F8.6)') deltas(j)
				ENDDO
				CLOSE(unit3)
			ENDDO 
		END IF

	END SUBROUTINE write_bd

	SUBROUTINE write_compile()
		USE global
		IMPLICIT NONE

		INTEGER :: i,j,d, k,stat
		CHARACTER(100) :: path_compile
		
		51 FORMAT('L = ', I3)
		WRITE(*,51) L
		IF (the_action == standard_action) THEN
			WRITE(*,*) betas
			d = SIZE(betas)/nbd
			DO i=1,d
				k = i*nbd-nbd+1
				OPEN(UNIT=unit5, file=folder_prog//'b'//trim(real2str(betas(k)))//'.f90')
				WRITE(unit5,*) "MODULE mparameters2"
				WRITE(unit5,*) "USE mparameters"
				WRITE(unit5,*) "IMPLICIT NONE"
				WRITE(unit5,*) "INTEGER, PARAMETER :: L="//trim(str(L))
				WRITE(unit5,*) "INTEGER, PARAMETER :: L2="//trim(str(L))//"**2"
				WRITE(unit5,*) "INTEGER, PARAMETER :: L3="//trim(str(L))//"**3"
				WRITE(unit5,*) "CHARACTER(*), PARAMETER :: folder_beta = &
					folder_main_pc//'betas/b"//trim(real2str(betas(k)))//"and"//trim(str(nbd-1))//".dat'"
				WRITE(unit5,*) "CHARACTER(*), PARAMETER :: folder_delta = folder_main_pc//'deltas/deltas.dat'"
				WRITE(unit5,*)"CHARACTER(*), PARAMETER :: the_action = "//standard_action
				WRITE(unit5,*) "SAVE"
				WRITE(unit5,*) "END MODULE mparameters2"
				CLOSE(unit5)


				path_compile = folder_main_pc//'comp/compile_b'//trim(real2str(betas(k)))//'and'//trim(str(nbd-1))//'.sh'
				OPEN(UNIT=unit4, IOSTAT=stat, file=path_compile)				
				WRITE(unit4,'("rm *.o *.mod")')
				!WRITE(unit4,*) "gfortran -c "//folder_prog//"seed.f90"
				WRITE(unit4,*) "gfortran -c "//folder_prog//"parameters.f90"
				WRITE(unit4,*)"gfortran -c "//folder_prog//"b"//trim(real2str(betas(k)))//".f90"
				WRITE(unit4,*)"gfortran -c "//folder_prog//"variables.f90"
				WRITE(unit4,*)"gfortran -c "//folder_prog//"initial.f90"
				WRITE(unit4,*)"gfortran -c "//folder_prog//"type_converter.f90"
				WRITE(unit4,*)"gfortran -c "//folder_prog//"aux_functions_and.f90"
				WRITE(unit4,*)"gfortran -c "//folder_prog//"wolff.f90"
				WRITE(unit4,*)"gfortran -c "//folder_prog//"take_meas.f90"
				WRITE(unit4,*)"gfortran -c "//folder_prog//"XYO2.f90"
				WRITE(unit4,*)"gfortran *.o -L$HOME/libf77/$ARCH -llapack -lblas"
				WRITE(unit4,*)"mv "//folder_main_pc//"progm/b"//trim(real2str(betas(k)))//".f90 "&
				//folder_main_pc//"comp/b"//trim(real2str(betas(k)))//".f90"
				WRITE(unit4,*)"mv a.out "//folder_prog//"3dL"//trim(str(L))//"_b"//trim(real2str(betas(k)))//".exe"
				WRITE(unit4,*)"rm *.o *.mod"
				CLOSE(unit4)
			ENDDO
		END IF 

		IF (the_action == delta_constraint) THEN
			WRITE(*,*) deltas
			d = SIZE(deltas)/nbd
			DO i=1,d
				k = i*nbd-nbd+1
				OPEN(UNIT=unit5, file=folder_prog//'d'//trim(real2str(deltas(k)))//'.f90')
				WRITE(unit5,*) "MODULE mparameters2"
				WRITE(unit5,*) "USE mparameters"
				WRITE(unit5,*) "IMPLICIT NONE"
				WRITE(unit5,*) "INTEGER, PARAMETER :: L="//trim(str(L))
				WRITE(unit5,*) "INTEGER, PARAMETER :: L2="//trim(str(L))//"**2"
				WRITE(unit5,*) "INTEGER, PARAMETER :: L3="//trim(str(L))//"**3"
				WRITE(unit5,*) "CHARACTER(*), PARAMETER :: folder_delta = &
					folder_main_pc//'deltas/d"//trim(real2str(deltas(k)))//"and"//trim(str(nbd-1))//".dat'"
				WRITE(unit5,*) "CHARACTER(*), PARAMETER :: folder_beta = folder_main_pc//'betas/betas.dat'"
				WRITE(unit5,*)"CHARACTER(*), PARAMETER :: the_action = delta_constraint"
				WRITE(unit5,*) "SAVE"
				WRITE(unit5,*) "END MODULE mparameters2"
				CLOSE(unit5)

				
				path_compile = folder_main_pc//'comp/compile_d'//trim(real2str(deltas(k)))//'and'//trim(str(nbd-1))//'.sh'
				OPEN(UNIT=unit4, file=path_compile)
				WRITE(unit4,'("rm *.o *.mod")')
				!WRITE(unit4,*)"gfortran -c "//folder_prog//"seed.f90"
				WRITE(unit4,*)"gfortran -c "//folder_prog//"parameters.f90"
				WRITE(unit4,*)"gfortran -c "//folder_prog//"d"//trim(real2str(deltas(k)))//".f90"
				WRITE(unit4,*)"gfortran -c "//folder_prog//"variables.f90"
				WRITE(unit4,*)"gfortran -c "//folder_prog//"initial.f90"
				WRITE(unit4,*)"gfortran -c "//folder_prog//"type_converter.f90"
				WRITE(unit4,*)"gfortran -c "//folder_prog//"aux_functions_and.f90"
				WRITE(unit4,*)"gfortran -c "//folder_prog//"wolff.f90"
				WRITE(unit4,*)"gfortran -c "//folder_prog//"take_meas.f90"
				WRITE(unit4,*)"gfortran -c "//folder_prog//"XYO2.f90"
				WRITE(unit4,*)"gfortran *.o -L$HOME/libf77/$ARCH -llapack -lblas"
				WRITE(unit4,*)"mv a.out "//folder_prog//"3dL"//trim(str(L))//"_d"//trim(real2str(deltas(k)))//".exe"
				WRITE(unit4,*)"rm *.o *.mod"	
				CLOSE(unit4)
			ENDDO
		END IF
		
	END SUBROUTINE write_compile

	SUBROUTINE compile_bd()
		USE global
		IMPLICIT NONE

		INTEGER :: i,d,k
		CHARACTER(100) :: path_compile

		IF (the_action == standard_action) THEN			
			d=SIZE(betas)/nbd
			DO i=1,d
				k = i*nbd-nbd+1
				path_compile = "bash "//folder_main_pc//"comp/compile_b"//trim(real2str(betas(k)))//"and"//trim(str(nbd-1))//".sh"
				CALL SYSTEM(path_compile)
			ENDDO
		END IF

		IF (the_action == delta_constraint) THEN
			d = SIZE(deltas)/nbd
			DO i=1,d
				k = i*nbd-nbd+1
				path_compile = "bash "//folder_main_pc//"comp/compile_d"//trim(real2str(deltas(k)))//"and"//trim(str(nbd-1))//".sh"
				CALL SYSTEM(path_compile)
			ENDDO
		END IF

	END SUBROUTINE compile_bd

	SUBROUTINE write_sub()
		USE global
		IMPLICIT NONE

		INTEGER :: i,d,k
		CHARACTER(100) :: path_sub

		!WRITE(*,*) folder_main_pc

		IF (the_action == standard_action) THEN
			d = SIZE(betas)/nbd
			DO i=1,d
				k = i*nbd-nbd+1
				path_sub = folder_main_pc//'comp/L'//trim(str(L))//'b'//trim(real2str(betas(k)))//'.sub'
				OPEN(UNIT=unit6, FILE=path_sub)
				WRITE(unit6,*) "#PBS -S /bin/sh"
				WRITE(unit6,*) "#PBS -N L"//trim(str(L))//"b"//trim(real2str(betas(k)))//".run"
				WRITE(unit6,*) "#PBS -q QuantPhysMC"
				WRITE(unit6,*) "#echo $SGE_TASK_ID"
				WRITE(unit6,*) "cd "//folder_prog
				WRITE(unit6,*) "./3dL"//trim(str(L))//"_b"//trim(real2str(betas(k)))//".exe"
				CLOSE(unit6)
			ENDDO
		END IF

		IF (the_action == delta_constraint) THEN
			d = SIZE(deltas)/nbd
			DO i=1,d
				k = i*nbd-nbd+1
				path_sub = folder_main_pc//'comp/L'//trim(str(L))//'d'//trim(real2str(deltas(k)))//'.sub'
				OPEN(UNIT=unit6, FILE=path_sub)
				WRITE(unit6,*) "#PBS -S /bin/sh"
				WRITE(unit6,*) "#PBS -N L"//trim(str(L))//"d"//trim(real2str(deltas(k)))//".run"
				WRITE(unit6,*) "#PBS -q QuantPhysMC"
				WRITE(unit6,*) "#echo $SGE_TASK_ID"
				WRITE(unit6,*) "cd "//folder_prog
				WRITE(unit6,*) "./3dL"//trim(str(L))//"_d"//trim(real2str(deltas(k)))//".exe"
				CLOSE(unit6)
			ENDDO
		END IF

	END SUBROUTINE write_sub

END MODULE functions
