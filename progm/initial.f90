MODULE init
	USE mvariables
	IMPLICIT NONE

	CONTAINS

	!SUBROUTINE init_from_file
	SUBROUTINE init_from_lfile(ind)
		INTEGER::i,j,k, ii,jj,kk, ind
		REAL(8)::angle

		OPEN(UNIT=unit3, file=folder_main_pc//'configs/field_configuration_0.400000_L_8.dat')
		
		DO i=1,(ind-1)*L**3
			READ(unit3,*)
		ENDDO
		!READ(unit3,*)
		S=0
		DO i=1,L; DO j=1,L; DO k=1,L
			READ(unit3,*) ii, jj, kk, S(i,j,k,1), S(i,j,k,2)!, angle
			!WRITE(*,*) i,j,k, S(i,j,k,1), S(i,j,k,2)
		ENDDO; ENDDO; ENDDO

		CLOSE(unit3)

		CALL init_var

	END SUBROUTINE init_from_lfile
	!END SUBROUTINE init_from_file

	SUBROUTINE init_from_file
		INTEGER::i,j,k, ii,jj,kk
		REAL(8)::angle

		OPEN(UNIT=unit3, file=folder_main_pc//'confgs/field_configuration_0.400000_L_8.dat')
		
		READ(unit3,*)
		S=0
		DO i=1,L; DO j=1,L; DO k=1,L
			READ(unit3,*) ii, jj, kk, S(i,j,k,1), S(i,j,k,2)!, angle
			!WRITE(*,*) i,j,k, S(i,j,k,1), S(i,j,k,2)
		ENDDO; ENDDO; ENDDO

		CLOSE(unit3)

		CALL init_var

	END SUBROUTINE init_from_file

	SUBROUTINE hot_start()
		REAL(8)::x, theta
		INTEGER::i,j,k

		CALL init_random_seed

		S=0.D0
		DO i=1,L; DO j=1,L; DO k=1,L
			CALL random_number(x)
			theta = 2.0*PI*(x-0.5)

			S(i,j,k,1) = DCOS(theta)
			S(i,j,k,2) = DSIN(theta)

			!WRITE(*,'(3I3,3F25.16)') i,j,k, S(i,j,k,:), DOT_PRODUCT(S(i,j,k,:),S(i,j,k,:))
		ENDDO; ENDDO; ENDDO
		!WRITE(*,*)

		CALL init_var

	END SUBROUTINE hot_start

	SUBROUTINE warm_start()
		REAL(8)::alpha, theta, de, r
		INTEGER::i,j,k

		CALL init_random_seed
		CALL random_number(r)

		alpha = 2.D0*PI*(r-0.5D0)

		S=0
		DO i=1,L; DO j=1,L; DO k=1,L

			CALL RANDOM_NUMBER(de)

			theta = alpha + delta*(de - 0.5)

			S(i,j,k,1) = DCOS(theta)
			S(i,j,k,2) = DSIN(theta)

			!WRITE(*,'(3I3,1X,4F20.16)') i,j,k, S(i,j,k,:), theta, DOT_PRODUCT(S(i,j,k,:),S(i,j,k,:))
		ENDDO; ENDDO; ENDDO

		!WRITE(*,*) alpha, alpha+delta*0.5, (alpha-delta*0.5)

		CALL init_var

	END SUBROUTINE warm_start

	SUBROUTINE init_var()
		INTEGER :: i, i2
		Dphi12=0d0;	Dphi23=0d0;	Dphi34=0d0;	Dphi41=0d0
		Dphi21=0d0;	Dphi15=0d0;	Dphi56=0d0;	Dphi62=0d0
		Dphi14=0d0;	Dphi47=0d0;	Dphi75=0d0;	Dphi51=0d0

		vortex1=0d0; vortex2=0d0; vortex3=0d0

		sum_vortex=0d0;	num_of_vort = 0; num_of_av = 0

		E=0.d0;	Mag=0.d0; Corr=0.0d0; O=0.0d0; F=0.D0

	END SUBROUTINE init_var

	SUBROUTINE init_arrays
		INTEGER :: i,j,k, i2
		do i = 1,L; do i2 = 1,L
			cos_arr(i, i2) = dcos(2*pi*(i-i2)/dble(L))
		enddo; enddo

		!DO i=1,L; DO j=1,L; DO k=1,L
		!	vertxs(i,j,k)=(i-1)*L2 + (j-1)*L + k
		!ENDDO; ENDDO; ENDDO 	
	END SUBROUTINE init_arrays


	!Condiciones de frontera periodicas
	!Autor: José Antonio
	!Copiado: 20-ene-22
	SUBROUTINE cfp()
		integer :: i
		
	    do i=1,L
		  ip(i) = i+1
		  im(i) = i-1
		end do
		ip(L) = 1
		im(1) = L

	END SUBROUTINE cfp

	!Programa que llama a la semilla
	!Autor: José Antonio
	!Copiado el: 20-ene-22
	SUBROUTINE init_random_seed()
	  implicit none
	  integer :: i, n, clock
	  integer, dimension(:), allocatable :: seed

	  call random_seed(size = n)
	  allocate(seed(n))

	  call system_clock(count=clock)

	  seed = clock + 1000000*(/ (i - 1, i = 1, n) /)
	  !write (*, *) seed
	  call random_seed(put = seed)

	  deallocate(seed)
	  
	END SUBROUTINE init_random_seed

END MODULE init