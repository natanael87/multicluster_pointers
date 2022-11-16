!Autor: Elías Polanco
!Configuraciones iniciales de los spines en S^1
!Versión original: 20-ene-22
PROGRAM XYO2
	USE wolff_step
	USE converter
	IMPLICIT NONE

	INTEGER :: i, j, jj, k, ll, d, stat
	CHARACTER(100)::data_path, data_path2
	REAL(4) :: time_begin, time_end, ti_init, tf_init, ti_therm, ti_meas, tf_therm, tf_meas
	REAL(4) :: secs, mins, hours

	CALL CPU_TIME(time_begin)

	CALL cfp

	ALLOCATE(madj(L**3,6), vmadj(L**3,8), avmadj(L**3,8))

	CALL read_bd

	CALL prompt2file

	CALL init_arrays

	IF (the_action == standard_action) THEN
		DO i=1,SIZE(betas)
			beta=betas(i)
			CALL CPU_TIME(ti_init)	
			CALL hot_start
			!CALL init_from_file
			CALL CPU_TIME(tf_init)	

			WRITE(*,"('beta  ', F8.6)")beta

			CALL CPU_TIME(ti_therm)
			DO j=1,Ntherm
				CALL mlt_cls_dinamics
			ENDDO
			CALL CPU_TIME(tf_therm)

			data_path=folder_data//'L'//trim(str(L))//'b'//trim(dble2str(betas(i)))//'.dat'
			OPEN(UNIT=unit1, IOSTAT=stat, FILE=data_path, STATUS='old')
			IF (STAT == 0) CLOSE(unit1, STATUS='delete')
			
			OPEN(UNIT=unit1, file=data_path, STATUS='unknown',POSITION="append", action = "write")
			WRITE(unit1,'(4A25,8X,4A8,1X,A26)') "E","Mag(2)"," ","F","<N_c>","N_vx","N_vy","N_vz","Corr(L)"

			data_path2=folder_data//'L'//trim(str(L))//'b'//trim(dble2str(betas(i)))//'cs.dat'
			OPEN(UNIT=unit5, IOSTAT=stat, FILE=data_path2, STATUS='old')
			IF (STAT == 0) CLOSE(unit5, STATUS='delete')
			
			OPEN(UNIT=unit5, file=data_path2, STATUS='unknown',POSITION="append", action = "write")
			

			CALL CPU_TIME(ti_meas)
			DO j=1,Nmeas*Nskip
				CALL mlt_cls_dinamics
				IF (MOD(j,Nskip)==0) THEN
					CALL take_meas
					!CALL find_strings
				ENDIF
			ENDDO
			CALL CPU_TIME(tf_meas)


			WRITE(unit1,'(4A25,1X,3A6,1X,A26)') "E","Mag(2)"," ","F","<N_c>","N_v","N_av","Corr(L)"

			15 FORMAT('# Initial confg: ',F16.8,'secs,',' Therma: ',F16.8,'secs, ', 'Take meas: ',F16.8,'secs.')
			WRITE(unit1,15) tf_init-ti_init, tf_therm-ti_therm, tf_meas-ti_meas

		ENDDO 
		
		CLOSE(unit5)
	ENDIF

	!time=time_end - time_begin
	!secs=INT(time); rsecs=time-secs


	IF (the_action == delta_constraint) THEN
		
		DO i=1,SIZE(deltas)	
			delta=deltas(i)
			cosd=COS(delta)
			CALL CPU_TIME(ti_init)	
			CALL warm_start
			CALL CPU_TIME(tf_init)

			WRITE(*,"('delta  ', F8.6)") delta
		
			CALL CPU_TIME(ti_therm)
			DO j=1,Ntherm
				CALL constraint_algorithm
			ENDDO
			CALL CPU_TIME(tf_therm)

			data_path=folder_data//'L'//trim(str(L))//'d'//trim(dble2str(deltas(i)))//'.dat'
			OPEN(UNIT=unit1, IOSTAT=stat, FILE=data_path, STATUS='old')
			IF (STAT == 0) CLOSE(unit1, STATUS='delete')

			OPEN(UNIT=unit1, file=data_path, STATUS='unknown',POSITION="append", action = "write")
			WRITE(unit1,'(4A25,8X,4A8,1X,A26)') "E","Mag(2)"," ","F","<N_c>","N_vx","N_vy","N_vz","Corr(L)"

			data_path2=folder_data//'L'//trim(str(L))//'b'//trim(dble2str(betas(i)))//'cs.dat'
			OPEN(UNIT=unit5, IOSTAT=stat, FILE=data_path2, STATUS='old')
			IF (STAT == 0) CLOSE(unit5, STATUS='delete')
			
			OPEN(UNIT=unit5, file=data_path2, STATUS='unknown',POSITION="append", action = "write")
			
			CALL CPU_TIME(ti_meas)
			DO j=1,Nmeas*Nskip
				CALL constraint_algorithm
				IF (MOD(j,Nskip)==0) THEN
					CALL take_meas
					!CALL find_strings
				ENDIF
			ENDDO
			CALL CPU_TIME(tf_meas)
			
			WRITE(unit1,'(3A25,1X,3A6,1X,A26)') "E", "Mag(2)", "", "<N_c>", "N_v", "N_av", "Corr(L)"

			17 FORMAT('# Initial confg: ',F16.8,'secs,',' Therma: ',F16.8,'secs, ', 'Take meas: ',F16.8,'secs.')
			WRITE(unit1,17) tf_init-ti_init, tf_therm-ti_therm, tf_meas-ti_meas

		ENDDO

		CLOSE(unit5)

	ENDIF

	IF (the_action == test_standard) THEN
		CALL test_b

		WRITE(unit1,*) '# Computing time: ', time_end - time_begin
		CLOSE(unit1)
	ENDIF 

	IF (the_action == test_constraint) THEN
		CALL test_d

		WRITE(unit1,*) '# Computing time: ', time_end - time_begin
		CLOSE(unit1)
	ENDIF

	IF (the_action == test_single) THEN
		CALL test_s

		WRITE(unit1,*) '# Computing time: ', time_end - time_begin
		CLOSE(unit1)
	ENDIF

	DEALLOCATE(madj, vmadj, avmadj)

	CALL CPU_TIME(time_end)

	secs = time_end - time_begin
	
	hours = secs/(3600.0) 
	mins  = hours - floor(hours); mins  = mins*60
	secs  = mins  - floor(mins);  secs  = secs*60

	16 FORMAT('#Computing time: ',I6,' hours  ',I2,' minutes ',F8.4,' seconds.')
	WRITE(unit1,16) FLOOR(hours), FLOOR(mins), secs
	WRITE(unit1,*) '#time in seconds', time_end - time_begin
	CLOSE(unit1)

	
END PROGRAM XYO2

SUBROUTINE prompt2file()
	USE converter
	USE my_functions
	IMPLICIT NONE

	IF (the_action == standard_action) THEN
		OPEN(UNIT=unit2, file=folder//'confgs/config_L'//trim(str(L))//'b'//trim(dble2str(betas(1)))//'.dat')
		WRITE(unit2,*) "standard action"
		WRITE(unit2,*) "L =", L
		WRITE(unit2,'(A12, 1X,'//trim(str(SIZE(betas)))//'F12.6)') "betas", betas
		CLOSE(unit2)
	ENDIF

	IF (the_action == delta_constraint) THEN
		OPEN(UNIT=unit2, file=folder//'confgs/config_L'//trim(str(L))//'d'//trim(dble2str(deltas(1)))//'.dat')
		WRITE(unit2,*) "constraint action"
		WRITE(unit2,*) "L =", L
		WRITE(unit2,'(A12, 1X,'//trim(str(SIZE(deltas)))//'F12.6)') "deltas", deltas
		CLOSE(unit2)
	ENDIF

END SUBROUTINE prompt2file

SUBROUTINE test_s()
	USE wolff_step
	USE converter
	IMPLICIT NONE

	INTEGER :: i,j,k, ii, stat
	CHARACTER(100)::data_path, data_path1, data_path2

	beta=0.5D0
	delta=2.5D0
	WRITE(*,*) 'beta', beta, 'delta', delta

!	CALL hot_start
	CALL warm_start

	data_path=folder_data//'L'//trim(str(L))//'d'//trim(dble2str(delta))//'.dat'
	OPEN(UNIT=unit1, IOSTAT=stat, FILE=data_path, STATUS='old')
	IF (STAT == 0) CLOSE(unit1, STATUS='delete')
	OPEN(UNIT=unit1, file=data_path, STATUS='unknown',POSITION="append", action = "write")
	WRITE(unit1,'(3A25,1X,3A6,1X,A26)') "E", "Mag(2)", "", "<N_c>", "N_v", "N_av", "Corr(L)"

	data_path2=folder//'confgs/wolff_L'//trim(str(L))//'_d'//trim(dble2str(delta))//'.dat'
	OPEN(UNIT=unit4, IOSTAT=stat, FILE=data_path2, STATUS='old')
			IF (STAT == 0) CLOSE(unit4, STATUS='delete')
	OPEN(UNIT=unit4, file=data_path2, STATUS='unknown',POSITION="append", action = "write")

	data_path1=folder//'confgs/L'//trim(str(L))//'_d'//trim(dble2str(delta))//'.dat'
	OPEN(UNIT=unit5, IOSTAT=stat, FILE=data_path1, STATUS='old')
			IF (STAT == 0) CLOSE(unit5, STATUS='delete')
	OPEN(UNIT=unit5, file=data_path1, STATUS='unknown',POSITION="append", action = "write")
	DO i=1,L; DO j=1,L; DO k=1,L
		WRITE(unit5,*) S(i,j,k,:)
	ENDDO; ENDDO; ENDDO
	
	DO ii=1,3
	!	CALL single_cluster
		!CALL dsingle_cluster
		CALL take_meas
		OPEN(UNIT=unit5, file=data_path1,STATUS='unknown',POSITION="append", action = "write")
		DO i=1,L; DO j=1,L; DO k=1,L
			WRITE(unit5,*) S(i,j,k,:)
		ENDDO; ENDDO; ENDDO
	ENDDO
	CLOSE(unit5)
	CLOSE(unit4)
	CLOSE(unit1)

END SUBROUTINE test_s

SUBROUTINE test_b()
	USE wolff_step
	USE converter
	IMPLICIT NONE

	INTEGER :: i,j,k


	WRITE(*,*) "L=", L

	!CALL hot_start
	!CALL vertices_array
	beta=0.5D0
	! Toma mediciones de las configuraciones del archivo indicado
	!OPEN(UNIT=unit1, file=folder_data//'L'//trim(str(L))//'b'//trim(dble2str(beta))//'.dat',STATUS='unknown')
	!WRITE(*,*) folder_data//trim(str(L))//'b'//trim(dble2str(beta))//'_meas.dat'
	!WRITE(unit1,'(3A25,1X,3A6,A25)') "E", "Mag(2)", "", "<N_c>", "N_v", "N_av", "Corr(L)"
	!DO i=1,10000
	!	CALL init_from_file(i)
	!	CALL take_meas
	!ENDDO
	
	WRITE(*,"(A8,1X, F8.6)") "beta", beta
!
!			!DO j=1,Ntherm
!			!	CALL mlt_cls_dinamics
!			!ENDDO

	! Mide Vórtices
	CALL init_from_file
	OPEN(UNIT=unit1, file=folder_data//trim(str(L))//'_vortex_confg.dat',  STATUS='unknown')
	WRITE(unit1,*) "i, j, k, plaq1(orthogonal to z dir.) plaq2(orthogonal to x dir.) plaq3(orthogonal to y dir.)"
	CALL take_meas
	CLOSE(unit1)

	! Usa alguna acción de lattice
	!OPEN(UNIT=unit1, file=folder_data//trim(str(L))//'_test.dat', STATUS='unknown',POSITION="append", action = "write")
	!WRITE(unit1,'(4A25,8X,4A8,1X,A26)') "E","Mag(2)"," ","F","<N_c>","N_vx","N_vy","N_vz","Corr(L)"
	!DO j=1,2
	!	CALL mlt_cls_dinamics
	!	CALL take_meas
	!ENDDO

	!DO i=1,L; DO j=1,L; DO k=1,L
	!	WRITE(*,"(3F30.16)") S
	!ENDDO; ENDDO; ENDDO

END SUBROUTINE test_b 

SUBROUTINE test_d()
	USE converter
	USE wolff_step
	IMPLICIT NONE

	INTEGER :: j

	! --------------DELTA CONSTRAINT--------------
		!CALL read_delta(folder_delta//'deltas/deltas.dat')
		!CALL prompt_constraint

		!WRITE(*,*) "L=", L
		!WRITE(*,'(A12, 1X,'//trim(str(SIZE(deltas)))//'F10.2)') "deltas", deltas

		!delta=deltas(1)
		!cosd=COS(delta)
		!WRITE(*,*) delta
		!CALL warm_start
		!!CALL init_from_file

		!!CALL take_meas

		!!DO j=1,Ntherm
		!!	!CALL constraint_algorithm
		!!	CALL mlt_cls_dinamics
		!!	!CALL take_meas
		!!ENDDO

		!OPEN(UNIT=unit1, file=folder_data//trim(str(L))//'_testd.dat',  STATUS='unknown')
		!CLOSE(unit1)

		!OPEN(UNIT=unit1, file=folder_data//trim(str(L))//'_testd.dat', STATUS='unknown',POSITION="append", action = "write")
		!WRITE(unit1,'(3A25,1X,3A6,A25)') "E", "Mag(2)", "", "<N_c>", "N_v", "N_av", "Corr(L)"
		!DO j=1,10
		!	CALL constraint_algorithm
		!	CALL take_meas
		!ENDDO
	
	
	!CLOSE(unit1)
END SUBROUTINE test_d