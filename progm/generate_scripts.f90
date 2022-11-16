PROGRAM generate
	USE functions
	IMPLICIT NONE

	CALL read_bd
	!WRITE(*,*) betas

	IF (MOD(SIZE(betas),nbd) == 0 .OR. MOD(SIZE(deltas),nbd) == 0)	THEN
		CALL write_bd
		CALL write_compile
		CALL compile_bd
		CALL write_sub
	ELSE
		WRITE(*,*) "El número de divisiones no es un numero entero"
		WRITE(*,'("Number of betas/deltas ",I3, ", Number of sets ", I3)') SIZE(betas),nbd

	END IF 

END PROGRAM generate
