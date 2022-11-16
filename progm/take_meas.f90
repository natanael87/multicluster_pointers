SUBROUTINE take_meas()
	USE wolff_step
	USE converter
	IMPLICIT NONE

	INTEGER :: i,j,k, i2,j2,k2!, nvx, nvy, nvz

!	Fondo(1)		Piso(2)			Lateral(3)
!	
!	x4 <----- x3	x1 <----- x2	x7 <----- x4	
!	|		   |	|		   |	|		   |
!	|		   |	|		   |	|		   |
!	x1 -----> x2	x5 -----> x6	x5 -----> x1
!
	
	CALL init_var
	!CALL calc_vortices
	CALL calc_F

	DO i=1,L
		DO j=1,L; DO k=1,L
 
			E = E - DOT_PRODUCT(S(i,j,k,:), S(ip(i),j,k,:)) &
			- DOT_PRODUCT(S(i,j,k,:), S(i,ip(j),k,:)) - DOT_PRODUCT(S(i,j,k,:),S(i,j,ip(k),:))

			Mag = Mag + S(i,j,k,:)
		ENDDO; ENDDO

		O(i,1) = sum(S(i,:,:,1))
		O(i,2) = sum(S(i,:,:,2))
		
		dot_product_OO =  dot_product(O(1,:),O(i,:))
		Corr(i) = Corr(i) + dot_product_OO
	ENDDO

	FMT1= '(3(F25.16,1X),F30.16, 4I8,1X,'//trim(str(L))//'(f28.16,1X))'
	WRITE(unit1,FMT1) E, Mag, F, num_of_clus, nvx, nvy, nvz, Corr

END SUBROUTINE take_meas