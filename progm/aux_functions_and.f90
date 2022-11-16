MODULE my_functions
	USE init
	IMPLICIT NONE

	REAL :: u

	CONTAINS

! Función para calcular el ángulo dirigido ente dos espines
	FUNCTION delt_phi(s1, s2)
		REAL(8), INTENT(IN):: s1(2), s2(2)
		REAL(8) :: delt_phi

		cos_phi = DOT_PRODUCT(s1,s2)/(NORM2(s1)*NORM2(s2))

		IF (ABS(cos_phi) > 1) THEN
			cos_phi = 1
		ENDIF

		IF ( Cross2(s1,s2) < 0 ) THEN
			delt_phi = -ACOS(cos_phi)
		ELSE
			delt_phi = ACOS(cos_phi)
		ENDIF

	END FUNCTION delt_phi

! Funcion para calcular producto punto
!FUNCTION NORM2(v)
!	REAL(8), INTENT(IN)::v
!	REAL(8) :: NORM2
!	NORM2 = DSQRT(DOT_PRODUCT(v,v))
!END FUNCTION NORM2

! Calcula el producto cruz de vectores
	FUNCTION Cross2(s1,s2)
		REAL(8) :: Cross2
		REAL(8), INTENT(IN)::s1(2), s2(2)

		Cross2 = s1(1)*s2(2)-s1(2)*s2(1)

	END FUNCTION Cross2

! Calcula vortices
	SUBROUTINE calc_vortices
	!INTEGER, INTENT(IN) :: i,j,k
	INTEGER :: i,j,k

	nvx=0; nvy=0; nvz=0

!	Fondo(1)		Piso(2)			Lateral Izq(3)
!	
!	x4 <----- x3	x1 <----- x2	x7 <----- x4	
!	|		   |	|		   |	|		   |
!	|		   |	|		   |	|		   |
!	x1 -----> x2	x5 -----> x6	x5 -----> x1
!

	DO i=1,L; DO j=1,L; DO k=1,L
		Dphi12 = delt_phi(S(i,j,k,:), S(i,ip(j),k,:))
		Dphi23 = delt_phi(S(i,ip(j),k,:), S(i,ip(j),ip(k),:))
		Dphi34 = delt_phi(S(i,ip(j),ip(k),:), S(i,j,ip(k),:))
		Dphi41 = delt_phi(S(i,j,ip(k),:), S(i,j,k,:))

		vortex2=Dphi12+Dphi23+Dphi34+Dphi41 ! Fondo(1)
		IF (NINT(vortex2) > 0) THEN
			num_of_vort = num_of_vort+1
		ELSE IF (NINT(vortex2)<0) THEN
			num_of_av = num_of_av+1
		ENDIF

		Dphi15 = delt_phi(S(i,j,k,:), S(ip(i),j,k,:))
		Dphi56 = delt_phi(S(ip(i),j,k,:), S(ip(i),ip(j),k,:))
		Dphi62 = delt_phi(S(ip(i),ip(j),k,:), S(i,ip(j),k,:))
		Dphi21 = delt_phi(S(i,ip(j),k,:), S(i,j,k,:))

		vortex1=Dphi21+Dphi15+Dphi56+Dphi62 ! Piso(2)
		IF (NINT(vortex1) > 0) THEN
			num_of_vort = num_of_vort+1
		ELSE IF (NINT(vortex1)<0) THEN
			num_of_av = num_of_av+1
		ENDIF			

		Dphi14 = delt_phi(S(i,j,k,:), S(i,j,ip(k),:))
		Dphi47 = delt_phi(S(i,j,ip(k),:), S(ip(i),j,ip(k),:))
		Dphi75 = delt_phi(S(ip(i),j,ip(k),:), S(ip(i),j,k,:))
		Dphi51 = delt_phi(S(ip(i),j,k,:), S(i,j,k,:))

		vortex3=Dphi14+Dphi47+Dphi75+Dphi51 ! Lateral(3)

		IF (NINT(vortex3) > 0) THEN
			num_of_vort = num_of_vort+1
		ELSE IF (NINT(vortex3)<0) THEN
			num_of_av = num_of_av+1
		ENDIF

		sum_vortex=sum_vortex+vortex1+vortex2+vortex3

		vortex1 = vortex1/(2*PI) !-Piso(2)
		vortex2 = vortex2/(2*PI) !-Fondo(1)
		vortex3 = vortex3/(2*PI) !-Lateral Izq(3)

		Svortex(i,j,k,1) = NINT(vortex1)	! Piso(2)
		Svortex(i,j,k,2) = NINT(vortex2)	! Fondo(1)
		Svortex(i,j,k,3) = NINT(vortex3)	! Lateral(3)

		nvx=nvx+ABS(vortex1) !-Piso(2)
		nvy=nvy+ABS(vortex2) !-Fondo(1)
		nvz=nvz+ABS(vortex3) !-Lateral Izq(3)

		!WRITE(unit5,'(3I3,1X,3I3)') i-1, j-1, k-1, NINT(vortex1),NINT(vortex2),NINT(vortex3)!, S(i,j,k,:), S(ip(i),j,k,:), &
	!S(ip(i),ip(j),k,:), S(i,ip(j),k,:)

	ENDDO; ENDDO; ENDDO
	
END SUBROUTINE calc_vortices

SUBROUTINE calc_F
	INTEGER :: i,j,k, i2, j2, k2

	DO i=1,L; DO j=1,L; DO k=1,L
		do i2 = 1,L; do j2 = 1,L; do k2 = 1,L
			F = F + dot_product(S(i2,j2,k2,:),S(i,j,k,:))*cos_arr(i, i2)
		enddo; enddo; enddo
	ENDDO; ENDDO; ENDDO
END SUBROUTINE calc_F

! Convierte coordenadas x,y,z a etiqueta de vértice (1,2,...,L**3)
	FUNCTION vv(i,j,k)
		INTEGER, INTENT(IN) :: i,j,k
		INTEGER :: vv

		vv = (i-1)*L**2 + (j-1)*L + k

	END FUNCTION vv

! Convierte etiqueta de vértice (1,2,...,L**3) a coordenadas x,y,z
	FUNCTION to_coord(vvertex)
		INTEGER, INTENT(IN) :: vvertex
		INTEGER :: to_coord(3)

		to_coord(1) = (vvertex-1)/L**2+1
		to_coord(2) = (MOD(vvertex-1,L**2))/L+1
		to_coord(3) = MOD(vvertex-1,L)+1

	END FUNCTION to_coord

! Subrutina para arrays dinámicos con elementos enteros
	SUBROUTINE AddToList(list, element)
	integer :: i, isize
	INTEGER, intent(in) :: element
	INTEGER, dimension(:), allocatable, intent(inout) :: list
	INTEGER, dimension(:), allocatable :: clist

	if(allocated(list)) then
	  isize = size(list)
	  allocate(clist(isize+1))         
	  clist(1:isize) = list
	  clist(isize+1) = element

	  deallocate(list)
	  call move_alloc(clist, list)

	else
	  allocate(list(1))
	  list(1) = element
	end if

	END SUBROUTINE AddToList

! Subrutina para arrays dinámicos con elementos reales(8)
	SUBROUTINE AddToListDbl(list, element)
	integer :: i, isize
	REAL(8), intent(in) :: element
	REAL(8), dimension(:), allocatable, intent(inout) :: list
	REAL(8), dimension(:), allocatable :: clist

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

	END SUBROUTINE AddToListDbl

! Lee el archivo de los valores de beta o delta
	SUBROUTINE read_bd()
	
	INTEGER :: i, stat
	REAL(8) :: buf

	IF ((the_action == standard_action) .OR. (the_action == test_standard)) THEN
		OPEN(UNIT=unit4, FILE=folder_beta, STATUS='old')
		DO
			read(unit4, *, iostat=stat) buf
			if (stat /= 0) exit
			call AddToListDbl(betas, buf)
			! process buf
		ENDDO
		CLOSE(unit4)
	END IF

	IF ((the_action == delta_constraint) .OR. (the_action == test_constraint)) THEN
		OPEN(UNIT=unit4, FILE=folder_delta, STATUS='old')
		DO
			read(unit4, *, iostat=stat) buf
			if (stat /= 0) exit
			call AddToListDbl(deltas, buf)
			! process buf
		ENDDO
		CLOSE(unit4)
	END IF

	END SUBROUTINE read_bd

! Creates a linked list
	SUBROUTINE add_to_linked(temp, head_, tail_)
		!USE type_of
	IMPLICIT NONE

	! Data dictionary: declare variable  types & definitions
	TYPE (my_value), POINTER :: head_		! Ptr to head of list
	TYPE (my_value), POINTER :: tail_		! Ptr to tail list
	!TYPE (my_value), POINTER :: ptr_out	! Temporary pointer
	INTEGER, INTENT(IN) :: temp				! Temporary variable
	INTEGER :: istat

	IF(.NOT. ASSOCIATED(head_)) THEN	! No values in list
		ALLOCATE(head_, STAT=istat)		! Allocate new value
		tail_ => head_					! Tail points to new value
		NULLIFY (tail_%next)			! Nullify 'next' in new value
		tail_%value = temp				! Store new number
	ELSE								! Values already in list
		ALLOCATE(tail_%next,STAT=istat)	! Allocate new value
		tail_=>tail_%next				! Tail points to new value
		NULLIFY(tail_%next)				! Nullify 'next' in new value
		tail_%value=temp				! Store number
	END IF

	END SUBROUTINE add_to_linked

! Free memory space
SUBROUTINE destroyLList(list)
	TYPE (my_value), POINTER :: list
	TYPE (my_value), POINTER :: next
	TYPE (my_value), POINTER :: dCurrent=>NULL(), dNext=>NULL()

	IF(.NOT. ASSOCIATED(list)) RETURN

	dCurrent => list

	!-Dealocate all data nodes in list
	DO WHILE(ASSOCIATED(dCurrent))
		dNext => dCurrent%next
		NULLIFY(dCurrent%next)
		DEALLOCATE(dCurrent)
		dCurrent => dNext
	END DO

	!-Deallocate the list itself
	!DEALLOCATE(list)

END SUBROUTINE destroyLList

END MODULE my_functions