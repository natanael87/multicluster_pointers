MODULE mvariables
	USE mparameters2
	IMPLICIT NONE

	REAL(8), DIMENSION(L,L,L,2) :: S 		 		! Red de espines
	INTEGER, DIMENSION(L,L,L,3) :: Svortex 		! Red de vortices
	INTEGER, DIMENSION(L) :: ip, im					! Conds de front periodicas para S
	!INTEGER, DIMENSION(L,L,L) :: vertxs			! Array de vertices de 1 a L^3
	
	! Vector de betas, deltas
	REAL(8), DIMENSION(:), ALLOCATABLE :: betas, deltas	
	REAL(8)::beta, delta, cosd					! Elementos del vector betas, deltas y cos(delta)
	
	! Matriz de enlaces
	INTEGER, DIMENSION(:,:), ALLOCATABLE :: madj, vmadj, avmadj

	REAL(8) :: cos_phi							! Variable para funcion del coseno de un ángulo

	! Vector para almacenar los elementos de un clúster
	INTEGER, DIMENSION(:), ALLOCATABLE :: icluster		

	! Variables para formatos
	CHARACTER(50)::FMT1, FMT2, FMT3

	! Varibles para tomar mediciones
	REAL(8) :: vortex1, vortex2, vortex3, sum_vortex
	REAL(8) :: Dphi12, Dphi23, Dphi34, Dphi41
	REAL(8) :: Dphi21,Dphi15,Dphi56,Dphi62,Dphi14,Dphi47,Dphi75,Dphi51
	INTEGER :: num_of_vort, num_of_av, nvx, nvy, nvz
	REAL(8) :: E, Mag(2), O(L,2), Corr(L), dot_product_OO, F, cos_arr(L,L)

	INTEGER::num_of_clus, nsweep
	
	TYPE :: my_value
		INTEGER :: value
		TYPE(my_value), POINTER :: next
	END TYPE	

	SAVE
END MODULE mvariables