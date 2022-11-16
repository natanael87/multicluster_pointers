MODULE wolff_step
	USE my_functions
	USE converter
	IMPLICIT NONE

	TYPE (my_value), POINTER :: ptr_out
	TYPE (my_value), POINTER :: head
	TYPE (my_value), POINTER :: tail

	REAL(8):: r(2), S_prime(2)
	REAL(8), PRIVATE :: DH, bprob, delta1
	INTEGER, PRIVATE :: nnf(5)
	LOGICAL, DIMENSION(L3) :: Vvisited


	CONTAINS

! Crea vector aleatorio
	SUBROUTINE rand_vec()
		REAL(8)::alpha, theta

		CALL random_number(alpha)
		theta = 2.0d0*PI*(alpha-0.5d0)

		r(1) = DCOS(theta)
		r(2) = DSIN(theta)		

	END SUBROUTINE rand_vec


! Crea enlaces, llama a subrotina que voltea clusters
	SUBROUTINE mlt_cls_dinamics
		INTEGER:: i,j,k, vertex, vertex2
		REAL(8):: r1, r2, r3

		madj=-1

		DH=0.d0

		CALL rand_vec()

		DO i=1,L; DO j=1,L; DO k=1,L

			vertex = (i-1)*L2 + (j-1)*L + k
			!nnv = neigh_vl(vertex)

			! Conexión hacia abajo
			DH = 2*DOT_PRODUCT(r,S(i,j,k,:))*DOT_PRODUCT(r,S(i,ip(j),k,:)) 
			IF (DH>0) THEN
				bprob = 1.D0-EXP(-DH*beta)
				CALL RANDOM_NUMBER(r2)
				IF (r2 < bprob) THEN
					vertex2 = (i-1)*L2 + (ip(j)-1)*L + k
					madj(vertex2,3) = vertex
					madj(vertex,4)=vertex2
					!WRITE(*,*) "*ABAJO", r2,bprob, link6(4), nnv(4)
				ENDIF
			ENDIF 

			! Conexión hacia la derecha
			DH = 2*DOT_PRODUCT(r,S(i,j,k,:))*DOT_PRODUCT(r,S(i,j,ip(k),:))
			IF (DH>0) THEN
				bprob = 1.D0-EXP(-DH*beta)
				CALL RANDOM_NUMBER(r3)
				IF (r3 < bprob) THEN
					vertex2 = (i-1)*L2 + (j-1)*L + ip(k)
					madj(vertex2,5) = vertex
					madj(vertex,6)=vertex2
					!WRITE(*,*) "*DERECHA", r3,bprob, link6(6), nnv(6)
				ENDIF
			ENDIF

			! Conexión hacia afuera
			DH = 2*DOT_PRODUCT(r,S(i,j,k,:))*DOT_PRODUCT(r,S(ip(i),j,k,:))
			IF (DH>0) THEN
				bprob = 1.D0-EXP(-DH*beta)
				CALL RANDOM_NUMBER(r1)
				IF (r1 < bprob) THEN
					vertex2 = (ip(i)-1)*L2 + (j-1)*L + k
					madj(vertex2,1) = vertex
					madj(vertex,2)=vertex2
					!WRITE(*,*) "*AFUERA", r1,bprob, link6(2), nnv(2)
				ENDIF
			ENDIF

		ENDDO; ENDDO; ENDDO

		!WRITE(*,*)
		!DO vertex=1,L**3
		!	WRITE(*,*) vertex, madj(vertex,:)
		!ENDDO
		!WRITE(*,*)

		CALL clust_id

	END SUBROUTINE mlt_cls_dinamics

! Acción del constraint
	SUBROUTINE constraint_algorithm
		INTEGER:: i,j,k, vertex, vertex2
		REAL(8):: cos_theta

		madj=-1

		delta1=0.0d0
		cos_theta=0.D0

		CALL rand_vec()

		DO i=1,L; DO j=1,L; DO k=1,L
			! posición del vértice
			vertex = (i-1)*L2 + (j-1)*L + k

			! Conexión hacia abajo
			S_prime = S(i,ip(j),k,:)-2*r*DOT_PRODUCT(r,S(i,ip(j),k,:))
			cos_theta=DOT_PRODUCT(S(i,j,k,:),S_prime)

			IF (cos_theta<cosd) THEN
				vertex2 = (i-1)*L2 + (ip(j)-1)*L + k
				madj(vertex2,3) = vertex
				madj(vertex,4)=vertex2
			ENDIF
			

			! Conexión hacia la derecha
			S_prime = S(i,j,ip(k),:)-2*r*DOT_PRODUCT(r,S(i,j,ip(k),:))
			cos_theta=DOT_PRODUCT(S(i,j,k,:),S_prime)
			
			IF (cos_theta<cosd) THEN
				vertex2 = (i-1)*L2 + (j-1)*L + ip(k)
				madj(vertex2,5) = vertex
				madj(vertex,6)=vertex2
			ENDIF					

			! Conexión hacia afuera
			S_prime = S(ip(i),j,k,:)-2*r*DOT_PRODUCT(r,S(ip(i),j,k,:)) 
			cos_theta=DOT_PRODUCT(S(i,j,k,:),S_prime)
			
			IF (cos_theta<cosd) THEN
				vertex2 = (ip(i)-1)*L2 + (j-1)*L + k
				madj(vertex2,1) = vertex
				madj(vertex,2)=vertex2
			ENDIF

		ENDDO; ENDDO; ENDDO


		CALL clust_id

	END SUBROUTINE constraint_algorithm

! Revisa uno a uno los vértices en búsqueda de clusters
	SUBROUTINE clust_id()
		INTEGER::i, vertex, istat
		REAL(8) :: r05 	! random number
		INTEGER :: k, ii, jj, kk

		! Usamos punteros en vez de la matriz original
		!ptr_madj => madj

		Vvisited = .false.
		num_of_clus = 0

		DO vertex=1,L**3
			NULLIFY(head)
			NULLIFY(tail)
			IF (Vvisited(vertex) .EQV. .false. ) THEN
				CALL add_to_linked(vertex, head, tail)
				CALL DFSl(vertex)
			ENDIF

			ptr_out=>head
			IF (ASSOCIATED(ptr_out)) num_of_clus = num_of_clus + 1
			
			CALL RANDOM_NUMBER(r05)
			IF (r05<0.5) THEN
				!WRITE(*,*) 'FLIP'
				flip: DO
					IF(.NOT. ASSOCIATED(ptr_out)) EXIT	! Pointer valid?
					ii=(ptr_out%value-1)/L**2+1
					jj=(MOD(ptr_out%value-1,L**2))/L+1
					kk=MOD(ptr_out%value-1,L)+1

					!WRITE(*,*) ptr_out%value, ii, jj, kk
					S(ii,jj,kk,:) = S(ii,jj,kk,:)-2.D0*r*DOT_PRODUCT(r,S(ii,jj,kk,:))
					ptr_out=>ptr_out%next 
					!WRITE(*,*) ptr_out%value
					!ptr_out=>ptr_out%next 
				END DO flip
			ENDIF

			!-Libera la memoria RAM
			CALL destroyLList(head)
		ENDDO

	END SUBROUTINE clust_id

! Subrutina recursiva para identificar los clusters
! Deep First Search algortithm(node:vertex)
	RECURSIVE SUBROUTINE DFSl(node)
		INTEGER, INTENT(IN)::node
		INTEGER::b

		! Empezamos con el primer elemento visitado (node)
		Vvisited(node) = .true.
		
		! Dentro de los enlaces de v va recorriendo sus primeros vecinos
		DO b=1,6
			IF (madj(node,b)>0) THEN
				IF (Vvisited(madj(node,b)) .EQV. .false.) THEN
					!WRITE(*,*) node, madj(node,b)
					! Agrega el vértice madj(node, b)
					!WRITE(*,*) madj(node,b), ASSOCIATED(head), head%value
					CALL add_to_linked(madj(node, b), head, tail)
					!WRITE(*,*) '* OK'
					! visita sus vecinos, luego los vecinos de sus vecinos...
					CALL DFSl(madj(node, b))
				ENDIF
			ENDIF
		ENDDO
	END SUBROUTINE DFSl

! Búsqueda de clusters de vórtices
	SUBROUTINE find_strings
		INTEGER :: i,j,k, l5, nghbrs(5)=0
		INTEGER :: ii, jj, kk, row, col, column
		REAL(8):: x
		INTEGER :: vertex, vertex1, vertex2, vertex3
		INTEGER :: v1, v2, v3, v4, v5, v6, v7, v8, v9 
		INTEGER :: v10, v11, v12, v13, v14, v15

		vmadj=-1
		avmadj=-1

		DO i=1,L; DO j=1,L; DO k=1,L

			vertex = (i-1)*L2 + (j-1)*L + k
			v1 = 3*vertex-2; v2 = 3*vertex-1; v3 = 3*vertex
			v4 = 3*((i-1)*L2 + (j-1)*L + ip(k))
			v5 = 3*((i-1)*L2 + (im(j)-1)*L + k)-1
			v6 = 3*((ip(i)-1)*L2 + (j-1)*L + k)-2

			nghbrs(3)=v4; nghbrs(4)=v5; nghbrs(5)=v6
			
			!-Fondo(1)
			vertex1 = (im(i)-1)*L2 + (j-1)*L + k
			v7 = 3*vertex1-2; v8 = 3*vertex1-1; v9 = 3*vertex1

			nghbrs(1)=v2; nghbrs(2)=v3

			CALL RANDOM_NUMBER(x); l5=x*5+1
			row = CEILING(REAL(nghbrs(l5))/3.0)
			IF (l5<3) THEN
				column=1
			ELSE
				column=6
			END IF
			ii=(row-1)/L2+1
			jj=MOD(row-1,L2)/L+1
			kk=MOD(row,L)+1
			col=MOD(nghbrs(l5)-1,3)+1 
			SELECT CASE (Svortex(i,j,k,1))
			CASE(1) ! -Vórtice
				IF (Svortex(ii,jj,kk,col)==1 .AND. vmadj(vertex,l5)>0) THEN
					vmadj(vertex,l5) = row
					vmadj(row,column) = vertex
				END IF
			CASE(-1) ! -Anti-Vórtice
				IF (Svortex(ii,jj,kk,col)==-1 .AND. avmadj(vertex,l5)>0) THEN
					avmadj(vertex,l5) = row
					avmadj(row,column) = vertex
				END IF
			END SELECT

			!-Piso(2)
			vertex2 = (i-1)*L2 + (ip(j)-1)*L + k 
			v10 = 3*vertex2-2; v11 = 3*vertex2-1; v12 = 3*vertex2

			nghbrs(3)=v4; nghbrs(4)=v5; nghbrs(5)=v6

			CALL RANDOM_NUMBER(x); l5=x*5+1
			row = CEILING(REAL(nghbrs(l5))/3.0)
			IF (l5==1) THEN
				column=1
			ELSE IF (l5==2) THEN
				column=2
			ELSE
				column=7
			END IF
			ii=(row-1)/L2+1
			jj=MOD(row-1,L2)/L+1
			kk=MOD(row,L)+1
			col=MOD(nghbrs(l5)-1,3)+1
			SELECT CASE (Svortex(i,j,k,2))
			CASE(1) ! -Vórtice
				IF (Svortex(ii,jj,kk,col)==1 .AND. vmadj(vertex,l5)>0) THEN
					vmadj(vertex,l5) = row
					vmadj(row,column) = vertex
				END IF
			CASE(-1) ! -Anti-Vórtice
				IF (Svortex(ii,jj,kk,col)==-1 .AND. avmadj(vertex,l5)>0) THEN
					avmadj(vertex,l5) = row
					avmadj(row,column) = vertex
				END IF
			END SELECT

			!-Lateral Izq(3
			vertex3 = (i-1)*L2 + (j-1)*L + im(k)
			v13 = 3*vertex3-2; v14 = 3*vertex3-1; v15 = 3*vertex3

			nghbrs(3)=v4; nghbrs(4)=v5; nghbrs(5)=v6

			CALL RANDOM_NUMBER(x); l5=x*5+1
			row = CEILING(REAL(nghbrs(l5))/3.0)
			IF (l5<3) THEN
				column=2
			ELSE
				column=8
			END IF
			ii=(row-1)/L2+1
			jj=MOD(row-1,L2)/L+1
			kk=MOD(row,L)+1
			col=MOD(nghbrs(l5)-1,3)+1
			SELECT CASE (Svortex(i,j,k,3))
			CASE(1) ! -Vórtice
				IF (Svortex(ii,jj,kk,col)==1 .AND. vmadj(vertex,l5)>0) THEN
					vmadj(vertex,l5) = row
					vmadj(row,column) = vertex
				END IF
			CASE(-1) ! -Anti-Vórtice
				IF (Svortex(ii,jj,kk,col)==-1 .AND. avmadj(vertex,l5)>0) THEN
					avmadj(vertex,l5) = row
					avmadj(row,column) = vertex
				END IF
			END SELECT

		ENDDO; ENDDO; ENDDO

		!CALL vclust_id
		!CALL avclust_id

	END SUBROUTINE find_strings

END MODULE wolff_step