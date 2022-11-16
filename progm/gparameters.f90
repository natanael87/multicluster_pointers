MODULE global
	IMPLICIT NONE
	INTEGER, PARAMETER :: L=8		! Lattice size
	INTEGER, PARAMETER :: nbd = 1	! Number of beta/delta values per job
	
	CHARACTER(*), PARAMETER :: folder_main_pc = '/home/elias/Git/3dXYO2_v2/'	! path of the main folder with all files
	CHARACTER(*), PARAMETER :: folder_prog = folder_main_pc//'progm/'			! file where the source code is placed

	CHARACTER(*), PARAMETER :: delta_constraint = 'delta_constraint'
	CHARACTER(*), PARAMETER :: standard_action = 'standard_action'
	CHARACTER(*), PARAMETER :: the_action = standard_action!delta_constraint!test_!

	INTEGER, PARAMETER :: unit1 = 11, unit2 = 12, unit3 = 13, unit4=14, unit5=15, unit6=16
END MODULE global
