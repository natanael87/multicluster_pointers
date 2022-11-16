MODULE mparameters
	IMPLICIT NONE

	INTEGER, PARAMETER :: Ntherm = 1D4	! Number of sweeps for thermalization
	INTEGER, PARAMETER :: Nskip = 50	! Number of sweeps between each measurement
	INTEGER, PARAMETER :: Nmeas = 1000	! Nummber of measurements

	REAL(8), PARAMETER :: PI = ACOS(-1.0D0)
	INTEGER, PARAMETER :: unit1 = 11, unit2 = 12, unit3 = 13, unit4=14, unit5 = 15

	CHARACTER(*), PARAMETER :: folder_data = '/home/elias/Git/Data3dXYfv/'		! path where data file is located
	CHARACTER(*), PARAMETER :: folder_main_pc = '/home/elias/Git/3dXYO2_v2/'	! file where the main folder of code is placed
	CHARACTER(*), PARAMETER :: folder_main_remote = '/home/eliaspolanco/2dxy/'	! file where the main folder of code is placed

	CHARACTER(*), PARAMETER :: folder = folder_main_pc


	CHARACTER(*), PARAMETER :: delta_constraint = 'delta'
	CHARACTER(*), PARAMETER :: standard_action = 'standard'
	CHARACTER(*), PARAMETER :: test_constraint = 'test_delta'
	CHARACTER(*), PARAMETER :: test_standard = 'test_beta'
	CHARACTER(*), PARAMETER :: test_single = 'test_single'

	SAVE
END MODULE mparameters
