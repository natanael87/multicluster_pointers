rm *.o *.mod

gfortran -c gparameters.f90
gfortran -c subroutinesg.f90
gfortran -c generate_scripts.f90

gfortran *.o -o executable.exe

rm *.o *.mod
./executable.exe
