# Makefile
uzura1: kind.o mpg.o crc.o mpg_io.o wav_io.o filter.o psycho.o layer1.o uzura1win.o
	gfortran -std=f2008 -o uzura1 kind.o mpg.o crc.o mpg_io.o wav_io.o filter.o psycho.o layer1.o uzura1win.o

kind.o: kind.f90
	gfortran -std=f2008 -c kind.f90

mpg.o: mpg.f90
	gfortran -std=f2008 -c mpg.f90

crc.o: crc.f90
	gfortran -std=f2008 -c crc.f90

mpg_io.o: mpg_io.f90
	gfortran -std=f2008 -c mpg_io.f90

wav_io.o: wav_io.f90
	gfortran -std=f2008 -c wav_io.f90

filter.o: filter.f90
	gfortran -std=f2008 -c filter.f90

psycho.o: psycho.f90
	gfortran -std=f2008 -c psycho.f90

layer1.o: layer1.f90
	gfortran -std=f2008 -c layer1.f90

uzura1win.o: uzura1win.f90
	gfortran -std=f2008 -c uzura1win.f90

clean:
	rm -f uzura1 kind.o mpg.o mpg_io.o wav_io.o filter.o crc.o psycho.o layer1.o uzura1win.o *.mod
