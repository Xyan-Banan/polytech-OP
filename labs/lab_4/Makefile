FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -Wno-unused-function -static-libgfortran -flto
FOPT=-O3 -ftree-vectorize -fopt-info-vec

all:
	$(FC) $(FFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o
	# $(FC) $(FFLAGS) $(FOPT) -c src/custom_types.f90 -J obj/ -o obj/custom_types.o
	$(FC) $(FFLAGS) $(FOPT) -c src/io.f90 -J obj/ -o obj/io.o
	$(FC) $(FFLAGS) $(FOPT) -c src/process.f90 -J obj/ -o obj/process.o
	$(FC) $(FFLAGS) $(FOPT) -c src/main.f90 -I obj/ -o obj/main.o
	# $(FC) $(FFLAGS) $(FOPT) -o bin/app ./obj/environment.o ./obj/custom_types.o ./obj/io.o ./obj/group_process.o obj/main.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/app ./obj/environment.o ./obj/io.o ./obj/process.o obj/main.o

clean:
	rm -rf obj/*
	rm -rf bin/*

run:
	cd ./bin; ./app;
	cat bin/output.txt