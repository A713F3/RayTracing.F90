

all: material.o hit_record.o ppm.o vec3.o ray.o sphere.o main.o
	gfortran -o main.out main.o ppm.o vec3.o ray.o sphere.o hit_record.o

ppm.o: ppm.f90
	gfortran -c ppm.f90

main.o: main.f90
	gfortran -c main.f90

vec3.o: vec3.f90
	gfortran -c vec3.f90

ray.o: ray.f90
	gfortran -c ray.f90

sphere.o: sphere.f90
	gfortran -c sphere.f90

hit_record.o: hit_record.f90
	gfortran -c hit_record.f90

material.o: material.f90
	gfortran -c material.f90