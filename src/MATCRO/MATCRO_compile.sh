#!/bin/sh

ifort -CB -fpe0 -traceback -g -o MATCRO-Soy MATCRO.f90 SUB_IO.f90 SUB_TINTERP.f90 SUB_RAD.f90 SUB_PHSYN.f90 SUB_SOIL.f90 SUB_CROP.f90 FNC.f90 -I/home/soft/local/netcdf/include -L/home/soft/local/netcdf/lib -lnetcdff



