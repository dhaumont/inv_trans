#!/bin/bash

set -x

for f90 in gfortran nvfortran
do
  echo "==> $f90 <=="

  x=inv_trans.$f90.x
  rm $x
  $f90 -g -o $x -O0 parkind1.F90 field_module.F90  inv_trans.F90 inv_trans_field_api.F90 main.F90
  ./$x

done

