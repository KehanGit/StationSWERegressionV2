#!/bin/bash
#SBATCH --nodes=1
#SBATCH --time=00:30:00
#SBATCH --qos=testing
#SBATCH --partition=shas-testing
#SBATCH --ntasks=24
#SBATCH --job-name=test
#SBATCH --output=test.%j.out

module purge
module load R/3.4.3
module load proj
module load gdal


Rscript 0_run_LRM.R

