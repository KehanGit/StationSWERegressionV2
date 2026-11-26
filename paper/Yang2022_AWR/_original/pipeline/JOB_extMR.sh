#!/bin/bash
#SBATCH --nodes=1
#SBATCH --time=1:00:00
#SBATCH --qos=normal
#SBATCH --partition=shas
#SBATCH --ntasks=16
#SBATCH --job-name=extMR
#SBATCH --output=extMR.%j.out

module purge
module load R/3.4.3
module load proj
module load gdal


Rscript 2_1_extract_snow_course_pillow_modelrec.R


