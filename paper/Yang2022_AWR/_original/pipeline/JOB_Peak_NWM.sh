#!/bin/bash
#SBATCH --nodes=1
#SBATCH --time=24:00:00
#SBATCH --qos=normal
#SBATCH --partition=shas
#SBATCH --ntasks=1
#SBATCH --job-name=peak
#SBATCH --output=peak.%j.out

module purge
module load R/3.4.3
module load proj
module load gdal


Rscript 5_pixelwise_peakSWE_PeakDate_NWM.R


