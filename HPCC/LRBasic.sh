#!/bin/bash

#SBATCH --time=00:59:00
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=2GB
#SBATCH --nodes=1

# Load the R module
module purge
module load R/4.3.3

# Get to the project directory
cd ~/SporeTrap

# Run the script
Rscript /mnt/home/checkjil/SporeTrap/Code/LRBasic.R

# Write job info
scontrol show job $SLURM_JOB_ID
