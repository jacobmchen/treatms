#!/bin/bash
#
# comment out the memory and cpu requirements
##SBATCH --mem=10G
##SBATCH --cpus-per-task=6 

# comment out to request time limit of two days
#SBATCH --time=2-00:00:00

# name the output and error files
#SBATCH --output=result_likelihood_ratio-%j.log
#SBATCH --error=error_likelihood_ratio-%j.err

# email me when the results are available
#SBATCH --mail-type=FAIL,END
#SBATCH --mail-user=jchen459@jhu.edu 

echo "starting simulations"

module load R

Rscript msis29.R

echo "simulation ended successfully"
