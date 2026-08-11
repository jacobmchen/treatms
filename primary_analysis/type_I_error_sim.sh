#!/bin/bash
#
# comment out the memory and cpu requirements
##SBATCH --mem=10G
##SBATCH --cpus-per-task=6 

# comment out to request time limit of two days
##SBATCH --time=2-00:00:00

# name the output and error files
#SBATCH --output=type_I_error-%a.log
#SBATCH --error=type_I_error-%a.err

# email me when the results are available
#SBATCH --mail-type=FAIL,END
#SBATCH --mail-user=jchen459@jhu.edu 

module load R

# run the script using the job number as the seed
Rscript rmst_analysis.R
