#!/bin/bash
#
# comment out the memory and cpu requirements
##SBATCH --mem=10G
##SBATCH --cpus-per-task=6 

# comment out to request time limit of two days
##SBATCH --time=2-00:00:00

# specify the number of tasks in the array
#SBATCH --array=0-99

# name the output and error files
#SBATCH --output=result_likelihood_ratio-%a.log
#SBATCH --error=error_likelihood_ratio-%a.err

# email me when the results are available
#SBATCH --mail-type=FAIL,END
#SBATCH --mail-user=jchen459@jhu.edu 

module load R

# run the script using the job number as the seed
Rscript likelihood_ratio_sim.R $SLURM_ARRAY_TASK_ID
