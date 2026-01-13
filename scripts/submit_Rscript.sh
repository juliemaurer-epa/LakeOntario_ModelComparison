#!/bin/csh

#SBATCH --job-name=Rscript
#SBATCH --time=05:00:00
#SBATCH --ntasks=1
#SBATCH --mem=180gb
#SBATCH --account=glhabs
#SBATCH --partition=compute
#SBATCH --error=R_error
#SBATCH --output=R_out

cd /work/GLHABS/GreatLakesEco/LakeOntario/scripts

module load intel
module load R

date

Rscript FVCOM_temp_hot-v-cold.R

date
