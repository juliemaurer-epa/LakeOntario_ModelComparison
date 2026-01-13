#!/bin/csh

#SBATCH --job-name=MvM
#SBATCH --time=02:00:00
#SBATCH --ntasks=1
#SBATCH --mem=80gb
#SBATCH --account=glhabs
#SBATCH --partition=compute
#SBATCH --error=MvM_error
#SBATCH --output=MvM_out

cd /work/GLHABS/GreatLakesEco/LakeOntario/scripts

module load intel
module load R

date

Rscript MvM_TP_FVCOM_EFDC_LOEM.R

date
