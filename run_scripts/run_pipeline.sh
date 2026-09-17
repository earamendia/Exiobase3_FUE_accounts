#!/bin/bash
# Runs pipeline to produce EXIOBASE final- and useful energy accounts
# Author: Emmanuel Aramendia
# Date: 05/03/2026
# Requirements: XXGB RAM, 1 core
# Approx. run time on Aire: ~6 hours

#SBATCH --job-name=fue_pipeline
#SBATCH --output=msg/output_%j.out
#SBATCH --error=msg/error_%j.err
#SBATCH --time=24:00:00
#SBATCH --mem=80G
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1

# Displaying job information
echo "Job Information:"
echo "Job ID: $SLURM_JOB_ID"
echo "Job Name: $SLURM_JOB_NAME"
echo "Node: $SLURM_NODENAME"
echo "CPUs per task: $SLURM_CPUS_PER_TASK"
echo "Memory per node: $SLURM_MEM_PER_NODE"
echo "Submit directory: $SLURM_SUBMIT_DIR"

# Exit immediately if any command fails
set -e

# Print commands as they execute (for debugging)
#set -x

# Actual workflow set out below

# Removing previously built results
rm -f $SCRATCH/WorkflowOutputs/Exiobase_FUE_vecs/outputs/*

# Loading miniforge and activating conda environment
module load miniforge/24.7.1 || {
	echo "ERROR: Failed to load miniforge"
	exit 1
}

# Activate working environment (assuming it has already been created)
conda activate rfue

# Setting working directory
cd /users/earear/gitrepos/Exiobase_FUE_vecs/

# Run targets pipeline
Rscript run_scripts/run_pipeline.R

# Check that the outputs are successfully created
if [! -f "$SCRATCH/WorkflowOutputs/Exiobase_FUE_vecs/outputs/energy_pba_cba_accounts.csv"]; then
	echo "ERROR: the output file energy_pba_cba_accounts.csv has not been created." 
	exit 1
fi

# Add something to export environment
conda-lock -f env/environment.yaml -p linux-64 --lockfile env/conda-lock.yaml

# Creating a run_metadata folder, and copy-pasting lockfile in there, with the results of the pipeline
mkdir /mnt/scratch/earear/Workflows/Exiobase_FUE_vecs/outputs/run_metadata
cp env/conda-lock.yaml /mnt/scratch/earear/Workflows/Exiobase_FUE_vecs/outputs/run_metadata/conda-lock.yaml

# Logging the hash of the current git commit
git rev-parse HEAD > /mnt/scratch/earear/Workflows/Exiobase_FUE_vecs/outputs/run_metadata/git_commit.txt
git branch --show-current > /mnt/scratch/earear/Workflows/Exiobase_FUE_vecs/outputs/run_metadata/git_branch.txt
git status --porcelain > /mnt/scratch/earear/Workflows/Exiobase_FUE_vecs/outputs/run_metadata/git_status.txt

# Logging date
date > /mnt/scratch/earear/Workflows/Exiobase_FUE_vecs/outputs/run_metadata/run_timestamp.txt

# Asserting that the job has been completed successfully (if we got to the end of the bash script)
echo "Job completed successfully"
