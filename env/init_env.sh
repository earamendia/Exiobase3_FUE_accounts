#! /bin/bash

# Loading miniforge
module load miniforge

# Set right filepath
cd /users/earear/gitrepos/Exiobase_FUE_vecs/

# Creating and activating conda environment
conda env create -f env/environment.yaml
conda activate rfue
