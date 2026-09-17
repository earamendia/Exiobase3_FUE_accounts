# Restoring environment with renv
# Not needed because all packages sorted out via conda
# renv::restore()

# Configuring output folder
targets::tar_config_set(store = "/mnt/scratch/earear/Workflows/Exiobase_FUE_vecs/_targets")

# Running pipeline
targets::tar_make()

