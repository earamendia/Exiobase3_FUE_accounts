# Exiobase3_FUE_accounts

This repository contains the code used to generate the final- and useful-stage energy and exergy accounts for EXIOBASE3.

## Running the pipeline

To run the pipeline, one needs to create a copy of the `setup_template.R` file and rename the copy `setup.R`, changing input parameters as appropriate. 

**Important note**: running this pipeline for the whole time series  (1995-2020) demands substantial resources (80GB of RAM memory).
The dataset was therefore generated running the pipeline on the Aire High Performance Cluster (University of Leeds).

### Required data

Then, one needs to download the EXIOBASE3 database and to modify the variable `path_to_exiobase_set` to the folder where the EXIOBASE3 data is used, e.g., `EXIOBASE3/`. One needs the following files and folders:

* the ixi tables, to be located in the `EXIOBASE3/` folder, under a year-by-year .zip folder, e.g., `IOT_1995_ixi.zip`. These tables are openly available on [Zenodo](https://doi.org/10.5281/zenodo.3583070);
* the pxp tables, to be located in the `EXIOBASE3/` folder, under a year-by-year .zip folder, e.g., `IOT_1995_pxp.zip`. These tables are openly available on [Zenodo](https://doi.org/10.5281/zenodo.3583070);
* the supply-use tables, to be located under `EXIOBASE3/sut/current/`. These files are not openly available and need to be requested to the EXIOBASE3 development team;
* the gross energy accounts, to be located under `EXIOBASE3/gross_energy_accounts/`, under a year-by-year folder, e.g., `IOT_1995_ixi/`. The gross energy accounts are not openly available and need to be requested to the EXIOBASE3 development team.

Additionally, one needs access to the International Energy Agency's World Energy Balances in the same format as provided in the `inputs/dummy_webs/dummy_webs.csv` file. A licence is needed to access the World Energy Balances.

To facilitate running the pipeline without the gross energy accounts, the supply-use tables, and the World Energy Balances, we provide dummy data (actual dataset values replaced by random numbers between 0 and 100) in the correct format (but only for a single country, Austria) that can be used to run and test the code:

* World Energy Balances: `inputs/dummy_webs/dummy_webs.csv`;
* Gross energy accounts: `inputs/dummy_gross_energy_accounts` (for years 1995 and 2020);
* Supply-use tables: `inputs/dummy_sut/` (only for Austria and 1995). To run the code with this dummy data, rename the folder to `sut/`, relocate it to your `EXIOBASE3/` folder to run the code with this dummy data, and clone the `AT_1995_sup.csv` for each `EXIOBASE3` region.

### Commands to run the pipeline

There are two options to run the pipeline, (1) using the `renv` package, and (2) using `conda` and the environment `.yaml` provided in `env/environment.yaml`. In practice, the `renv` method
allows for a quick setup and was used for code development on a local machine, while the conda environment method was used to deploy the code on the HPC and to generate the actual dataset. The recommended
and most robust method is to use conda environment described in the `env/environment.yaml`.

#### Using renv

Launch R, and then run the following lines:

- `renv::restore()`
- `targets::tar_make()`

Note that this method may not work well on a non-Windows machine.

#### Using conda

Run the following lines from the terminal:

- `conda env create -f env/environment.yaml`
- `conda activate rfue`
- `Rscript run_scripts/run_pipeline.R`
