# Spatial sampling heterogeneity limits the detectability of deep time latitudinal biodiversity gradients

Author(s): [Lewis A. Jones](mailto:l.jones16@imperial.ac.uk), [Christopher D. Dean](mailto:christopherdaviddean@gmail.com), [Philip D. Mannion](mailto:philipdmannion@gmail.com), [Alexander Farnsworth](mailto:alex.Farnsworth@bristol.ac.uk), [Peter A. Allison](mailto:p.a.allison@imperial.ac.uk)

This repository contains the data and code required to run the analyses and results of the article, "Spatial sampling heterogeneity limits the detectability of deep time latitudinal biodiversity gradients" (Jones et al. 2020). 

To cite the paper: 
> Lewis A. Jones, Christopher D. Dean, Philip D. Mannion, Alexander Farnsworth, Peter A. Allison. 2020. Spatial sampling heterogeneity limits the detectability of deep time latitudinal biodiversity gradients. (TBC).

<p align="center">
  <img src="https://github.com/LewisAJones/LBG_sim/blob/master/figures/paper_workflow.png" alt="" width="500"/>
</p>

-------

## Data
The data is organised in the folder "data" as follows:

* **raw_data**
This folder contains the collections from the Paleobiology Database, as well as stratigraphic stage and period bins used in this study.

-------

## Analyses
Data analyses is set up to run as follows:

* **./R/options.R**
This script defines the options to use in the study (e.g. latitudinal bin size, and spatial resolution).

* **./R/sim_LBGs.R**
This script simulates the stage-level flat-, unimodal- and bimodal-type latitudinal biodiversity gradients.

* **./R/run_analyses.R**
This script runs the analyses.

-------
