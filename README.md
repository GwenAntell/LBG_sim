# Spatial sampling heterogeneity limits the detectability of deep time latitudinal biodiversity gradients

Author(s): [Lewis A. Jones](mailto:l.jones16@imperial.ac.uk), [Christopher D. Dean](mailto:christopherdaviddean@gmail.com), [Philip D. Mannion](mailto:philipdmannion@gmail.com), [Alexander Farnsworth](mailto:alex.Farnsworth@bristol.ac.uk), [Peter A. Allison](mailto:p.a.allison@imperial.ac.uk)

This repository contains the data and code required to run the analyses and results of the article, "Spatial sampling heterogeneity limits the detectability of deep time latitudinal biodiversity gradients" (Jones et al. 2020). 

To cite the paper: 
> Lewis A. Jones, Christopher D. Dean, Philip D. Mannion, Alexander Farnsworth, Peter A. Allison. 2020. Spatial sampling heterogeneity limits the detectability of deep time latitudinal biodiversity gradients. (TBC).

<p align="center">
  <img src="https://github.com/LewisAJones/LBG_sim/blob/master/figures/paper_workflow.png" alt="" width="400"/>
</p>

-------

## Data
The data is organised in the folder "data" as follows:

* **./data/raw_data/**
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

<p align="center">
  <img src="https://github.com/LewisAJones/LBG_sim/blob/master/figures/simulation_workflow.png" alt="" width="500"/>
</p>

-------

## Results
The data is organised in the folder "results" as follows:

* **./results/flat/**
This folder contains the data related to simulated/sampled/sampling-standardised flat-type latitudinal biodiversity gradients.

* **./results/unimodal/**
This folder contains the data related to simulated/sampled/sampling-standardised unimodal-type latitudinal biodiversity gradients.

* **./results/bimodal/**
This folder contains the data related to simulated/sampled/sampling-standardised bimodal-type latitudinal biodiversity gradients.

* **./results/compiled_LBGs/**
This folder contains the data related to all simulated/sampled/sampling-standardised latitudinal biodiversity gradients. The folder also includes stage-level plots.

* **./results/SSC/**
This folder contains results related to spatial sampling coverage.

* **./results/MST/**
This folder contains results related to summed minimum-spanning tree length.

* **./results/GIF/**
This folder contains plots of all stage-level sampling grids.

<p align="center">
  <img src="https://github.com/LewisAJones/LBG_sim/blob/master/results/GIF/spatial_sampling.gif" alt="" width="600"/>
</p>

* **./results/frequency_dist/**
This folder contains results from Kolmogorov–Smirnov two-sample tests between simulated and empirical range size and occurrence frequency distributions.

* **./results/SLD/**
This folder contains total displacement results between pair-wise combinations of diversity curves.

* **./results/corr_test/**
This folder contains results from Pearson's correlation coefficient tests between pair-wise combinations of diversity curves.

* **./results/KS_test/**
This folder contains results from Kolmogorov–Smirnov two-sample tests between pair-wise combinations of diversity curves.

* **./results/max_lat/**
This folder contains results related to the palaeolatitudinal bin with peak richness.

* **./results/Global/**
This folder contains results related to global sampled richness.

-------
