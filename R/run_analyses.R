# This script makes all the figures (intermediate and final) and
# reports metadata, summary statistics, and main statistical tests
# based on the richness rasters produced by R/sim_LBGs.R
# - which runs in conjunction w subscripts/unimodal.R, bimodal.R, and flat.R

#run analyses

#spatial sampling
source("./R/subscripts/generate_shallow_marine_grids.R") #generate shallow marine masks
rm(list=ls())
# GSA - not possible to run because DEMs are missing

source("./R/subscripts/bin_collections.R") #bin collections for sampling and computing SSC
rm(list=ls())
source("./R/subscripts/calculate_ssc.R") #calculate SSC
rm(list=ls())
source("./R/figures/heat_map_SSC_plots.R") #plot SSC
rm(list=ls())
source("./R/subscripts/calculate_mst.R") #calculated MST
rm(list=ls())
source("./R/figures/heat_map_MST_plots.R") #plot SSC
rm(list=ls())
source("./R/figures/combine_sampling_plots.R") #combine spatial sampling plots
rm(list=ls())
source("./R/figures/global_sampling_plot.R") #global sampling plot
rm(list=ls())
source("./R/figures/GIF.R") #generate GIF and spatial sampling grid plots 
rm(list=ls())

#frequency distributions
source("./R/subscripts/range_freq_dist.R") #range frequency distribution analyses
rm(list=ls())
source("./R/subscripts/occ_freq_dist.R") #occurrence frequency distribution analyses
rm(list=ls())
source("./R/subscripts/format_KS_tables.R") #format tables
rm(list=ls())

#LBG analyses

source("./R/figures/LBG_type_plot.R") #generate LBG type figure 
rm(list=ls())
# GSA - heatmap of seed cell probability, i.e. initial gradient

source("./R/subscripts/compute_LBGs.R") #compute LBGs for simulated data
rm(list=ls())
# this script does summary stats on simulation data from /results folder
# exports a png and summary csv of richness by lat bin, for each stage
# ca. 8 min runtime for each LDG type

source("./R/subscripts/sample_LBGs.R") #sample and compute LBGs for simulated data
rm(list=ls())
# exactly analogous to compute_LBGs.R except the input data gets masked by shelf area

#source("./R/subscripts/rarefy_LBGs.R") #rarefy sampled simulated data
#rm(list=ls())
# exactly analogous to compute_LBGs.R again except the input data is the masked data
# (csv's output by sample_LBGs), which this subscript rarefies before calculating stats

# compare simulated, sampled (preserved), and rarefied LDGs of each type (Fig 3)
source("./R/subscripts/compile_data.R") #compiled LBG data
rm(list=ls())
source("./R/figures/LBG_plots.R") #generate individual LBG plots
rm(list=ls())
source("./R/figures/heat_map_plots.R") #summary heat map graphic of LBGs
rm(list=ls())

#calculate residuals
source("./R/figures/heat_map_residual_plots.R")
rm(list=ls())

#Calculate displacement
source("./R/subscripts/calculate_sld.R")
rm(list=ls())
source("./R/figures/SLD_plot.R")
rm(list=ls())
source("./R/figures/SLD_type_plot.R")
rm(list=ls())
source("./R/figures/SLD_type_plot_all.R")
rm(list=ls())

#Pearson tests
source("./R/subscripts/calculate_pearson.R")
rm(list=ls())
source("./R/figures/Pearson_plot.R")
rm(list=ls())

#KS tests
source("./R/subscripts/KS_test.R")
rm(list=ls())
source("./R/figures/KS_plot.R")
rm(list=ls())

#calculate maximum palaeolatitudinal richness
source("./R/subscripts/get_max.R")
rm(list=ls())
source("./R/subscripts/format_get_max_table.R")
rm(list=ls())
source("./R/figures/max_plot.R")
rm(list=ls())
# produces Fig. S14

#linear models
source("./R/subscripts/linear_models_SSC.R")
rm(list=ls())
source("./R/subscripts/linear_models_MST.R")
rm(list=ls())


#global diversity
source("./R/subscripts/calculate_global_div.R") #calculate global diveristy
rm(list=ls())
source("./R/figures/global_div_plot.R") #generate global diversity plot
rm(list=ls())
source("./R/subscripts/linear_models_global.R") #generate global linear models
rm(list=ls())


