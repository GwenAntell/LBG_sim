#run analyses

#spatial sampling
source("./R/subscripts/generate_shallow_marine_grids.R") #generate shallow marine masks
rm(list=ls())
source("./R/subscripts/bin_collections.R") #bin collections for sampling and computing SSC
rm(list=ls())
source("./R/subscripts/calculate_ssc.R") #calculate SSC
rm(list=ls())
source("./R/figures/heat_map_SSC_plots.R") #plot SSC
rm(list=ls())
source("./R/subscripts/calculate_mst.R")
rm(list=ls())
source("./R/figures/heat_map_MST_plots.R") #plot SSC
rm(list=ls())
source("./R/figures/combine_sampling_plots.R") #combine spatial sampling plots
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
source("./R/subscripts/compute_LBGs.R") #compute LBGs for simulated data
rm(list=ls())
source("./R/subscripts/sample_LBGs.R") #sample and compute LBGs for simulated data
rm(list=ls())
source("./R/subscripts/rarefy_LBGs.R") #rarefy sampled simulated data
rm(list=ls())
source("./R/subscripts/compile_data.R") #compiled LBG data
rm(list=ls())
source("./R/figures/LBG_plots.R") #generate individual LBG plots
rm(list=ls())
source("./R/figures/heat_map_plots.R") #summary heat map graphic of LBGs
rm(list=ls())

#Frechet distance
source("./R/subscripts/calculate_frechet.R")
rm(list=ls())

#KS tests
source("./R/subscripts/KS_test.R")
rm(list=ls())

#slope estimates
source("./R/subscripts/slope_estimates.R") #calculate slope estimates
rm(list=ls())
source("./R/figures/slope_plot.R") #calculate slope estimates
rm(list=ls())

#linear models
source("./R/subscripts/linear_models_SSC.R")
rm(list=ls())
source("./R/subscripts/linear_models_MST.R")
rm(list=ls())


#global diversity
source("./R/subscripts/calculate_global_div.R") #calculate global diveristy
rm(list=ls())
source("./R/figures/global_div_plot.R") #generate global diveristy plot
rm(list=ls())


