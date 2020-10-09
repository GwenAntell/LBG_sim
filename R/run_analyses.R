#run analyses

#spatial sampling
source("./R/subscripts/generate_shallow_marine_grids.R") #generate shallow marine masks
rm(list=ls())
source("./R/subscripts/bin_collections.R") #bin collections for sampling and computing SSC
rm(list=ls())
source("./R/subscripts/calculate_ssc.R") #calculate SSC
rm(list=ls())
source("./R/figures/heat_map_SSC_plots.R") #plot SSC

#frequency distributions
source("./R/subscripts/range_freq_dist.R") #range frequency distribution analyses
rm(list=ls())
source("./R/subscripts/occ_freq_dist.R") #occurrence frequency distribution analyses
rm(list=ls())

#LBG analyses
source("./R/subscripts/compute_LBGs.R") #compute LBGs for simulated data
rm(list=ls())
source("./R/subscripts/sample_LBGs.R") #sample and compute LBGs for simulated data
rm(list=ls())
source("./R/subscripts/rarefy_LBGs.R") #rarefy sampled simulated data
rm(list=ls())
source("./R/subscripts/compile_data.R") #compiled LBG data
rm(list=ls())
source("./R/figures/LBG_plots.R")
rm(list=ls())
source("./R/figures/heat_map_plots.R")
rm(list=ls())

source("./R/subscripts/calculate_sld.R") #calculate SLD
rm(list=ls())
source("./R/figures/SLD_type_plot.R") #SLD figure of LBG type
rm(list=ls())

source("./R/subscripts/slope_estimates.R") #calculate slope estimates
rm(list=ls())
source("./R/figures/slope_plot.R") #calculate slope estimates
rm(list=ls())

#global diversity
source("./R/subscripts/calculate_global_div.R") #calculate global diveristy
rm(list=ls())
source("./R/figures/global_div_plot.R") #generate global diveristy plot
rm(list=ls())


