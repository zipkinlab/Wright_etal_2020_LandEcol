# A hierarchical analysis of habitat area, connectivity, and quality on amphibian diversity across spatial scales

### Alexander D. Wright, Evan H. Campbell Grant, and Elise F. Zipkin

### Landscape Ecology

### Code/Data DOI:

### Please contact the first author for questions about the code or data: Alexander D. Wright (adwright@msu.edu)

________________________________________________________________________________________________________________________________________

## Abstract:

*Context*

Habitat fragmentation can alter species distributions and lead to reduced diversity at multiple scales. Yet, the literature describing fragmentation effects on biodiversity patterns is contradictory, possibly because most studies fail to integrate spatial scale into experimental designs and statistical analyses. Thus, it is difficult to extrapolate the effects of fragmentation to large-scaled systems in which conservation management is of immediate importance.

*Objectives*

To examine the influence of fragmentation on biodiversity across scales, we (1) estimated the effects of habitat area, connectivity, and quality at both local (i.e. community) and regional (i.e. metacommunity) scales; and (2) evaluated the direction, magnitude, and precision of these estimates at both spatial scales.

*Methods*

We developed a multi-region community occupancy model to analyze 13 years (2005-2017) of amphibian monitoring data within the National Capital Region, a network of U.S. National Parks.

*Results*

Overall, we found a positive effect of park size and a negative effect of isolation on species richness at the park-level (i.e. metacommunity), and generally positive effects of wetland area, connectivity, and quality on species richness at the wetland-level (i.e. community), although parameter estimates varied among species. Covariate effects were less precise, but effect sizes were larger, at the local wetland-level as compared to the park-level scale.

*Conclusions*

Our analysis reveals how scale can mediate interpretation of results from scientific studies, which might help explain conflicting narratives concerning the impacts of fragmentation in the literature. Our hierarchical framework can help managers and policymakers elucidate the relevant spatial scale(s) to target conservation efforts.

## Analysis Code
1. [MRCM.R](https://github.com/lxwrght/Wright_etal_InReview_LandEcol/blob/master/MRCM.R): R Code to run the multiregion community occupancy model (via JAGS through R).
2. [MRCM.txt](https://github.com/lxwrght/Wright_etal_InReview_LandEcol/blob/master/MRCM.txt): Multiregion community occupancy JAGS model.
3. [MRCM_Results-Plots.R](https://github.com/lxwrght/Wright_etal_InReview_LandEcol/blob/master/MRCM_Results-Plots.R): Post analysis script for published tables, results, and figures.

## Data
1. [MRCM_data.R](https://github.com/lxwrght/Wright_etal_InReview_LandEcol/blob/master/MRCM_data.R) - An R object that contains all formatted data objects used in "MRCM.R" and "MRCM_Results-Plots.R", including:
   - Formatted array of detection data: X
   - Standardized Covariate Data: Park_area (area of each park), Forest_cov (percent forest cover of each park), Isol (percent Isolation of each park), Site_area (area of each wetland), Hydro_state (hydroperiod of each wetland), Conn (connectivity of each wetland), Precip (cumulative precipitation of each park), Cond (conductivity of each wetland), JDay (julian date)
   - Covariate Data (not standardized): Cond_unscaled, Conn_unscaled, Site_area_unscaled, Forest_cov_unscaled, Isol_unscaled, JDay_unscaled, Park_area_unscaled
   - Model notation values: J (# of sites), minJ (index of first site of each region), maxJ (index of last site of each region), K (# of replicates for site by year), Y (# of years), minY (first year a park was surveyed), I (# of species), M (# of zeroes for data augmentation), R (# of parks) 

