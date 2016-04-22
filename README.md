# timeless
A Bayesian Network model for clustering time-course sequencing data, implemented in Matlab and R



The scripts uploaded here are an example of a model assuming a linear trajectory of cell lineages (time-points) with 3 histone modifications measured at 4 time-points. 

The file "allCounts.txt" is an example input file with sample data showing average values across some promoter regions in mouse.

To follow this example, run the 4 scripts in the following order:

1- getNormalizedFoldChange.r -- This will produce two files: allCountsNorm.txt and allFold.txt. This is an R script

2- learnModel.m -- This will produce one file: model-11.mat. This is a Matlab script

3- getClusters.m -- This will produce one file: classes-11.txt. This is a Matlab script

4- plotValues.r -- This will produce one file: vals-11.pdf. This is an R script


"classes-11.txt" contains the cluster assignments for the regions found in "allCountsNorm.txt". "vals-11.pdf" shows plots of the average values for regions in each cluster. Note that since the input file is a very small example set with only about 100 regions, the output you might get from running it through this pipeline will not be sensible.


To run those scripts, you need the BNT Matlab toolbox (note that you need to adjust the path to it in the two Matlab scripts). You also need the R packages "limma" and "psych" (for plotting).
