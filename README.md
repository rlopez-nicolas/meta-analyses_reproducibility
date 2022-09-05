# Analytical reproducibility of published meta-analyses on clinical psychological interventions

## General description
This repository contains materials, data, and code related to this [project](https://osf.io/79j2t)

## Files

• *manuscript.rmd* - RMD file that compiles the manuscript and runs all the necessary scripts. 

• *apa.csl* - Citation style files that produces the reference section

• *meta-analyses_reproducibility.bib* - Bibliographic file with all references. 

## Folders

### analysis 

R script files needed to carry out data preparation and analysis. They are run from the RMD *manuscript.rmd* file.

### data

Data files necessary for the whole process. They are subdivided according to the stage at which they are used. The data used at each stage are preserved to ensure the reproducibility of all results reported in the paper. 
In the sub-folder "corrected_data" are all the primary data files in their corrected version after the different stages. 

### results 

Contains the different results of each stage in the form of figures or tables. They are produced by the master script *manuscript.rmd*. 

