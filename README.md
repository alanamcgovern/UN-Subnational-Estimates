# UN Pipeline Overview

## Summary

This document provides detailed instructions for obtaining direct, smoothed direct, and urban/rural stratified Beta-binomial estimates for U5MR and NMR, as well as maps and figures of these results.

## The Data Source

Explain how to get on to CSDE, how to download what you need

## The Pipeline

#### Step 0: Prepare R
Make sure your R version is at least 4.1.0 (also RStudio is using R 4.1.0 or more recent version). Otherwise, installation of dependent packages might fail.

#### Step 1: Setting up file structure

- Create a new directory and put the entire "Rcode" folder and the script, "create_folder.R"
- Inside this same directory, create a folder called 'Info' and put downloaded info file in (need more instruction)
- In RStudio, run create_folder.R. This will create the necessary file structure. *Make sure to specify the country of interest*


#### Step 2: Downloading Data

- Put the HIV, IGME, and UR_sampling folders in the newly created 'Data' folder (info on where to find these)
- Download data folder for country of interest (should contain DHS data and GADM files)


#### Step 3: Data Processing

Run the 'DataProcessing.R' script from the 'Rcode' folder. *Make sure to specify the country of interest at the top of the script!!*

#### Step 4: Direct Estimates

Run Direct_SmoothDirect.R. This script will calculate direct and smoothed direct NMR and U5MR estimates and generate figures. *Make sure to specify the country of interest where it is requested at the top of the script*

#### Step 5: Addressing Stratification

- Obtain Admin-1 level urban fraction (See 'UR fraction' folder)
- Run UR_thresholding.R to obtain urban/rural sampling weights to be used in the Beta-binomial model.

#### Step 6: Beta-Binomial Estimates

- Run BB8.R and *Make sure to specify the country of interest at the top of the script!!*. This will fit U5MR and NMR models at the National, Admin-1, Admin-2 levels and draw benchmarked and un-benchmarked posterior estimates.






