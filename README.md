# UN Pipeline Overview

**TO DO: check BB8 model runs properly, determine which countires need UR stratification, create info files

**Waiting on: CSDE info (data pulls + cluster use), MICS implementation, benchmarking in BB8 model

## Summary

This document provides detailed instructions for obtaining direct, smoothed direct, and urban/rural stratified Beta-binomial estimates for U5MR and NMR, as well as maps and figures of these results.

## The Data Source

Explain how to get on to CSDE, how to download what you need

## The Pipeline

#### Step 0: 
Make sure your R version is at least 4.1.0 (also RStudio is using R 4.1.0 or more recent version). Otherwise, installation of dependent packages might fail.

#### Step 1: 
Create a new directory and put the entire "Rcode" folder and the script, "create_folder.R"

#### Step 2: 
Inside this same directory, create a folder called 'Info' and put downloaded info file in (need more instruction)

#### Step 3:
Again in the same directory, create a folder called 'Data' 1) put HIV data in folder, 2) create a subfolder with name of country and put downloaded IGME estimates in that folder

#### Step 4: 
In RStudio, run create_folder.R. This will create the necessary file structure. *Make sure to specify the country of interest*

#### Step 5:
Get dhsFlat and dhsStata for each survey year and GADM shapefiles from database (more info later)

#### Step 6: 
Run each script from the "Rcode" folder in the following order. This will process the data and calculate urban fractions for the stratified BB8 model. *Make sure to specify the country of interest where it is requested at the top each script!!*

1. DataProcessing.R

2. ur_prop.R (add a lot more info)

3. UR_thresholding.R

#### Step 7:
Run Direct_SmoothDirect.R. These script will calculate direct and smoothed direct estimates for (U5MR) and generate figures for them (polygon and spaghetti plots). *Make sure to specify the country of interest where it is requested at the top of each script*

#### Step 8:
Run BB8.R (add a lot more info after model is figured out)






