# analytics

Scripts to process NeoTree data.

## merge-data.R

This is the main script to be run. It calls the other scripts in this directory.

It has some things which should be edited on to run it on your system:
* Working directory
* Path to json files
* Old json files (i.e. those previously processed)
* Old database (i.e. of matched admission/discharge pairs and unmatched discharges)
* Old unmatched admissions

## conversion-functions.R

Where most of the code for processing the json, finding matches between admission and discharges etc. is. These functions are currently (25/10/19) quite byzantine and complex and would ideally be improved. 

## plot-generation.R

Code to plot the data generated.

## summary-statistics.R

Code to produce summary statistics e.g. admissions per week by healthcare worker. 
