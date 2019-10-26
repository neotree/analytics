# analytics

Scripts to process NeoTree data. 

Author: Liam Shaw

## merge-data.R

This is the main script to be run. It calls the other scripts in this directory.

It has some things which should be edited on to run it on your system:
* Working directory
* Path to json files
* Old json files (i.e. those previously processed)
* Old admissions
* Old discharges

The way the script works in brief is that it checks which json files have historically already been processed. It then only reads in and processes those that haven't been analysed before. Any new admissions and discharges that haven't been seen before are added to the old admissions and discharges files from previous run of the script. Matched pairs of admissions/discharges are then found. The final database is all matched admission/discharge pairs with unmatched discharges appended (some discharges will never have an associated admission). Each run of the script generates a random string (e.g. JMWJD3607G) which is used as a prefix for all output files to avoid overwriting. However, this may over time lead to a folder filling up with lots of files. 

## conversion-functions.R

Where most of the code for processing the json, finding matches between admission and discharges etc. is. These functions are currently (25/10/19) quite byzantine and complex and would ideally be improved. 

## plot-generation.R

Code to plot the data generated.

## summary-statistics.R

Code to produce summary statistics e.g. admissions per week by healthcare worker. 
