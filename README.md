# Neotree analytics scripts

Scripts to process data from [NeoTree](http://www.neotree.org/), an Android app designed for managing neonatal patients in low-resource settings.

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

## custom-functions.R

Previously called 'conversion-functions.R'. Where most of the code for processing the json, finding matches between admission and discharges etc. is. These functions are currently (25/10/19) quite byzantine and complex and would ideally be improved. 

## statistics.R

All code to generate statistics and graphs for healthcare workers (e.g. number of admissions per week).
* Called by merge-data.R.
* Calls monthly_report.Rmd and sister_in_charge_report.Rmd to automatically generate these outputs.

## monthly_report.Rmd
RMarkdown script to generate Powerpoint presentation for monthly healthcare worker report. Essentially some preamble about the project for new healthcare workers then just one graph per slide (all plots generated in statistics.R).

## sister_in_charge_report.Rmd
RMarkdown script to generate HTML page with key statistics for the Sister in Charge of the neonatal unit. This report is then printed on the unit each month.

## report_template.pptx
Template Powerpoint presentation to be called my monthly_report.Rmd for better formatting. Currently not used and only contains NeoTree logo. Might be useful in the future.

## plot-generation.R

Now deleted.

## summary-statistics.R

Now deleted.
