File Krall_speciation.csv was sent to JRKrall by EPA in 11/2018

1. dat-sep-year.sh: File to take Krall_speciation.csv and separate into years, e.g. Krall_speciation-2000.csv.  
2. ussa-fulldat.R: File to take yearly speciation data (e.g. Krall_speciation-2000.csv) and format together based on information we need (columns, MDL, detection limits) and ensure dates are correct.  Also creates more concise method variable.
    * Creates dataset for monitor summary
    * Creates dataset for constituents of interest
    * Creates dataset for methods summary
3. check-methods-data.R: Checks methods listed for each constituent.
4. check-speciation-data.R: Checks for qualifiers, methods, time, date
5. blanks.R: Takes all blanks data and takes average for each constituent/year to blank correct metals.
6. adjust-speciation.R: File to blank correct and adjust EC/OC from unadjusted TOT to TOR (based on technical manuals from Sunni).  
