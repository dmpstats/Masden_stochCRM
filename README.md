# Stochastic Collision Risk Modelling (sCRM)

This repo is initialised with the code from Masden's (2015) work for marine scotland, which is described here:
https://data.marine.gov.scot/dataset/developing-avian-collision-risk-model-incorporate-variability-and-uncertainty-r-code

The zip-file of the code can be downloaded directly [here](https://data.marine.gov.scot/sites/default/files//Masden%202015%20code_FinalVersion.zip)

## Motivation
The code is largely presented "as is", with no particular plan for improvement beyond non-trival bugs. This code formed the basis of further work in developing a user interface, as found here, albeit in markedly altered form:
https://www2.gov.scot/Topics/marine/marineenergy/mre/current/StochasticCRM

Fixes for some non-trivial bugs found in the course of the above work are propagated here to the Masden code, for parties interested in maintaining this, or using it for other works.

## Contents

* There is the original Masden code (in code_FinalVersion/code_FinalVersion as per the unzipped files).
* The folder _comparison with Band spreadsheet_ contains:
    * a copy of the Masden code
    * a copy of the Band spreadsheet, set to caclulate for Northern Gannets
    * a file with the subsequent estimates from this (assuming 98% avoidance) for options 1 to 3
    * a file `BandMode_comparison_file.R` based on Masden's `BandModel.r` but with parameters to match the spreadsheet
    * files within the _data/_ directory that also replicate the values in the Band spreadsheet

In short, you can run `BandMode_comparison_file.R` and it will present the collision estimates and compare them to those from the Band spreadsheet. At this juncture (after some minor fixes as propagated to the main code files in code_FinalVersion/ on 11/12/19) there seems to be a quite consistent small downwards bias on the estimates of about -0.7%.

# References:
Masden, E. (2015) Developing an avian collision risk model to incorporate variability and uncertainty. [R computer code]

