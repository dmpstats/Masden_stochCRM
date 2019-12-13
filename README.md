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

In short, you can run `BandMode_comparison_file.R` and it will present the collision estimates and compare them to those from the Band spreadsheet. If the Band spreadsheet is considered the benchmark, then the "bias" of the Masden code can be calculated. At this juncture (after some minor fixes as propagated to the main code files in code_FinalVersion/ on 11/12/19) there seems to be a quite consistent small downwards bias on the estimates for option 1 and 2 of about -0.7%.

__Option 3__: There is a fundamental difference between the Masden and Band treatments for option 3 calculations. Masden employ a relationship between the wind-speed and the pitch to determine rotor speed, whereas the Band spreadsheet has a constant specified. To make these comparable, the functional relationship in the Masden code can be skipped by setting `bandParameterisation <- T` in the main R file, where the various other user inputs lie. With this in place, there is about a 0.4% upwards bias in the option 3 calculations. 

# References:
Masden, E. (2015) Developing an avian collision risk model to incorporate variability and uncertainty. [R computer code]

