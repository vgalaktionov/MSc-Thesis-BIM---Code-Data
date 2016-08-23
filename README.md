# MSc Thesis BIM - Code & Data

This repository contains all the data and R code needed to reproduce the results for my MSc thesis. The thesis itself is included as pdf file.

`datasets.zip` should be extracted into a folder named `datasets`.

The `library.R` file contains all the shared functions used in the calculations.
This needs to be loaded first.

Other R source files are one-off scripts used to:

* determine PV installed capacity
* calculate descriptives
* answer all research questions
* generate graphs

It is *highly* recommended to run this code under OSX or Linux. 
Under Windows, several functions will not take advantage of multicore processors.
