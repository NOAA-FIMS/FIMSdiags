# FIMSdiags

[![R-CMD-check](https://github.com/NOAA-FIMS/FIMSdiags/actions/workflows/call-r-cmd-check.yml/badge.svg)](https://github.com/NOAA-FIMS/FIMSdiags/actions/workflows/call-r-cmd-check.yml)

FIMSdiags is a companion package to [FIMS](https://github.com/NOAA-FIMS/FIMS). It allows users to take a FIMS model and run common diagnostic analyses on their models. FIMSdiags is in active development and we are continuously adding features and improving current ones. Currently, the available diagnostics in the package are:

-   Retrospective analysis

-   Likelihood profile

## Learning FIMSdiags

Each FIMSdiags function has a vignette describing how to apply them to a FIMS model. These are available in the <a href="https://noaa-fims.github.io/FIMSdiags/articles/index.html" target="_blank">articles</a> section of the FIMS github.io website.

The help documentation for each function is available within R via a command like `?FIMSdiags::run_fims_likelihood`.

## Installing FIMSdiags  

Install the most recent version of FIMSdiags using the following code:

``` R
install.packages("remotes")
remotes::install_github("NOAA-FIMS/FIMSdiags")
```
or using {renv}: 
```R
renv::install("NOAA-FIMS/FIMSdiags")
# Update lockfile once package is installed
renv::snapshot()
```

## Getting Help 

If you encounter a bug, please open an [issue](https://github.com/NOAA-FIMS/FIMSdiags/issues) and provide a minimal reproducible example. If you have a question or an idea for future development, feel free to post to the [Discussion Board](https://github.com/orgs/NOAA-FIMS/discussions). We welcome all ideas and suggestions. 

## NOAA Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

Software code created by U.S. Government employees is not subject to copyright in the United States (17 U.S.C. section 105). The United States/Department of Commerce reserve all rights to seek and obtain copyright protection in countries other than the United States for Software authored in its entirety by the Department of Commerce. To this end, the Department of Commerce hereby grants to Recipient a royalty-free, nonexclusive license to use, copy, and create derivative works of the Software outside of the United States.

------------------------------------------------------------------------

<img src="https://raw.githubusercontent.com/nmfs-general-modeling-tools/nmfspalette/main/man/figures/noaa-fisheries-rgb-2line-horizontal-small.png" alt="NOAA Fisheries" height="75"/>

[U.S. Department of Commerce](https://www.commerce.gov/) \| [National Oceanic and Atmospheric Administration](https://www.noaa.gov) \| [NOAA Fisheries](https://www.fisheries.noaa.gov/)