
<!-- README.md is generated from README.Rmd. Please edit that file -->

# broadcastPower

<!-- badges: start -->

Compute broadcast power and compare with FCC data. <!-- badges: end -->

The simplest part of this package is the function `FCCquery`. That
creates a character string that begins
`https://transition.fcc.gov/fcc-bin/` and the uses
[`xml2::read_html`](https://xml2.r-lib.org/reference/read_xml.html) to
extract information on all the broadcasters with `dist km` of a (lat,
lon) location.

More sophisticated parts of this package will process fast fourier
transforms of signal strenth for each broadcaster measured by
appropriate test equipment.

## Installation

You can install the development version of broadcastPower from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sbgraves237/broadcastPower")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(broadcastPower)
(FCCstr1 <- FCCqueryString())
#> [1] "https://transition.fcc.gov/fcc-bin/fmq?list=4&dist=80&dlat2=38&mlat2=54&slat2=12.0000000000095&NS=N&dlon2=-77&mlon2=0&slon2=-36.0000000000184&EW=W"
```

This returns all the broadcasters within 80 km of FCC headquarters in
Washington, DC. The function `FCCquery` will then pass that to
`xml2::read_html` to download the desired information and parse it into
a `data.frame`.
