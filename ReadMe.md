### MFASTR: An R package to make automatic shear wave splitting measurements

This package is based on the MFAST (Multiple Filter Automatic Splitting Technique) codes. It executes the same fortran executables as the original codes but within an R framework.

The original codes can be found [here](http://mfast-package.geo.vuw.ac.nz/) are the the property of the authors therein. If you are not familiar with the codes then review them and their documentation. MFASTR attempts replicate their functionality. 

This package is still currently in the early stages of development so it is recommended that you use the original codes This package has not been tested for multiple use cases and it not ready for use.

#### Installation

First install the devtools package

```r
install.packages("devtools")
```

And finally install MFASTR

```r
library(devtools)
install_github("shearwavesplitter/MFASTR")
```

To source the MFASTR functions
```r
library(MFASTR)
```

To update MFASTR

```r
library(devtools)
install_github("shearwavesplitter/MFASTR")
```