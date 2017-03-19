### MFASTR: An R package to make automatic shear wave splitting measurements

This package is based on the MFAST (Multiple Filter Automatic Splitting Technique) codes. It executes the same fortran executables as the original codes but within an R framework.

The original codes can be found [here](http://mfast-package.geo.vuw.ac.nz/) are the the property of the authors therein. If you are not familiar with the codes then review them and their documentation. MFASTR attempts replicate their functionality. 

This package is still currently in the early stages of development so it is recommended that you use the original codes. This package has not been tested for multiple use cases and it not ready for use.

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

To source the MFASTR functions (you must do this everytime you begin a new session)
```r
library(MFASTR)
```

To update MFASTR

```r
library(devtools)
install_github("shearwavesplitter/MFASTR")
```
#### Example

To run measurements on events in a target directory. Picks must be relative to the start of the trace and the P-wave pick must be in the 'a' header on the Z component.


```r
library(MFASTR)
path <- "~/path/to/folder"
do_station_simple(path)
```

To run on the MFAST sample data first create an empty directory

```r
library(MFASTR)
path <- "~/path/to/emptyfolder"
write_sample(path)
do_station_simple(path)
```

Or, for the very local sample data (this will not match the original MFAST sample results due to a bug in MFAST v2.2 -- see closed issues)

```r
library(MFASTR)
path <- "~/path/to/emptyfolder"
write_sample(path,type="verylocal")
do_station_simple(path,type="verylocal",sheader="t5")
```

#### do_station_simple options

Both the alpine fault (default) and TVZ velocity models are built into the package. To use the TVZ velocity model

```r
path <- "~/path/to/folder"
do_station_simple(path,tvel=ak135_taupo)
```

To use your own velocity model you must input the path to the .tvel file

```r
path <- "~/path/to/folder"
tvp <- "~/path/to/tvel/model.tvel"
do_station_simple(path,tvelpath=tvp)
```

To use the verylocal variation of the codes/filters

```r
path <- "~/path/to/folder"
do_station_simple(path,type="verylocal")
```

By default the Spick is assumed to be in the t0 header. To change this to t2 (for example)

```r
path <- "~/path/to/folder"
do_station_simple(path,sheader="t2")
```

If you want to run the measurements on more than the best three filters

```r
path <- "~/path/to/folder"
x <- 5 #run measurements on the five best filters
do_station_simple(path,filtnum=x)
```

And finally, you can define the suffixes of your files to be something other than the default .e, .n, .z of MFAST 

```r
path <- "~/path/to/folder"
E <- ".HHE"
N <- ".HHN"
Z <- ".HHZ"
do_station_simple(path,suffe=E,suffn=N,suffz=Z)
```
