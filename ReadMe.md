### MFASTR: An R package to make automatic shear wave splitting measurements

This package is based on the MFAST (Multiple Filter Automatic Splitting Technique) codes. It executes the same fortran executables as the original codes but within an R framework. As of version 1.4 MFASTR defaults to using zerophase filters and not downsampling waveforms. 

The original codes can be found [here](http://mfast-package.geo.vuw.ac.nz/) are the the property of the authors therein. If you are not familiar with the original codes then review them and their documentation. 

The aims of this package are to: increase computation speed (with parallel computing), remove dependance on external programs, have a simple installation, and provide flexibility for advanced users. 



#### Installation

First install the devtools package

```r
install.packages("devtools")
```

And finally install MFASTR

```r
devtools::install_github("shearwavesplitter/MFASTR")
```


To source the MFASTR functions (you must do this everytime you begin a new session)
```r
library(MFASTR)
```

To update MFASTR

```r
devtools::install_github("shearwavesplitter/MFASTR")

```

##### devtools won't install?
If you cannot install the devtools package you can download MFASTR directly from this github page, and zip it as a .tar.gz file
```r
install.packages(c("signal","circular","TauP.R"))
install.packages("MFASTR-master.tar.gz",repos= NULL,type="source")
```


#### Example

To run measurements on events in a target directory. The P-wave pick must be in the 'a' header on the Z component and the S-wave pick must be on the E component (and defaults to 't0'). Suffixes are automatically detected but must be the final letter of the filenames.


```r
library(MFASTR)
path <- "~/path/to/folder"
do_station_simple(path)
```

Measurements are run in parallel and defaults to automatically determining the best number of threads/cores to use. The no_threads option allows you to specify how many threads to use. Set no_threads=1 for verbose mode.

```r
library(MFASTR)
path <- "~/path/to/folder"
do_station_simple(path,no_threads=1) #Run in verbose mode with on thread on one core
```


To run on the MFAST sample data first create an empty directory
For comparison, the summary files for the sample data from the original MFAST can be found stored under the varibles lhor2 (for the normal sample) and BLO (for the verylocal sample).
NOTE: If you want identical results to MFAST then you must set downsample=TRUE and zerophase=FALSE. The results will not match exactly for verylocal version due to [a bug being fixed in MFASTR.](https://github.com/shearwavesplitter/issues/2)

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

Or to use the ak135 velocity model

```r
path <- "~/path/to/folder"
data(ak135)
do_station_simple(path,tvel=ak135)
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

To create the all6 plots for a particular station (if your event suffixes are different you have to redefine them here)

```r
path <- "~/path/to/folder"
all6_station(path)
```