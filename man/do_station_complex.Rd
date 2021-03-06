% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/do_station_complex.R
\name{do_station_complex}
\alias{do_station_complex}
\title{Run MFAST with more options}
\usage{
do_station_complex(path, sheader = "t0", nwbeg = 5, fdmin = 0.3,
  fdmax = 8, t_win_freq = 3, tlagmax = 1, Ncmin = 5, Mmax = 15,
  snrmax = 3, t_win_snr = 3, t_err = 0.02, filtnum = 3,
  type = "normal", filter = NULL, tvelpath = NULL, tvel = ak135_alp,
  suffe = ".e", suffn = ".n", suffz = ".z", zerophase = TRUE,
  no_threads = NULL, mc.preschedule = TRUE, downsample = FALSE,
  biglong = FALSE, Iendian = 1)
}
\arguments{
\item{path}{Path to folder}

\item{sheader}{SAC header the S-wave pick is stored in}

\item{nwbeg}{number of start times tested}

\item{fdmin}{Minimum allowed dominant frequency}

\item{fdmax}{Maximum allowed dominant frequency}

\item{t_win_freq}{Window to calculate the dominant frequency (s)}

\item{tlagmax}{Maximum allowed time delay (s)}

\item{Ncmin}{Minimum number of points in an acceptable cluster}

\item{Mmax}{maximum number of clusters}

\item{snrmax}{Minimum snr allowed for a good filter}

\item{t_win_snr}{Window for SNR  (s)}

\item{t_err}{Modification to t_win_snr to account for error in S-pick  (s)}

\item{filtnum}{Number of filters to test}

\item{type}{Which of the MFAST default settings and filters to use. If a P-wave pick is present, type="verylocal" uses it to set t_win_snr}

\item{filter}{User defined set of filters (this overrides the filter selected with type).}

\item{tvelpath}{Path to a .tvel file containing the velocity model (overrides tvel)}

\item{tvel}{A tvel file read with readtvel (ak135_alp and ak135_taupo are already loaded)}

\item{suffe}{Suffix of east component}

\item{suffn}{Suffix of north component}

\item{suffz}{Suffix of vertical component}

\item{no_threads}{Number of threads to run measurements on. Set to 1 for verbose mode. Defaults to the number of cores}

\item{downsample}{Downsample if sampling rate is less than 0.01s (Defaults to FALSE, originally used to decrease computational loads)}

\item{biglong}{logical, TRUE=long=8 bytes}

\item{Iendian}{Endian-ness of the data: 1,2,3: "little", "big", "swap". Default = 1 (little)}
}
\value{
A dataframe containing the summary file
}
\description{
Run shear wave splitting measurements on a folder of events with more options
}
\examples{
# Run on measurements the normal sample data with defaults
write_sample("~/mfast/sample_data/raw_data")
do_station_complex(path="~/mfast/sample_data/raw_data")

# Run measurements with your own defined filters
filt_low <- c(0.1,0.2,0.5)
filt_high <- c(1,2,3)
filts <- cbind(filt_low,filt_high)
write_sample("~/mfast/sample_data/raw_data")
do_station_complex(path="~/mfast/sample_data/raw_data",filter=filts)
}
