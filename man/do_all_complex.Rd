% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_do_all_complex.R
\name{do_all_complex}
\alias{do_all_complex}
\title{Run MFAST on multiple stations with more options}
\usage{
do_all_complex(path, sheader = "t0", nwbeg = 5, fdmin = 0.3, fdmax = 8,
  t_win_freq = 3, tlagmax = 1, Ncmin = 5, Mmax = 15, snrmax = 3,
  t_win_snr = 3, t_err = 0.02, filtnum = 3, type = "normal",
  filter = NULL, tvelpath = NULL, tvel = ak135_alp, suffe = ".e",
  suffn = ".n", suffz = ".z", zerophase = TRUE, no_threads = NULL,
  biglong = FALSE, Iendian = 1)
}
\arguments{
\item{path}{Path to folder containing folders with events}

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

\item{biglong}{logical, TRUE=long=8 bytes}

\item{Iendian}{Endian-ness of the data: 1,2,3: "little", "big", "swap". Default = 1 (little)}
}
\value{
A dataframe containing a summary of all the stations
}
\description{
Run shear wave splitting measurements on multiple folders/stations
}
\examples{
# Run on measurements three folders of the normal sample data
write_sample("~/mfast/sample_data/raw_data")
write_sample("~/mfast/sample_data/raw_data2")
write_sample("~/mfast/sample_data/raw_data3")
do_all_complex(path="~/mfast/sample_data")
}
