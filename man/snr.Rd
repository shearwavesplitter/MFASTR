% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fn_snr.R
\name{snr}
\alias{snr}
\title{S-wave SNR}
\usage{
snr(E, N, s, p = -12345, dt, t_win_snr = 3, t_err = 0.05, b = 0,
  type = "normal")
}
\arguments{
\item{E}{Vector signal of the east component}

\item{N}{Vector signal of the north component}

\item{s}{S-wave pick time}

\item{p}{P-wave pick time}

\item{dt}{Sample interval}

\item{t_win_snr}{Window for SNR (s)}

\item{t_err}{Modification to t_win_snr to account for error in S-pick (s)}

\item{type}{If type is set to "verylocal" then the P-wave pick (if present) is used to set t_win_snr}
}
\value{
Signal to noise ratio around the S-wave pick
}
\description{
Determine the signal to noise ratio around the S-wave pick (workhorse of filter_spread)
}
