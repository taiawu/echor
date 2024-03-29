% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distribute_across_source.R
\name{distribute_across_source}
\alias{distribute_across_source}
\title{Distribute transfers across all source wells of a given compound}
\usage{
distribute_across_source(transfers, .echo_drop_nL = 25)
}
\arguments{
\item{transfers}{a tibble containging the transfer steps, as calculated by get_transfer_steps()}

\item{.echo_drop_nL}{the tarnsfer drop volume for the echo to be used. Defaults to 25 nL.}
}
\value{
the transfers tibble, with transfers now distributed across all source wells containing that compound.
}
\description{
Distribute transfers across all source wells of a given compound
}
