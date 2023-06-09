% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_basic_stats_functions.R
\name{agencies_populous_title_stats}
\alias{agencies_populous_title_stats}
\title{Agency most populous titles and their composition}
\usage{
agencies_populous_title_stats(data, top_n_titles = 1)
}
\arguments{
\item{data}{dataframe. Pass the full or subset of the cleaned dataset using \code{payeqfunctions::clean_data()} to this function}

\item{top_n_titles}{numeric. Number of top populous titles per agency. Default is set to 1}
}
\value{
Dataframe of most populous titles per agency with their details
}
\description{
Finding the most populous title(s) in every agency and the title's general demographic breakdowns.
}
\details{
TASKS that use this:
\itemize{
\item Agency Findings
}

NOTE: This function will ONLY work if the data input is generated by \code{payeqfunctions::clean_data()} due to the naming scheme.
}
\examples{
\dontrun{
# Agency Findings
agencies_populous_title_stats(data = cleaned_dataset)
}
}
