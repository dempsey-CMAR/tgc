% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_in_growing_seasons_single.R
\name{filter_in_growing_seasons_single}
\alias{filter_in_growing_seasons_single}
\title{Keep observations that occur during growing seasons}
\usage{
filter_in_growing_seasons_single(
  dat,
  full_season = TRUE,
  trend_threshold = 4,
  superchill_threshold = -0.7,
  max_season = 540
)
}
\arguments{
\item{dat}{Data frame with at least three columns: \code{TIMESTAMP} (must be
possible to convert to POSIXct), \code{DEPTH}, and \code{VALUE}. If column
\code{VARIABLE} is included, it must have one unique entry. May also
include columns with grouping variables passed to \code{...}. Other columns
will be ignored.}

\item{full_season}{Logical argument. The default, \code{full_season = TRUE}
will only return data for groups with a full season of data (e.g.,
\code{START_SEASON} and \code{END_SEASON} as returned by
\code{identify_growing_seasons(dat, full_season = TRUE)} are both NOT
\code{NA}).

If \code{full_season = FALSE}, data for all groups will be returned;
the \code{START_SEASON} and \code{END_SEASON} dates default to the first
and last timestamp in \code{...} and \code{DEPTH}.}

\item{trend_threshold}{The threshold for "trending up". Default is
\code{trend_threshold = 4}. The last observation above
\code{trend_threshold} that does not return below \code{trend_threshold}
triggers the beginning of the growing season for each \code{DEPTH} and
group in \code{...}.}

\item{superchill_threshold}{The threshold for "superchill". Default is
\code{superchill_threshold = -0.7}. The first observation below
\code{superchill_threshold} triggers the end of the growing season for each
group in \code{DEPTH} and \code{...}.}

\item{max_season}{The length of the growing season in months for groups that
have no temperature observations below \code{superchill_threshold}. Default
is \code{max_season = 540} days (~18 months). Note: units are days because
adding 18 months to August 30 or August 31 results in \code{NA} (because
February 30 and February 31 are not real dates).}
}
\value{
Returns \code{dat}, filtered to keep observations that occur during
growing seasons identified in \code{identify_growing_seasons}, with an
additional column \code{SEASON} to label the seasons ("S1", "S2", ...).
Observations may be duplicated in consecutive seasons.
}
\description{
Keep observations that occur during growing seasons
}
\details{
Keeps observations that occur during growing seasons as defined by
\code{identify_growing_seasons()} (i.e., filters out observations that do
not occur during the growing seasons).

This function only works for a single \code{STATION}. For multiple
\code{STATION}s, use \code{filter_in_growing_seasons()}.

The growing season starts when the temperature crosses
\code{trend_threshold} and does not return below \code{trend_threshold}
(e.g., 4-degrees trending up) and ends 1 minute before the first
observation of \code{superchill_threshold}.

The function looks for \code{START_SEASON} in January to August to avoid
temperature drops caused by hurricanes (typically in September and October)
and Winter cooling (November, December).

The function looks for \code{END_SEASON} in January to May of the next year
(spring superchill).
}
