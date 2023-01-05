## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(ympes)
breaks_to_interval(breaks = c(0L, 1L, 5L, 15L, 25L, 45L, 65L))

## -----------------------------------------------------------------------------
cut_ages(ages = 0:9, breaks = c(0L, 1L, 5L, 15L, 25L, 45L, 65L))
cut_ages(1:10, breaks = 6L)
x <- cut_ages(1:100, breaks = c(0L, 1L, 5L, 15L, 25L, 45L, 65L))
str(x)
head(x$interval)

## -----------------------------------------------------------------------------
# by default counts are split equally across ages within intervals
split_interval_counts(
    lower_bounds = c(0L, 5L, 10L),
    upper_bounds = c(5L, 10L, 20L),
    counts = c(5L, 10L, 30L)
)

# Population weightings to apply for individual years can be specified by
# the weights argument. If these are specified, they must be of length
# `max_upper` and represent weights in the range 0:(max_upper - 1).
max_upper <- 20L
weights <- integer(max_upper)
weights[c(TRUE, FALSE)] <- 1L
split_interval_counts(
    lower_bounds = c(0L, 5L, 10L),
    upper_bounds = c(5L, 10L, 20L),
    counts = c(5L, 10L, 30L),
    max_upper = max_upper,
    weights <- weights
)

## -----------------------------------------------------------------------------
# default ages generated as 0:(length(counts) - 1L) if only counts provided.
aggregate_age_counts(counts = 1:65, breaks = c(0L, 1L, 5L, 15L, 25L, 45L, 65L))

# Values below the minimum break are counted as NA
aggregate_age_counts(counts = 1:65, breaks = 50L)

# NA ages are also handled with their own grouping
ages <- 1:65
ages[1:44] <- NA
aggregate_age_counts(
    counts = 1:65,
    ages = ages,
    breaks = c(0L, 1L, 5L, 15L, 25L, 45L, 65L)
)

## -----------------------------------------------------------------------------
# census data
data(pop_dat)
pop_dat

# each row is for the same region so discard for moment
dat <- subset(pop_dat, select = c(age_category, value))

# extract upper and lower bounds
dat <- transform(
    dat,
    lower_bound = as.numeric(sub("\\[([0-9]+), .+)", "\\1", age_category)),
    upper_bound = as.numeric(sub(".+, (.+))", "\\1", age_category))
)

head(dat, n=10)

# recategorise based on ages
with(
    dat,
    reaggregate_interval_counts(
        lower_bounds = lower_bound,
        upper_bounds = upper_bound,
        counts = value,
        breaks = c(0L, 1L, 5L, 15L, 25L, 45L, 65L),
        max_upper = 100L,
        weights = NULL
    )
)

