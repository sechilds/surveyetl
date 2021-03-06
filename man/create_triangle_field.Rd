% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_triangle_field.R
\name{create_triangle_field}
\alias{create_triangle_field}
\title{Create Statistical Significance}
\usage{
create_triangle_field(df)
}
\arguments{
\item{df}{The data frame containing `p.value` and `effect_size`.}
}
\value{
The data frame with a `significance` column
  added.
}
\description{
Create a star indicator based on the p.value.
}
\details{
Statistical tests return p-values (in a column called
p.value - if you use the `broom` package) and
we also create `effect_size` using the `lsr` package.
this creates the variable `triangle` that
shows both the direction, the statistical significance
and the effect size.
}
