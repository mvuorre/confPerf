Metacognition and mathematics performance
================
true
2019-05-08

Data and analyses related to Vuorre & Metcalfe (*“Metacognition about
Mathematics is Excellent but the Relation between Performance and
Resolution is Confounded by Guessing”*; in prep).

  - Data from all six experiments are in a single table at
    `data/merged.csv`
  - Analyses are documented as an [R
    Markdown](https://rmarkdown.rstudio.com/)
    ([Radix](https://rstudio.github.io/radix/)) website at
    `docs/index.html`, which can be viewed at
    <https://mvuorre.github.io/mathPerfConf>
  - The materials are archived on OSF at <https://osf.io/np3cu/>, and
    github at <https://github.com/mvuorre/mathPerfConf>

## Reproduce

To reproduce these analyses, copy all files to a local drive. Activate
the `mathPerfConf` R Project in R Studio, and run `build.R`. The
analysis code is contained in `.Rmd` R Markdown files, which can also be
executed individually in interactive mode.

## Data description

``` r
dat <- read_csv("data/merged.csv", guess_max = 25000)
glimpse(dat, 1)
## Observations: 24,576
## Variables: 10
## $ exp      <dbl> …
## $ subject  <chr> …
## $ session  <dbl> …
## $ item     <chr> …
## $ standard <chr> …
## $ response <dbl> …
## $ key      <dbl> …
## $ accuracy <dbl> …
## $ con      <dbl> …
## $ con6     <dbl> …
```

We binned confidence ratings to six bins (Experiments 1-3 already had
six-point ratings, 4 & 6 were slider ratings, 5 had a 0-10 Likert
scale). The raw confidence ratings are variable `con`, binned ratings
are variable `con6`.
