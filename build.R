# Build mathPerfConf project (run R code and build website in docs/)

library(rmarkdown)
library(radix)

# Delete all existing html files
clean_site()

# Build website (including rendering all .Rmd files)
render_site(envir = new.env())

# Produce analysis documents for all experiments (from _analyze.Rmd)
for (i in 1:6) {
  render(
    # envir = new.env(),
    input = "_analyze.Rmd", 
    params = list(experiment = i),
    output_file = paste0("docs/Experiment-", i, ".html")
  )  
}
