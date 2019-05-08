# Build mathPerfConf project (run R code and build website in docs/)

library(rmarkdown)
library(radix)

# Delete all existing html files
clean_site()
file.remove("README.md")

# Build website (including rendering all .Rmd files)
render_site(envir = new.env())

# Produce analysis documents for all experiments (from _analyze.Rmd)
for (i in 1:6) {
  render(
    input = "_analyze.Rmd", 
    params = list(experiment = i),
    output_file = paste0("docs/Experiment-", i, ".html")
  )
  # Ensure all analyses start in clean environment
  rm(list = ls())
}

# Create README.md
render("index.Rmd", output_format = "github_document", output_file = "README.md")
