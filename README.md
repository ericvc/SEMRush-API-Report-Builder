SEMRush API Report Builder
================

## Installation

This R Shiny app provides a user interface to craft customized queries to the SEMRush SEO database. It is powered by the `semRush` package for R. The `semRush` package contains R functions that generate database queries, sends queries to the SEMRush datavase via REST API, and returns the resulting data back into the R environment for further analyses.

To launch, simply download this GitHub repo to local storage. If you open the file `app.R` using the RStudio IDE, you can run the program by selecting the `Run App` button near the top of the script. Otherwise, ensure you have an interactive R session, and use the command `runApp()` to start.

## Features

Users can select one of five report "topics" (advertising, keywords, etc.) to begin. The sidebar menu options will update dynamically in response to the user selections. This ensures only relevant options for report types, domain/keyword inputs, and export columns are presented to the user. The datatables returned by queries can be exported as a `.CSV` file.

## semRush

To download `semRush` as a standalone R package, visit the package [GitHub repository](github.com/ericvc/semRush) or perform a remote installation using `devtools::github_install('ericvc/semRush')`
