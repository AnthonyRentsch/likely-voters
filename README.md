# Developing improved likely voter models for presidential election polling

This repository contains all of the scripts I will use for the analysis portion of my thesis project. 

The thesis project is being completed during the 2017-2018 academic year to satisfy the University of Massachusetts Amherst Commonwealth Honors College requirements.

# Data

* Cooperative Congressional Election Study (CCES) surveys from 2008, 2012, and 2016, as well as the cumulative file. Find that data [here](https://cces.gov.harvard.edu/).
* Catalist turnout data (Professor Brian Schaffner has access to an academic subscription to Catalist, which he has generously allowed me use for this project).

# Scripts

`load.R` - Read in, clean, and pool data from CCES. Read in Catalist data.

`raw-vote-intent.R` - Script where I look at how self-reported voting intentions changed among different racial groups between 2012 and 2016.

`cutoff_models.Rmd` - Markdown file where I create/define cutoff likely voter models and evaluate their performance on an individual level and use them to make election predictions. View the output [here](http://htmlpreview.github.io/?https://github.com/AnthonyRentsch/thesis_LikelyVoters/blob/master/cutoff_models.html).

`probabalistic_models.Rmd` 