# The Elusive ‘Likely Voter’: Improving Prediction of Who Will Vote

This repository contains all of the scripts I will use for the analysis portion of my thesis project, which is being completed during the 2017-2018 academic year to satisfy University of Massachusetts Amherst Commonwealth Honors College requirements. 

This project is supported by the Society for Political Methodology's Advanced Empirical Research on Politics for Undergraduates Program (AERoPUP). Read more about that program [here](https://www.cambridge.org/core/membership/spm/about-us/awards/advanced-empirical-research-on-politics-for-undergraduates-program-aeropup).

# Data

* Cooperative Congressional Election Study (CCES) surveys from 2008, 2012, and 2016, as well as the cumulative file. Find that data [here](https://cces.gov.harvard.edu/).
* Catalist turnout data (Professor Brian Schaffner has access to an academic subscription to Catalist, which he has generously allowed me use for this project).

# Scripts

`load.R` - Read in, clean, and pool data from CCES. Read in Catalist data.

`raw-vote-intent.R` - Script where I look at how self-reported voting intentions changed among different racial groups between 2012 and 2016.

`national_models.Rmd` - Markdown file where I create/define likely voter models and evaluate their performance on an individual level and use them to make election predictions at the national level. View the output [here](https://github.com/AnthonyRentsch/thesis_LikelyVoters/blob/master/national_models.md).

`state_models.Rmd` - Similar to `national_models.Rmd` but goes state by state. View the output [here](https://github.com/AnthonyRentsch/thesis_LikelyVoters/blob/master/state_models.md).

`summary_graphics.R` - Script where I create four summary graphics for results/discussion section. Two compare results to all my modeling approaches and two act as mock-ups for how journalists or pollsters could use my work to when reporting on polls.

`analysis_test.Rmd` - Test markdown file for cutoff likely voter models. This file only uses 2016 data throughout and is meant to be a draft of my analysis.