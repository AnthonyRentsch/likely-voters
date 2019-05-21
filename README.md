# The Elusive Likely Voter: Improving Electoral Predictions with More Informed Vote Propensity Models

# Abstract

 > Political commentators have offered evidence that the “polling misses” of 2016 were caused by a number of factors. This project focuses on one explanation, that likely voter models – tools used by pre-election pollsters to predict which survey respondents are most likely to make up the electorate and, thus, whose responses should be used to calculate election predictions – were flawed. While models employed by different pollsters vary widely, it is difficult to systematically study them because they are often considered part of pollsters’ methodological black box. In this paper, we use Cooperative Congressional Election Study surveys since 2008 to build a probabilistic likely voter model that not only takes into account the stated intentions of respondents to vote, but also other demographic variables that are consistently strong predictors of both turnout and over-reporting. Using this model, which we term the Perry-Gallup and demographics (PGaD) approach, we show that we are able to reduce the bias and error created by likely voters model to a negligible amount. This likely voter approach uses variables that pollsters already collect for weighting purposes and thus should be relatively easy to implement in future elections.
 
 
# Overview

This repository is a collection of all of the work that has been done for this project. This project started out as my undergraduate thesis during the 2017-2018 academic year at the University of Massachusetts Amherst. Since then, the project has evolved in a number of ways. 

* It's been presented at the annual conferences for the [American Association of Public Opinion Research](https://aapor.secure-platform.com/a/solicitations/10/sessiongallery/129/application/2280) (AAPOR) in 2019 and the [Society for Political Methodology](https://www.cambridge.org/core/membership/spm/about-us/awards/advanced-empirical-research-on-politics-for-undergraduates-program-aeropup) (SPM) in 2018.
* It's been used to create predictions for the [2018 midterm elections](https://www.washingtonpost.com/news/monkey-cage/wp/2018/11/10/these-5-charts-explain-who-voted-how-in-the-2018-midterm-election/?noredirect=on&utm_term=.85aefbdf179f) using CCES data.
* It's currently under review at a political science journal.

This project has been supported by SPM's [Advanced Empirical Research on Politics for Undergraduates Program](https://www.cambridge.org/core/membership/spm/about-us/awards/advanced-empirical-research-on-politics-for-undergraduates-program-aeropup) (AERoPUP). I've also received travel funding from AAPOR via their [Student Travel Award](https://www.aapor.org/Conference-Events/Awards/Award-Winners.aspx).


# Folders

**thesis** - original scripts, mostly markdown files, used for my thesis project

**journal** - scripts used for journal article

**cces2018** - script to produce vote propensity scores for 2018 CCES and a csv of respondent ID and predictions (note: this was published *before* the 2018 midterm election)

**writing_presentations** - presentations of various versions of this project

**misc** - various exploratory analyses


# Data

* Cooperative Congressional Election Study (CCES) surveys from 2008, 2012, and 2016, as well as the cumulative file. Find that data [here](https://cces.gov.harvard.edu/).
* Catalist turnout data (Brian Schaffner has access to an academic subscription to Catalist, which he has generously allowed me use for this project). This was only used for exploratory analyses.
