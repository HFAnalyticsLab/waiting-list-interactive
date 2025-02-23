# Elective Care Waiting List Interactive Calculator

## Description

This repository includes code used to create The Health Foundation's [Waiting List Interactive Calculator](https://thehealthfoundation.shinyapps.io/waiting-list-interactive/) which was published in our long read [The NHS waiting list: when will it peak?](https://www.health.org.uk/features-and-opinion/features/the-nhs-waiting-list-when-will-it-peak). Full information on background, context, and methods can be found in the [technical appendix](https://www.health.org.uk/sites/default/files/2023-10/Waiting%20list%20technical%20appendix_0.pdf) and corresponding article. 

## Data sources

Data on referrals, completed pathways and the waiting list from April 2016 onward are taken from the [Referral to Treatment (RTT) Waiting Times dataset](https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/). Numbers with estimates for missing values for RTT pathways and waiting list size  were used.

Data on industrial action is taken from the [Potential industrial action in the NHS webpage](https://www.england.nhs.uk/publication/preparedness-for-potential-industrial-action-in-the-nhs/). This provides the number of elective procedures and outpatient appointments that were rescheduled as a result of industrial action.

## Requirements

These scripts were written in R version 4.0.2 and RStudio Version 1.1.383. 

Package requirements and versions have been tracked using the [renv](https://rstudio.github.io/renv/articles/renv.html) lockfile included in the repo. 

All data is publicly available (as described above). Data needed to run the app are included in the `data` folder of this repo, including the RTT data used and a lookup table on number of working days per month. All necessary code is included in this repo, although some scripts reference copyright images and font files which are not publicly available. 

## Contributors

Authors: Kathryn Marszalek, Melissa Co, Tim Gardner, Freya Tracey, Charles Tallack

Code in this repo contributed by: Melissa Co, Chris Beeley, Marc Brazzill, Tatjana Marks, Kathryn Marszalek, Freya Tracey

Special thanks to Ellen Coughlan and Paul Chappell for their support and insights.

## License

This project is licensed under the [MIT License](https://opensource.org/license/mit/).
