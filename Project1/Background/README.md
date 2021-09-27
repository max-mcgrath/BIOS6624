# Project 1
## Data Overview
This project is a secondary data analysis of the Multicenter AIDS
Cohort Study, an ongoing prospective cohort study of the natural and treated
histories of HIV-1 infection in homosexual and bisexual men in 4 major cities in
the United States.

Highly active antiretroviral treatment (HAART) is the standard treatment for HIV
infected patients.  Our dataset includes up to 8 years of longitudinal
laboratory and quality of life measures, as well as demographic and other health
information, on HIV infected men after beginning HAART.  The subjects were seen
annually.  Year 0 data are from the subjects’ last untreated visit, just before
beginning HAART.  All other visits (year 1 up to 8) are on treatment.  

## Question of Interest
We are interested in understanding how treatment response
2 years after initiating HAART differs between subjects who report using hard
drugs, such as heroine and cocaine, at baseline and other subjects, who did not
report hard drug use at baseline.

## Biologic Motivation
There is limited evidence from laboratory in vitro and
animal studies that the use of hard drugs inhibits the immune system and
increases HIV replication; however, results have not been clear in human
studies.  If drug users have poor treatment response compared to others, we may
need to consider more aggressive treatment strategies or more actively encourage
patients to enroll in drug rehabilitation programs.

## Information about variables
We have 4 measures of treatment response.  The
first two are laboratory measures, viral load (VLOAD), which is the number of
HIV copies in a mL of blood, and the second is CD4+ T cell count (LEU3N), a
measure of immunologic health.  In untreated HIV infection, viral load increases
over time and CD4+ T cell counts decline as the immune system is attacked by the
virus.  Once treatment is initiated, we expect viral load to decrease rapidly
and CD4 counts to recover.  Our last two measures are quality of life measures
from the SF-36.  The first is the aggregate physical quality of life score
(AGG_PHYS) and the second is the aggregate mental quality of life score
(AGG_MENT).  These scores range from 0 to 100, with higher scores indicating
better quality of life.  We are not sure what happens to quality of life after
initiating treatment - while in theory subjects’ improving health should result
in increased quality of life, the side effects of these treatments are
significant.  If subjects experience declines in quality of life after
initiating treatment we would be concerned that they would stop treatment.

Some of our colleague’s statistical team has been doing their analysis Bayesian.
We are requesting that the analysis be done in both a Bayesian and non-Bayesian
framework and compared.
