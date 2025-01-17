# Line graphs

## Time Series

From [this](https://bsky.app/profile/bencasselman.bsky.social/post/3l5hfgfbjs72n) Ben Casselman post

- **Job openings per month** seasonally adjusted with two lines: one of reported figure and the other of trailing 3/12 month average? 
  - Time period: Last year/3 years/5 years 
  - Dashed horizontal line: non-recession average? 
- **Job openings rate**: seasonally adjusted vacancies per 1,000 jobs: one of reported figure and the other of trailing 3/12 month average? 
  - Time period: Last year/3 years/5 years 
  - Dashed horizontal line: non-recession average? 
  - ~~Does this need additional data sources of payroll employment from [BLS CES](https://www.bls.gov/ces/)?~~ ← No, there is a rate for this in the JOLTS data 
- **Job openings per unemployed worker**: seasonally adjusted. adjusted for misclassification?? 
  - Time period: Last year/3 years/5 years 
  - Dashed horizontal line: non-recession average? 
  - ~~Does this need additional data sources of payroll employment from [BLS CPS](https://www.bls.gov/cps/)?~~ ← No, there is a rate for this in the JOLTS data
- **Hiring overall & rate per 1,000 employees**: seasonally adjusted, one of reported figure & other of trailing 3/12 month average?
  - Time period: Last year/3 years/5 years
  - Dashed horizontal line: non-recession average?
- **Quits overall & rate per 1,000 employees**: seasonally adjusted, one of reported figure & other of trailing 3/12 month average?
  - Time period: Last year/3 years/5 years
  - Shaded recession periods
  - Dashed horizontal line: non-recession average?
- **Quits and layoff rates relative to their prepandemic averages**
  - Time period: last year/3 years/5 years
  - Shaded recession periods
  - Pandemic layoffs truncated

From [this](https://www.bls.gov/opub/btn/volume-7/measuring-employer-and-employee-confidence-in-the-economy.htm) BLS beyond the numbers piece.
"The ratio, which contrasts voluntary separations (Q) with involuntary separations (LD), provides a measure to gauge employers’ and employees’ confidence in the economy. The measure also can be used to analyze trends in employment levels in different industries...A value greater than 1.0 indicates that employee confidence is strong, while a value less than 1.0 indicates that employees are not so confident."

- **Quits/Layoffs rate** a.k.a the "labor leverage ratio": seasonally adjusted, one of reported figure & other of trailing 3/12 month average?
  - Shaded above/below solid line at 1 with captions of "employee optimism" v. "employee pessimism"

## Bar Graphs
- **Job openings rate by [NAICS supersector](https://www.bls.gov/sae/additional-resources/naics-supersectors-for-ces-program.htm)**
  - Latest month, seasonally adjusted
  - Include seasonally adjusted year-over-year change
- **Job openings per unemployed persons ratio by NAICS supersector**
  - Latest month, seasonally adjusted
  - Include seasonally adjusted year-over-year change
- **Labor Market "Churn"**
  - Year-over-year change in hiring rate & quits rate by NAICS supersector
- **Labor Market "Leverage"**
  - Current quits/layoffs ratio by NAICS supersector
  - Latest month, seasonally adjusted
  - Include seasonally adjusted year-over-year change

## Scatter Plots
From [this](https://www.bls.gov/charts/job-openings-and-labor-turnover/job-openings-unemployment-beveridge-curve.htm) BLS Graphic

- The Beveridge Curve (unemployment rate on x-axis vs. job openings rate on y-axis)
  - For latest non-recession business cycles (Dec. '01 to Nov. '07; Jul. '09 to Feb. '20; May '20 to current)
  
## Maps
- Latest & year-over-year change of 3 month trailing average of
  - **Hires**, **Job Openings**, **Layoffs & Discharges**, and **Quits** rate
  as well as **Labor Leverage** ratio by U.S. state

## Scatterplot
- X-axis: Quits rate, Y-axis: Layoffs & discharge rate by state with size of
bubble the industry size and color [Census region](https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf).
Label some of the outliers with [ggrepel](https://ggrepel.slowkow.com/.

