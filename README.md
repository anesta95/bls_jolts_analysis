# BLS Job Openings and Labor Turnover Survey Analysis

This project contains code that downloads, analyzes, and visualizes data
from the [Job Openings and Labor Turnover Survey](https://www.bls.gov/jlt/) (JOLTS) 
that is run monthly by the U.S. Department of Labor's Bureau of Labor Statistics. 

Data analysis and visualization is executed by the [R](https://www.r-project.org/) 
code in the `analysis.R` and `functions.R` files in the top-level directory.
Project libraries and other resources needed to run the code can be managed through 
the [renv](https://rstudio.github.io/renv/) reproducible environment in the `renv`
folder and `renv.lock` and `.Rprofile` files in the top-level directory.

Details about data measures and data and graphics files are provided below.

## Data Measures

This project currently analyzes the following [data estimates](https://www.bls.gov/opub/hom/jlt/concepts.htm) from JOLTS:

* __Job openings__: includes all positions that are open on the last business day of the reference month. A job is open only if it meets all three of these conditions:
  * A specific position exists and there is work available for that position. The position can be full-time or part-time, and it can be permanent, short-term, or seasonal.
  * The job could start within 30 days, whether or not the employer can find a suitable candidate during that time.
  * The employer is actively recruiting workers from outside the establishment to fill the position. Active recruiting means that the establishment is taking steps to fill a position. It may include advertising in news­papers, on television, or on the radio; posting internet notices, posting “help wanted” signs, networking or making word-of-mouth announcements; accepting applications; interviewing candidates; contacting employment agencies; or soliciting employees at job fairs, state or local employment offices, or similar sources.
* __Hires__: include all additions to the payroll during the entire reference month, including new and rehired employees; full-time and part-time employees, permanent, short-term, and seasonal employees; employees who were recalled to a job at the location following a layoff (formal suspension from pay status) lasting more than 7 days; on-call or intermittent employees who returned to work after having been formally separated; workers who were hired and separated during the month, and transfers from other locations.
* __Quits__: include employees who left voluntarily with the exception of retirements or transfers to other locations.
* __Layoffs and discharges__: include involuntary separations initiated by the employer including layoffs with no intent to rehire; layoffs (formal suspension from pay status) lasting or expected to last more than 7 days; discharges resulting from mergers, downsizing, or closings; firings or other discharges for cause; terminations of permanent or short-term employees; and terminations of seasonal employees (whether or not they are expected to return the next season).
* __Unemployment persons per job opening__: The [unemployed people per job opening ratio](https://www.bls.gov/opub/btn/volume-11/what-is-the-unemployed-people-per-job-openings-ratio-a-21-year-case-study-into-unemployment-trends.htm) is constructed by taking the number of unemployed people and dividing it by the number of job openings each month. These two components come from two different BLS surveys. The number of unemployed people is an estimate that comes from the [Current Population Survey](https://www.bls.gov/cps/) (CPS); the Local Area Unemployment Statistics (LAUS) program models CPS data to produce unemployment estimates for states. The job openings data for national and state estimates are from the Job Openings and Labor Turnover Survey (JOLTS).
* __Labor Leverage__: The [labor leverage ratio](https://www.bls.gov/opub/btn/volume-7/measuring-employer-and-employee-confidence-in-the-economy.htm) is the number of voluntary separations (quits) divided by the number involuntary separations (layoffs and discharges). This ratio provides a measure to gauge employers’ and employees’ confidence in the economy. A value greater than 1.0 (when quits exceed layoffs and discharges) indicates that employee confidence is strong, while a value less than 1.0 (when layoffs and discharges exceed quits) indicates that employees are not so confident.

Each data measure visualized in this project in this project is the [measure rate](https://www.bls.gov/help/def/jt.htm#rate/level) rather than measure level. The job openings rate is computed by dividing the number of job openings by the sum of employment and job openings and multiplying that quotient by 100. The other data element rates (hires, quits, layoffs and discharges) are computed by dividing the data element level by employment and multiplying that quotient by 100. 

## Data Files and Graphics

### Data conventions
Every econ analysis data CSV file at minimum will have the following columns:
* `date`: The date associated with the data in the data row. The date will be in `YYYY-MM-DD` format regardless of the time period the date captures. All dates will be the first day of the time period. For example, data for April 2025 will be displayed as `2025-04-01`. Data for Q2 2025 will be `2025-04-01`. Data for the year 2025 will be `2025-01-01`. This will have a data type `double` with a class of `Date`.
* `date_period_text`: The time period that each row of the data captures. The most common formats are `Monthly`, `Quarterly`, and `Annually`. This will have a data type and class of `character`.
* `value`: The value that is being measured in the data. This will have a data type of `double` and a class of `numeric`.
* `data_element_text`: What the data in the `value` column describes. This will have a data type and class of `character`.
* `data_measure_text`: The mathematical expression the data in the `value` column is expressed as. The most common are `Level`, `Rate`, `Ratio`, `Percentage`, `Proportion`, and `Index`. This will have a data type and class of `character`.
* `date_measure_text`: The change in dates measured by the data in the `value` column. The most common are `Current`, `Year-over-year`, `Month-over-month` and `Quarter-over-quarter`. This will have a data type and class of `character`.
* `data_transform_text`: Any mathematical transformations applied to the data. The most common are `Raw`, `Percent change`, `Annualized`, `Trail N` where `N` is a number of periods in the `date_period_text` column. There can be multiple transformations for each row. Transformations are delimited by semi-colons `;` and are stated _in order of transformation_. For example, `Trail 3;Percent Change` will be the percentage change between the trailing 3 period average of the current period — denoted in the `date` column — and the trailing 3 period average of the previous period which is deduced from the `date_measure_text`. Conversely, `Percent Change;Trail 3` will be the trailing 3 period average applied to the percentage change between the current period and the previous period across the data series. This will have a data type and class of `character`.
* `geo_entity_type_text`: The geographic entity _type_ the data in the `value` column is covering. This will have a data type and class of `character`. If the region is in the United States there is a good chance it will be within the [Census Bureau Geographic Entity Hierarchy](https://www2.census.gov/geo/pdfs/reference/geodiagram.pdf).
* `geo_entity_text`: The name(s) geographic entity/entities that are described by the data.
* `viz_type_text`: The type of visualization made by the data in the `value` column. The most common are `Time series line`, `Bar`, `Map`, and `Scatter`. This will have a data type and class of `character`.

### Naming conventions
All graphics are PNG files in the `charts` directory. Every data visualization 
has a corresponding CSV file that was used to create it in the `data` directory.
Both CSVs and PNGs are named with the following format where each aspect of the 
data is delimited with a dash `-` and spaces are replaced with underscores `_`.

Data and visualization files will be named in the following order:

1. `date`
2. `date_period_text`
3. `data_element_text`
4. `data_measure_text`
5. `date_measure_text`
6. `data_transform_text`
7. `geo_entity_type_text`
8. `geo_entity_text`
9. _Any other aspects of the data specific to the release that are needed to uniquely identify it._ Examples include `industry_text`, `size_class_text`, `seas_adj_text`, among others.
10. `viz_type_text`

#### Examples
* CSV file: `2025-04-01-monthly-quits-rate-current-2_data_transform-nation-us-total_nonfarm-all_size_classes-seasonally_adjusted-time_series_line.csv`
* PNG file: `2025-04-01-monthly-quits-rate-current-2_data_transform-nation-us-total_nonfarm-all_size_classes-seasonally_adjusted-time_series_line.png`

Every column in the dataset with the `_text` suffix will be included in the filename, in addition to the `date` column. Data files will also include columns that have further information that is _not_ needed to uniquely identify the data series. Examples of this include the `value` column, variables with the `_code` suffix such as `industry_code`, `fips_code`,`preliminary_code`, as well as `moe`, and `moe_level`, among others. 


This specific repository will have data with the following variables:
### Included data

| Variable Name     | Variable Data Class | Variable Description                                                                                                                                                                                                                                                                                                                                       |
| ----------------- | ------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| date              | Date                | Date associated with data row. Will be in `YYYY-MM-DD` format. Monthly data will automatically be coded as the first day of said month, i.e. January 2025 is `2025-01-01`                                                                                                                                                                                  |
| data_element_text | character           | The [data element](https://download.bls.gov/pub/time.series/jt/jt.dataelement) that is represented by the `value` column. One of `Hires`, `Job openings`, `Labor leverage ratio`, `Layoffs and discharges`, `Quits`, or `Unemployed persons per job opening ratio`.                                                                                        |
| data_measure_text       | character           | The description of the what the numerical value the data in the `value` column is measuring. One of `Rate`, `Ratio`, or `Level`.                                                                                                                                                                                                                           |
| date_period_text  | character           | The time period that each row of the data captures. This will be `monthly`.                                                                                                                                                                                                                                                                                |
| date_measure_text | character           | The change in dates measured by the data in the value column. These will include `Current`, `Year-over-year`, or `Month-over-month`.                                                                                                                                                                                 |
| data_transform_text       | character           | The description of what mathematical transformation(s) have been applied to the data in the `value` column. Multiple transformations delimited by semi-colons `;`. Can be `Raw`, `Trail 3`, or `Trail3;Percent Change`                                                                                                                                                                                                                           |
| geo_entity_type_text  | character           | The geographic entity type that is present in the `geo_entity_text` column. This will be either `Nation` or `State`.                                                                                                                                                                                                                                                    |
| geo_entity_text       | character           | The name(s) geographic entity/entities that are described by the data. These are defined by the [U.S. Census Bureau](https://www2.census.gov/geo/pdfs/reference/geodiagram.pdf).                                                                                                                                                                                               |
| industry_text     | character           | The [NAICS supersector](https://www.bls.gov/sae/additional-resources/naics-supersectors-for-ces-program.htm) that the data is associated with. If multiple supersectors are included, the chart filename will denote with `N_industry` where `N` is the number of industries in the data file.                                                                                                                 |
| size_class_text   | character           | The [firm size class](https://download.bls.gov/pub/time.series/jt/jt.sizeclass) that the data is associated with. If multiple firm sizes are included, the chart filename will denote with `N_size_class` where `N` is the number of firm size classes in the data file.                                                                                                                                        |
| seas_adj_text     | character           | Text that will denote if the data in the `value` column is seasonally-adjusted or not.                                                                                                                                                                                                                                                                     |
| viz_type_text     | character           | The visualization type the data is used for. One of `ts_line`, `bar`, `map`, or `scatter` which stand for time series line chart, bar chart map and scatter plot.                                                                                                                                                                                          |
| state_abb         | character           | The two character [USPS state abbreviation](https://www.bls.gov/respondents/mwr/electronic-data-interchange/appendix-d-usps-state-abbreviations-and-fips-codes.htm) that the data is associated with. *This figure is only present in data files with a `viz_type_text` of `Scatter`. This figure is __not__ included in the data or chart filenames.* |
| fips_code         | character           | The two digit [FIPS code](https://www.bls.gov/respondents/mwr/electronic-data-interchange/appendix-d-usps-state-abbreviations-and-fips-codes.htm) that the data is associated with. *This figure is only present in data files with a `viz_type_text` of `Scatter`. This figure is __not__ included in the data or chart filenames.*                   |
| Layoffs Rate      | numeric             | The actual numeric value of the layoffs rate associated with the data. *This figure is only present in data files with a `viz_type_text` of `Scatter`. This figure is __not__ included in the data or chart filenames.*                                                                                                                                |
| Quits Rate        | numeric             | The actual numeric value of the quits rate associated with the data. *This figure is only present in data files with a `viz_type_text` of `Scatter`. This figure is __not__ included in the data or chart filenames.*                                                                                                                                  |
| value             | numeric             | The actual numerical value of data that is described by the columns with the `_text` suffix. *This figure is __not__ included in the data or chart filenames.*                                                                                                                                                                                             |