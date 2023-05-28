## Cleaning:
- One-hot-encoding for population type
- Grades letter was replaced by normalized numeric representation, so that sorting and comparison of grades is easier later
- smoothed data for samplesizes by creating bins and replacing values by median of the contents of the bin (had to use Fisher-Jenks algorithm because normal binning was not appropriate for the shape of the data)
## Imputation:
- Missing grade was replaced with C+ according to instructions from FiveThirtyEight
- Any missing value in "...poll_johnson" were replaced by global mean of the column
- Any missing value in "...poll_mcmullin " were replaced by zero because there were too many missing values to use global mean of the column

# Datasets version description:
|   |   |   |
|---|---|---|
| polls_us_election_2016_v2.csv | renamed form "polls_us_election_2016_clean" (Data cleaning and imputing)  | @EvaGostiuk |
| polls_us_election_2016_v3.csv | added columns "poll_wt" and "duration" | @alliselwah |
| polls_us_election_2016_v4.csv | merged polls_us_election_2016_v3.csv with "pollster-stats-full.xlsx" | @EvaGostiuk |
