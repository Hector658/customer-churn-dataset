# Customer Churn Dataset & EDA

Synthetic dataset generation and exploratory data analysis for a simulated online sports betting platform, built to study customer segmentation, activity patterns, and retention.

This is the first stage of a multi-part portfolio project. The data generated here feeds two downstream repos:
- [customer-churn-etl](https://github.com/Hector658/Customer-Churn-ETL) — a Postgres + dbt pipeline built on top of this dataset
- *(software engineering project — coming soon)*

> **Project status:** Dataset generation and EDA complete.

## Contents

- `01_dataset_generation.ipynb` — generates the synthetic customers, bets, events, and participants data
- `02__EDA.R` — exploratory analysis: data quality checks, customer segmentation, temporal activity trends, and cohort retention

## Dataset

The dataset is synthetically generated and covers activity from 2024 to 2025. Four entities:

### Customers
`user_id`, `register_date`, `age`, `residence_country`, `payment_method`, `segment_client`, `active_days`, `annual_bets`, `n_bets`, `betting_dates`

Customers are grouped into four behavioral segments: **Recreational**, **Occasional**, **Seasonal**, and **High Value**.

### Bets
`user_id`, `placement_date`, `event_id`, `bet_type`, `market`, `participant_1`, `participant_2`, `selection`, `bet_amount`, `fee`, `result`, `bet_status`, `return_amount`, among others.

### Events
`event_id`, `event_date`, `sport`, `competition`, `home_team`, `away_team`, `home_strength`, `away_strength`, `result`, among others. Roughly 10,000 events across football, basketball, tennis, and baseball, spanning competitions like LaLiga, Premier League, Champions League, NBA, MLB, Liga MX, ATP, and WTA.

### Participants
`participant_id`, `participant_name`, `sport`, `competition`, `popularity_score`, `strength_score`, `home_advantage` — team/player-level attributes used to simulate realistic event outcomes.

## EDA highlights

The R script covers:
1. Data quality checks (missing values, duplicates, referential consistency between tables)
2. Customer segmentation (distribution by segment, country, payment method)
3. Temporal analysis: monthly activity trends, per-customer activity patterns, and cohort retention
4. An automated key-findings summary printed at the end of the script

During this analysis, several data consistency issues were identified and documented (e.g., mismatches between a customer's stated bet count and their actual transaction count) — these are used intentionally in the downstream ETL project to demonstrate automated data quality testing.

## Tech stack

- Python (pandas, numpy) — dataset generation
- R (tidyverse, lubridate, ggplot2) — exploratory analysis

## Disclaimer

This project uses synthetic data created for educational and portfolio purposes. Customer segments, betting behavior, and event characteristics are simulated assumptions and should not be interpreted as real-world statistics. No real customer information is used.
