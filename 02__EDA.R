# Customer Churn Analytics
# Exploratory Data Analysis
# R / Tidyverse

# ============================================================
# 1. Load packages
# ============================================================

library(arrow)
library(tidyverse)
library(lubridate)


# ============================================================
# 2. Load data
# ============================================================

bets <- read_parquet("bets.parquet")
customers <- read_parquet("customers.parquet")
events <- read_parquet("events.parquet")
participants <- read_parquet("participants.parquet")


# ============================================================
# 3. Data overview
# ============================================================

## 3.1 Dimensions ---------------------------------------------------
dim(customers)
dim(bets)
dim(events)
dim(participants)

## 3.2 Missing values ------------------------------------------------
colSums(is.na(customers))
colSums(is.na(bets))
colSums(is.na(events))
colSums(is.na(participants))

## 3.3 Duplicate rows -------------------------------------------------
sum(duplicated(customers))
sum(duplicated(bets))
sum(duplicated(events))
sum(duplicated(participants))

## 3.4 Consistency checks ----------------------------------------------

# n_bets (customers) vs bets counted from transactions
bets_by_customer <- bets %>%
    count(user_id, name = "bets_from_transactions")

customer_check <- customers %>%
    select(user_id, n_bets) %>%
    left_join(bets_by_customer, by = "user_id") %>%
    mutate(
        bets_from_transactions = replace_na(bets_from_transactions, 0),
        difference = n_bets - bets_from_transactions
    )

customer_check %>%
    summarise(
        customers = n(),
        exact_match = sum(difference == 0),
        mismatches = sum(difference != 0)
    )

# bets referencing events that don't exist / events with no bets
event_check <- bets %>%
    distinct(event_id) %>%
    anti_join(events %>% select(event_id), by = "event_id")

nrow(event_check)

events_without_bets <- events %>%
    anti_join(bets %>% distinct(event_id), by = "event_id")

nrow(events_without_bets)

# participants referenced in bets vs participants table
bet_participants <- bind_rows(
    bets %>% select(participant = participant_1),
    bets %>% select(participant = participant_2)
) %>%
    distinct()

participant_check <- bet_participants %>%
    anti_join(
        participants %>% select(participant_name),
        by = c("participant" = "participant_name")
    )

nrow(bet_participants)
nrow(participant_check)


# ============================================================
# 4. Customer segmentation
# ============================================================

## 4.1 Distribution across categorical fields --------------------------
customers %>% count(segment_client)
customers %>% count(residence_country, sort = TRUE)
customers %>% count(payment_method, sort = TRUE)

customers %>%
    count(segment_client) %>%
    mutate(percentage = n / sum(n) * 100)

## 4.2 Age / bets summary stats -----------------------------------------
customers %>%
    summarise(
        min_age = min(age),
        mean_age = mean(age),
        median_age = median(age),
        max_age = max(age),
        min_bets = min(n_bets),
        mean_bets = mean(n_bets),
        median_bets = median(n_bets),
        max_bets = max(n_bets)
    )

customers %>%
    group_by(segment_client) %>%
    summarise(
        customers = n(),
        mean_bets = mean(n_bets),
        median_bets = median(n_bets),
        min_bets = min(n_bets),
        max_bets = max(n_bets),
        mean_annual_bets = mean(annual_bets),
        median_annual_bets = median(annual_bets)
    )

## 4.3 Bet amount by segment -----------------------------------------------
bets %>%
    count(segment_client) %>%
    mutate(percentage = n / sum(n) * 100)

bets %>%
    group_by(segment_client) %>%
    summarise(
        total_bet_amount = sum(bet_amount, na.rm = TRUE),
        mean_bet_amount = mean(bet_amount, na.rm = TRUE),
        median_bet_amount = median(bet_amount, na.rm = TRUE)
    )

segment_summary <- bets %>%
    group_by(segment_client) %>%
    summarise(
        total_bet_amount = sum(bet_amount, na.rm = TRUE),
        total_bets = n()
    ) %>%
    left_join(
        customers %>% count(segment_client, name = "customers"),
        by = "segment_client"
    ) %>%
    mutate(bet_amount_per_customer = total_bet_amount / customers)

segment_summary

## 4.4 Plots ------------------------------------------------------------
customers %>%
    count(segment_client) %>%
    ggplot(aes(x = segment_client, y = n)) +
    geom_col() +
    labs(
        title = "Customers by Client Segment",
        x = "Client Segment",
        y = "Number of Customers"
    ) +
    theme_minimal()

ggplot(customers, aes(x = n_bets)) +
    geom_histogram(bins = 40) +
    labs(
        title = "Distribution of Number of Bets per Customer",
        x = "Number of Bets",
        y = "Number of Customers"
    ) +
    theme_minimal()

ggplot(customers, aes(x = segment_client, y = n_bets)) +
    geom_boxplot() +
    labs(
        title = "Number of Bets by Client Segment",
        x = "Client Segment",
        y = "Number of Bets"
    ) +
    theme_minimal()

bets %>%
    group_by(segment_client) %>%
    summarise(total_bet_amount = sum(bet_amount, na.rm = TRUE)) %>%
    ggplot(aes(x = segment_client, y = total_bet_amount)) +
    geom_col() +
    labs(
        title = "Total Bet Amount by Client Segment",
        x = "Client Segment",
        y = "Total Bet Amount"
    ) +
    theme_minimal()

ggplot(segment_summary, aes(x = segment_client, y = bet_amount_per_customer)) +
    geom_col() +
    labs(
        title = "Bet Amount per Customer by Segment",
        x = "Client Segment",
        y = "Bet Amount per Customer"
    ) +
    theme_minimal()


# ============================================================
# 5. Temporal analysis
# ============================================================

# ------------------------------------------------------------
# 5.1 Monthly activity
# ------------------------------------------------------------

monthly_activity <- bets %>%
    mutate(month = floor_date(as.Date(placement_date), "month")) %>%
    group_by(month) %>%
    summarise(
        active_customers = n_distinct(user_id),
        total_bets = n(),
        total_bet_amount = sum(bet_amount, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(
        bets_per_active_customer = total_bets / active_customers,
        bet_amount_per_active_customer = total_bet_amount / active_customers
    )

monthly_activity

monthly_activity %>%
    ggplot(aes(x = month, y = total_bets)) +
    geom_line() +
    labs(title = "Number of Bets Over Time", x = "Month", y = "Number of Bets") +
    theme_minimal()

monthly_activity %>%
    ggplot(aes(x = month, y = active_customers)) +
    geom_line() +
    labs(title = "Active Customers Over Time", x = "Month", y = "Active Customers") +
    theme_minimal()

monthly_activity %>%
    ggplot(aes(x = month, y = bets_per_active_customer)) +
    geom_line() +
    labs(title = "Average Bets per Active Customer", x = "Month", y = "Bets per Active Customer") +
    theme_minimal()

monthly_activity %>%
    ggplot(aes(x = month, y = bet_amount_per_active_customer)) +
    geom_line() +
    labs(title = "Monthly Bet Amount per Active Customer", x = "Month", y = "Bet Amount per Active Customer") +
    theme_minimal()

# Monthly activity broken down by segment
monthly_activity_segment <- bets %>%
    mutate(month = floor_date(as.Date(placement_date), "month")) %>%
    group_by(month, segment_client) %>%
    summarise(
        active_customers = n_distinct(user_id),
        total_bets = n(),
        total_bet_amount = sum(bet_amount, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(
        bets_per_customer = total_bets / active_customers,
        amount_per_customer = total_bet_amount / active_customers
    )

monthly_activity_segment %>%
    ggplot(aes(x = month, y = active_customers, color = segment_client)) +
    geom_line(linewidth = 1) +
    labs(
        title = "Monthly Active Customers by Segment",
        x = "Month", y = "Active Customers", color = "Segment"
    ) +
    theme_minimal()

monthly_activity_segment %>%
    ggplot(aes(x = month, y = bets_per_customer, color = segment_client)) +
    geom_line(linewidth = 1) +
    labs(
        title = "Monthly Bets per Active Customer",
        x = "Month", y = "Bets per Active Customer", color = "Segment"
    ) +
    theme_minimal()

monthly_activity_segment %>%
    ggplot(aes(x = month, y = amount_per_customer, color = segment_client)) +
    geom_line(linewidth = 1) +
    labs(
        title = "Monthly Bet Amount per Active Customer",
        x = "Month", y = "Bet Amount per Active Customer", color = "Segment"
    ) +
    theme_minimal()

# New registrations over time
customers %>%
    mutate(month = floor_date(register_date, "month")) %>%
    count(month) %>%
    ggplot(aes(x = month, y = n)) +
    geom_line() +
    labs(title = "New Customer Registrations Over Time", x = "Month", y = "New Customers") +
    theme_minimal()

# Seasonality (bets per year / month)
bets %>%
    mutate(
        month = floor_date(placement_date, "month"),
        year = year(placement_date)
    ) %>%
    count(year, month)


# ------------------------------------------------------------
# 5.2 Activity per customer
# ------------------------------------------------------------

## Engagement: time from registration to first bet ----------------------
first_bet <- bets %>%
    group_by(user_id) %>%
    summarise(first_bet_date = min(placement_date), .groups = "drop")

customer_engagement <- customers %>%
    select(user_id, register_date) %>%
    left_join(first_bet, by = "user_id") %>%
    mutate(
        days_to_first_bet = as.numeric(difftime(first_bet_date, register_date, units = "days")),
        ever_bet = !is.na(first_bet_date)
    )

customer_engagement %>%
    summarise(
        customers = n(),
        customers_with_bet = sum(ever_bet),
        customers_without_bet = sum(!ever_bet),
        mean_days = mean(days_to_first_bet, na.rm = TRUE),
        median_days = median(days_to_first_bet, na.rm = TRUE),
        min_days = min(days_to_first_bet, na.rm = TRUE),
        max_days = max(days_to_first_bet, na.rm = TRUE)
    )

customer_engagement %>%
    filter(ever_bet) %>%
    mutate(
        first_bet_bucket = case_when(
            days_to_first_bet == 0 ~ "Same day",
            days_to_first_bet <= 3 ~ "1-3 days",
            days_to_first_bet <= 7 ~ "4-7 days",
            days_to_first_bet <= 30 ~ "8-30 days",
            TRUE ~ "31+ days"
        )
    ) %>%
    count(first_bet_bucket)

# Does onboarding speed change by segment?
customer_engagement %>%
    left_join(customers %>% select(user_id, segment_client), by = "user_id") %>%
    filter(ever_bet) %>%
    group_by(segment_client) %>%
    summarise(
        customers = n(),
        mean_days = mean(days_to_first_bet),
        median_days = median(days_to_first_bet)
    )

customer_engagement %>%
    filter(ever_bet) %>%
    ggplot(aes(x = days_to_first_bet)) +
    geom_histogram(bins = 30) +
    labs(title = "Days to First Bet", x = "Days", y = "Customers") +
    theme_minimal()

customer_engagement %>%
    left_join(customers %>% select(user_id, segment_client), by = "user_id") %>%
    filter(ever_bet) %>%
    ggplot(aes(x = segment_client, y = days_to_first_bet)) +
    geom_boxplot() +
    labs(title = "Days to First Bet by Customer Segment", x = "Customer Segment", y = "Days to First Bet") +
    theme_minimal()

## Activity: betting frequency -------------------------------------------
customer_activity <- bets %>%
    mutate(bet_date = as.Date(placement_date)) %>%
    group_by(user_id) %>%
    summarise(
        total_bets = n(),
        active_days = n_distinct(bet_date),
        bets_per_active_day = total_bets / active_days,
        .groups = "drop"
    )

customer_activity %>%
    left_join(customers %>% select(user_id, segment_client), by = "user_id") %>%
    group_by(segment_client) %>%
    summarise(
        customers = n(),
        mean_bets = mean(total_bets),
        median_bets = median(total_bets),
        mean_active_days = mean(active_days),
        median_active_days = median(active_days),
        mean_bets_per_active_day = mean(bets_per_active_day),
        median_bets_per_active_day = median(bets_per_active_day)
    )

customer_activity %>%
    left_join(customers %>% select(user_id, segment_client), by = "user_id") %>%
    ggplot(aes(x = segment_client, y = total_bets)) +
    geom_boxplot() +
    scale_y_log10() +
    labs(title = "Bet Frequency by Customer Segment", x = "Customer Segment", y = "Total Bets (log scale)") +
    theme_minimal()

## Recency: time since last bet -------------------------------------------
max_bet_date <- max(bets$placement_date)

last_bet <- bets %>%
    group_by(user_id) %>%
    summarise(last_bet_date = max(placement_date), .groups = "drop") %>%
    mutate(days_since_last_bet = as.numeric(difftime(max_bet_date, last_bet_date, units = "days")))

last_bet %>%
    left_join(customers %>% select(user_id, segment_client), by = "user_id") %>%
    group_by(segment_client) %>%
    summarise(
        customers = n(),
        mean_days = mean(days_since_last_bet),
        median_days = median(days_since_last_bet),
        min_days = min(days_since_last_bet),
        max_days = max(days_since_last_bet)
    )

## Combined customer feature table -----------------------------------------
segment_behavior <- bets %>%
    group_by(user_id) %>%
    summarise(
        total_bet_amount = sum(bet_amount, na.rm = TRUE),
        average_bet_amount = mean(bet_amount, na.rm = TRUE),
        .groups = "drop"
    )

customer_features <- customer_engagement %>%
    select(user_id, days_to_first_bet) %>%
    left_join(customer_activity, by = "user_id") %>%
    left_join(last_bet %>% select(user_id, days_since_last_bet), by = "user_id") %>%
    left_join(segment_behavior, by = "user_id") %>%
    left_join(customers %>% select(user_id, segment_client), by = "user_id")

glimpse(customer_features)

customer_features %>%
    select(days_to_first_bet, days_since_last_bet, total_bets, active_days, bets_per_active_day) %>%
    cor(use = "complete.obs")

customer_features %>%
    summarise(
        min_bets = min(total_bets, na.rm = TRUE),
        q1_bets = quantile(total_bets, 0.25, na.rm = TRUE),
        median_bets = median(total_bets, na.rm = TRUE),
        mean_bets = mean(total_bets, na.rm = TRUE),
        q3_bets = quantile(total_bets, 0.75, na.rm = TRUE),
        p95_bets = quantile(total_bets, 0.95, na.rm = TRUE),
        p99_bets = quantile(total_bets, 0.99, na.rm = TRUE),
        max_bets = max(total_bets, na.rm = TRUE)
    )

customer_features %>%
    summarise(
        min_amount = min(total_bet_amount, na.rm = TRUE),
        q1_amount = quantile(total_bet_amount, 0.25, na.rm = TRUE),
        median_amount = median(total_bet_amount, na.rm = TRUE),
        mean_amount = mean(total_bet_amount, na.rm = TRUE),
        q3_amount = quantile(total_bet_amount, 0.75, na.rm = TRUE),
        p95_amount = quantile(total_bet_amount, 0.95, na.rm = TRUE),
        p99_amount = quantile(total_bet_amount, 0.99, na.rm = TRUE),
        max_amount = max(total_bet_amount, na.rm = TRUE)
    )

customer_features %>%
    summarise(
        min_recency = min(days_since_last_bet, na.rm = TRUE),
        q1_recency = quantile(days_since_last_bet, 0.25, na.rm = TRUE),
        median_recency = median(days_since_last_bet, na.rm = TRUE),
        mean_recency = mean(days_since_last_bet, na.rm = TRUE),
        q3_recency = quantile(days_since_last_bet, 0.75, na.rm = TRUE),
        p95_recency = quantile(days_since_last_bet, 0.95, na.rm = TRUE),
        p99_recency = quantile(days_since_last_bet, 0.99, na.rm = TRUE),
        max_recency = max(days_since_last_bet, na.rm = TRUE)
    )

customer_features %>%
    select(user_id, segment_client, total_bets, total_bet_amount) %>%
    arrange(desc(total_bet_amount)) %>%
    slice_head(n = 10)

customer_features %>%
    group_by(segment_client) %>%
    summarise(
        customers = n(),
        median_recency = median(days_since_last_bet, na.rm = TRUE),
        mean_recency = mean(days_since_last_bet, na.rm = TRUE),
        q3_recency = quantile(days_since_last_bet, 0.75, na.rm = TRUE),
        p95_recency = quantile(days_since_last_bet, 0.95, na.rm = TRUE)
    )

# Plots -----------------------------------------------------------------
ggplot(customer_features, aes(x = total_bets)) +
    geom_histogram(bins = 50) +
    labs(title = "Distribution of Total Bets per Customer", x = "Total Bets", y = "Customers") +
    theme_minimal()

ggplot(customer_features, aes(x = total_bet_amount)) +
    geom_histogram(bins = 50) +
    labs(title = "Distribution of Total Bet Amount per Customer", x = "Total Bet Amount", y = "Customers") +
    theme_minimal()

ggplot(customer_features, aes(x = days_since_last_bet)) +
    geom_histogram(bins = 50) +
    labs(title = "Distribution of Days Since Last Bet", x = "Days Since Last Bet", y = "Customers") +
    theme_minimal()

ggplot(customer_features, aes(x = days_since_last_bet)) +
    geom_histogram(bins = 50) +
    coord_cartesian(xlim = c(0, 50)) +
    labs(title = "Recency Distribution (0-50 Days)", x = "Days Since Last Bet", y = "Customers") +
    theme_minimal()

ggplot(customer_features, aes(x = days_since_last_bet, fill = segment_client)) +
    geom_histogram(bins = 40, alpha = 0.7) +
    facet_wrap(~segment_client, scales = "free_y") +
    labs(title = "Recency Distribution by Customer Segment", x = "Days Since Last Bet", y = "Customers") +
    theme_minimal()

ggplot(customer_features, aes(x = segment_client, y = days_since_last_bet)) +
    geom_boxplot() +
    labs(title = "Recency by Customer Segment", x = "Customer Segment", y = "Days Since Last Bet") +
    theme_minimal()

ggplot(segment_behavior %>% left_join(customers %>% select(user_id, segment_client), by = "user_id"),
       aes(x = total_bet_amount, color = segment_client)) +
    geom_histogram(bins = 50, fill = NA) +
    labs(title = "Bet Frequency vs Total Bet Amount", x = "Total Bet Amount", color = "Customer Segment") +
    theme_minimal()

customer_activity %>%
    left_join(segment_behavior, by = "user_id") %>%
    left_join(customers %>% select(user_id, segment_client), by = "user_id") %>%
    ggplot(aes(x = total_bets, y = total_bet_amount, color = segment_client)) +
    geom_point(alpha = 0.5) +
    labs(
        title = "Bet Frequency vs Total Bet Amount",
        x = "Total Bets", y = "Total Bet Amount", color = "Customer Segment"
    ) +
    theme_minimal()


# ------------------------------------------------------------
# 5.3 Cohort retention
# ------------------------------------------------------------

customer_cohort <- bets %>%
    group_by(user_id) %>%
    summarise(cohort_month = floor_date(min(placement_date), "month"), .groups = "drop")

cohort_activity <- bets %>%
    mutate(activity_month = floor_date(placement_date, "month")) %>%
    select(user_id, activity_month, segment_client) %>%
    distinct() %>%
    left_join(customer_cohort, by = "user_id") %>%
    mutate(
        months_since_cohort = interval(cohort_month, activity_month) %/% months(1)
    )

cohort_retention <- cohort_activity %>%
    group_by(cohort_month, months_since_cohort) %>%
    summarise(active_customers = n_distinct(user_id), .groups = "drop")

cohort_sizes <- cohort_retention %>%
    filter(months_since_cohort == 0) %>%
    select(cohort_month, cohort_size = active_customers)

cohort_retention <- cohort_retention %>%
    left_join(cohort_sizes, by = "cohort_month") %>%
    mutate(retention = active_customers / cohort_size)

cohort_retention

cohort_sizes %>%
    arrange(cohort_month)

cohort_retention %>%
    filter(months_since_cohort <= 6) %>%
    arrange(cohort_month, months_since_cohort)

# Retention matrix (cohorts from 2024 onward)
cohort_retention_main <- cohort_retention %>%
    filter(cohort_month >= as.POSIXct("2024-01-01"))

cohort_retention_main %>%
    select(cohort_month, months_since_cohort, retention) %>%
    tidyr::pivot_wider(
        names_from = months_since_cohort,
        values_from = retention,
        names_prefix = "M"
    )


# ============================================================
# 6. Key findings
# ============================================================

# Automated summary of the main findings, built from the objects already
# computed in the previous sections. Printed with cat() so it can be
# pasted directly into a report or email.

## 6.1 Data quality -------------------------------------------------------
n_customers <- nrow(customers)
n_mismatches <- sum(customer_check$difference != 0)
n_orphan_events <- nrow(event_check)
n_events_no_bets <- nrow(events_without_bets)
n_orphan_participants <- nrow(participant_check)

## 6.2 Segmentation ---------------------------------------------------------
top_segment_customers <- customers %>%
    count(segment_client, sort = TRUE) %>%
    slice_head(n = 1)

top_segment_value <- segment_summary %>%
    arrange(desc(bet_amount_per_customer)) %>%
    slice_head(n = 1)

## 6.3 Monthly activity -----------------------------------------------------
first_month <- min(monthly_activity$month)
last_month <- max(monthly_activity$month)

active_start <- monthly_activity %>% filter(month == first_month) %>% pull(active_customers)
active_end <- monthly_activity %>% filter(month == last_month) %>% pull(active_customers)
active_growth_pct <- (active_end - active_start) / active_start * 100

amount_start <- monthly_activity %>% filter(month == first_month) %>% pull(bet_amount_per_active_customer)
amount_end <- monthly_activity %>% filter(month == last_month) %>% pull(bet_amount_per_active_customer)
amount_growth_pct <- (amount_end - amount_start) / amount_start * 100

## 6.4 Activity per customer -------------------------------------------------
median_days_to_first_bet <- median(customer_engagement$days_to_first_bet, na.rm = TRUE)
pct_never_bet <- mean(!customer_engagement$ever_bet) * 100
median_total_bets <- median(customer_features$total_bets, na.rm = TRUE)
median_recency <- median(customer_features$days_since_last_bet, na.rm = TRUE)
pct_inactive_30d <- mean(customer_features$days_since_last_bet > 30, na.rm = TRUE) * 100

## 6.5 Cohort retention -------------------------------------------------------
retention_by_month <- cohort_retention_main %>%
    filter(months_since_cohort %in% c(1, 3, 6)) %>%
    group_by(months_since_cohort) %>%
    summarise(avg_retention = mean(retention, na.rm = TRUE), .groups = "drop")

get_retention <- function(m) {
    val <- retention_by_month %>% filter(months_since_cohort == m) %>% pull(avg_retention)
    if (length(val) == 0) NA_real_ else val
}

retention_m1 <- get_retention(1)
retention_m3 <- get_retention(3)
retention_m6 <- get_retention(6)

## 6.6 Print summary -----------------------------------------------------------
cat("\n===== KEY FINDINGS =====\n\n")

cat("-- Data quality --\n")
cat(sprintf("Total customers: %d\n", n_customers))
cat(sprintf("Customers with n_bets inconsistent vs transactions: %d\n", n_mismatches))
cat(sprintf("Bets referencing an event_id with no matching event: %d\n", n_orphan_events))
cat(sprintf("Events with no bets at all: %d\n", n_events_no_bets))
cat(sprintf("Participants in bets with no match in the participants table: %d\n\n", n_orphan_participants))

cat("-- Segmentation --\n")
cat(sprintf(
    "Segment with the most customers: %s (%d customers)\n",
    top_segment_customers$segment_client, top_segment_customers$n
))
cat(sprintf(
    "Segment with the highest bet amount per customer: %s (%.2f)\n\n",
    top_segment_value$segment_client, top_segment_value$bet_amount_per_customer
))

cat("-- Monthly activity --\n")
cat(sprintf("Period analyzed: %s to %s\n", first_month, last_month))
cat(sprintf("Change in active customers: %.1f%%\n", active_growth_pct))
cat(sprintf("Change in bet amount per active customer: %.1f%%\n\n", amount_growth_pct))

cat("-- Activity per customer --\n")
cat(sprintf("Median days to first bet: %.1f\n", median_days_to_first_bet))
cat(sprintf("Percentage of customers who never placed a bet: %.1f%%\n", pct_never_bet))
cat(sprintf("Median total bets per customer: %.0f\n", median_total_bets))
cat(sprintf("Median days since last bet (recency): %.1f\n", median_recency))
cat(sprintf("Percentage of customers inactive for more than 30 days: %.1f%%\n\n", pct_inactive_30d))

cat("-- Cohort retention (average across 2024+ cohorts) --\n")
cat(sprintf("Month 1 retention: %.1f%%\n", retention_m1 * 100))
cat(sprintf("Month 3 retention: %.1f%%\n", retention_m3 * 100))
cat(sprintf("Month 6 retention: %.1f%%\n", retention_m6 * 100))

cat("\n=========================\n")