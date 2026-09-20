# Customer Churn Analytics
# Exploratory Data Analysis
# R / Tidyverse

library(arrow)
library(tidyverse)
library(lubridate)

# Load datasets
bets <- read_parquet("bets.parquet")
customers <- read_parquet("customers.parquet")
events <- read_parquet("events.parquet")
participants <- read_parquet("participants.parquet")

# Dataset dimensions
dim(customers)
dim(bets)
dim(events)
dim(participants)

# Missing values
colSums(is.na(customers))
colSums(is.na(bets))
colSums(is.na(events))
colSums(is.na(participants))

# Duplicate rows
sum(duplicated(customers))
sum(duplicated(bets))
sum(duplicated(events))
sum(duplicated(participants))

bets_by_customer <- bets %>%
    count(user_id, name = "bets_from_transactions")

customer_check <- customers %>%
    select(user_id, n_bets) %>%
    left_join(bets_by_customer, by = "user_id") %>%
    mutate(
        difference = n_bets - bets_from_transactions
    )

customer_check %>%
    summarise(
        customers = n(),
        exact_match = sum(difference == 0),
        mismatches = sum(difference != 0)
    )

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

event_check <- bets %>%
    distinct(event_id) %>%
    anti_join(
        events %>% select(event_id),
        by = "event_id"
    )

nrow(event_check)

events_without_bets <- events %>%
    anti_join(
        bets %>% distinct(event_id),
        by = "event_id"
    )

nrow(events_without_bets)



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

nrow(participant_check)


nrow(bet_participants)


# EDA CLIENT DISTRIBUTION
customers %>%
    count(segment_client)

customers %>%
    count(residence_country, sort = TRUE)

customers %>%
    count(payment_method, sort = TRUE)


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

# PLOTS 

customers %>%
    count(segment_client) %>%
    ggplot(aes(x=segment_client, y=n))+
    geom_col()+labs(
        title = "Customer by Client Segment",
        x="Client Segment",
        y="Number of Customers"
    )+
    theme_minimal()


customers %>%
    count(segment_client) %>%
    mutate(
        percentage = n/sum(n)*100
    )

customers %>% 
    summarise(
        mean_age = mean(age),
        median_age = median(age),
        min_age = min(age),
        max_age = max(age)
    )

customers %>% 
    summarise(
        mean_n_bets = mean(n_bets),
        median_n_bets = median(n_bets),
        min_n_bets = min(n_bets),
        max_n_bets = max(n_bets)
    )

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

customers %>%
    group_by(segment_client) %>%
    summarise(
        customers = n(),
        mean_bets = mean(n_bets),
        median_bets = median(n_bets),
        min_bets = min(n_bets),
        max_bets = max(n_bets)
    )

customers %>%
    group_by(segment_client) %>%
    summarise(
        mean_annual_bets = mean(annual_bets),
        mean_n_bets = mean(n_bets),
        median_annual_bets = median(annual_bets),
        median_n_bets = median(n_bets)
    )

bets %>%
    count(segment_client) %>%
    mutate(
        percentage = n / sum(n) * 100
    )

bets %>% 
    group_by(segment_client) %>%
    summarise(total_bet_amount = sum(bet_amount, na.rm = TRUE)) %>%
    ggplot(aes(x = segment_client, y = total_bet_amount)) +
    geom_col() +
    labs(
        title = "Bet Amount by Client Segment",
        x = "Client Segment",
        y = "Total Bet Amount"
    ) +
    theme_minimal()




bets %>%
    group_by(segment_client) %>%
    summarise(
        total_bet_amount = sum(bet_amount),
        mean_bet_amount = mean(bet_amount),
        median_bet_amount = median(bet_amount)
    )

bets %>%
    group_by(segment_client) %>%
    summarise(
        total_bet_amount = sum(bet_amount),
        total_bets = n()
    ) %>%
    left_join(
        customers %>%
            count(segment_client, name = "customers"),
        by = "segment_client"
    ) %>%
    mutate(
        bet_amount_per_customer = total_bet_amount / customers
    )



segment_summary <- bets %>%
    group_by(segment_client) %>%
    summarise(
        total_bet_amount = sum(bet_amount),
        total_bets = n()
    ) %>%
    left_join(
        customers %>%
            count(segment_client, name = "customers"),
        by = "segment_client"
    ) %>%
    mutate(
        bet_amount_per_customer = total_bet_amount / customers
    )

ggplot(segment_summary, aes(x = segment_client, y = bet_amount_per_customer)) +
    geom_col() +
    labs(
        title = "Bet Amount per Customer by Segment",
        x = "Client Segment",
        y = "Bet Amount per Customer"
    ) +
    theme_minimal()

# BETS PER MONTH
bets %>%
    mutate(month = floor_date(placement_date, "month")) %>%
    count(month) %>%
    ggplot(aes(x = month, y = n)) +
    geom_line() +
    labs(
        title = "Number of Bets Over Time",
        x = "Month",
        y = "Number of Bets"
    ) +
    theme_minimal()


# NUMBER OF ACTIVE CLIENTS
bets %>%
    mutate(month = floor_date(placement_date, "month")) %>%
    group_by(month) %>%
    summarise(
        bets = n(),
        active_customers = n_distinct(user_id)
    ) %>%
    ggplot(aes(x = month, y = active_customers)) +
    geom_line() +
    labs(
        title = "Active Customers Over Time",
        x = "Month",
        y = "Active Customers"
    ) +
    theme_minimal()

# MEAN BETS PER ACTIVE CLIENT 
bets %>%
    mutate(month = floor_date(placement_date, "month")) %>%
    group_by(month) %>%
    summarise(
        bets = n(),
        active_customers = n_distinct(user_id),
        bets_per_active_customer = bets / active_customers
    ) %>%
    ggplot(aes(x = month, y = bets_per_active_customer)) +
    geom_line() +
    labs(
        title = "Average Bets per Active Customer",
        x = "Month",
        y = "Bets per Active Customer"
    ) +
    theme_minimal()

# STATIONALITY
bets %>%
    mutate(
        month = floor_date(placement_date, "month")
    ) %>%
    count(month) %>%
    mutate(
        month_name = month(month, label = TRUE)
    )

# PER YEAR 
bets %>%
    mutate(
        month = floor_date(placement_date, "month"),
        year = year(placement_date)
    ) %>%
    count(year, month)

# REGISTER DATE
customers %>%
    mutate(month = floor_date(register_date, "month")) %>%
    count(month) %>%
    ggplot(aes(x = month, y = n)) +
    geom_line() +
    labs(
        title = "New Customer Registrations Over Time",
        x = "Month",
        y = "New Customers"
    ) +
    theme_minimal()

# HOW LONG IT TAKES TO MAKE THE FIRST BET?
first_bet <- bets %>%
    group_by(user_id) %>%
    summarise(
        first_bet_date = min(placement_date)
    )

customer_engagement <- customers %>%
    select(user_id, register_date) %>%
    left_join(first_bet, by = "user_id") %>%
    mutate(
        days_to_first_bet = as.numeric(
            difftime(first_bet_date, register_date, units = "days")
        )
    )

# CUSTOMER ENGAGEMENT
customer_engagement %>%
    summarise(
        mean_days = mean(days_to_first_bet),
        median_days = median(days_to_first_bet),
        min_days = min(days_to_first_bet),
        max_days = max(days_to_first_bet)
    )

# CLIENTS WHO DID A BET
customer_engagement %>%
    summarise(
        mean_days = mean(days_to_first_bet, na.rm = TRUE),
        median_days = median(days_to_first_bet, na.rm = TRUE),
        min_days = min(days_to_first_bet, na.rm = TRUE),
        max_days = max(days_to_first_bet, na.rm = TRUE)
    )

# BOTH TYPE OF CLIENTS
customer_engagement %>%
    summarise(
        customers = n(),
        customers_with_bet = sum(!is.na(first_bet_date)),
        customers_without_bet = sum(is.na(first_bet_date)),
        mean_days = mean(days_to_first_bet, na.rm = TRUE),
        median_days = median(days_to_first_bet, na.rm = TRUE),
        min_days = min(days_to_first_bet, na.rm = TRUE),
        max_days = max(days_to_first_bet, na.rm = TRUE)
    )

# CUSTOMER ENGAGEMENT
customer_engagement <- customer_engagement %>%
    mutate(
        ever_bet = !is.na(first_bet_date)
    )

customer_engagement %>%
    count(ever_bet)

customer_engagement %>%
    group_by(ever_bet) %>%
    summarise(
        customers = n()
    )

# HOW FAST THEY START TO BET
customer_engagement %>%
    filter(ever_bet) %>%
    ggplot(aes(x = days_to_first_bet)) +
    geom_histogram(bins = 30) +
    labs(
        title = "Days to First Bet",
        x = "Days",
        y = "Customers"
    ) +
    theme_minimal()

customer_engagement %>%
    filter(ever_bet) %>%
    ggplot(aes(y = days_to_first_bet)) +
    geom_boxplot() +
    labs(
        title = "Days to First Bet",
        y = "Days"
    ) +
    theme_minimal()

customer_engagement %>%
    filter(ever_bet) %>%
    summarise(
        mean_days = mean(days_to_first_bet),
        median_days = median(days_to_first_bet),
        min_days = min(days_to_first_bet),
        max_days = max(days_to_first_bet)
    )

# INTERVALS DAYS TO FIRST BET
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

# SEGMENTATION

# THE ONBOARDING BEHAVIOR CHANGES WITH THE SEGMENT CLIENT
customer_engagement %>%
    left_join(
        customers %>% select(user_id, segment_client),
        by = "user_id"
    ) %>%
    filter(ever_bet) %>%
    group_by(segment_client) %>%
    summarise(
        customers = n(),
        mean_days = mean(days_to_first_bet),
        median_days = median(days_to_first_bet)
    )

customer_engagement %>%
    left_join(
        customers %>% select(user_id, segment_client),
        by = "user_id"
    ) %>%
    filter(ever_bet) %>%
    ggplot(aes(
        x = segment_client,
        y = days_to_first_bet
    )) +
    geom_boxplot() +
    labs(
        title = "Days to First Bet by Customer Segment",
        x = "Customer Segment",
        y = "Days to First Bet"
    ) +
    theme_minimal()

# CUSTOMER ACTIVITY / BETTING FREQUENCY

bets_per_customer <- bets %>%
    count(user_id, name = "total_bets")

summary(bets_per_customer$total_bets)

bets_per_customer %>%
    ggplot(aes(x = total_bets)) +
    geom_histogram(bins = 50) +
    labs(
        title = "Distribution of Bets per Customer",
        x = "Total Bets",
        y = "Customers"
    ) +
    theme_minimal()

bets %>%
    count(user_id, name = "total_bets") %>%
    left_join(
        customers %>% select(user_id, segment_client),
        by = "user_id"
    ) %>%
    group_by(segment_client) %>%
    summarise(
        customers = n(),
        mean_bets = mean(total_bets),
        median_bets = median(total_bets),
        min_bets = min(total_bets),
        max_bets = max(total_bets)
    )

active_days <- bets %>%
    mutate(bet_date = as.Date(placement_date)) %>%
    group_by(user_id) %>%
    summarise(
        active_days = n_distinct(bet_date)
    )


bets %>%
    count(user_id, name = "total_bets") %>%
    left_join(
        customers %>% select(user_id, segment_client),
        by = "user_id"
    ) %>%
    ggplot(aes(
        x = segment_client,
        y = total_bets
    )) +
    geom_boxplot() +
    labs(
        title = "Bet Frequency by Customer Segment",
        x = "Customer Segment",
        y = "Total Bets"
    ) +
    theme_minimal()

scale_y_log10()


active_days <- bets %>%
    mutate(
        bet_date = as.Date(placement_date)
    ) %>%
    group_by(user_id) %>%
    summarise(
        active_days = n_distinct(bet_date)
    )

active_days %>%
    left_join(
        customers %>% select(user_id, segment_client),
        by = "user_id"
    ) %>%
    group_by(segment_client) %>%
    summarise(
        customers = n(),
        mean_active_days = mean(active_days),
        median_active_days = median(active_days),
        min_active_days = min(active_days),
        max_active_days = max(active_days)
    )


customer_activity <- bets %>%
    mutate(
        bet_date = as.Date(placement_date)
    ) %>%
    group_by(user_id) %>%
    summarise(
        total_bets = n(),
        active_days = n_distinct(bet_date),
        bets_per_active_day = total_bets / active_days
    )



customer_activity %>%
    left_join(
        customers %>% select(user_id, segment_client),
        by = "user_id"
    ) %>%
    group_by(segment_client) %>%
    summarise(
        customers = n(),
        mean_bets_per_active_day = mean(bets_per_active_day),
        median_bets_per_active_day = median(bets_per_active_day)
    )


max_bet_date <- max(bets$placement_date)
max_bet_date


last_bet <- bets %>%
    group_by(user_id) %>%
    summarise(
        last_bet_date = max(placement_date)
    ) %>%
    mutate(
        days_since_last_bet = as.numeric(
            difftime(max_bet_date, last_bet_date, units = "days")
        )
    )



last_bet %>%
    summarise(
        mean_days = mean(days_since_last_bet),
        median_days = median(days_since_last_bet),
        min_days = min(days_since_last_bet),
        max_days = max(days_since_last_bet)
    )

last_bet %>%
    ggplot(aes(x = days_since_last_bet)) +
    geom_histogram(bins = 40) +
    labs(
        title = "Days Since Last Bet",
        x = "Days Since Last Bet",
        y = "Customers"
    ) +
    theme_minimal()


last_bet %>%
    ggplot(aes(y = days_since_last_bet)) +
    geom_boxplot() +
    labs(
        title = "Distribution of Days Since Last Bet",
        y = "Days Since Last Bet"
    ) +
    theme_minimal()


last_bet %>%
    left_join(
        customers %>% select(user_id, segment_client),
        by = "user_id"
    ) %>%
    group_by(segment_client) %>%
    summarise(
        customers = n(),
        mean_days = mean(days_since_last_bet),
        median_days = median(days_since_last_bet),
        min_days = min(days_since_last_bet),
        max_days = max(days_since_last_bet)
    )


customer_features <- customer_engagement %>%
    select(user_id, days_to_first_bet) %>%
    left_join(
        customer_activity,
        by = "user_id"
    ) %>%
    left_join(
        last_bet %>% select(user_id, days_since_last_bet),
        by = "user_id"
    )



customer_features %>%
    select(
        days_to_first_bet,
        days_since_last_bet,
        total_bets,
        active_days,
        bets_per_active_day
    ) %>%
    cor(use = "complete.obs")



customer_features <- customer_engagement %>%
    select(
        user_id,
        days_to_first_bet
    ) %>%
    left_join(
        customer_activity,
        by = "user_id"
    ) %>%
    left_join(
        last_bet %>%
            select(user_id, days_since_last_bet),
        by = "user_id"
    ) %>%
    left_join(
        customers %>%
            select(user_id, segment_client),
        by = "user_id"
    )



glimpse(customer_features)





monthly_activity <- bets %>%
    mutate(
        month = floor_date(as.Date(placement_date), unit = "month")
    ) %>%
    group_by(month) %>%
    summarise(
        active_customers = n_distinct(user_id),
        total_bets = n(),
        total_bet_amount = sum(bet_amount),
        .groups = "drop"
    )


monthly_activity <- monthly_activity %>%
    mutate(
        bets_per_active_customer =
            total_bets / active_customers,
        
        bet_amount_per_active_customer =
            total_bet_amount / active_customers
    )


monthly_activity



monthly_activity %>%
    select(
        month,
        active_customers,
        total_bets,
        total_bet_amount,
        bets_per_active_customer,
        bet_amount_per_active_customer
    )


monthly_activity %>%
    ggplot(aes(x = month, y = active_customers)) +
    geom_line() +
    labs(
        title = "Monthly Active Customers",
        x = "Month",
        y = "Active Customers"
    ) +
    theme_minimal()



monthly_activity %>%
    ggplot(aes(x = month, y = bets_per_active_customer)) +
    geom_line() +
    labs(
        title = "Bets per Active Customer",
        x = "Month",
        y = "Bets per Active Customer"
    ) +
    theme_minimal()



monthly_activity %>%
    select(
        month,
        active_customers,
        total_bets,
        total_bet_amount,
        bets_per_active_customer,
        bet_amount_per_active_customer
    )



monthly_activity %>%
    ggplot(aes(x = month, y = bet_amount_per_active_customer)) +
    geom_line() +
    labs(
        title = "Bet Amount per Active Customer",
        x = "Month",
        y = "Bet Amount per Active Customer"
    ) +
    theme_minimal()

monthly_activity %>%
    ggplot(aes(x = month, y = bet_amount_per_active_customer)) +
    geom_line() +
    labs(
        title = "Monthly Bet Amount per Active Customer",
        x = "Month",
        y = "Bet Amount per Active Customer"
    ) +
    theme_minimal()


monthly_activity %>%
    select(
        month,
        active_customers,
        total_bets,
        total_bet_amount,
        bets_per_active_customer,
        bet_amount_per_active_customer
    )





monthly_activity %>%
    summarise(
        min_bet_amount_per_customer = min(bet_amount_per_active_customer),
        max_bet_amount_per_customer = max(bet_amount_per_active_customer),
        mean_bet_amount_per_customer = mean(bet_amount_per_active_customer),
        median_bet_amount_per_customer = median(bet_amount_per_active_customer)
    )



monthly_activity %>%
    select(month, bet_amount_per_active_customer) %>%
    print(n = 24)


monthly_activity %>%
    ggplot(aes(x = month, y = bet_amount_per_active_customer)) +
    geom_line() +
    labs(
        title = "Monthly Bet Amount per Active Customer",
        x = "Month",
        y = "Bet Amount per Active Customer"
    ) +
    theme_minimal()


segment_behavior <- bets %>%
    group_by(user_id) %>%
    summarise(
        total_bets = n(),
        total_bet_amount = sum(bet_amount),
        average_bet_amount = mean(bet_amount),
        active_days = n_distinct(as.Date(placement_date)),
        bets_per_active_day = total_bets / active_days
    ) %>%
    left_join(
        customers %>%
            select(user_id, segment_client),
        by = "user_id"
    )



segment_behavior %>%
    group_by(segment_client) %>%
    summarise(
        customers = n(),
        avg_bets = mean(total_bets),
        avg_bet_amount = mean(total_bet_amount),
        avg_bet_size = mean(average_bet_amount),
        avg_active_days = mean(active_days),
        avg_bets_per_active_day = mean(bets_per_active_day)
    )

segment_behavior %>%
    group_by(segment_client) %>%
    summarise(
        customers = n(),
        avg_bets = mean(total_bets),
        avg_bet_amount = mean(total_bet_amount),
        avg_bet_size = mean(average_bet_amount),
        avg_active_days = mean(active_days),
        avg_bets_per_active_day = mean(bets_per_active_day)
    )


segment_behavior %>%
    group_by(segment_client) %>%
    summarise(
        customers = n(),
        median_bets = median(total_bets),
        median_bet_amount = median(total_bet_amount),
        median_bet_size = median(average_bet_amount),
        median_active_days = median(active_days),
        median_bets_per_active_day = median(bets_per_active_day)
    )


ggplot(
    segment_behavior,
    aes(
        x = total_bets,
        y = total_bet_amount,
        color = segment_client
    )
) +
    geom_point(alpha = 0.5) +
    labs(
        title = "Bet Frequency vs Total Bet Amount",
        x = "Total Bets",
        y = "Total Bet Amount",
        color = "Customer Segment"
    ) +
    theme_minimal()



segment_behavior %>%
    group_by(segment_client) %>%
    summarise(
        avg_bet_size = mean(total_bet_amount / total_bets),
        median_bet_size = median(total_bet_amount / total_bets)
    )


customer_features %>%
    summarise(
        min_bets = min(total_bets, na.rm = TRUE),
        q1_bets = quantile(total_bets, 0.25, na.rm = TRUE),
        median_bets = median(total_bets, na.rm = TRUE),
        mean_bets = mean(total_bets, na.rm = TRUE),
        q3_bets = quantile(total_bets, 0.75, na.rm = TRUE),
        max_bets = max(total_bets, na.rm = TRUE)
    )

customer_features <- customer_features %>%
    left_join(
        segment_behavior %>%
            select(user_id, total_bet_amount, average_bet_amount),
        by = "user_id"
    )




customer_features %>%
    summarise(
        min_amount = min(total_bet_amount, na.rm = TRUE),
        q1_amount = quantile(total_bet_amount, 0.25, na.rm = TRUE),
        median_amount = median(total_bet_amount, na.rm = TRUE),
        mean_amount = mean(total_bet_amount, na.rm = TRUE),
        q3_amount = quantile(total_bet_amount, 0.75, na.rm = TRUE),
        max_amount = max(total_bet_amount, na.rm = TRUE)
    )




ggplot(customer_features, aes(x = total_bets)) +
    geom_histogram(bins = 50) +
    labs(
        title = "Distribution of Total Bets per Customer",
        x = "Total Bets",
        y = "Customers"
    ) +
    theme_minimal()




ggplot(customer_features, aes(x = total_bet_amount)) +
    geom_histogram(bins = 50) +
    labs(
        title = "Distribution of Total Bet Amount per Customer",
        x = "Total Bet Amount",
        y = "Customers"
    ) +
    theme_minimal()



customer_features %>%
    summarise(
        max_amount = max(total_bet_amount, na.rm = TRUE)
    )

customer_features %>%
    summarise(
        min_bets = min(total_bets, na.rm = TRUE),
        q1_bets = quantile(total_bets, 0.25, na.rm = TRUE),
        median_bets = median(total_bets, na.rm = TRUE),
        mean_bets = mean(total_bets, na.rm = TRUE),
        q3_bets = quantile(total_bets, 0.75, na.rm = TRUE),
        max_bets = max(total_bets, na.rm = TRUE)
    )



customer_features %>%
    select(user_id, segment_client, total_bets, total_bet_amount) %>%
    arrange(desc(total_bet_amount)) %>%
    slice_head(n = 10)




customer_features %>%
    summarise(
        min_amount = min(total_bet_amount, na.rm = TRUE),
        q1 = quantile(total_bet_amount, 0.25, na.rm = TRUE),
        median = median(total_bet_amount, na.rm = TRUE),
        q3 = quantile(total_bet_amount, 0.75, na.rm = TRUE),
        p95 = quantile(total_bet_amount, 0.95, na.rm = TRUE),
        p99 = quantile(total_bet_amount, 0.99, na.rm = TRUE),
        max_amount = max(total_bet_amount, na.rm = TRUE)
    )


customer_features %>%
    summarise(
        min_bets = min(total_bets, na.rm = TRUE),
        q1 = quantile(total_bets, 0.25, na.rm = TRUE),
        median = median(total_bets, na.rm = TRUE),
        q3 = quantile(total_bets, 0.75, na.rm = TRUE),
        p95 = quantile(total_bets, 0.95, na.rm = TRUE),
        p99 = quantile(total_bets, 0.99, na.rm = TRUE),
        max_bets = max(total_bets, na.rm = TRUE)
    )


# RECENCY

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

ggplot(customer_features, aes(x = days_since_last_bet)) +
    geom_histogram(bins = 50) +
    labs(
        title = "Distribution of Days Since Last Bet",
        x = "Days Since Last Bet",
        y = "Customers"
    ) +
    theme_minimal()

customer_features %>%
    group_by(segment_client) %>%
    summarise(
        customers = n(),
        median_recency = median(days_since_last_bet, na.rm = TRUE),
        mean_recency = mean(days_since_last_bet, na.rm = TRUE),
        q3_recency = quantile(days_since_last_bet, 0.75, na.rm = TRUE),
        p95_recency = quantile(days_since_last_bet, 0.95, na.rm = TRUE)
    )



ggplot(customer_features, aes(x = days_since_last_bet)) +
    geom_histogram(bins = 50) +
    labs(
        title = "Distribution of Days Since Last Bet",
        x = "Days Since Last Bet",
        y = "Customers"
    ) +
    theme_minimal()


ggplot(customer_features, aes(
    x = days_since_last_bet,
    fill = segment_client
)) +
    geom_histogram(bins = 40, alpha = 0.7) +
    facet_wrap(~segment_client, scales = "free_y") +
    labs(
        title = "Recency Distribution by Customer Segment",
        x = "Days Since Last Bet",
        y = "Customers"
    ) +
    theme_minimal()


ggplot(
    customer_features,
    aes(
        x = segment_client,
        y = days_since_last_bet
    )
) +
    geom_boxplot() +
    labs(
        title = "Recency by Customer Segment",
        x = "Customer Segment",
        y = "Days Since Last Bet"
    ) +
    theme_minimal()


ggplot(
    customer_features,
    aes(
        x = days_since_last_bet
    )
) +
    geom_histogram(
        bins = 50
    ) +
    coord_cartesian(xlim = c(0, 50)) +
    labs(
        title = "Recency Distribution (0–50 Days)",
        x = "Days Since Last Bet",
        y = "Customers"
    ) +
    theme_minimal()




monthly_activity <- bets %>%
    mutate(month = floor_date(placement_date, "month")) %>%
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


monthly_activity %>%
    ggplot(aes(x = month, y = active_customers, color = segment_client)) +
    geom_line() +
    labs(
        title = "Monthly Active Customers by Segment",
        x = "Month",
        y = "Active Customers"
    ) +
    theme_minimal()






monthly_activity %>%
    ggplot(aes(x = month, y = bets_per_customer, color = segment_client)) +
    geom_line() +
    labs(
        title = "Monthly Bets per Active Customer",
        x = "Month",
        y = "Bets per Active Customer"
    ) +
    theme_minimal()



monthly_activity <- bets %>%
    mutate(month = floor_date(placement_date, "month")) %>%
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


monthly_activity



monthly_activity %>%
    ggplot(aes(x = month, y = active_customers, color = segment_client)) +
    geom_line(linewidth = 1) +
    labs(
        title = "Monthly Active Customers by Segment",
        x = "Month",
        y = "Active Customers",
        color = "Segment"
    ) +
    theme_minimal()




monthly_activity %>%
    ggplot(aes(x = month, y = bets_per_customer, color = segment_client)) +
    geom_line(linewidth = 1) +
    labs(
        title = "Monthly Bets per Active Customer",
        x = "Month",
        y = "Bets per Active Customer",
        color = "Segment"
    ) +
    theme_minimal()


monthly_activity %>%
    ggplot(aes(
        x = month,
        y = amount_per_customer,
        color = segment_client
    )) +
    geom_line(linewidth = 1) +
    labs(
        title = "Monthly Bet Amount per Active Customer",
        x = "Month",
        y = "Bet Amount per Active Customer",
        color = "Segment"
    ) +
    theme_minimal()




library(lubridate)

customer_cohort <- bets %>%
    group_by(user_id) %>%
    summarise(
        cohort_month = floor_date(min(placement_date), "month"),
        .groups = "drop"
    )


cohort_activity <- bets %>%
    mutate(activity_month = floor_date(placement_date, "month")) %>%
    select(user_id, activity_month, segment_client) %>%
    distinct() %>%
    left_join(customer_cohort, by = "user_id")





cohort_activity <- cohort_activity %>%
    mutate(
        months_since_cohort = interval(
            cohort_month,
            activity_month
        ) %/% months(1)
    )



cohort_retention <- cohort_activity %>%
    group_by(cohort_month, months_since_cohort) %>%
    summarise(
        active_customers = n_distinct(user_id),
        .groups = "drop"
    )




cohort_sizes <- cohort_retention %>%
    filter(months_since_cohort == 0) %>%
    select(
        cohort_month,
        cohort_size = active_customers
    )




cohort_retention <- cohort_retention %>%
    left_join(cohort_sizes, by = "cohort_month") %>%
    mutate(
        retention = active_customers / cohort_size
    )

cohort_retention



cohort_retention %>%
    filter(months_since_cohort <= 6) %>%
    arrange(cohort_month, months_since_cohort)


cohort_sizes %>%
    arrange(cohort_month)



cohort_retention_main <- cohort_retention %>%
    filter(cohort_month >= as.POSIXct("2024-01-01"))



cohort_retention_main %>%
    select(cohort_month, months_since_cohort, retention) %>%
    tidyr::pivot_wider(
        names_from = months_since_cohort,
        values_from = retention,
        names_prefix = "M"
    )