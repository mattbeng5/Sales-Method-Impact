# Sales Method Impact

## Table of Contents
- [Project Overview](project-overview)
- [Data Source](data-source)
- [Tools](tools)
- [Data Cleaning and Preparation](data-cleaning-and-preparation)
- [Analysis](Analysis)
- [Recommendations](Recommendations)


## Project Overview
The purpose of this project is to assess the impact different sales methods have on revenue. With the release of a new product, three sales methods were applied to a customer base. The first method was an email only sales method where our customers recieved an email with product information on the launch day which was followed up by a second email three weeks later. The second method was a call only sales method where customers were called by our sales reps and had on average a 30 minute conversation. The third method was a hybrid of the two where customers were sent a product information email on the launch day and received a follow up phone call one week later which was on average 10 minutes. Throughout this period, we generated ＄1.4 million dollars in revenue, we will be analyzing the effect each sales method had on our revenue stream. The key performance indicator (KPI) we will be using is the median revenue by sales method. The median is resilient to extreme values and offers a clear view of a “typical” sale and tracking this metric will highlight which approach consistently yields higher value transactions.

## Data Source
The data is sourced from "product_sales.csv" which contains sales data relating to a new product launch.

## Tools
- R
   - tidyverse, lubridate	
  
## Data Cleaning and Preparation 
In the initial phase of our analysis we performed the following steps:
1. Replace null values in revenue column with median revenue, save as new column: revenue_imputed.
2. Standardize entries in sales_method column.

```R
# Load Libraries and Data
library(tidyverse)
library(lubridate)

sales_data <- read_csv("/kaggle/input/product-line-sales-method-data/product_sales.csv")

# Checking for Null Values
summary(sales_data)

# Replace 1074 NA values in revenue column with median revenue
median_revenue <- median(sales_data$revenue, na.rm=TRUE)

sales_data <-
    sales_data %>%
    mutate(revenue_imputed = if_else(is.na(revenue), median_revenue, revenue)) %>%
    relocate(revenue_imputed, .after=revenue)

# Ensure entries for sales_method are standardized
unique(sales_data$sales_method)

# Standardize sales_method column
sales_data <- sales_data %>%
			  mutate(sales_method = tolower(sales_method))

sales_data <- sales_data %>%
			  mutate(sales_method = if_else(str_detect(sales_method, regex("^\\s*em\\s*\\+\\s*call\\s*$", ignore_case = TRUE)), "email + call", sales_method))

sales_data <- sales_data %>% 
              mutate(sales_method = str_to_title(sales_method))

unique(sales_data$sales_method)
```
Now that our data set has been cleaned and validated, we can begin our analysis. The first portion will be an overview of the population. We tested our three sales methods on 15,000 customers, next we'll see how many customers per group.

## Analysis
Now that our data set has been cleaned and validated, we can begin our analysis. The first portion will be an overview of the population. We tested our three sales methods on 15,000 customers, next we'll see how many customers per group.

```R
# How many customers for each approach
customer_approach <- 
    sales_data %>%
	group_by(sales_method) %>%
	summarize(method_count = n()) %>%
	arrange(desc(method_count))

# Proportion of customers for each approach
prop_approach <- 
    customer_approach %>%
    mutate(proportion =round(method_count / sum(method_count), 2)) %>%
    select(sales_method, proportion) %>%
    distinct()

# Count and Proportion Visualizations
count_plot <- 
    ggplot(customer_approach, aes(sales_method, method_count, fill = sales_method)) + 
    geom_bar(stat = "identity") + 
    labs(x = "Sales Method", y = "Count of Customers", title = "Count of Customers by Sales Method") + 
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    scale_fill_brewer(palette = "Pastel1")

prop_plot <-
    ggplot(prop_approach, aes(sales_method, proportion, fill = sales_method)) +
    geom_col(width = 0.9) +             
    coord_flip() +                      
    scale_y_continuous(labels = scales::percent) +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Proportion of Customers by Sales Method",
       x = "Sales Method",  y = "Proportion") + 
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5))
```
The distribution of sales methods over the population is unbalanced, with 50% being in the email group, 33% being in the call group, and 17% being in the hybrid group.

Now we will look at how the revenue is distributed over our 6 week period. 
```R
# What does the spread of revenue look like?
summary(sales_data$revenue_imputed)

# Create data frame with summary of revenue spread
revenue_range <- 238.32 - 32.54
revenue_IQR <- 106.07 - 53.04
revenue_MAD <- round(mad(sales_data$revenue_imputed), 2)
revenue_robust_CV <- round(revenue_MAD / abs(median(sales_data$revenue_imputed)) * 100, 2)
lower_boundary_revenue <- 53.04 - 1.5 * revenue_IQR
upper_boundary_revenue <- 106.07 + 1.5 * revenue_IQR 
revenue_outlier_count <- length(boxplot.stats(sales_data$revenue_imputed)$out)


summary_revenue_df <- 
    tibble(statistic = c("Range", "IQR", "Min", "Lower Boundary", "Upper Boundary", "Max", "Mean Absolute Deviation", "CV", "Outlier Count"), 
    	 value = c(revenue_range, revenue_IQR, 32.54, lower_boundary_revenue, upper_boundary_revenue, 238.32, revenue_MAD, revenue_robust_CV, revenue_outlier_count))
summary_revenue_df

# Visualize with boxplot
revenue_boxplot <- ggplot(sales_data, aes(x = "", y = revenue_imputed)) +
geom_boxplot(fill = "skyblue", color = "darkblue") +
labs(x="", y = "Revenue", title = "Boxplot of Revenue") + 
theme_minimal() +
theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.text.x = element_blank())
revenue_boxplot
```
Throughout our 6 week period, the central 50% of our sales fell between ＄53.04 and ＄106.07, yielding an interquartile range of ＄53.03. There were 1,031 outliers all representing a sale of at least ＄185.62. We will now stratify the revenue by sales method to compare their performances.
```R
# Group Revenue by Sales Method Summary Table
grouped_summary <- sales_data %>%
  group_by(sales_method) %>%
  summarize(
    min_rev = min(revenue_imputed),
    Q1 = round(quantile(revenue_imputed, 0.25), 2),
    Q3 = round(quantile(revenue_imputed, 0.75), 2),
	median = round(median(revenue_imputed), 2),  
    max_rev = max(revenue_imputed),
	range = (max_rev - min_rev),
    IQR = round(IQR(revenue_imputed), 2),
    MAD = round(mad(revenue_imputed),2),
	robust_cv = round(MAD / abs(median(revenue_imputed)) * 100, 2),
	outliers = 	length(boxplot.stats(revenue_imputed)$out))		 
grouped_summary


# Boxplot Comparison 
grouped_plot <- 
    ggplot(sales_data, aes(x = sales_method, y = revenue_imputed)) + 
    geom_boxplot(fill = "skyblue", color = "darkblue") +
    labs(title = "Revenue by Sales Method", x = "Sales Method", y = "Revenue") + 
    coord_flip() +
    theme_minimal() +  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
grouped_plot
```
The "Revenue by Sales Method" highlights clear disparities across our methods. The email-plus-follow-up-call approach produces the highest revenues, with its maximum sale of ＄238.32, the top transaction in the entire six-week period, still falling within its non-outlier range. For this group, the interquartile range (IQR) spans ＄149.82 (Q1) to ＄189.54 (Q3), and the lowest sale recorded (＄89.50) coincides with the overall median. Because its upper whisker exceeds the ＄185.62 outlier threshold computed for the full dataset, this method generates all of the extreme values noted in the overall “Boxplot of Revenue.” By contrast, the next-best performer—email only—tops out at $148.97, well below that cutoff. The call only method yields our lowest distribution, with its max sale equally the median sale of the whole dataset.

The next portion of our analysis will look at how the weekly revenue for each sales method changes over time.
```R
# Group sales methods and get their weekly revenue 
method_revenue_week <- 
    sales_data %>%
	group_by(week, sales_method) %>%
	summarize(weekly_revenue = round(sum(revenue_imputed), 0))

# Proportional weekly revenue
prop_method_revenue_week <- 
    method_revenue_week %>%
    group_by(week) %>%
    mutate(total_weekly_rev = sum(weekly_revenue)) %>%
    ungroup() %>%
    mutate(prop_weekly_rev = round((weekly_revenue / total_weekly_rev),2))

# Revenue Over Time by Sales Method Line Plot
ggplot(method_revenue_week, aes(week, weekly_revenue, color = sales_method)) +
geom_line(linewidth = 1.2) + 
geom_point(size = 2, alpha = 0.8) +  
labs(title = "Revenue Over Time by Sales Method", x = "Week", y = "Revenue Imputed", color = "Sales Method") + 
theme_minimal() +  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
```
Examining the “Revenue over Time by Sales Method” line plot reveals three distinct trajectories. The Email-only campaign surged immediately after launch—peaking at ＄250,000 in week 1—but then fell sharply, ending at just ＄25,000 in week 6. A modest rebound between weeks 3 and 4 coincides with the follow-up email sent in week 3. In contrast, the combined Email + Call approach exhibited steady growth through week 5 before dipping slightly in the final period. Meanwhile, the Call-only channel remained the weakest performer—despite accounting for roughly 20 percent of total weekly revenue—and showed minimal variance over the six-week run.

## Recommendations
The KPI for this analysis was median revenue of each sales method. There were large differences in each methods KPI: call method had a median of ＄49.94, email method had a median of ＄94.28, and the hybrid method had a median sale of ＄182.14. Based off this, the hybrid method leads us to the highest revenue. To maximize revenue and streamline our efforts, we propose the following:
1. Discontinue the Call-Only Sales Method
  - This method generates our lowest median sale (＄49.94) despite being the most time consuming.
  - Eliminating this could free up our sales reps for higher-yield strategies.
2. Standardize the Email + Call Sales Method for New Launches
  - This method generates our highest median sale (＄182.14) and every big-ticket (outlier) sale over the 6 week period.
  - By week 6, this group was responsible for nearly 70% of our weekly revenue despite only containing 17.2% of customer population.
  - Follow up calls average only 10 minutes, remaining efficient but also giving our customer a more personalized experience than a follow up email.
3. Increase Data Collection Quality
  - There were 1,074 transactions with no revenue recorded(imputed using overall median)
  - Missing data undermines our ability to detect genuine performance changes and accurately assess each channel’s ROI.
  - Implement a system check that prevents closing a ticket at the point of sale until the revenue amount has been entered.





