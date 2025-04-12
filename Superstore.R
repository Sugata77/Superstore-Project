# Load essential libraries

# install.packages("tidyverse")
library(tidyverse) # Data Manipulation & Visualization and plotting
library(lubridate) # Date and Time Manipulation
library(ggplot2) # Visualization
library(dplyr) # For data cleaning and transformation
library(readr) # Read CSV/Excel
# install.packages("forecast")
library(forecast) # Time Series Forecasting
library(tseries) # Statistical Analysis
# install.packages("caret")
library(caret) # Machine Learning Models
library(PerformanceAnalytics) # Financial Analysis
library(stringi) # For handling text data
library(knitr) # For showing results in tables
# install.packages("kableExtra")
library(kableExtra) # For formatting nice tables in reports

# Load Data
data <- read.csv("Superstore.csv")

# View first few rows
head(data)

# Check structure
str(data)



# Data Cleaning & Preparation

# Summary Statistics:

# Summary of all columns
summary(data)

# Check missing values:

# See total missing values in each column
colSums(is.na(data))

# see percentage of missing values:

missing_percent <- colSums(is.na(data)) / nrow(data) * 100
print(missing_percent)

anyNA(data)


# Convert Order Date and Ship Date to Date format

data$Order.Date <- as.Date(data$Order.Date, format = "%m/%d/%Y")
data$Ship.Date <- as.Date(data$Ship.Date, format = "%m/%d/%Y")

head(data)

# Dealing with duplicate:

# Check duplicate rows
sum(duplicated(data))

# Identify character columns
char_cols <- sapply(data, is.character)

# Apply UTF-8 conversion to all character columns
data[char_cols] <- lapply(data[char_cols], function(x) stri_encode(x, from = "", to = "UTF-8"))

# Removing extra space before and after texts

data[char_cols] <- lapply(data[char_cols], str_trim)

# sample check:

data$Product.Name <- str_trim(data$Product.Name)

head(data$Product.Name)

## Convert categorical columns to factors
data$Category <- as.factor(data$Category)
data$Sub.Category <- as.factor(data$Sub.Category)
data$Segment <- as.factor(data$Segment)
data$Ship.Mode <- as.factor(data$Ship.Mode)
data$Region <- as.factor(data$Region)
data$State <- as.factor(data$State)
data$City <- as.factor(data$City)

head(data)

# creating profit margin:

# Profit Margin (%) = (Profit / Sales) * 100. For every 1 Dollar of sales, how much profit can be made.

# Creating Profit Margin Column in the data
data$Profit_Margin <- ifelse(
  data$Sales == 0,
  NA,
  (data$Profit / data$Sales) * 100
)
# View the data with new column
head(data[, c("Sales", "Profit", "Profit_Margin")])

# reorder column:

cleaned_data <- data[, c(
  "Row.ID", "Order.ID", "Order.Date", "Ship.Date", "Ship.Mode",
  "Customer.ID", "Customer.Name", "Segment", "Country", "City", "State", "Postal.Code", "Region",
  "Product.ID", "Category", "Sub.Category", "Product.Name",
  "Sales", "Quantity", "Discount", "Profit", "Profit_Margin"
)]
# Structure of Data
str(cleaned_data)

# Summary of Data
summary(cleaned_data)

# Check Missing Values
colSums(is.na(cleaned_data))

# Export as CSV (Recommended)
write.csv(cleaned_data, "cleaned_data.csv", row.names = FALSE)

# Now the data is cleaned, and we use this cleaned data for further analysis.



# Q1: Which regions and product categories contribute most to overall profit, and which ones are causing losses?

# View Unique Regions
unique(cleaned_data$Region)

# Count of Unique Regions
length(unique(cleaned_data$Region))

# there are 4 regions.

# Count Number of Categories per Region

cleaned_data %>%
  group_by(Region) %>%
  summarise(Unique_Categories = n_distinct(Category))

# Categories:

cleaned_data %>%
  group_by(Region) %>%
  summarise(Unique_Categories = paste(unique(Category), collapse = ", "))

table(cleaned_data$Region, cleaned_data$Category)


# Group by Region & Category and Calculate Total Profit
profit_summary <- cleaned_data %>%
  group_by(Region, Category) %>%
  summarise(Overall_Profit = sum(Profit, na.rm = TRUE)) %>%
  arrange(Region, Category)

# View the result
print(profit_summary)


# Check Column Names
colnames(profit_summary)

# Clean Column Names (remove unwanted spaces)
colnames(profit_summary) <- trimws(colnames(profit_summary))

# Top 3 Profit Making (Positive Profit Only)
top3_profit <- profit_summary %>%
  filter(Overall_Profit > 0) %>%
  arrange(desc(Overall_Profit)) %>%
  head(3)

cat("Top 3 Profit Making:\n")
print(top3_profit)

# Top 3 Loss Making (Minimum Profit)
top3_loss <- profit_summary %>%
  arrange(Overall_Profit) %>%
  head(3)

cat("\nTop 3 Loss Making:\n")
print(top3_loss)


# Assuming the summarized data is stored in profit_summary
# Columns = Region, Category, Overall_Profit

# Plot
ggplot(profit_summary, aes(
  x = reorder(paste(Region, Category, sep = " - "), Overall_Profit),
  y = Overall_Profit, fill = Overall_Profit > 0
)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
  labs(
    title = "Overall Profit by Region and Category",
    x = "Region - Category",
    y = "Overall Profit"
  ) +
  theme_minimal()


# Profit/Loss Distribution across Regions & Categories

ggplot(profit_summary, aes(x = reorder(Category, Overall_Profit), y = Overall_Profit, fill = Overall_Profit > 0)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Region) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
  labs(title = "Profit / Loss Distribution across Regions & Categories", x = "Category", y = "Profit")



# In the given data set, the most profitable segment is Office Supplies in the West region,
# followed by Technologies in East and West.
# Recommendation: Focus marketing and expansion strategies on Office Supplies in West,
# and Technology in East and West regions for maximizing revenue.


# On the other hand, Furniture in the Central region is facing a loss of -$2871.
# This indicates that although the Central region is generally profitable in Technology and Office Supplies,
# Furniture category requires attention for cost control, discount strategy, or supply chain optimization

"This analysis enables the company to focus on high-performing region-category combinations while addressing loss-making segments through cost control and strategic interventions."




## Q2: What are the top 10 under performing products in terms of profit margin, and what patterns can you observe across their shipping cost, customer segment, or region?

# Top 10 Products with Minimum Profit Margin


# Step 1: Select Top 10 Under performing Products (Lowest Profit Margin)

min10_products_patterns <- cleaned_data %>%
  arrange(Profit_Margin) %>%                           # Arrange in ascending order
  select(Product.Name, Region, Segment, Ship.Mode, 
         Sales, Profit, Profit_Margin) %>%             # Select relevant columns
  head(10)                                             # Top 10 records

View(data.frame(min10_products_patterns))

# Step 2: Pattern Analysis — Frequency Count

# Count of Products by Region
table(min10_products_patterns$Region)

# Count of Products by Segment
table(min10_products_patterns$Segment)

# Count of Products by Shipping Mode
table(min10_products_patterns$Ship.Mode)

# Step 3: Visualization — Top 10 Products with Profit Margin

ggplot(min10_products_patterns, aes(x = reorder(Product.Name, Profit_Margin), 
                                    y = Profit_Margin, 
                                    fill = Profit_Margin > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Underperforming Products by Profit Margin",
       x = "Product Name",
       y = "Profit Margin") +
  scale_fill_manual(values = c("TRUE" = "green", "FALSE" = "red"),
                    name = "Profit > 0") +
  theme_minimal()



## Region Pattern:

# All the Bottom 10 Products (with the lowest profit margins) belong to the Central Region.

# This clearly indicates that the Central region is highly prone to products incurring heavy losses or negative profit margins.

# The Central region may have issues like high operational costs, poor customer demand, aggressive discounting, or poor cost control leading to negative profitability.

## Shipping Mode Pattern:

# Majority of the loss-making products were shipped through Standard Class mode (8 out of 10 products).

# Very few were shipped using Second Class shipping (2 products).

# Standard Class shipping, despite being slower and cheaper, has not contributed to profitability in this case.
# Higher product returns.
# Low product pricing with high shipping/handling costs.
# Storage & delivery inefficiencies.

## Customer Segment Pattern:
# Loss-making products are largely purchased by Consumer Segment customers (7 out of 10 products).
# Few were purchased by Corporate Segment customers (3 products).
# Business Insight:
# Consumer segment customers seem highly price-sensitive in this dataset, leading to high losses possibly due to:
# High discounting.
# Low-priced products with poor margins.
# Increased shipping cost burden per order.

## Overall Summary Table of Patterns:

# Parameter	Observation	Business Impact / Risk
# Region	All 10 products from Central	Major risk area for loss-making products
# Shipping Mode	8 Products via Standard Class	Shipping cost inefficiencies observed
# Customer Segment	7 Products to Consumers	High price sensitivity & discount risks
# Recommendations:
# Investigate the Central region’s cost structure.
# Reassess the pricing and shipping strategy for Consumer segment products.
# Explore whether certain product categories (like vacuums, surge protectors) should be desisted or re-priced.
# Optimize the Standard Class shipping logistics for cost efficiency.




## Q3: Are there any clear seasonality or trends in sales and profit over time (monthly or quarterly)?


"Monthly"

# Create Month-Year Column
cleaned_data$Month_Year <- format(cleaned_data$Order.Date, "%b-%Y")

# Group by Month-Year
sales_profit_monthly <- cleaned_data %>%
  group_by(Month_Year) %>%
  summarise(
    Total_Sales = sum(Sales),
    Total_Profit = sum(Profit)
  ) %>%
  arrange(as.Date(paste0("01-", Month_Year), format = "%d-%b-%Y"))

sales_profit_monthly

# Plot with Month-Year in X-axis
ggplot(sales_profit_monthly, aes(x = factor(Month_Year, levels = Month_Year))) +
  geom_line(aes(y = Total_Sales, color = "Sales", group = 1), linewidth = 1) +
  geom_line(aes(y = Total_Profit, color = "Profit", group = 1), linewidth = 1) +
  labs(
    title = "Monthly Sales & Profit Trend",
    x = "Month-Year",
    y = "Amount",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8))





# Key Observations:


# The chart displays the Monthly Sales and Profit Trend for a business from November 2013 to February 2018.

# Sales (Blue Line):

# Sales exhibit significant fluctuations over time, with noticeable peaks and valleys.
# There is a general upward trend in sales, especially from 2016 onward, indicating growth in revenue.
# The highest sales value is observed around late 2017.

# Profit (Red Line):

# Profit remains relatively low compared to sales throughout the period.
# Although profits also show fluctuations, they are less pronounced than sales.
# There is no clear upward trend in profits, suggesting that while sales are increasing, profitability may not be improving proportionally.

# Seasonality:

# Both sales and profit seem to follow a seasonal pattern, with spikes occurring at regular intervals, possibly indicating higher activity during specific months (e.g., holiday seasons or end-of-year periods).

# Sales vs. Profit Relationship:

# Despite rising sales, profits remain consistently low, which could indicate high operational costs, discounts, or other factors affecting profitability.

# Insights:

# The business is growing in terms of revenue but may need to focus on improving profit margins.
# The seasonal spikes could be leveraged further by optimizing inventory or marketing strategies during peak periods.
# A deeper analysis of costs and pricing strategies might help in converting higher sales into better profits.
# This chart highlights the importance of balancing revenue growth with profitability for sustainable business success.


"Quarterly"
  

# Create Quarter-Year Column

cleaned_data$Quarter_Year <- paste0(quarters(cleaned_data$Order.Date), "-", format(cleaned_data$Order.Date, "%Y"))


# Create a Helper Column for Sorting
cleaned_data$Quarter_Num <- paste0(format(cleaned_data$Order.Date, "%Y"), "-", quarters(cleaned_data$Order.Date))

# Aggregate Sales & Profit Quarterly
sales_profit_quarterly <- cleaned_data %>%
  group_by(Quarter_Year, Quarter_Num) %>%
  summarise(
    Total_Sales = sum(Sales),
    Total_Profit = sum(Profit)
  ) %>%
  arrange(Quarter_Num)
sales_profit_quarterly

# Plot Quarterly Trend
ggplot(sales_profit_quarterly, aes(x = factor(Quarter_Year, levels = Quarter_Year))) +
  geom_line(aes(y = Total_Sales, color = "Sales", group = 1), linewidth = 1) +
  geom_line(aes(y = Total_Profit, color = "Profit", group = 1), linewidth = 1) +
  labs(
    title = "Quarterly Sales & Profit Trend",
    x = "Quarter-Year",
    y = "Amount",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10))


## Key Observations:

# Sales Trend (Cyan Line):

# Sales show a cyclical pattern, with peaks and troughs occurring periodically.
# There is an overall upward trend in sales over the years, with Q4-2017 showing the highest sales amount.
# Seasonal spikes are noticeable, indicating higher sales in certain quarters.

# Profit Trend (Red Line):

# Profits remain relatively flat compared to sales, with minor fluctuations.
# The profit values are significantly lower than sales throughout the period.
# While there is some growth in profits over time, it is not as pronounced as the growth in sales.

# Disparity Between Sales and Profit:

# The gap between sales and profit is substantial, suggesting high costs or low-profit margins despite increasing sales.
# Even during periods of peak sales, profits do not show a corresponding sharp increase.

# Insights:

# The company may need to investigate cost structures or pricing strategies to improve profitability.
# The seasonal nature of sales could be leveraged for better inventory and marketing planning.
# Despite growing sales, the relatively stagnant profit trend may indicate inefficiencies or challenges in converting revenue into profit.
# This chart highlights the need for strategic focus on profitability while sustaining sales growth.



# Optional task:

# Seasonality heatmap:

# Extract Month & Year
cleaned_data$Month <- format(cleaned_data$Order.Date, "%b")
cleaned_data$Year <- format(cleaned_data$Order.Date, "%Y")

# Aggregate Sales
heatmap_data <- cleaned_data %>%
  group_by(Year, Month) %>%
  summarise(Total_Sales = sum(Sales))

heatmap_data

# Heatmap Plot
ggplot(heatmap_data, aes(x = Month, y = Year, fill = Total_Sales)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightyellow", high = "darkred") +
  labs(title = "Sales Seasonality Heatmap", fill = "Sales") +
  theme_minimal()



# key insights:


" This is a Sales Seasonality Heatmap.
It shows monthly sales patterns from 2014 to 2017.
The x-axis represents the months (Jan to Dec).
The y-axis represents the years (2014 to 2017).

The color shows the sales amount:
  
Light color = Low Sales
Dark color = High Sales

Key Insights:

Sales are highest in November (2017) — very dark color.
December and September also show high sales every year.
February has the lowest sales — very light color.
Sales are generally better in the second half of the year (Aug to Dec).

Conclusion:

This heatmap helps identify:
Best sales months → Nov, Dec, Sep
Slow sales months → Feb, Jan
Seasonal sales trends over the years."

# year on year growth rate:

# Yearly Aggregation
yearly_data <- cleaned_data %>%
  group_by(Year) %>%
  summarise(Total_Sales = sum(Sales), Total_Profit = sum(Profit))

# Calculate YoY Growth
yearly_data <- yearly_data %>%
  mutate(Sales_YoY_Growth = round((Total_Sales / lag(Total_Sales) - 1) * 100, 2),
         Profit_YoY_Growth = round((Total_Profit / lag(Total_Profit) - 1) * 100, 2))

yearly_data

# key insights:

"•	In 2014, sales were ₹4.84 lakh and profit was ₹49k.
•	In 2015, sales dropped slightly by 2.83%, but profit increased to ₹61k.
•	In 2016, sales grew strongly by 29.5%, profit also increased to ₹81k.
•	In 2017, sales further grew by 20.4%, and profit reached ₹93k.
Sales and Profit both increased well after 2015. The business is showing good growth from 2016 onwards."






## Q4: Which states or cities show high sales but low profits, and what corrective actions might be suggested based on the analysis?

# for states

# Create Sale_Profit_Ratio column with 2 decimal places
cleaned_data <- cleaned_data %>%
  mutate(Sale_Profit_Ratio = ifelse(Sales == 0, 0, round(Profit / Sales, 2)))


# Find 5 States with Lowest Sale_Profit_Ratio
lowest_states <- cleaned_data %>%
  group_by(State) %>%
  summarise(Avg_Sale_Profit_Ratio = mean(Sale_Profit_Ratio, na.rm = TRUE)) %>%
  arrange(Avg_Sale_Profit_Ratio) %>%
  slice_head(n = 5)

# set a KPI:

lowest_states <- lowest_states %>%
  mutate(KPI_Status = case_when(
    Avg_Sale_Profit_Ratio == 0 ~ "Target Met",  # Exactly 0
    Avg_Sale_Profit_Ratio < 0 ~ "Loss",         # Negative ratio
    Avg_Sale_Profit_Ratio > 0 ~ "Profit"        # Positive ratio
  ))

# View the result
print(lowest_states)

# plot

ggplot(lowest_states, aes(x = reorder(State, Avg_Sale_Profit_Ratio), y = Avg_Sale_Profit_Ratio)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(
    title = "States with Lowest Sale-Profit Ratio",
    x = "State",
    y = "Average Sale-Profit Ratio"
  ) +
  theme_minimal()



# A negative Sale_Profit_Ratio means the company is facing losses (Profit < 0) despite sales.

# Possible Reasons:


# High Discounting practices → Cutting too much into profit margins
# Higher Shipping or Operating Costs in these states
# Poor Inventory Management → Overstocking or obsolescence
# Unprofitable Product Mix → Selling low-margin products
# Inefficient Supply Chain in these regions
# Regional Competition forcing aggressive pricing

# Recommended Corrective Actions:


# Discount Strategy	Reassess discount policies in these states. Apply targeted discounts only on slow-moving products. Avoid blanket discounting.
# Product Portfolio	Perform product-wise profit analysis → Identify and phase out unprofitable products. Promote high-margin products.
# Operational Costs	Analyze supply chain costs (logistics, warehousing). Optimize routes or suppliers to reduce overheads.
# Regional Marketing	Conduct market research in these states to understand customer behavior and price sensitivity. Modify campaigns accordingly.
# Pricing Strategy	Consider revising the pricing model or bundling products for better profitability.

# Illinois and Texas are major concern areas with a significantly negative Sale-Profit Ratio.
# Immediate corrective actions are recommended to control discounting practices, reassess product strategies, and optimize operational costs in these states.



# for cities:

# Group by City, calculate average Sale_Profit_Ratio
lowest_cities <- cleaned_data %>%
  group_by(City) %>%
  summarise(Avg_Sale_Profit_Ratio = mean(Sale_Profit_Ratio, na.rm = TRUE)) %>%
  arrange(Avg_Sale_Profit_Ratio) %>% # Ascending order
  head(5) # Bottom 5 Cities


# set a KPI:

lowest_cities <- lowest_cities %>%
  mutate(KPI_Status = case_when(
    Avg_Sale_Profit_Ratio == 0 ~ "Target Met",  # Exactly 0
    Avg_Sale_Profit_Ratio < 0 ~ "Loss",         # Negative ratio
    Avg_Sale_Profit_Ratio > 0 ~ "Profit"        # Positive ratio
  ))

# Display the result
print(lowest_cities)

# Plot:


ggplot(lowest_cities, aes(x = reorder(City, Avg_Sale_Profit_Ratio), y = Avg_Sale_Profit_Ratio)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(
    title = "Cities with Lowest Sale-Profit Ratio",
    x = "City",
    y = "Average Sale-Profit Ratio"
  ) +
  theme_minimal()



# Interpretation of Findings:

" The bottom 5 cities — Abilene, Romeoville, Deer Park, Missouri City, Littleton — have very poor Sale_Profit_Ratio, indicating these cities are incurring heavy losses relative to sales"
" Especially Abilene with -2.7 means for every 1 unit of sales, the company is losing 2.7 units — a critical situation!"

# Possible Reasons for Low Sale_Profit_Ratio:

# Heavy Discounts or Promotions	Aggressive discounting to boost sales leading to losses.
# High Operational Costs	Shipping cost, warehouse cost, or delivery inefficiencies in those cities.
# Product Returns	Higher product return rates affecting profitability.
# Wrong Product Mix	Selling low-margin or loss-making products.
# Low Customer Lifetime Value	One-time buyers with minimal repeat purchases.

# Suggested Corrective Actions:

# Conduct pricing analysis specific to these cities.
# Optimize or restrict discounts on non-profitable products.
# Implement minimum profit margin thresholds.
# Optimize Logistics & Delivery Costs
# Audit shipping costs for these cities.
# Explore cheaper logistics partners.
# Consider regional warehouses for cost efficiency.
# Analyze which product categories are dragging down profits.
# Focus on promoting high-margin products.
# Consider discontinuing consistently loss-making products.
# Identify high-value customer segments in these cities.
# Targeted loyalty programs or exclusive deals for profitable segments.
# Personalized marketing instead of mass discounts.
# Investigate operational bottlenecks.
# Streamline supply chain for these regions.
# Analyze inventory holding costs.




# Optional task:

# Is Discount strongly correlated with low profit?

cor(cleaned_data$Discount, cleaned_data$Profit, use = "complete.obs")

cor.test(cleaned_data$Discount, cleaned_data$Profit,
         alternative = "less",  # because H1: ρ < 0
         method = "pearson")

# Remarks:


# The correlation between Discount and Profit is –0.2194875, with a very small p-value.
# Therefore, there is sufficient statistical evidence to conclude that there is a significant negative correlation between Discount and Profit.
# This implies that products with higher discounts are more likely to generate lower profit.



