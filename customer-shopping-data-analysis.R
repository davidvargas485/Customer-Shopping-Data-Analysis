##################################
# Customer Shopping Data Analysis
# By: David Vargas
# Date: 5/14/2023
##################################

# Load necessary packages
library(tidyverse)
library(lubridate)
library(dplyr)
library(skimr)
library(tidyquant)
library(ggplot2)

# Establish working directory
setwd("/Users/davidvargas/Downloads")

# Import the data
shopping_data <- read_csv("customer_shopping_data.csv")

# Inspect the data
view(shopping_data)
summary(shopping_data)
skim(shopping_data)

# Let's standardize the date values in the "invoice_date" column
shopping_data$inv_date <- parse_date_time(shopping_data$invoice_date, orders = c('dmy','mdy'))
shopping_data$inv_date <- date(shopping_data$inv_date)

# Remove the uncleaned invoice_date column
shopping_data <- shopping_data %>% 
  select(-c(9))

# Let's create a new column for the month of each invoice
shopping_data$month <- month(shopping_data$inv_date)

# Let's create a new column for the year of each invoice
shopping_data$year <- year(shopping_data$inv_date)

# Validate the data
str(shopping_data)
view(shopping_data)

# Create a rank for the shopping malls in terms of usage
mall_usage <- shopping_data %>% 
  select(shopping_mall) %>% 
  group_by(shopping_mall) %>% 
  summarize(usage=n()) %>% 
  arrange(-usage)

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#999999","#CC6666","#9999CC", "#66CC99")
ggplot(aes(x=reorder(shopping_mall,-usage),y=usage,fill=shopping_mall), data=mall_usage)+
  geom_bar(stat='identity')+
  theme_tq()+
  labs(title="Shopping Mall Usage",subtitle="Ranked by Most Purchases to Least")+
  labs(x="Shopping Mall")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
  scale_y_continuous(expand = c(0.01,0), name="Number of Purchases", labels = scales::comma)+
  theme(legend.position="none")+
  scale_fill_manual(values=cbPalette)

# Let's visualize the age range of our customers.
age_data <- shopping_data %>% 
  distinct(customer_id, age) %>% 
  arrange(age)

age <- age_data %>%
  select(age) %>% 
  group_by(age) %>% 
  summarize(count=n()) %>% 
  arrange(age)

ggplot(aes(x=age), data=shopping_data)+
  geom_bar(fill="#56B4E9")+
  theme_tq()+
  labs(title="Customer Age Range",subtitle="Number of People per Age")+
  labs(x="Age")+
  scale_y_continuous(expand = c(0.01,0), name="Number of People", labels = scales::comma)

# Let's visualize the gender of our customers.
gender_data <- shopping_data %>%
  distinct(customer_id,gender)

gender <- gender_data %>% 
  select(gender) %>% 
  group_by(gender) %>% 
  summarize(count=n()) %>% 
  arrange(-count)

ggplot(aes(x=gender,y=count,fill=gender), data=gender)+
  geom_bar(stat='identity')+
  labs(title="Customer Population Breakdown by Gender", subtitle = " ")+
  labs(x="Gender")+
  scale_y_continuous(expand = c(0.01,0), name="Number of People", labels = scales::comma)+
  theme_tq()+
  scale_fill_manual(values=c("#56B4E9","#D55E00"))+
  theme(legend.position="none")

# What is the most used payment method?
payment_usage <- shopping_data %>% 
  select(payment_method) %>% 
  group_by(payment_method) %>% 
  summarize(usage=n()) %>% 
  arrange(-usage)
tibble(payment_usage)

ggplot(aes(x=reorder(payment_method,-usage),y=usage,fill=payment_method), data=payment_usage)+
  geom_bar(stat='identity')+
  labs(title="Payment Method Usage",subtitle="Ranked by Most Times Used to Least")+
  labs(x="Payment Method")+
  scale_y_continuous(expand = c(0.01,0), name="Number of Times Used", labels = scales::comma)+
  theme_tq()+
  theme(legend.position="none")+
  scale_fill_manual(values=cbPalette)

# What was the preferred payment method for each age group?
shopping_data$age_group <- 
  case_when(
    shopping_data$age > 0 & shopping_data$age <= 23 ~ '23 or Younger',
    shopping_data$age > 23 & shopping_data$age <= 46 ~ '24 to 46',
    shopping_data$age > 46 & shopping_data$age <= 69 ~ '47 to 69'
  )

payment_age <- shopping_data %>% 
  select(payment_method, age_group) %>% 
  group_by(payment_method, age_group) %>% 
  summarize(count=n()) %>% 
  arrange(payment_method,-count)

legend <- "Age Group"
ggplot(aes(x=payment_method,y=count,fill=age_group),data=payment_age)+
  geom_bar(position='dodge',stat='identity')+
  scale_y_continuous(expand = c(0.01,0), name="Number of Times Used", labels = scales::comma)+
  labs(title="Payment Method Usage")+
  labs(subtitle = "Differentiated by Age Group")+
  labs(x="Payment Method")+
  theme_tq()+
  scale_fill_manual(legend,values=c("#56B4E9","#D55E00","#E18092"))

# What was the preferred payment method for each gender?

payment_gender <- shopping_data %>% 
  select(payment_method, gender) %>% 
  group_by(payment_method, gender) %>% 
  summarize(count=n()) %>% 
  arrange(payment_method,-count)

legend2 <- "Gender"
ggplot(aes(x=payment_method,y=count,fill=gender),data=payment_gender)+
  geom_bar(position='dodge',stat='identity')+
  scale_y_continuous(expand = c(0.01,0), name="Number of Times Used", labels = scales::comma)+
  labs(title="Payment Method Usage")+
  labs(subtitle = "Differentiated by Gender")+
  labs(x="Payment Method")+
  theme_tq()+
  theme(panel.background = element_rect(fill="white"))+
  scale_fill_manual(legend2,values=c("#56B4E9","#D55E00"))

# Which months were the busiest?
busiest_months <- shopping_data %>% 
  filter(year != 2023 | month !=3) %>% 
  select(month,year,invoice_no) %>% 
  group_by(month,year) %>% 
  summarize(num_invoices=n()) %>% 
  arrange(year,month)

busiest_months <- busiest_months %>% 
  group_by(month) %>% 
  summarize(avg_num_invoices=mean(num_invoices)) %>% 
  arrange(-avg_num_invoices)

tibble(busiest_months)

ggplot(aes(x=reorder(month,month),y=avg_num_invoices,fill=factor(month)), data=busiest_months)+
  geom_bar(stat='identity')+
  labs(title="Average Total Number of Invoices Per Month",subtitle="From 2021 to 2023")+
  labs(x="Month")+
  scale_y_continuous(expand = c(0.01,0), name="Avg. Total Invoices", labels = scales::comma)+
  theme_tq()+
  theme(legend.position="none")+
  scale_fill_manual(values=cbPalette)

# What are the most popular product categories?
pop_product <- shopping_data %>% 
  select(category) %>% 
  group_by(category) %>% 
  summarize(count=n()) %>% 
  arrange(-count)
tibble(pop_product)

ggplot(aes(x=reorder(category,-count),y=count,fill=category), data=pop_product)+
  geom_bar(stat='identity')+
  theme_tq()+
  labs(title="Number of Purchases per Product Category",subtitle="Ranked by Most Purchased to Least")+
  labs(x="Product Category")+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
  scale_y_continuous(expand = c(0.01,0), name="Number of Purchases", labels = scales::comma)+
  theme(legend.position="none")+
  scale_fill_manual(values=cbPalette)

# How many purchases of each product category did various age groups make?
purchases_age <- shopping_data %>% 
  select(category, age_group) %>% 
  group_by(category, age_group) %>% 
  summarize(count=n()) %>% 
  arrange(category, -count)

ggplot(aes(x=reorder(category,-count),y=count,fill=age_group),data=purchases_age)+
  geom_bar(position='dodge',stat='identity')+
  scale_y_continuous(expand = c(0.01,0), name="Number of Purchases", labels = scales::comma)+
  labs(title="Number of Purchases per Product Category")+
  labs(subtitle = "Differentiated by Age Group")+
  labs(x="Product Category")+
  theme_tq()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
  theme(panel.background = element_rect(fill="white"))+
  scale_fill_manual(legend,values=c("#56B4E9","#D55E00","#E18092"))

# How many purchases of each product category did each gender make?

purchases_gender <- shopping_data %>% 
  select(category, gender) %>% 
  group_by(category, gender) %>% 
  summarize(count=n()) %>% 
  arrange(category,-count)

ggplot(aes(x=reorder(category,-count),y=count,fill=gender),data=purchases_gender)+
  geom_bar(position='dodge',stat='identity')+
  scale_y_continuous(expand = c(0.01,0), name="Number of Purchases", labels = scales::comma)+
  labs(title="Number of Purchases per Product Category")+
  labs(subtitle = "Differentiated by Gender")+
  labs(x="Product Category")+
  theme_tq()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
  theme(panel.background = element_rect(fill="white"))+
  scale_fill_manual(legend2,values=c("#56B4E9","#D55E00"))

# In what price range were most purchases made by each age group?
shopping_data$amount_spent <- 
  case_when(
    shopping_data$price > 0 & shopping_data$price <= 250 ~ 'A: $0 to $250',
    shopping_data$price > 250 & shopping_data$price <= 500 ~ 'B: $251 to $500',
    shopping_data$price > 500 & shopping_data$price <= 750 ~ 'C: $501 to $750',
    shopping_data$price > 750 & shopping_data$price <= 1000 ~ 'D: $751 to $1,000',
    shopping_data$price > 1000 & shopping_data$price <= 1250 ~ 'E: $1,001 to $1,250',
    shopping_data$price > 1250 & shopping_data$price <= 1500 ~ 'F: $1,251 to $,1500',
    shopping_data$price > 1500 ~ 'G: Over $1,500'
  )

spend_age <- shopping_data %>% 
  select(amount_spent, age_group) %>% 
  group_by(amount_spent, age_group) %>% 
  summarize(count=n()) %>% 
  arrange(amount_spent,-count)

ggplot(aes(x=amount_spent,y=count,fill=age_group),data=spend_age)+
  geom_bar(position='dodge',stat='identity')+
  scale_y_continuous(expand = c(0.01,0), name="Number of Purchases", labels = scales::comma)+
  labs(title="Number of Purchases in Each Spending Range")+
  labs(subtitle = "Differentiated by Age Group")+
  labs(x="Amount Spent in USD")+
  theme_tq()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
  theme(panel.background = element_rect(fill="white"))+
  scale_fill_manual(legend,values=c("#56B4E9","#D55E00","#E18092"))

# In what price range were most purchases made by each gender?
spend_gender <- shopping_data %>% 
  select(amount_spent, gender) %>% 
  group_by(amount_spent, gender) %>% 
  summarize(count=n()) %>% 
  arrange(amount_spent,-count)

ggplot(aes(x=amount_spent,y=count,fill=gender),data=spend_gender)+
  geom_bar(position='dodge',stat='identity')+
  scale_y_continuous(expand = c(0.01,0), name="Number of Purchases", labels = scales::comma)+
  labs(title="Number of Purchases in Each Spending Range")+
  labs(subtitle = "Differentiated by Gender")+
  labs(x="Amount Spent in USD")+
  theme_tq()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
  theme(panel.background = element_rect(fill="white"))+
  scale_fill_manual(legend2,values=c("#56B4E9","#D55E00","#E18092"))

# Which months have generated the highest total revenue?
month_revenue <- shopping_data %>% 
  filter(year != 2023 | month !=3) %>% 
  select(month,year,price) %>% 
  group_by(month,year) %>% 
  summarize(total_revenue=sum(price)) %>% 
  arrange(year,month)

month_revenue <- month_revenue %>% 
  group_by(month) %>% 
  summarize(avg_total_revenue=mean(total_revenue)) %>% 
  arrange(-avg_total_revenue)

ggplot(aes(x=reorder(month,month),y=avg_total_revenue,fill=factor(month)),data=month_revenue)+
  geom_bar(position='dodge',stat='identity')+
  scale_y_continuous(expand = c(0.01,0), name="Avg. Total Revenue in USD", labels = scales::comma)+
  labs(title="Average Total Revenue per Month")+
  labs(subtitle = "From 2021 to 2023")+
  labs(x="Month")+
  theme_tq()+
  theme(legend.position="none")+
  scale_fill_manual(values=cbPalette)

# Compare the total revenues of each year.
year_revenue <- shopping_data %>% 
  filter(year != 2023 | month !=3) %>% 
  select(month,year,price) %>% 
  group_by(month,year) %>% 
  summarize(total_revenue=sum(price)) %>% 
  arrange(year,month)

year_revenue$year <- factor(year_revenue$year)

legend3 <- "Year"
x_axis_labels <- 1:12
ggplot() + geom_line(data = year_revenue, aes(x = month, y = total_revenue, color = year))+
  labs(title="Total Monthly Revenue in USD", subtitle="Differentiated by Year")+
  labs(x="Month",y="Total Revenue in USD")+
  theme_tq()+
  scale_color_manual(legend3,values=cbPalette)+
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)




