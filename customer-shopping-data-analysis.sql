# Create a rank for the shopping malls in terms of usage.

SELECT
  shopping_mall,
  COUNT(invoice_no) AS usage
FROM
  projects.customer_shopping_data
GROUP BY
  shopping_mall
ORDER BY
  usage DESC;

# Let's get a better understanding of the age of our customers.

WITH ageCTE AS (
SELECT 
  DISTINCT customer_id,
  age
FROM
  projects.customer_shopping_data
)
SELECT
  age,
  COUNT(age) AS num_people
FROM
  ageCTE
GROUP BY 
  age
ORDER BY
  age;

# Customer gender breakdown

WITH genderCTE AS (
SELECT 
  DISTINCT customer_id,
  gender
FROM
  projects.customer_shopping_data
)
SELECT
  gender,
  COUNT(gender) AS num_people
FROM
  genderCTE
GROUP BY
  gender
ORDER BY
  num_people DESC;

# What is the most used payment method?

SELECT
  payment_method,
  COUNT(payment_method) AS usage
FROM
  projects.customer_shopping_data
GROUP BY
  payment_method
ORDER BY
  usage DESC;

# What was the preferred payment method for each age group?

SELECT
  payment_method,
  age_group,
  COUNT(payment_method) AS times_used
FROM
  (
  SELECT
    CASE
      WHEN age BETWEEN 0 AND 23 THEN "0 to 23"
      WHEN age BETWEEN 24 AND 46 THEN "24 to 46"
      WHEN age BETWEEN 47 AND 69 THEN "47 to 69"
    END AS age_group,
    payment_method
  FROM
    projects.customer_shopping_data
  )
GROUP BY
  payment_method,
  age_group
ORDER BY
  payment_method,
  times_used DESC;

# What was the preferred payment method for each gender?

SELECT
  payment_method,
  gender,
  COUNT(payment_method) AS times_used
FROM
  projects.customer_shopping_data
GROUP BY
  payment_method,
  gender
ORDER BY
  payment_method,
  times_used DESC;

# Which months were the busiest? (Average total number of invoices per month from 2021 to 2023)

WITH busiest_months AS (
SELECT
  (SELECT CONCAT(CAST(EXTRACT(YEAR FROM DATE (invoice_date)) AS STRING),"-",CAST(EXTRACT(MONTH FROM DATE 
  (invoice_date)) AS STRING))) AS date,
  COUNT(invoice_no) AS num_invoices
FROM
  projects.customer_shopping_data
GROUP BY 
  date
ORDER BY
  date
)
SELECT
  AVG(num_invoices) AS avg_num_invoices,
  SPLIT(date, '-')[OFFSET(1)] AS month
FROM
  busiest_months
WHERE
  date != '2023-3' /* Must remove march of 2023 because there is only 8 days of data for this month */
GROUP BY
  month
ORDER BY
  avg_num_invoices DESC;

# Which were the most popular product categories?

SELECT
  category,
  COUNT(category) AS num_purchases
FROM
  projects.customer_shopping_data
GROUP BY 
  category
ORDER BY
  num_purchases DESC;

# How many purchases of each product category did various age groups make?

WITH age_groupCTE AS (
SELECT
  CASE
    WHEN age BETWEEN 0 AND 23 THEN "0 to 23"
    WHEN age BETWEEN 24 AND 46 THEN "24 to 46"
    WHEN age BETWEEN 47 AND 69 THEN "47 to 69"
  END AS age_group,
  category
FROM
  projects.customer_shopping_data
)
SELECT
  category,
  age_group,
  COUNT(age_group) AS num_purchases
FROM
  age_groupCTE
GROUP BY
  category,
  age_group
ORDER BY
  category,
  num_purchases DESC;

# How many purchases of each product category did each gender make?

SELECT
  category,
  gender,
  COUNT(gender) AS num_purchases
FROM
  projects.customer_shopping_data
GROUP BY
  category,
  gender
ORDER BY
  category,
  num_purchases DESC;

# In what price range were most purchases made by each age group?

WITH priceCTE AS (
SELECT
  CASE
    WHEN price BETWEEN 0 AND 250 THEN'A: $0 to $250'
    WHEN price BETWEEN 250 AND 500 THEN 'B: $251 to $500'
    WHEN price BETWEEN 500 AND 750 THEN 'C: $501 to $750'
    WHEN price BETWEEN 750 AND 1000 THEN 'D: $751 to $1,000'
    WHEN price BETWEEN 1000 AND 1250 THEN 'E: $1,001 to $1,250'
    WHEN price BETWEEN 1250 AND 1500 THEN 'F: $1,251 to $,1500'
    WHEN price > 1500 THEN 'G: Over $1,500'
  END AS amount_spent,
  CASE
    WHEN age BETWEEN 0 AND 23 THEN "0 to 23"
    WHEN age BETWEEN 24 AND 46 THEN "24 to 46"
    WHEN age BETWEEN 47 AND 69 THEN "47 to 69"
  END AS age_group
FROM
  projects.customer_shopping_data
)
SELECT
  amount_spent,
  age_group,
  COUNT(amount_spent) AS num_purchases
FROM
  priceCTE
GROUP BY
  amount_spent,
  age_group
ORDER BY
  amount_spent,
  num_purchases DESC;

# In what price range were most purchases made by each gender?

WITH priceCTE2 AS (
SELECT
  CASE
    WHEN price BETWEEN 0 AND 250 THEN'A: $0 to $250'
    WHEN price BETWEEN 250 AND 500 THEN 'B: $251 to $500'
    WHEN price BETWEEN 500 AND 750 THEN 'C: $501 to $750'
    WHEN price BETWEEN 750 AND 1000 THEN 'D: $751 to $1,000'
    WHEN price BETWEEN 1000 AND 1250 THEN 'E: $1,001 to $1,250'
    WHEN price BETWEEN 1250 AND 1500 THEN 'F: $1,251 to $,1500'
    WHEN price > 1500 THEN 'G: Over $1,500'
  END AS amount_spent,
  gender
FROM
  projects.customer_shopping_data
)
SELECT
  amount_spent,
  gender,
  COUNT(amount_spent) AS num_purchases
FROM
  priceCTE2
GROUP BY
  amount_spent,
  gender
ORDER BY
  amount_spent,
  num_purchases DESC;

# Which months have generated the highest average total revenue?

WITH monthly_revenueCTE AS (
SELECT
  (SELECT CONCAT(CAST(EXTRACT(YEAR FROM DATE (invoice_date)) AS STRING),"-",CAST(EXTRACT(MONTH FROM DATE (invoice_date)) AS STRING))) AS date,
  SUM(price) AS total_revenue
FROM
  projects.customer_shopping_data
GROUP BY 
  date
ORDER BY
  date
)
SELECT
  AVG(total_revenue) AS avg_total_revenue,
  SPLIT(date, '-')[OFFSET(1)] AS month
FROM
  monthly_revenueCTE
WHERE
  date != '2023-3'
GROUP BY
  month
ORDER BY
  avg_total_revenue DESC;

# Compare the monthly total revenue for each year.

SELECT
  month,
  year,
  total_revenue
FROM
  (SELECT
    EXTRACT(year FROM invoice_date) AS year,
    EXTRACT(month FROM invoice_date) AS month,
    ROUND(SUM(price),2) AS total_revenue
  FROM
    projects.customer_shopping_data
  GROUP BY
    month,
    year)
ORDER BY
  year,
  month;
