# ノック1
SELECT * FROM payment;

# ノック2
SELECT
 payment_id,
 customer_id
FROM
 payment
WHERE
 customer_id = 1
;

# ノック3
SELECT
 first_name,
 last_name
FROM
 customer
WHERE
 first_name = 'KELLY'
;

# ノック4
SELECT
 first_name,
 last_name
FROM
 customer
WHERE
 first_name = 'KELLY'
 AND last_name = 'KNOTT'
;

# ノック5
SELECT
 first_name,
 last_name
FROM
 customer
WHERE
 first_name = 'KELLY'
 OR first_name = 'MARIA'
;

# ノック6
SELECT
 first_name,
 last_name
FROM
 customer
WHERE
 NOT (
 first_name = 'KELLY'
 OR first_name = 'MARIA'
 );

# ノック7
SELECT
 first_name,
 last_name
FROM
 customer
WHERE
 first_name IN ('AARON', 'ADAM', 'ANN')
;

# ノック8
SELECT
 payment_id,
 amount
FROM
 payment
WHERE
 amount >= 6.99
;

# ノック9
SELECT
 payment_id,
 amount
FROM
 payment
WHERE
 amount != 0.99
;

# ノック10
SELECT
 rental_id,
 return_date
FROM
 rental
WHERE
 return_date IS NULL
;

# ノック11
SELECT
 rental_id,
 return_date
FROM
 rental
WHERE
 return_date IS NOT NULL
;

# ノック12
SELECT
 customer_id,
 first_name,
 last_name
FROM
 customer
WHERE
 customer_id BETWEEN 11 AND 13
;

# ノック13
SELECT
 title,
 description
FROM
 film
WHERE
 description LIKE '%Amazing%'
;

# ノック14
SELECT
 title,
 description
FROM
 film
WHERE
 description NOT LIKE '%Amazing%';

# ノック15
SELECT COUNT(*) FROM payment;

# ノック16
SELECT
 DISTINCT customer_id
FROM
 payment
;

# ノック17
SELECT
 COUNT(DISTINCT customer_id)
FROM
 payment
;

# ノック18
SELECT
 customer_id,
 last_name
FROM
 customer
ORDER BY
 last_name
;

# ノック19
SELECT
 customer_id,
 first_name,
 last_name
FROM
 customer
ORDER BY
 customer_id DESC
;

# ノック20
SELECT
 first_name,
 last_name
FROM
 customer
ORDER BY
 customer_id DESC
LIMIT 3
;

# ノック21
SELECT
 customer_id,
 COUNT(*) AS payment_count
FROM
 payment
GROUP BY
 customer_id
ORDER BY
 payment_count DESC
LIMIT 3
;

# ノック22
SELECT
 amount * 110 AS amount_yen
FROM
 payment
LIMIT 3
;

# ノック23
SELECT
 CONCAT(
 ROUND(amount * 110),
 'yen'
 ) AS amount_yen
FROM
 payment
LIMIT 3
;

# ノック24
SELECT
  payment_id,
  first_name,
  last_name
FROM
  payment
  LEFT JOIN customer
    ON payment.customer_id
      = customer.customer_id
;

# ノック25
SELECT
  payment_id,
  payment.customer_id,
  amount
FROM
  payment
  INNER JOIN customer
    ON payment.customer_id
      = customer.customer_id
WHERE
  first_name = 'BRIAN'
  AND last_name = 'WYMAN'
;

# ノック26
SELECT
  category.name AS name,
  COUNT(category.name) AS film_cnt
FROM
  film
  INNER JOIN film_category
    USING(film_id)
  INNER JOIN category
    USING(category_id)
GROUP BY
  category.name
HAVING
  COUNT(category.name) >= 65
ORDER BY
  film_cnt DESC
;

# ノック27
SELECT
  payment_id,
  amount,
  CASE
    WHEN amount > 5 THEN 'expensive'
    WHEN amount > 1 THEN 'modest'
    ELSE 'cheap'
  END AS price_range
FROM
  payment
;

# ノック28
SELECT
  COUNT(*)
FROM
  film
WHERE
  description ~ '(Thou|Insi)ghtful'
;

# ノック29
SELECT
  customer_id,
  SUM(amount) AS total_sales
FROM
  payment
GROUP BY
  customer_id
ORDER BY
  total_sales DESC
LIMIT 5
;

# ノック30
SELECT
  CAST(payment_date AS DATE) AS p_date,
  SUM(amount) AS total_sales
FROM
  payment
GROUP BY
  p_date
ORDER BY
  p_date
;

# ノック31
SELECT
  EXTRACT(YEAR FROM payment_date) AS yyyy,
  EXTRACT(MONTH FROM payment_date) AS mm,
  SUM(amount) AS total_sales
FROM
  payment
GROUP BY
  yyyy,
  mm
ORDER BY
  yyyy,
  mm
;

# ノック32
SELECT
  SUM(amount) AS total_sales
FROM
  payment
WHERE
  CAST(payment_date AS DATE)
    BETWEEN '2007-01-01' AND '2007-01-31'
;

# ノック33
SELECT
  AVG(total_sales),
  MIN(total_sales),
  MAX(total_sales)
FROM (SELECT
    customer_id,
    SUM(amount) AS total_sales
  FROM
    payment
  GROUP BY
    customer_id
  ) AS customer_payment
;

# ノック34
SELECT
  last_name
FROM
  customer AS c
  INNER JOIN payment_p2007_05 AS p
    ON c.customer_id = p.customer_id
;

# ノック35
SELECT
  last_name
FROM
  customer
WHERE
  customer_id IN
    (SELECT
      customer_id
    FROM
      payment_p2007_05
    )
;

# ノック36
SELECT
  last_name
FROM
  customer AS c
WHERE
  EXISTS (
    SELECT
      1
    FROM
      payment_p2007_05 AS p
    WHERE
      c.customer_id = p.customer_id
  )
;

# ノック37
SELECT
  DISTINCT customer_id
FROM
  payment_p2007_01
UNION SELECT
  DISTINCT customer_id
FROM
  payment_p2007_05
;

# ノック38
SELECT
  DISTINCT customer_id
FROM
  payment_p2007_05
INTERSECT
SELECT
  DISTINCT customer_id
FROM
  payment_p2007_05
;

# ノック39
SELECT
  DISTINCT customer_id
FROM
  payment_p2007_01
INTERSECT
SELECT
  DISTINCT customer_id
FROM
  payment_p2007_02
INTERSECT
SELECT
  DISTINCT customer_id
FROM
  payment_p2007_03
;

# ノック40
SELECT
  DISTINCT customer_id
FROM
  payment_p2007_05
EXCEPT
SELECT
  DISTINCT customer_id
FROM
  payment_p2007_01
;

# ノック41
SELECT
  DISTINCT customer_id
FROM
  payment_p2007_01
UNION SELECT
  DISTINCT customer_id
FROM
  payment_p2007_05
ORDER BY
  customer_id ASC
LIMIT 3
;

# ノック42
WITH loyal_customers AS (
  SELECT
    customer_id,
    COUNT(*) AS cnt
  FROM
    payment_p2007_01
  GROUP BY
    customer_id
  HAVING
COUNT(*) >= 7
) SELECT
email FROM
  customer AS c
  INNER JOIN loyal_customers AS lc
    ON c.customer_id = lc.customer_id
WHERE
  c.active = 1
;

# ノック43
SELECT
  cl.name,
  COUNT(*) AS cnt,
  RANK() OVER (
    ORDER BY COUNT(*) DESC
  ) AS ranking
FROM
  payment_p2007_01 AS p
  INNER JOIN customer_list AS cl
    ON p.customer_id = cl.id
GROUP BY
  cl.name
;

# ノック44
SELECT
  cl.id,
  cl.country,
  COUNT(*) AS cnt,
  RANK() OVER (
    PARTITION BY cl.country
    ORDER BY COUNT(*) DESC
  ) AS rank
FROM
  payment_p2007_01 AS p
  INNER JOIN customer_list AS cl
    ON p.customer_id = cl.id
GROUP BY
  cl.id, cl.country
;

# ノック45
SELECT
  cl.id,
  cl.country,
  COUNT(*) AS cnt,
  ROUND(AVG(COUNT(*)) OVER (
    PARTITION BY cl.country
  ), 2) AS avg_pay,
  RANK() OVER (
    PARTITION BY cl.country
    ORDER BY COUNT(*) DESC
  ) AS rank
FROM
  payment_p2007_01 AS p
  INNER JOIN customer_list AS cl
    ON p.customer_id = cl.id
GROUP BY
  cl.id, cl.country
;

# ノック46
SELECT
  cl.id,
  cl.country,
  COUNT(*) AS cnt,
  SUM(COUNT(*)) OVER (
    PARTITION BY cl.country
  ) AS total_pay,
  RANK() OVER (
    PARTITION BY cl.country
    ORDER BY COUNT(*) DESC
  ) AS rank
FROM
  payment_p2007_01 AS p
  INNER JOIN customer_list AS cl
    ON p.customer_id = cl.id
GROUP BY
  cl.id, cl.country
;

# ノック47
SELECT
  cl.country,
  COUNT(*) AS count,
  SUM(COUNT(*)) OVER (
    ORDER BY COUNT(*) DESC
    ROWS BETWEEN
      UNBOUNDED PRECEDING
      AND CURRENT ROW
  ) AS cumulative_count
FROM
  payment_p2007_01 AS p
  INNER JOIN customer_list AS cl
    ON p.customer_id = cl.id
GROUP BY
  cl.country
ORDER BY
  count DESC
;

# ノック48
SELECT
  cl.country,
  ROUND(
    SUM(COUNT(*)) OVER (
      ORDER BY COUNT(*) DESC
      ROWS BETWEEN
        UNBOUNDED PRECEDING
        AND CURRENT ROW
    ) / SUM(COUNT(*)) OVER (),
    2
  ) AS cumulative_percent
FROM
  payment_p2007_01 AS p
  INNER JOIN customer_list AS cl
    ON p.customer_id = cl.id
GROUP BY
  cl.country
ORDER BY
  COUNT(*) DESC
;

# ノック49
SELECT
  CAST(payment_date AS DATE) AS d,
  COUNT(*)
FROM
  payment AS p
WHERE
  CAST(payment_date AS DATE)
    BETWEEN '2007-04-06' AND '2007-04-12'
GROUP BY
d ORDER BY
  d ASC
;

# ノック50
SELECT
  CAST(payment_date AS DATE) AS d,
  COUNT(*),
  ROUND(AVG(COUNT(*)) OVER (
    ORDER BY
      CAST(payment_date AS DATE) ASC
    ROWS BETWEEN
      2 PRECEDING
      AND CURRENT ROW
  ), 2) AS moving_avg
FROM
  payment AS p
WHERE
  CAST(payment_date AS DATE)
    BETWEEN '2007-04-06' AND '2007-04-12'
GROUP BY d
ORDER BY
  d ASC
;
