--Restuarant owner
--5 tables
--1x fact ,4x dimension
--search google how to add foreign key
--write SQL 3 queries analyze data
--1x subquery / with
--SQLite command
.mode markdown
.header on
  
--1st Dimension table
CREATE TABLE DimMenu (
  menuID INT NOT NULL PRIMARY KEY,
  menu VARCHAR,
  category VARCHAR,
  price$ REAL
 ) ;

INSERT INTO DimMenu VALUES 
  (1,'Clam Chowder Soup' ,'Main'     ,11.50),
  (2,'Shrimp Salad'      ,'Main'     ,18.50),
  (3,'Chicken Tenders'   ,'Appetizer',12.50),
  (4,'Fish and Chips'    ,'Appetizer',13.00),
  (5,'Fish Tacos'        ,'Main'     ,13.50),
  (6,'Hot Chocolate'     ,'Beverage' ,3.50),
  (7,'Soda'              ,'Beverage' ,3.00);

--2nd Dimension Table
CREATE TABLE DimCustomer (
  customerID INT NOT NULL PRIMARY KEY,
  name VARCHAR,
  registered_date DATE,
  gender VARCHAR
);

INSERT INTO DimCustomer VALUES
  (1,'Emma'  ,'2020-07-07','Prefer not to say'),
  (2,'Harry' ,'2022-07-10','Male'),
  (3,'John'  ,'2022-08-01','Male'),
  (4,'Kris'  ,'2021-05-15','Prefer not to say'),
  (5,'Luna'  ,'2021-10-20','Female');

--3rd Dimension Table
CREATE TABLE DimStaff (
  staffID INT NOT NULL PRIMARY KEY,
  staff_name VARCHAR,
  hired_date DATE,
  employment_status VARCHAR
);

INSERT INTO DimStaff VALUES
  (1,'Ammy'  ,'2020-01-01','Full time'),
  (2,'Anne'  ,'2020-03-21','Part time'),
  (3,'Tony'  ,'2022-04-28','Part time'),
  (4,'Lisa'  ,'2022-08-20','Full time'),
  (5,'Bam'   ,'2021-07-16','Full time');

--4th Dimension Table
CREATE TABLE DimOrdertype (
  typeID INT NOT NULL PRIMARY KEY,
  type VARCHAR,
  channel VARCHAR
);

INSERT INTO DimOrdertype VALUES
  (1,'Dine in'    , NULL),
  (2,'Take away'  , NULL),
  (3,'Delivery'   , 'Grubhub'),
  (4,'Delivery'   , 'Doordash');

--Fact table
CREATE TABLE orders (
  order_id INT NOT NULL PRIMARY KEY,
  order_date DATE,
  menuID INT,
  quantity INT,
  customerID INT,
  staffID INT,
  typeID INT,
  FOREIGN KEY (menuID)     REFERENCES DimMenu(menuID),
  FOREIGN KEY (customerID) REFERENCES DimCustomer(customerID),
  FOREIGN KEY (staffID)    REFERENCES DimStaff(staffID),
  FOREIGN KEY (typeID)     REFERENCES DimOrdertype(typeID)
);

INSERT INTO orders VALUES
  (1,'2022-08-01',3,2,1,1,1),
  (2,'2022-08-01',1,3,2,1,2),
  (3,'2022-08-02',2,1,1,2,1),
  (4,'2022-08-02',1,4,3,5,1),
  (5,'2022-08-02',7,6,3,5,2),
  (6,'2022-08-03',6,4,2,3,3),
  (7,'2022-08-04',1,5,4,4,4),
  (8,'2022-08-04',1,6,5,4,4),
  (9,'2022-08-04',1,1,1,4,3),
  (10,'2022-08-04',5,1,2,2,1),
  (11,'2022-08-05',5,5,4,4,4),
  (12,'2022-08-05',4,2,1,2,2),
  (13,'2022-08-06',7,2,4,5,1),
  (14,'2022-08-06',3,8,3,2,1),
  (15,'2022-08-06',5,1,5,1,1);

--analayze data
--Q1.Which menu is the best seller?
SELECT 
  dim_m.menu, 
  dim_m.price$ * ord.quantity AS total_price$
FROM orders AS ord
JOIN DimMenu AS dim_m
ON ord.menuID = dim_m.menuID
GROUP BY 1
ORDER BY 2 DESC
LIMIT 1 ;
--ANS1.Clam Chowder Soup

--Q2.Who are the most frequently order ?
SELECT
  dim_c.name,
  count(ord.customerID) AS sum_orders
FROM orders AS ord
JOIN DimCustomer AS dim_c
ON dim_c.customerID = ord.customerID
GROUP BY ord.customerID
ORDER BY sum_orders 
LIMIT 1 ;
--ANS2.Luna

--Q3.How many orders which come from 'Doordash' ?
SELECT
  type,
  delivery_ch,
  count(delivery_ch) AS sum_deli
FROM (
      SELECT
        ord.typeID,
        dim_o.type,
        COALESCE(dim_o.channel,'Not Delivery') AS delivery_ch
      FROM orders AS ord
      JOIN DimOrdertype AS dim_o
      ON ord.typeID = dim_o.typeID 
      )
WHERE delivery_ch = 'Doordash' ;
--ANS3. 3 0rders from Doordash Delivery

--Q4.Which 'Full Time' Staff get the least income to our restaurant ?
WITH sub AS (
  SELECT
    dim_s.staff_name,
    dim_m.price$ * ord.quantity AS total_income$
  FROM orders AS ord
  JOIN DimMenu AS dim_m ON ord.menuID = dim_m.menuID
  JOIN DimStaff AS dim_s ON ord.staffID = dim_s.staffID
  WHERE dim_s.employment_status = 'Full time'
  GROUP BY staff_name 
)
SELECT 
  staff_name,
  MAX(total_income$) AS max_income$
FROM sub ;
--ANS4.Lisa

