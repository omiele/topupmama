## Libraries
library(dplyr)
library(tidyverse)
library(plyr)

## Read the data files
# Kenya Data
kenya_orders <- read.csv('D:/Documents/freelance/topmama/Data Dumps/Kenya Orders.csv')
kenya_customers <- read.csv('D:/Documents/freelance/topmama/Data Dumps/Kenya Customers.csv') 
kenya_deliveries <- read.csv('D:/Documents/freelance/topmama/Data Dumps/Kenya Deliveries.csv')

# Nigeria Data
nigeria_orders <- read.csv('D:/Documents/freelance/topmama/Data Dumps/Nigeria Orders.csv')
nigeria_customers <- read.csv('D:/Documents/freelance/topmama/Data Dumps/Nigeria Customers.csv') 
nigeria_deliveries <- read.csv('D:/Documents/freelance/topmama/Data Dumps/Nigeria Deliveries.csv')

### Removing unnecessary characters within the deliveries data
kenya_deliveries$Order_ID = substring(kenya_deliveries$Order_ID, 4)
kenya_deliveries$Order_ID = substr(kenya_deliveries$Order_ID,1,nchar(kenya_deliveries$Order_ID)-2)
nigeria_deliveries$Order_ID = substring(nigeria_deliveries$Order_ID, 4)
nigeria_deliveries$Order_ID = substr(nigeria_deliveries$Order_ID,1,nchar(nigeria_deliveries$Order_ID)-2)

## Data Merging to one file per country
# Kenya
kenya_deliveries$Order_ID = strtoi(kenya_deliveries$Order_ID)
kenya_data <- left_join(kenya_deliveries, kenya_orders, 
                          by = c("Order_ID"="Order.ID"))
kenya_data <- left_join(kenya_data, kenya_customers, 
                        by = c("Customer.ID"="Customer.ID"))
kenya_data$Country = 'KE'

# Nigeria
nigeria_deliveries$Order_ID = strtoi(nigeria_deliveries$Order_ID)
nigeria_data <- left_join(nigeria_deliveries, nigeria_orders, 
                        by = c("Order_ID"="Order.ID"))
nigeria_data <- left_join(nigeria_data, nigeria_customers, 
                        by = c("Customer.ID"="Customer.ID"))
nigeria_data$Country = 'NG'

### Row binding the 2 dataframes into 1 file
names(nigeria_data)[names(nigeria_data) == 'Number.of.Employees'] <- 'Number.of.employees'
all_data <- rbind.fill(kenya_data, nigeria_data)

#### Data Cleaning
## Remove columns with 50% or more missing values
new_all_data <- all_data[, which(colMeans(!is.na(all_data)) >= 0.5)]

# Removing duplicates based on the order id column
new_all_data <- new_all_data[!duplicated(new_all_data$Order_ID), ]

# Remove unrequired columns based on frequencies
new_all_data[ ,c('Review', 'Order.Preparation.Time','Debt.Amount','Flat.Discount','Checkout.Template.Name',
                        'Checkout.Template.Value','Last.Used.Platform','Is.Blocked','Language','Outstanding.Amount',
                        'Upload.restuarant.location', 'Taxable.Amount','Additional.Charge','Promo_Applied','X','Pricing',
                 'Earning','Task_Category','Tip.y','Tip.x','Discount.y','Delivery_Charges','Delivery.Charge','Tax','Ref_Images',
                 'X.1','Transaction.ID','Remaining.Balance')] <- list(NULL)

### Total_Time_Taken.min, Task_Details_QTY, Unit.Price, Cost.Price, Total.Price, Order.Total,Sub.Total, Distance..in.km., 
### Redeemed.Loyalty.Points, Consumed.Loyalty.Points, Country, Loyalty.Points, Category.Name
## Factors - Country and Category.Name

### Linear regression for loyalty-points prediction  - No EDA done but diagnostics checked
lm_data <- new_all_data %>% select(Total_Time_Taken.min., Task_Details_QTY, Unit.Price, Cost.Price, Total.Price, 
                                   Order.Total,Sub.Total, Distance..in.km.,Redeemed.Loyalty.Points, Consumed.Loyalty.Points,
                                   Loyalty.Points,Country, Category.Name)
lm_data_1 <- sapply(lm_data[,1:(length(lm_data)-2)],as.numeric)
lm_data_2 <- cbind(lm_data_1,lm_data[,(length(lm_data)-1):length(lm_data)])
lm_data_2$Category.Name = as.factor(lm_data$Category.Name)
lm_data_2$Country = as.factor(lm_data$Country)

## Modelling Data - Filter into separate countries
ke_model_data <- lm_data_2 %>% filter(Country=='KE')
drop <- c("Country")
ke_model_data = ke_model_data[,!(names(ke_model_data) %in% drop)]
ng_model_data <- lm_data_2 %>% filter(Country=='NG')
ng_model_data = ng_model_data[,!(names(ng_model_data) %in% drop)]

### The model
model_1 <- lm(Loyalty.Points~., data=ke_model_data)
summary(model_1)
plot(model_1)
model_2 <- lm(Loyalty.Points~., data=ng_model_data)
summary(model_2)
plot(model_2)

### Writing data to CSV
write.csv(new_all_data,"all_data.csv", row.names = FALSE)
