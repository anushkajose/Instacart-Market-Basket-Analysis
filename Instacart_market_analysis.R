aisles <- read.csv("C:/Users/Anushka/Desktop/Courses/Sem II/Computing Stat/Final Project/aisles.csv/aisles.csv")
departments <- read.csv("C:/Users/Anushka/Desktop/Courses/Sem II/Computing Stat/Final Project/departments.csv/departments.csv")
order_products_train <- read.csv("C:/Users/Anushka/Desktop/Courses/Sem II/Computing Stat/Final Project/order_products__train.csv/order_products__train.csv")
orders <- read.csv("C:/Users/Anushka/Desktop/Courses/Sem II/Computing Stat/Final Project/orders.csv/orders.csv")
products <- read.csv("C:/Users/Anushka/Desktop/Courses/Sem II/Computing Stat/Final Project/products.csv/products.csv")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggwordcloud)
library(arules)
library(arulesViz)
library(png)

orders %>% glimpse()
products %>% glimpse()
aisles %>% glimpse()

## 1
#Combining products data frame with aisles data frame.
#The aisle_id in aisles is in ascending order, whereas the aisle_id in products is not ordered. Hence sorting the data in products based on aisle_id in ascending order.

products_aisles_order_df <- products %>% arrange(aisle_id)
products_aisles_order_df %>% glimpse()

products_aisles_order_df <- products_aisles_order_df %>% group_by(aisle_id)
products_aisles_order_df %>% glimpse()

####To see which aisle had more products
products_aisle_count <- products_aisles_order_df %>% count()
products_aisle_count %>% as.data.frame() %>% slice_max(n,n=1)
products_aisle_count %>% as.data.frame() %>% slice_min(n,n=1)

### Joining products_aisles_order_df and aisles
products_aisles_order_df <- products_aisles_order_df %>% left_join(aisles) %>% glimpse()



## 2
#Combining departments data frame to products_aisles_order_df.
departments %>% glimpse()

#Sorting the data frame in ascending order of department_id.
products_depart_order_df <- products_aisles_order_df %>% arrange(department_id)
products_depart_order_df %>% glimpse()

products_depart_order_df <- products_depart_order_df %>% group_by(department_id)
products_depart_order_df %>% glimpse()

### Joining products_depart_order_df and departments
products_depart_order_df <- products_depart_order_df %>% left_join(departments) %>% glimpse()


##3
#Combining orders_products_train data frame to products_depart_order_df
order_products_train %>% glimpse()

#Sorting the data frame in ascending order of department_id.
products_order_df <- products_depart_order_df %>% arrange(product_id)
products_order_df %>% glimpse()

products_order_df <- products_order_df %>% group_by(product_id)
products_order_df %>% glimpse()

### Joining products_depart_order_df and departments
products_order_df <- products_order_df %>% left_join(order_products_train) %>% glimpse()


##3
#Combining orders data frame to products_order_df
orders %>% glimpse()

#Grouping based on order_id
orders_df <- products_order_df %>% group_by(order_id)
orders_df %>% glimpse()

### Joining orders_df and orders
orders_df <- orders_df %>% left_join(orders) %>% glimpse()


##Changing the order of the columns
orders_df <- orders_df %>% relocate("order_id","user_id","order_number","eval_set",
                                    "order_dow","order_hour_of_day","days_since_prior_order",
                                    "product_id","product_name","aisle_id","aisle",
                                    "department_id","department","add_to_cart_order","reordered")


orders_df %>% glimpse()


## 4
## Checking for missing values

# count the number of missing values in each column
missing_counts <- colSums(is.na(orders_df))

# create a table with the missing value counts
missing_table <- data.frame(
  column_names = names(missing_counts),
  missing_count = missing_counts,
  row.names = NULL
)

missing_table

#Checking if the missing rows are same for the 15 columns
missing_rows <- which(!complete.cases(orders_df[, 1:15]))
length(unique(missing_rows))
#The missing rows seem to be the same. Hence removing these rows.

## Omitting the missing values
orders_df <- na.omit(orders_df)
colSums(is.na(orders_df))

#Subsetting based on user_id with maximum orders.
#### Adding number of orders per user.
orders_df <- orders_df %>% group_by(user_id, order_number) %>% add_count() %>% glimpse()
names(orders_df)[16] <- "numorders_per_usr"
orders_df %>% glimpse()

# Get the users with the 55 or more orders
orders <- orders_df %>%
  filter(numorders_per_usr >= 55)

orders %>% glimpse()


#removing id's
orders <-  select(orders,-c(eval_set,product_id,aisle_id,department_id))
orders <- orders %>% filter(!department == "missing" & !department == "other")
orders %>% glimpse()

orders <- orders %>% relocate(c(aisle,department,product_name),.before =  order_dow) %>% glimpse()


## EDA
# 1.The most commonly bought products and its respective department 
orders <- orders %>% ungroup() %>% glimpse()
# group by department and product
dept_prod_counts <- orders %>%
  group_by(department, product_name) %>%
  filter(reordered == 1) %>%
  summarise(prod_count = n()) %>%
  ungroup()

# select top 10 products for each department
top_prod <- dept_prod_counts %>%
  group_by(department) %>%
  filter(prod_count >= 5) 

top_prod <- top_prod %>%  arrange(desc(prod_count)) 
top_prod$department <- factor(top_prod$department)
top_prod %>% glimpse()


top_prod %>%
  ggplot(aes(prod_count, product_name, fill = department)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ department, scales = "free_y", ncol = 4) 


## 2. The most common aisles which are reordered from
aisle_reorder <- orders %>%
                filter(reordered == 1) %>%
                group_by(department,aisle) %>% 
                summarise(num_reorders = n()) %>% 
                arrange(desc(num_reorders)) %>%
                ungroup() %>%
                glimpse()
                
        
aisle_reorder%>%
slice_max(num_reorders, n = 50) %>%
ggplot(aes(label = aisle, size = num_reorders, color = department))+
  geom_text_wordcloud() +
  scale_size_area(max_size = 10)

#3 Market Basket Analysis

####### Convert the data to a transaction format
#transactions <- orders %>%
 # group_by(user_id,order_number) %>%
  #summarise(products = list(as.character(product_name)))

###### Perform market basket analysis using the Apriori algorithm
#rules <- apriori(transactions$products, parameter = list(support = 0.001, confidence = 0.8, maxlen = 3))
#save(rules, file = "saved_rules.RDA")

######Sorting
#sorted_rules <- sort(rules, by = "support", decreasing = TRUE)
#save(sorted_rules, file = "sorted_rules.RDA")
#load("sorted_rules.RDA")

##### Print the top 20 rules
#top20 <- inspect(head(sorted_rules, n = 50))
#save(top20, file = "top20_new.RDA")
load("top20_new.RDA")
top20

#Plotting
plot(sorted_rules,method="graph",measure = "support",shading = "support",
     limit = 50,engine = "interactive" )

img <- readPNG("RPlots2.png")
grid::grid.raster(img)

#4. Hourly order pattern

orders %>% group_by(order_hour_of_day) %>% summarize(count = n()) %>% mutate(percentage = count/sum(count)) %>% ggplot(aes(x = as.factor(order_hour_of_day), y = percentage)) + geom_col() + labs(y = "Percentage of Order", title = "Hourly Orders")


#5. Topic Modeling

library(topicmodels)
library(dplyr)
library(tidyr)
library(tidytext)

# Convert order data to product data
products <- orders %>%
  ungroup() %>%
  select(order_id, product_name) 

# Create document-term matrix
prod_dtm <- products %>%
  count(order_id, product_name) %>%
  cast_tdm(product_name, order_id, n)

dtm <- products %>%
  mutate(product_name = str_replace_all(product_name, " ", "_")) %>%
#  count(order_id, product_name) %>%
  unnest_tokens(term, product_name) %>%
  cast_tdm(term, order_id)

#lda <- LDA(prod_dtm, k = 12, control = list(seed = 123))
#save(lda, file = "lda.Rda")


#load("lda.Rda")
#lda

#Word distribution associated with each topic
#lda_topics <- tidy(lda, matrix = "beta")
#head(lda_topics, n = 20)
#tail(lda_topics)
















