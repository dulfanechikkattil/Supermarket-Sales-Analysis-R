library(ggplot2)
library(dplyr)


#-----------------------------------------------------------------
# barplot
#-----------------------------------------------------------------
ggplot(df, aes(x = City, y = `Tax 5%`,fill=`Customer type`)) +
  geom_boxplot() +
  labs(title = "City by tax",
       x = "City", y = "tax 5%") +
  theme_minimal()


df %>%
  group_by(City) %>%
  summarise(Total_Tax = sum(`Tax 5%`)) %>%
  ggplot(aes(x = reorder(City, Total_Tax), y = Total_Tax, fill = City)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +
  labs(title = "Total Tax by City",
       x = "City",
       y = "Total Tax") +
  theme_minimal() +
  theme(legend.position = "none")

#-----------------------------------------------------------------
# bar plot
#-----------------------------------------------------------------

df %>%
  group_by(Gender, `Product line`) %>%
  summarise(Total_Tax = sum(`Unit price`), .groups = "drop") %>%
  ggplot(aes(x = `Product line`, y = Total_Tax, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  coord_flip() +
  labs(title = "Unit price by Product Line (Gender Comparison)",
       x = "Product Line",
       y = "Total Unit price") +
  theme_minimal()


df %>%
  mutate(Total = `Unit price` * Quantity) %>%
  ggplot(aes(x = Gender, y = Total, fill = `Customer type`)) +
  geom_boxplot() +
  labs(title = "Spending Distribution by Customer Type",
       x = "Customer Type",
       y = "Total Spending") +
  theme_minimal()
#--------------------------------------------------------
# bar plot
#--------------------------------------------------------
df %>%
  group_by(`Product line`) %>%
  summarise(Total_Quantity = sum(Quantity)) %>%
  ggplot(aes(x = reorder(`Product line`, Total_Quantity), 
             y = Total_Quantity, 
             fill = `Product line`)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +
  labs(title = "Product Demand (Quantity Sold)",
       x = "Product Line",
       y = "Total Quantity") +
  theme_minimal() +
  theme(legend.position = "none")


#-------------------------------------------------
# line plot
#------------------------------------------------
ggplot(df, aes(x = Quantity, y = `Tax 5%`)) +
  geom_line(stat = "smooth") +
  labs(title = "Relationship between Quantity and Tax")


df %>%
  group_by(City) %>%
  summarise(avg_tax = mean(`Tax 5%`)) %>%
  ggplot(aes(x = City, y = avg_tax, group = 1)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Average Tax by City")

head(df)
sapply(df,class)


df %>%
  group_by(`Product line`) %>%
  summarise(avg_price = mean(`Unit price`)) %>%
  ggplot(aes(x = `Product line`, y = avg_price, group = 1)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Average Unit Price by Product Line",
       x = "Product Line",
       y = "Average Unit Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



