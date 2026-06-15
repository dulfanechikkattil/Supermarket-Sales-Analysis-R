#===========================================================
#Supermarket Sales New Data
#===========================================================




#load data  
library(dplyr)
#install.packages("readr")
library(readr)
df=read_csv("supermarket_sales new.csv")
head(df)


# Check shape
dim(df)

# Check coloumns,summary and datatype 
names(df)
sapply(df,class)
summary(df)

# Check Missing Values of each columns
library(dplyr)
sum(is.na(df))
data.frame(
  col_datatype=sapply(df,class),
  missing_count=sapply(df, function(x)sum(is.na(x)))
)

# Check  duplicates an
sum(duplicated(df))

# change char type of Tax 5% to numeric
df$`Tax 5%` <- as.numeric(df$`Tax 5%`)
df$`Tax 5%` <- as.numeric(gsub("%", "", df$`Tax 5%`))
df$`Tax 5%` <- as.numeric(gsub("[^0-9.]", "", df$`Tax 5%`))
str(df)

# cleared tax 5% column
df <- df %>%
  dplyr::mutate(`Tax 5%` = `Unit price` * Quantity * 0.05)

df %>%
  dplyr::mutate(Tax_check = `Unit price` * Quantity * 0.05) %>%
  dplyr::summarise(
    match = sum(`Tax 5%` == Tax_check),
    total = dplyr::n()
  )

# drop unwanted rows

drop_cols<-c("Invoice ID","Branch")
df<-df[,!(names(df)%in%drop_cols)]
head(df)


# checking outliers 

num_cols <- sapply(df, is.numeric)

par(mfrow = c(2,3))  # adjust rows & columns based on number of variables

for(col in names(df)[num_cols]){
  boxplot(df[[col]],
          main = paste("Boxplot of", col),
          ylab = col,
          col = "pink")
}
# capping for handling outliers

for(col in names(df)[num_cols]){
  
  Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  # Capping
  df[[col]][df[[col]] < lower] <- lower
  df[[col]][df[[col]] > upper] <- upper
}
# check outliers after capping

for(col in names(df)[num_cols]){
  boxplot(df[[col]],
          main = paste("Boxplot of", col),
          ylab = col,
          col = "red")
}
#------
#grouby 
#------
colnames(df)


df %>%
  group_by(Gender) %>%
  summarise(
    Total_Sales = sum(`Unit price`* Quantity),
    Total_Tax = sum(Tax.5.),
    Transactions = n()
  )



df%>% group_by(`Customer type`)%>%
  summarise(
    avg_quanity=mean(Quantity),
    avg_Unit_price=mean(`Unit price`),
    avg_tax5=mean(`Tax 5%`)
  )

df %>%
  group_by(`Product line`) %>%
  summarise(Total_Tax = sum(`Tax 5%`),
            Avg_Tax = mean(`Tax 5%`),
            Count = n()) %>%
  arrange(desc(Total_Tax))


df %>%
  group_by(`Customer type`) %>%
  summarise(Avg_Spending = mean(`Unit price` * Quantity),
            Total_Tax = sum(`Tax 5%`),
            Avg_Quantity = mean(Quantity))



df %>%
  group_by(City) %>%
  summarise(Total_Tax = sum(`Tax 5%`),
            Avg_Quantity = mean(Quantity)) %>%
  arrange(desc(Total_Tax))





