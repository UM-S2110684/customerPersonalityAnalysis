#Read data set
df <- read.csv("marketing_campaign.csv",sep = '\t')

#Data Cleaning Process
#Remove "Z_CostContact" and "Z_Revenue" that non-informative
df <- subset(df, select = -c(Z_CostContact, Z_Revenue))
#Feature Engeneering
#Encode "Education" and "Marital_Status" into numeric forms
table(df$Marital_Status)
table(df$Education)
df["Marital_Status"][df["Marital_Status"] == 'Alone'] <- 'Single'
df["Marital_Status"][df["Marital_Status"] == 'YOLO'] <- 'Single'
df["Marital_Status"][df["Marital_Status"] == 'Absurd'] <- 'Single'

# calculate the age instead of 'Year-Birth'
df['Year_Birth']= 2022-df['Year_Birth']
names(df)[names(df)=="Year_Birth"] <- "Age"
#Total spending on all items
df['Total_Spent'] = df["MntWines"]+ df["MntFruits"]+ df["MntMeatProducts"]
+ df["MntFishProducts"]+ df["MntSweetProducts"]+ df["MntGoldProds"]

# creating a new feature indicates the number of days customer engaged to the company
df$Dt_Customer <- as.Date(df$Dt_Customer, format="%d-%m-%Y")
newest_customer <- max(df$Dt_Customer)
df['newest_customer'] = newest_customer
df['days_engaged'] <- (df['newest_customer'] - df['Dt_Customer'])
df <- subset(df, select = -c(Dt_Customer, newest_customer))

df <- na.omit(df)
df <- subset(df, Age<100 & Income<600000)

df['Education_encode'] <- as.numeric(as.factor(df$Education))
df['Marital_Status_encode'] <- as.numeric(as.factor(df$Marital_Status))
df_enc <- subset(df, select=c(Education_encode, Education))
df_enc1 <- subset(df, select=c(Marital_Status_encode, Marital_Status))
df <- subset(df, select = -c(Education, Marital_Status))