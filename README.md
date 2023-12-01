# Customer_Complaint_Resolution_Financial_Product_Model

## Introduction

The Consumer Financial Protection Bureau (CFPB) receives consumer complaints about the financial product and with the consent of the consumer, they publish them on the website (data.gov). The data contains different variables such as Company name, products, types of issues, timely resolution, company response to consumers (resolution status), etc. For this project, we will consider a one-year complaint database from January 2021 to August 2022. The data contains 996,046 rows and 19 columns. We will be using ‘Tableau’ to perform exploratory analysis and visualization, and ‘R programming’ to perform statistical analysis and model building.

## Key Question and Hypothesis

Through this data, we would like to answer the key question about the important factors that can affect complaint resolution. Our hypothesis is that the complaint resolution depended on the type of financial product. 
In order to get the answer, for our analysis, we will be targeting the variable “Company response to consumers’. This will help us to determine if the complaints are ‘closed’ or ‘in progress’. We would mainly like to explore what are the important variables that affect complaint resolution and whether are there any specific relationships or statistics with the target variable.

Key Questions:

1.	What factors affect the complaints resolution status?
2.	Is there any specific relationship with the complaint resolution status?


```{r complaints, include= FALSE}

data <- read.csv('Complaints_final.csv')

data1 <- data %>% filter(as.Date(Date.received) > "2021-01-01")
rows <- nrow(data1)
columns <- ncol(data1)
```

## Data Pre-processing

The data consists of 996,046 rows and 19 variables. In order to perform analysis, the first and most important step is to perform data pre-processing (data cleaning). The data we choose has 1 numerical data type and 18 categorical data-type. As we are dealing with most of the categorical data types, we have to deal with the high cardinality issue. Data pre-processing helps to normalize data and remove unwanted observations that can affect the accuracy of the analysis. 

The following data pre-processing steps in order to clean the data:

•	Checking unique IDs:
•	Checking the null values/ missing values:
•	Imputation
•	Feature Engineering

``` {r data , include = FALSE}
rows
columns
data1 %>% inspect_types()
```

```{r pressure, include=FALSE}
################################ Checking unique IDs ################################
unique <- data1 %>% 
  group_by(Complaint.ID ) %>%
  summarise(uni = sum()) %>%
  arrange( desc(uni))

max(unique$uni)
min(unique$uni)
mean(unique$uni)

################################ Counting blank spaces ################################
for(i in 1:ncol(data1))
{
  print(colnames(data1)[i])
  print((sum(data1[,i] =="")/length(data1[,i]))*100)
}

# There are more than 50% of the blank spaces in the column 'Consumer.complaint.narrative',
# 'Company.public.response' & 'Tags'

################################ Counting N/A spaces ################################

m <- for(i in 1:ncol(data1))
{
  print(colnames(data1)[i])
  print((sum(data1[,i] == "N/A")/length(data1[,i]))*100)
}

# There is 100% N/A values in column 'Consumer.disputed'
colnames(data1)

################################ Irrelevant features ################################

# We feel that column 'Consumer.consent.provided.' is not necessary for our prediction, hence we will remove that. The reason being the column tells that if the customer provided the consent to publish the complaint publicly which is an irrelevant feature for out prediction model

################################ Deleting Columns ################################

# Column Deletion: As column consumer disputed contains 100% N/A values, 
# Consumer.complaint.narrative has 60% blank rows, Company.public.response has 
# 50% black rows and, Tags has 90% blank rows hence deleting these columns

data1 <- data1[ ,-c(1,7,8,12,13,18)]

head(data1)
inspect_types(data1)

# data1 %>% group_by(Consumer.consent.provided) %>% summarise(x = n())
# colnames(data1)

################################ Imputing missing value with mode ################################

getmode <- function(v)
{
  v=v[nchar(as.character(v))>0]
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
for (cols in colnames(data1)) {
  if (cols %in% names(data1[,sapply(data1, is.numeric)]))
  {
    data1<-data1 %>% mutate(!!cols := replace(!!rlang::sym(cols), is.na(!!rlang::sym(cols)), mean(!!rlang::sym(cols), na.rm=TRUE)))
  }
  else
  {
    
    data1<-data1 %>% mutate(!!cols := replace(!!rlang::sym(cols), !!rlang::sym(cols)== "" | !!rlang::sym(cols)== "N/A", getmode(!!rlang::sym(cols)))) 
  }
}

str(data1)

################################ Categorizing column 'company response to consumer' ################################

data1$Company.response.to.consumer <- ifelse(data1$Company.response.to.consumer == "Closed with explanation" |
                                               data1$Company.response.to.consumer == "Closed with monetary relief" |
                                               data1$Company.response.to.consumer == "Closed with non-monetary relief",
                                             "Closed", "In progress")
unique(data1$Company.response.to.consumer)

response <- data1 %>% group_by(Company.response.to.consumer) %>%
  summarise(count = n())
unique(response)
new_rows = nrow(data1)
new_columns = ncol(data1)

################################ Categorizing column 'State' ################################

data1$State <- gsub("UNITED STATES MINOR OUTLYING ISLANDS", "UM", data1$State)

NE.name <- c("Connecticut","Maine","Massachusetts","New Hampshire",
             "Rhode Island","Vermont","New Jersey","New York",
             "Pennsylvania")
NE.abrv <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA")
NE.ref <- c(NE.name,NE.abrv)

MW.name <- c("Indiana","Illinois","Michigan","Ohio","Wisconsin",
             "Iowa","Kansas","Minnesota","Missouri","Nebraska",
             "North Dakota","South Dakota")
MW.abrv <- c("IN","IL","MI","OH","WI","IA","KS","MN","MO","NE",
             "ND","SD")
MW.ref <- c(MW.name,MW.abrv)

S.name <- c("Delaware","District of Columbia","Florida","Georgia",
            "Maryland","North Carolina","South Carolina","Virginia",
            "West Virginia","Alabama","Kentucky","Mississippi",
            "Tennessee","Arkansas","Louisiana","Oklahoma","Texas")
S.abrv <- c("DE","DC","FL","GA","MD","NC","SC","VA","WV","AL",
            "KY","MS","TN","AR","LA","OK","TX")
S.ref <- c(S.name,S.abrv)

W.name <- c("Arizona","Colorado","Idaho","New Mexico","Montana",
            "Utah","Nevada","Wyoming","Alaska","California",
            "Hawaii","Oregon","Washington")
W.abrv <- c("AZ","CO","ID","NM","MT","UT","NV","WY","AK","CA",
            "HI","OR","WA")
W.ref <- c(W.name,W.abrv)

T.name <- c("Virgin Islands", "Puerto Rico", "Armed Forces", "American Samoa", "Armed Forces America", "Guam", "Armed Forces Pacific","Northern Mariana Islands","Marshall Islands","Uninted States Minor islands")
T.abrv <- c("VI","PR","AE", "AS", "AA","GU","AP","MP","MH","UM")
T.ref <- c(T.name, T.abrv)

region.list <- list(
  Northeast=NE.ref,
  Midwest=MW.ref,
  South=S.ref,
  West=W.ref,
  Territory = T.ref)

data1$region <- sapply(data1$State, 
                       function(x)
                         names(region.list) [grep(x, region.list)]
                       )

```

```{r cvf, include=TRUE}
# Grouping the categories into few to reduce the high cardinality
data2 <- data1 %>% dplyr::select(State,region )
head(data2)

```

```{r fgd, include=TRUE}
################################ Grouping Company ################################
comp <- data1 %>% 
  group_by(Company) %>%
  summarise(cnt = n()) %>%
  mutate( perc = round(cnt/ sum(cnt)*100, digits = 2)) %>%
  arrange(desc(perc))

data_top <- comp[1:10,]

#filtering only top 10 companies name.
'%!in%' <- Negate('%in%')
data1$Company <- ifelse(data1$Company %!in% data_top$Company, "Other", data_top$Company)

```
In this step, we will remove the columns which has more than 50% missing and N/A values. These columns are "X", "Consumer.complaint.narrative", "Company.public.response", "Tags" and "Consumer.disputed." 
Since the complaint resolution is dependent on the 'Company.response.to.consumer' we have imputed the column into two categories: Complaints which are closed as "Closed" and Complaints which are not closed "In progress". The columns are noe reduced to 14.

```{r rty, include=FALSE}

################################ Issues ################################
issues <- data1 %>% 
  group_by(Issue) %>%
  summarise(cnt = n()) %>%
  mutate( perc = round(cnt/ sum(cnt)*100, digits = 2)) %>%
  arrange(desc(perc))

data_top_issues <- issues[1:10,]
'%!in%' <- Negate('%in%')
data1$Issue <- ifelse(data1$Issue %!in% data_top_issues$Issue, "Other", data_top_issues$Issue)

#View(data1)

```

```{r ss, include = FALSE}
new_rows
new_columns
data1 %>% inspect_types()
```

## Exploratory Analysis

### Correlation Test

There are more categorical variables in this data and hence we'll use Chi Square test to find the correlation between the Complaint resolution and other factors. In this project, we assume that **Timely response, Product and complaint channels** might be important factors that affect the complaint resolution and hence the Chi square test will be used for finding correlation.

```{r ee, include=FALSE}

# Timely Response

complaints <- data1 %>%
  dplyr::select(Company.response.to.consumer, Timely.response.)

complaints %>% group_by(Company.response.to.consumer, Timely.response.) %>%
  summarise( cnt = n())

con_complaint = table(complaints)
print(con_complaint)
 chisq.test(con_complaint)

# Product

unique(data1$Company.response.to.consumer)

df_prod <- data1 %>%
  dplyr::select(Company.response.to.consumer, Product) 

df_prod %>% group_by(Company.response.to.consumer, Product) %>%
  summarise( cnt = n())

con_prod = table(df_prod)

chisq.test(con_prod)

# Complaint Channel type

df_sub <- data1 %>%
  dplyr::select(Company.response.to.consumer, Submitted.via) 

df_sub %>% group_by(Company.response.to.consumer, Submitted.via) %>%
  summarise(cnt = n())

con_sub = table(df_sub)

chisq.test(con_sub, simulate.p.value = TRUE)

```

```{r cf, include=TRUE}
#Timely Response
chisq.test(con_complaint)
#Product
chisq.test(con_prod)
#Complaint channel type
chisq.test(con_sub, simulate.p.value = TRUE)

```
As observed in the result, the p-value of all the above three variables is less than 0.05 and this shows that, 'Timely response', 'Product' and 'complaint channels' shows a strong relation with complaint resolution. But in order to validate and check are there any other factors that affect the complaint resolution we will perform a statistical classifier analysis to conclude our hypothesis. We will use **Random Forest** classifier model to arrive at important features/ variables that affect complaint resolution.


## Model Fitting and Statistical Analysis

```{r qq, include= FALSE}

# Preparing data

colnames(data1)

df_random <- data1 %>% dplyr::select(Product, Issue, Company, region, Submitted.via, Company.response.to.consumer,Timely.response.)

ncol(df_random)
#data$num[data$num > 1] <- 1
str(df_random)

#### Main data frame for model####
df_random2 <- transform(
  df_random,
  Product=as.factor(Product),
  Issue = as.factor(Issue),
  Company = as.factor(Company),
  region = as.factor(region),
  Submitted.via = as.factor(Submitted.via),
  Company.response.to.consumer = as.factor(Company.response.to.consumer),
  Timely.response. = as.factor(Timely.response.)
)

# str(df_random2)
# sapply(df_random2, class)
# 
# companies.rf = randomForest(Company.response.to.consumer ~ ., data = df_random2, importance = TRUE, ntree = 100)
# companies.rf

# colSums(is.na(df_random2))
# 
# importance(companies.rf, type = 2)
# 
# varImpPlot(companies.rf)

# Node impurity is measured by mean decrease in Gini index, and permutation is measured by mean decrease in accuracy.

# "Global" variable importance is the mean decrease of accuracy over all out-of-bag cross validated predictions, when a given variable is permuted after training, but before prediction. "Global" is implicit. Local variable importance is the mean decrease of accuracy by each individual out-of-bag cross validated prediction. Global variable importance is the most popular, as it is a single number per variable, easier to understand, and more robust as it is averaged over all predictions.

```

## Model 1: Logistic Regression

The Logistic Regression model is used to predict the complaint. The data was split into 70% for training and 30% for testing. The model fitted in the training data set and then tested on the testing data set.

```{r thyj}
### Logistic Regression Model
# Data Splitting

head(df_random2)

set.seed(271)

random_split <- initial_split(df_random2, prop = 0.70,
                               strata = Company.response.to.consumer)

random_training <- random_split %>%
  training()

random_test <- random_split %>%
  testing()

# Feature Engineering

random_recipe <- recipe(Company.response.to.consumer ~ .,
                         data = df_random2) %>% 
                 step_YeoJohnson(all_numeric(), -all_outcomes()) %>% 
                 step_normalize(all_numeric(), -all_outcomes()) %>% 
                 step_dummy(all_nominal(), -all_outcomes())

# Checking transformations

random_recipe %>% 
  prep(training = df_random2) %>% 
  bake(new_data = NULL)

## Specifying logistic regression model

logistic_model <- logistic_reg() %>% 
                  set_engine('glm') %>% 
                  set_mode('classification')

## Creating a workflow

logistic_wf <- workflow() %>% 
               add_model(logistic_model) %>% 
               add_recipe(random_recipe)

## Fitting a model
logistic_fit <- logistic_wf %>% 
                last_fit(split = random_split)

## Collecting predictions
logistic_results <-  logistic_fit %>% 
                     collect_predictions()

## Evaluating model performance
# ROC curve
roc_curve(logistic_results, 
          truth = Company.response.to.consumer, 
          estimate = .pred_Closed) %>% 
  autoplot()

# ROC AUC
roc_auc(logistic_results, 
        truth = Company.response.to.consumer,
        .pred_Closed)

# Confusion matrix
# conf_mat(logistic_results, 
#          truth = Company.response.to.consumer,
#          estimate = .pred_Closed)

```
We checked the model prediction using the measure ‘Area under curve ROC curve’ (AUC). AUC is the measure to evaluate how well the model classifies the correctness of the outcome. Higher the AUC, better the model is good at classifying the outcome correctly. 
As observed that the AUC of the logistic regression model is 0.68. This means that the model fairly predicts the outcome correctly.


## Model 2 : Random Forest

The Random forest is another model used to predict the complaint. The data was split into 70% for training and 30% for testing. The model fitted in the training data set and then tested on the testing data set.

```{r tyiuq}

set.seed(271)

model_rf = randomForest(Company.response.to.consumer ~ ., data = df_random2, importance = TRUE, ntree = 100)

print(model_rf)

importance(model_rf, type = 2)

varImpPlot(model_rf)

# Predictions on the training set
predicttrain_rf = predict(model_rf, data = train)

# Predictions on the testing set

predicttest_rf = predict(model_rf, newdata = test, type = "class")

prediction_rf = prediction(as.numeric(predicttest_rf), test$Company.response.to.consumer)

perform_rf = performance(prediction_rf, measure = "tpr", x.measure = "fpr")
plot(perform_rf)

auc = performance(prediction_rf, measure = "auc")
auc = auc@y.values[[1]]
print(auc)

```
We check the model prediction using the measure ‘Area under curve ROC curve’ (AUC). As observed that the AUC of the random forest is 0.5. This means that the model might not be able to distinguish between positive and negative outcomes correctly.
Through the Variable important plot in Figure -3, it can be seen that the topmost important feature is Product which aligns with our assumption. The other important variables by order can also observed.


## Findings and Conclusion

After performing the exploratory analysis and building a prediction model, the following things can be interpreted:

•	It is observed from the exploratory analysis that the ‘Product’, ‘Timely response’ and ‘Submitted via’ are co-related to the complaint resolution

•	Most of the complaints are related to the Product Credit, for instance, credit reporting and repairs. It can be seen that the ‘Synchrony Financial’ company receives the most complaints. Overall 98.9% of the complaints gets resolved on time and the topmost issue of the company is managing the account.

•	The statistical model is evaluated using the performance measure area under the ROC curve. It can be seen that the logistic regression model is better at predicting the complaint resolution than random forest. The accuracy of logistic regression is 0.68 which is fair but not good enough to predict the complaint resolution. This means, we need more data or features to improve our accuracy of predicting the complaint resolution.

*Model Evaluation using Area under curve*

- Logistic Regression -	0.68
- Random Forest -	0.50

## Challenges

The major challenges faced in this project are in developing the hypothesis, uncleaned dataset and the large volume data. The initial assumptions made by looking at the data were revisited multiple times when we started exploring the data. During exploratory analysis, we realized that the data gives information on ‘Complaint Resolution’ status and not the ‘Timely Response’, which was our initial assumption. As we framed our final hypothesis, the next challenge we faced is on building the model on the large and uncleaned data. We had to check and remove the irrelevant and missing values from the data that we did in Data Pre-processing step. Lastly, the challenge was to deal with the large amount of data. The data has nearly 1 million rows i.e. it is a high cardinality data. The model took a large space and longer run time than usual. We had to perform feature engineering and data cleaning in order to predict accurately and reduce the run time of the model.
