library(ggplot2)
library(MASS)
library(car)
library(dplyr)
if (!require("ggrepel")) install.packages("ggrepel") #install ggrepel if not available
if (!require("leaps")) install.packages("leaps")  #install leapsif not available
library(ggrepel)
library(leaps)



# PLEASE MAKE SURE THE PLOTS PANEL IS LARGE ENOUGH FOR ALL GRAPHS
# TO BE PLOTTED. ELSE, THERE WILL BE AN ERROR IN RUNNING CODE

#************************ Reading, exploring cleaning the data *********************************************
# Read the CSV file and drop redundant attributes.
# Do some exploratory data analysis
# Reduce the dataset to only rows with complete cases
# Variable Selection
# Find outliers and influential points. Delete outliers which are influential
#****************************************************************************************************

movies <- read.csv("movie_metadata1.csv", header = TRUE, stringsAsFactors = FALSE)
attach(movies)


#**************************** Exploratory Data Analysis *********************************************
# Do some basic data analysis to introduce and study the data set:
# 1. Number of movies released per year
# 2. Most profitable leading actors/directors
# 3. Most profitable movies ever
# 4. Top 20 directors with most movies/most highly rated movies
# 5. Top 10 genres with most movies 
# 6. Average rating for actor
# 7. Gross earnings across time
# ***************************************************************************************************

#1. Histogram of number of movies per year since sound films were introduced in 1916
ggplot(data=movies, aes(movies$title_year)) + 
  geom_histogram(binwidth=1, na.rm = TRUE, alpha = 0.7, aes(fill=..count..)) +
  labs(x="Year", y="Number of Movies", title="Number of Movies Released From 1916 to 2016") +
  scale_fill_gradient("Count") +
  scale_x_continuous(breaks = seq(1920, 2010,10))


#2. Top 20 Most Profitable Movies (not adjusted for inflation)

#Top 20 based on absolute profit
movies.profit <- movies %>% 
  arrange(desc(profit)) %>% 
  top_n(20, profit)
Genre = movies.profit$Genre_1

#Plot
ggplot(movies.profit, aes(x=budget/1000000, y=profit/1000000, color=Genre)) + 
  geom_point(size = 2) + geom_text_repel(aes(label = movie_title), size = 2.5) + 
  xlab("Budget (in million USD)") + ylab("Profit (in million USD") + 
  ggtitle("20 Most Profitable Movies")


#3. Top 20 based on ROI
movies.roi <- movies %>% 
  filter(budget >10000) %>% 
  arrange(desc(ROI)) %>% 
  top_n(20, ROI)
Genre = movies.roi$Genre_1

ggplot(movies.roi, aes(x=budget/1000000, y=profit/1000000,color=Genre)) + 
  geom_point(size = 2) + 
  geom_text_repel(aes(label = movie_title), size = 3)  + 
  xlab("Budget (in million USD)") + ylab("Budget (in million USD)") + 
  ggtitle("top 20 Movies Based on ROI")

#4.#Top 20 most profitable directors

movie.directors <- movies %>% 
  group_by(director_name) %>%
  select(director_name, budget, gross, profit) %>%
  na.omit() %>% 
  summarise(films = n(), budget = sum(as.numeric(budget)), 
            gross = sum(as.numeric(gross)), 
            profit = sum(as.numeric(profit))) %>% 
  arrange(desc(profit)) %>% 
  top_n(20, profit)

#Plot
ggplot(movie.directors, aes(x=films, y=profit/1000000)) + 
  geom_point(size = 2, color="red") + 
  geom_text_repel(aes(label = director_name), size = 3) + 
  xlab("Number of Films") + ylab("Profit (in million USD") + ggtitle("Most Profitable Directors")

#Remove redundant columns and reduce to complete cases only

redundantCols <-   c("color","actor_3_name","actor_3_facebook_likes","facenumber_in_poster",
                     "plot_keywords","movie_imdb_link","content_rating","aspect_ratio",
                     "director_name","actor_2_name", "actor_1_name", "Genre_1", "Genre_2",
                     "Genre_3", "Genre_4", "Genre_5", "gross", "movie_title",
                     "language", "country", "title_year", "movie_facebook_likes",
                     "ROI", "Profitability.level,profit")


reducedMovies <- movies[, !(names(movies) %in% redundantCols)]
reducedMovies <- reducedMovies[complete.cases(movies),]
summary(reducedMovies)

#Variable selection. We use imdb_score as the response variable  and determine which of the following
#explanatory variables should be included in the regression model. 

leaps = regsubsets(imdb_score ~ ., data = reducedMovies)
#view the ranked models according to Adj R^2 value
leaps
plot(leaps, scale = "adjr2")

#According to the plot, Adj R^2 is at highest without director_facebook_likes. So we remove
#that attribute and make a new model

reducedMovies <- subset(reducedMovies, select = -c(director_facebook_likes))

#Initial model after variable selection
mod.fit0 <- lm(imdb_score ~ ., data = reducedMovies)
sum.fit0 <- summary(mod.fit0)
sum.fit0 
#Multiple R-squared:  0.3371,	Adjusted R-squared:  0.3358 


#First, remove influential outliers so they don't affect the regression too much

di <- rstandard(mod.fit0)
outliers <- di[di >= 3 | di <= -3]
dffits.i <- round(dffits(model = mod.fit0),3)
cutoff <- 4/((nrow(reducedMovies)-length(mod.fit0$coefficients)-2))
plot(mod.fit0, which=4, cook.levels=cutoff)
abline(h=cutoff, col ="red")
influential <-influencePlot(mod.fit0, id.method="noteworthy", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
#Observation 167 is way off the cutoff line. 
#If it appears in the list of outliers, we delete it

if (any(names(outliers) == 167)) { reducedMovies <- reducedMovies[-167,]} 


#Reduced model without influential outliers
reduced.mod.fit0 <- lm(imdb_score ~ ., data = reducedMovies)
reduced.sum.fit0 <- summary(reduced.mod.fit0)
reduced.sum.fit0 
#Multiple R-squared:  0.3371,	Adjusted R-squared:  0.3358 


## Split data

## 80% of the sample size
smp_size <- floor(0.80 * nrow(reducedMovies))

## set the seed to make your partition reproductible
set.seed(126)
train_ind <- sample(seq_len(nrow(reducedMovies)), size = smp_size)

train <- reducedMovies[train_ind, ]
test <- reducedMovies[-train_ind, ]

#Begin modelling on the train data by making the first linear model 
#with the selected variables

mod.fit1 <- lm(imdb_score ~ ., data = train)
sum.fit1 <- summary(mod.fit1)
sum.fit1
#Multiple R-squared:  0.3418,	Adjusted R-squared:  0.3403  
#We want to increase the Adjusted R^2


#Global test of model adequacy. Note that the values may vary depending on the training and
#test set
anv <- anova(mod.fit1)
anv
SSR <- sum(anv$"Sum Sq"[1:8]) 
SSR
MSR <- SSR/8             
MSR
MSE <- tail(anv$"Mean Sq", n=1)   
MSE
FStat = MSR/MSE            
FStat                     
1-pf(FStat,7,3412)  #p-value=0
#Conclusion: Global Test to Model Adequacy: Reject H0. 
#There is statistical evidence to prove that at least one variable is 
#useful in estimating imdb_score.

################ Test model assumptions ############################
par(mfrow=c(2,2))
plot(mod.fit1)
#Residuals vs fitted has a funnel shape, normality assumptions is  violated


#1. Check for homoscedasticity (Non-constant variance)
ncvTest(mod.fit1)
ncv <- spreadLevelPlot(mod.fit1)
power <- ncv$PowerTransformation
power
#suggested power transformation is 2.665698 

mod.fit2 <- lm(imdb_score^power ~ .,data = train)
sum.fit2 <- summary(mod.fit2)
#Multiple R-squared:  0.4263,	Adjusted R-squared:  0.4249
par(mfrow=c(2,2))
plot(mod.fit2)

#Normality assumption is well-satisfied. So we won't check it again


#2. Linearity
#check the component plus residual plots. Nonlinearity in any of the plots suggest
#we may have not properly modeled the functional form of a particular predictor in the regression

crPlots(mod.fit2)
#Based on the plots, we should attempt transformations on duration and perhaps num_voted_users,
# actor_2_facebook_likes and num_users_for_reviews, num_voted_users

#We look at the added variable plots (indicating partial regression) to see
#how we can transform the other predictors
avPlots(mod.fit2)

############################ Transformations on X #############################


mod.fit3 <- lm(imdb_score^power ~ num_critic_for_reviews + 
                 (budget)  + 
                 (num_voted_users) +
                 num_user_for_reviews +
                 sqrt(duration) + 
                 cast_total_facebook_likes + 
                 actor_1_facebook_likes + 
                 actor_2_facebook_likes, 
               data = train)
sum.fit3 <- summary(mod.fit3)
sum.fit3
#Multiple R-squared:  0.4305,	Adjusted R-squared:  0.4292


mod.fit4 <- lm(imdb_score^power ~ num_critic_for_reviews + 
                 (budget)  + 
                 (num_voted_users) +
                 num_user_for_reviews +
                 1/(duration) + 
                 cast_total_facebook_likes + 
                 actor_1_facebook_likes + 
                 actor_2_facebook_likes, 
               data = train)
sum.fit4 <- summary(mod.fit4)
sum.fit4
#Multiple R-squared:  0.3401,	Adjusted R-squared:  0.3387


mod.fit5 <- lm(imdb_score^power ~ num_critic_for_reviews + 
                 (budget)  + 
                 (num_voted_users) +
                 num_user_for_reviews +
                 log10(duration) + 
                 cast_total_facebook_likes + 
                 actor_1_facebook_likes + 
                 actor_2_facebook_likes, 
               data = train)
sum.fit5 <- summary(mod.fit5)
sum.fit5

#Multiple R-squared:  0.4319,	Adjusted R-squared:  0.4306 
#So we use log10(duration)


mod.fit6 <- lm(imdb_score^power ~ num_critic_for_reviews + 
                 (budget)  + 
                 (num_voted_users) +
                 sqrt(num_user_for_reviews) +
                 log10(duration) + 
                 cast_total_facebook_likes + 
                 actor_1_facebook_likes + 
                 actor_2_facebook_likes, 
               data = train)
sum.fit6 <- summary(mod.fit6)
#Multiple R-squared:  0.4256,	Adjusted R-squared:  0.4243 

mod.fit7 <- lm(imdb_score^power ~ num_critic_for_reviews + 
                 (budget)  + 
                 (num_voted_users) +
                 num_user_for_reviews +
                 log10(duration) + 
                 cast_total_facebook_likes + 
                 actor_1_facebook_likes + 
                 sqrt(actor_2_facebook_likes), 
               data = train)
sum.fit7 <- summary(mod.fit7)
#Multiple R-squared:  0.4286,	Adjusted R-squared:  0.4272  

mod.fit8 <- lm(imdb_score^power ~ num_critic_for_reviews + 
                 (budget)  + 
                 sqrt(num_voted_users) +
                 num_user_for_reviews +
                 log10(duration) + 
                 cast_total_facebook_likes + 
                 actor_1_facebook_likes + 
                 (actor_2_facebook_likes), 
               data = train)
sum.fit8 <- summary(mod.fit8)
#Multiple R-squared:  0.4506,	Adjusted R-squared:  0.4494 

mod.fit9 <- lm(imdb_score^power ~ num_critic_for_reviews + 
                 (budget)  + 
                 sqrt(num_voted_users) +
                 num_user_for_reviews +
                 log10(duration) + 
                 cast_total_facebook_likes + 
                 actor_1_facebook_likes + 
                 sqrt(actor_2_facebook_likes), 
               data = train)
sum.fit9 <- summary(mod.fit9)
#Multiple R-squared:  0.451,	Adjusted R-squared:  0.4497 

mod.fit10 <- lm(imdb_score^power ~ num_critic_for_reviews + 
                  sqrt(budget)  + 
                  sqrt(num_voted_users) +
                  sqrt(num_user_for_reviews) +
                  log10(duration) + 
                  cast_total_facebook_likes + 
                  actor_1_facebook_likes + 
                  sqrt(actor_2_facebook_likes), 
                data = train)
sum.fit10 <- summary(mod.fit10)
#Multiple R-squared:  0.5001,	Adjusted R-squared:  0.499

#the budget variable is has relatively larger values than the other attributes, so we use
# a log transformation to scale it

mod.fit11 <- lm(imdb_score^power ~ num_critic_for_reviews + 
                  log10(budget)  + 
                  sqrt(num_voted_users) +
                  sqrt(num_user_for_reviews) +
                  log10(duration) + 
                  cast_total_facebook_likes + 
                  actor_1_facebook_likes + 
                  sqrt(actor_2_facebook_likes), 
                data = train)
sum.fit11 <- summary(mod.fit11)
#Multiple R-squared:  0.5004,	Adjusted R-squared:  0.4993

mod.fit12 <- lm(imdb_score^power ~ num_critic_for_reviews + 
                  log10(budget)  + 
                  sqrt(num_voted_users) +
                  sqrt(num_user_for_reviews) +
                  log10(duration) + 
                  cast_total_facebook_likes + 
                  sqrt(actor_1_facebook_likes) + 
                  sqrt(actor_2_facebook_likes), 
                data = train)
sum.fit12 <- summary(mod.fit12)
#Multiple R-squared:  0.5003,	Adjusted R-squared:  0.4991

mod.fit13 <- lm(imdb_score^power ~ num_critic_for_reviews + 
                  log10(budget)  + 
                  sqrt(num_voted_users) +
                  sqrt(num_user_for_reviews) +
                  log10(duration) + 
                  cast_total_facebook_likes + 
                  1/(actor_1_facebook_likes) + 
                  1/(actor_2_facebook_likes), 
                data = train)
sum.fit13 <- summary(mod.fit13)
#Multiple R-squared:  0.4989,	Adjusted R-squared:  0.498

mod.fit14 <- lm(imdb_score^power ~ sqrt(num_critic_for_reviews) + 
                  log10(budget)  + 
                  sqrt(num_voted_users) +
                  sqrt(num_user_for_reviews) +
                  log10(duration) + 
                  cast_total_facebook_likes + 
                  sqrt(actor_1_facebook_likes) + 
                  sqrt(actor_2_facebook_likes), 
                data = train)
sum.fit14 <- summary(mod.fit14)
#Multiple R-squared:  0.5015,	Adjusted R-squared:  0.5004

transformations <- list(mod.fit1,mod.fit2,mod.fit3,mod.fit4,mod.fit5,mod.fit6,
                        mod.fit7,mod.fit8,mod.fit9,mod.fit10,mod.fit11,
                        mod.fit12,mod.fit13,mod.fit14)


#Extracting all Adj R^2 values and selecting the maximum
adjR2 <- sapply(transformations,function(x) summary(x)$adj.r.squared)
adjR2
max(adjR2) #thus, we choose mod.fit14. 

########## INTERACTIONS ################


fullInter <- lm(imdb_score ~ .*., data = train)
formula(fullInter)
summary(fullInter)

# We take the interaction between :
# 1. duration and number of users votes (Hypothesis: Avid movie watchers like longer films)
# 2. number of critic reviews with number of voted users (Hypothesis: As the number of positive 
#    critic reviews increases, more people will be persuaded to watch)
# 3. Budget with number of voted users (Higher budget -> Better marketing -> More people to watch)

mod.fit15 <- lm(imdb_score^power ~ sqrt(num_critic_for_reviews) + 
                  log10(budget)  + 
                  sqrt(num_voted_users) +
                  sqrt(num_user_for_reviews) +
                  log10(duration) + 
                  cast_total_facebook_likes + 
                  sqrt(actor_1_facebook_likes) + 
                  sqrt(actor_2_facebook_likes) +
                  duration:num_voted_users +
                  num_critic_for_reviews:num_voted_users +
                  budget:num_voted_users,
                data = train)
sum.fit15 <- summary(mod.fit15)
#Multiple R-squared:  0.5077,	Adjusted R-squared:  0.5061 

mod.fit16 <- lm(imdb_score^power ~ sqrt(num_critic_for_reviews) + 
                  log10(budget)  + 
                  sqrt(num_voted_users) +
                  sqrt(num_user_for_reviews) +
                  log10(duration) + 
                  cast_total_facebook_likes + 
                  sqrt(actor_1_facebook_likes) + 
                  sqrt(actor_2_facebook_likes) +
                  num_critic_for_reviews:num_voted_users,
                data = train)
sum.fit16 <- summary(mod.fit16)
#Multiple R-squared:  0.5032,	Adjusted R-squared:  0.5019 

mod.fit17 <- lm(imdb_score^power ~ sqrt(num_critic_for_reviews) + 
                  log10(budget)  + 
                  sqrt(num_voted_users) +
                  sqrt(num_user_for_reviews) +
                  log10(duration) + 
                  cast_total_facebook_likes + 
                  sqrt(actor_1_facebook_likes) + 
                  sqrt(actor_2_facebook_likes) +
                  budget:num_voted_users,
                data = train)
sum.fit17 <- summary(mod.fit17)
#Multiple R-squared:  0.5053,	Adjusted R-squared:  0.504

mod.fit18 <- lm(imdb_score^power ~ sqrt(num_critic_for_reviews) + 
                  log10(budget)  + 
                  sqrt(num_voted_users) +
                  sqrt(num_user_for_reviews) +
                  log10(duration) + 
                  cast_total_facebook_likes + 
                  sqrt(actor_1_facebook_likes) + 
                  sqrt(actor_2_facebook_likes) +
                  duration:num_voted_users,
                data = train)
sum.fit18 <- summary(mod.fit18)
#Multiple R-squared:  0.5062,	Adjusted R-squared:  0.5049

mod.fit19 <- lm(imdb_score^power ~ sqrt(num_critic_for_reviews) + 
                  log10(budget)  + 
                  sqrt(num_voted_users) +
                  sqrt(num_user_for_reviews) +
                  log10(duration) + 
                  cast_total_facebook_likes + 
                  sqrt(actor_1_facebook_likes) + 
                  sqrt(actor_2_facebook_likes) +
                  num_critic_for_reviews:num_voted_users +
                  duration:num_voted_users,
                data = train)
sum.fit19 <- summary(mod.fit19)
#Multiple R-squared:  0.5062,	Adjusted R-squared:  0.5048 

mod.fit20 <- lm(imdb_score^power ~ sqrt(num_critic_for_reviews) + 
                  log10(budget)  + 
                  sqrt(num_voted_users) +
                  sqrt(num_user_for_reviews) +
                  log10(duration) + 
                  cast_total_facebook_likes + 
                  sqrt(actor_1_facebook_likes) + 
                  sqrt(actor_2_facebook_likes) +
                  num_critic_for_reviews:num_voted_users +
                  budget:num_voted_users,
                data = train)
sum.fit20 <- summary(mod.fit20)
#Multiple R-squared:  0.5053,	Adjusted R-squared:  0.5039 


mod.fit21 <- lm(imdb_score^power ~ sqrt(num_critic_for_reviews) + 
                  log10(budget)  + 
                  sqrt(num_voted_users) +
                  sqrt(num_user_for_reviews) +
                  log10(duration) + 
                  cast_total_facebook_likes + 
                  sqrt(actor_1_facebook_likes) + 
                  sqrt(actor_2_facebook_likes) +
                  duration:num_voted_users +
                  budget:num_voted_users,
                data = train)
sum.fit21 <- summary(mod.fit21)
#Multiple R-squared:  0.5069,	Adjusted R-squared:  0.5055


#thus we choose mod.fit15 as our final model

final.fit <- mod.fit15

########################## GLM MODEL ###########################

# We perform ordinal regression by using Profitability.level as the response variables and the 
# variables chosen from our variable selection process

# We scale the data since the vcov() method in polr uses the approx. Hessian
# and the model matrix should be sensibly with all columns 
# having range order of one.

trainsubjects <- match(rownames(train), rownames(movies))
profitcolumn <- movies[trainsubjects,c("Profitability.level")]
glmTrain <- cbind(train, profitcolumn)

glmTrain <- data.frame(Profitability.level=factor(glmTrain[,"profitcolumn"]),
                       scale(glmTrain[,c("num_critic_for_reviews","duration",
                                         "cast_total_facebook_likes","actor_1_facebook_likes",
                                         "actor_2_facebook_likes", "num_voted_users",
                                         "budget")]))

olm <- polr(as.factor(Profitability.level) ~  .,
            data=glmTrain, na.action = na.omit, Hess = TRUE)
summary(olm)

#Trying with different methods

summary(update(olm, method = "probit", Hess = TRUE), digits = 3)
summary(update(olm, method = "logistic", Hess = TRUE), digits = 3)
summary(update(olm, method = "cloglog", Hess = TRUE), digits = 3)
summary(update(olm, method = "loglog", Hess = TRUE), digits = 3)

# Adding interaction terms
head(predict(olm, glmTrain, type = "p" ))

#Include the chi-squared statistic relative to the original model
#Take the square of every predictor variable
#Use Akaike Information Criterion to get the optimal model
addterm(olm, ~.^2, test = "Chisq")
olm2 <- stepAIC(olm, ~.^2)
olm2
summary(olm2)

#Choose olm2 as final olm

final.olm <- olm2
final.olm.coef = data.frame(coef(summary(final.olm)))
final.olm.coef$pval = round((pnorm(abs(final.olm.coef$t.value), lower.tail = FALSE) * 2),2)
final.olm.coef


#Relative risk ratios 
ratios <- cbind(exp(coef(final.olm)))
ratios

# The ratios table gives the odds of a variable leveling up when it increases 
# by one unit whil keeping others constant. For example, if budget increases by a unit
# it is 2.3795 times likely to be in a higher,more profitable category.


############################### MODEL VALIDATION ###############################
# We test for the full multiple regression model and also  ordinal regression
# Profitability levels are categorized like so:
# 1.Extremely Unprofitable
# 2.Unprofitable
# 3.Broke even
# 4.Profitable
# 5.Moderately Profitable
# 6.Extremely profitable


#Model validation for multiple regression model

predicted.score <- predict(final.fit, test)
predicted.score <- sapply(predicted.score, function(x) round(x^(1/power),2))
comparison <- data.frame(test$imdb_score,predicted.score)
errors <- comparison$test.imdb_score - comparison$predicted.score
comparison <- cbind(comparison,errors)

ggplot(comparison,aes(x = row.names(comparison),group = 1)) +
  geom_line(aes(y = comparison$test.imdb_score, colour = "Actual Values"),lwd=0.60) + 
  geom_line(aes(y = comparison$predicted.score, colour = "Predicted Values"),lwd=0.60) +
  ggtitle("Actual Values and Predicted Values") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

p1<-ggplot(final.fit, aes(final.fit$fitted.values, final.fit$residuals))+geom_point()
p1<-p1+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")
p1<-p1+xlab("Fitted values")+ylab("Residuals")
p1<-p1+ggtitle("Residual vs Fitted Plot")+theme_bw() 
p1

#Model validation for GLM

testsubjects <- match(rownames(test), rownames(movies))
profitcolumn <- movies[testsubjects,c("Profitability.level")]
glmTest <- cbind(test, profitcolumn)

glmTest <- data.frame(Profitability.level=factor(glmTest[,"profitcolumn"]),
                      scale(glmTest[,c("num_critic_for_reviews","duration",
                                       "cast_total_facebook_likes","actor_1_facebook_likes",
                                       "actor_2_facebook_likes", "num_voted_users",
                                       "budget")]))

glmprediction = data.frame(predict(final.olm, newdata = glmTest, "probs"))

#Classify the probability to group
glmprediction$predict_op = colnames(glmprediction)[apply(glmprediction,1,which.max)]  
#1 stand for row, 2 stand for columns

#confusion matrix
p_op = glmprediction$predict_op #predicted prog
a_op = glmTest$Profitability.level #actual prog
c_table = table(a_op, p_op)
c_table

if(!(is.element('Broke.Even', colnames(c_table)))) {
  newCol <- rep(0,nrow(c_table)) 
  c_table <- cbind(c_table, 'Broke.Even' = newCol )}


if(!(is.element('Extremely.Profitable', colnames(c_table)))) {
  newCol <- rep(0,nrow(c_table)) 
  c_table <- cbind(c_table, 'Extremely.Profitable' = newCol )}


if(!(is.element('Extremely.Unprofitable', colnames(c_table)))) {
  newCol <- rep(0,nrow(c_table)) 
  c_table <- cbind(c_table, 'Extremely.Unprofitable' = newCol )}


if(!(is.element('Moderately.Profitable', colnames(c_table)))) {
  newCol <- rep(0,nrow(c_table)) 
  c_table <- cbind(c_table, 'Moderately.Profitable' = newCol )}


if(!(is.element('Profitable', colnames(c_table)))) {
  newCol <- rep(0,nrow(c_table)) 
  c_table <- cbind(c_table, 'Profitable' = newCol )}

if(!(is.element('Unprofitable', colnames(c_table)))) {
  newCol <- rep(0,nrow(c_table)) 
  c_table <- cbind(c_table, 'Unprofitable' = newCol )}

c_table


overall.accuracy <- ((c_table['Extremely Unprofitable', 'Extremely.Unprofitable'] + 
                        c_table['Unprofitable', 'Unprofitable'] + 
                        c_table['Broke Even', 'Broke.Even'] +
                        c_table['Profitable', 'Profitable'] +
                        c_table['Moderately Profitable', 'Moderately.Profitable'] +
                        c_table['Extremely Profitable', 'Extremely.Profitable'])/nrow(glmTest))*100
overall.accuracy             

#Overall accuracy of 37.63189. Probably due to not many instances of Break Even movies
# and Moderately Profitable movies.
