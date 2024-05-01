---
title: "Analyzing predictors that make a Board Game popular"
author: "Joshua Zhong"
date: "04/30/2024"
output:
 html_document:
  toc: yes
  toc_depth: 4
  toc_float: yes
  fig_width: 6
  fig_caption: yes
  number_sections: yes
  theme: readable
  editor_options:
  chunk_output_type: console
---




```{=html}

<style type="text/css">

/* Cascading Style Sheets (CSS) is a stylesheet language used to describe the presentation of a document written in HTML or XML. it is a simple mechanism for adding style (e.g., fonts, colors, spacing) to Web documents. */

h1.title {  /* Title - font specifications of the report title */
  font-size: 24px;
  color: DarkRed;
  text-align: center;
  font-family: "Gill Sans", sans-serif;
}
h4.author { /* Header 4 - font specifications for authors  */
  font-size: 20px;
  font-family: system-ui;
  color: DarkRed;
  text-align: center;
}
h4.date { /* Header 4 - font specifications for the date  */
  font-size: 18px;
  font-family: system-ui;
  color: DarkBlue;
  text-align: center;
}
h1 { /* Header 1 - font specifications for level 1 section title  */
    font-size: 22px;
    font-family: "Times New Roman", Times, serif;
    color: navy;
    text-align: left;
}
h2 { /* Header 2 - font specifications for level 2 section title */
    font-size: 20px;
    font-family: "Times New Roman", Times, serif;
    color: navy;
    text-align: left;
}

h3 { /* Header 3 - font specifications of level 3 section title  */
    font-size: 18px;
    font-family: "Times New Roman", Times, serif;
    color: navy;
    text-align: left;
}

h4 { /* Header 4 - font specifications of level 4 section title  */
    font-size: 18px;
    font-family: "Times New Roman", Times, serif;
    color: darkred;
    text-align: left;
}

body { background-color:white; }

.highlightme { background-color:yellow; }

p { background-color:white; }

</style>
```


```{r options, echo = FALSE, include = FALSE}

options(scipen = 2)


   library(tidyverse)
   library(GPArotation)
   library(psych)
   library(nFactors)
   library(rmarkdown)
   library(knitr)
   library(parameters)
   library(corrplot)
   library(ggcorrplot)
   library(ggfortify)
   library(dplyr)
   require(ggplot2)
   require(GGally) 
   require(CCA)
   require(olsrr)
   require(cocron)

if (!require("summarytools")) {
   install.packages("summarytools")
   library(summarytools)
}

if (!require("EBImage")) {
   install.packages("EBImage")
   library(EBImage)
}

# Specifications of outputs of code in code chunks
knitr::opts_chunk$set(
	echo = FALSE,
	message = FALSE,
	warning = FALSE,
	comment = FALSE,
	results = TRUE, 
	digits = 4
)


```


```{r import data}

games <- read.csv("https://raw.githubusercontent.com/JZhong01/STA321/main/games.csv")


```

# Introduction

BoardGameGeek (BGG) is a comprehensive database and community centered around board games. BGG provides extensive details on a myriad of games, including game mechanics, themes, and user interactions such as ratings and reviews. Our analysis utilizes a data set extracted from BGG, focusing on several key variables that offer insights into the popularity and reception of these games among the gaming community. The goal of this report is to provide a statistical analysis of board games, particularly exmaining how various factors such as game complexity and intended audience age affects a game's average rating. 

## Data Source

The data set under consideration is sourced from BoardGameGeek, an authoritative and extensive database for board games. It contains detailed information on thousands of games, including user ratings, recommended ages, play times, and much more. This platform is not only a repository of game information but also a community for board game enthusiasts.

## Variables of Interest

Our analysis focuses on the following variables as provided by BGG: 

- AvgRating: The average rating given to a game, which serves as our response variable (Y-variable).
- GameWeight: An index that measures game complexity and difficulty. 
- MinPlayers: Minimum number of players recommended for the game.
- MaxPlayers: Maximum number of players recommended for the game. 
- ComAgeRec: The community's recommended age for players. 
- MfgPlaytime: The Manufacturer's stated playtime for a game. 

These variables were selected to explore relationships between the game's complexity, suitability for various age groups, playtime, and its overall rating within the community. 

# Exploratory Data Analysis 

For our Exploratory Data Analysis, we will be looking at the Descriptive Statistics, generating histograms to assess general patterns, trends, and shape, and finally performing correlation analysis.

## Descriptive Statistics

```{r Descriptive Stats}
par(mfrow = c(2,2))

descr(games$AvgRating)
descr(games$GameWeight)
descr(games$MinPlayers)
descr(games$MaxPlayers)
descr(games$ComAgeRec)
descr(games$MfgPlaytime)

```

## Histograms

```{r Histograms}
par(mfrow = c(2,2))

hist(games$AvgRating, xlab = "Average Rating", main = "Histogram of Average BGG Board Game Rating" )
hist(games$GameWeight, xlab = "Game Weight", main = "Histogram of BGG Game Weights" )
hist(games$MinPlayers, xlab = "Minimum Player Counts", main = "Histogram of Minimum Player Counts" )
hist(games$MaxPlayers, breaks=c(0, 5, 10, 20, 50, Inf), xlim=c(0, 50), xlab = "Maximum Player Counts", main = "Histogram of Board Maximum Player Counts")
hist(games$ComAgeRec, xlab = "Community Age Recommendation", main = "Histogram of BGG's Age Recommendation" )
hist(games$MfgPlaytime, breaks=c(0, 30, 60, 90, 120, 150, 180, Inf), xlim=c(0, 180), xlab = "Manufacturer's Stated Playtime", main = "Histogram of Board Game Playtimes" )

```

Looking at the histograms, we see that 3 of the distributions - Board Game Ratings, Minimum Player Counts, and BGG Community Age Recommendation - show a relatively normal distribution. 

The histogram for BGG Game Weights appears moderately right-skewed; however, because the sample size N = 21925, we can use the Central Limit Theorem to assume that the distribution is approximately normally distributed. 

However, the histograms for Maximum player counts and Manufacturer's stated playtime are heavily right-skewed. We will perform transformations to try and adjust because that clearly fails the assumption of normality that's required for Linear Regression. 


## Correlation Analysis

```{r pairwise plots}

new_games <- games[c("AvgRating", "GameWeight", "MinPlayers", "MaxPlayers", "ComAgeRec", "MfgPlaytime")]
    

pairs.panels(new_games, pch=6, main="Pair-wise Scatter Plot")

```

- AvgRating and GameWeight: A correlation of 0.48 suggests a moderate positive relationship, indicating that games with higher complexity (or GameWeight) tend to have higher ratings.
- GameWeight and MfgPlaytime: A correlation of 0.65 is relatively strong, suggesting that more complex games also tend to have longer play times.
- MinPlayers and MaxPlayers: These variables have very little correlation with most other variables.

Overall our pairwise plots show that other than Game Weight and Community Age Recommendation, most of these variables may not contribute much to the variability in the response variable. 


# Our Models 

Now, we'll look at the candidate models and their residual analyses. 

## Full Model 

The initial full linear model will consist of 5 predictor variables. The linear model is represented as the following equation: 

\(AvgRating = \beta_0 + \beta_1 * GameWeight + \beta_2 * MinPlayers + \beta_3 * MaxPlayers + \beta_4 * ComAgeRec + \beta_5 * MfgPlaytime\)


```{r full model}

initial_model <- lm(AvgRating~., data = new_games)

kable(summary(initial_model)$coef, caption = "Statistics of Regression Coefficients", digits = 2, scientific = TRUE)

```

We now look at the residual analysis. 

```{r full model resid analysis}

par(mfrow = c(2,2))
plot(initial_model)

```

The conditions seem to be satisfied for the most part. The Residuals vs Fitted plots shows that the condition of linearity is confirmed. The Scale-Location plot shows that constant variance is satisfied. The Residuals vs Leverage plot shows that there are a handful of influential points (large outliers or leverage points), but this shouldn't impact the model's performance significantly. 

However, the Q-Q plot isn't linear meaning that the assumption of normality is insufficient. As a result, we will explore other models. 

## Stepwise Regression Model

We will be using bidirectional stepwise regression to create our second candidate model. Stepwise regression is a method of automatic variable selection used in statistical modeling, particularly useful when dealing with data sets that contain multiple potential predictors. This method simplifies the model-building process by systematically adding or removing variables based on specific criteria and evaluating the statistical significance of each model iteration. Bidirectional stepwise regression means that we'll be adding and removing predictor variables from the model in an iterative fashion and eventually leaves us with only the appropriate candidate variables. 

```{r stepwise model}

step_model <- stepAIC(initial_model, direction = "both", trace = FALSE)

kable(summary(step_model)$coef, caption = "Statistics of Regression Coefficients", digits = 2, scientific = TRUE)


```

Running a bidirectional Stepwise regression removed MfgPlaytime from the function, which resulted in an improved model - increasing the F-statistic from F(5,16389) = 1323 to F(4,16390) = 1654. 

Let's look now at the Residual plots. 

```{r step model resid analysis}

par(mfrow = c(2,2))
plot(step_model)

```

The conditions seem to be satisfied . The Residuals vs Fitted plots shows that the condition of linearity is confirmed. The Scale-Location plot shows that constant variance is satisfied. The Residuals vs Leverage plot shows that there are no more influential points. The Q-Q plot shows a slight skew, but the sample size seems large enough for the central limit theorem to apply. 

## Box-Cox Transformed Model

The Box-Cox transformation is a statistical technique used to stabilize variance and normalize data, which is particularly useful for enhancing the performance of models that assume normality and homoscedasticity (constant variance). This transformation applies a power function to the response variable, parameterized by a lambda (λ) value, which determines the specific transformation applied. 

When λ equals zero, the Box-Cox transformation becomes a natural logarithm transformation, and for other values, it modifies the data by raising it to a power specified by λ. By transforming the data in this way, the Box-Cox transformation can make skewed data more symmetric and normally distributed, improving the accuracy and validity of statistical analyses and predictive models. 

```{r boxcox transform}

boxcox(AvgRating~., data = new_games, lambda = seq(1.5,2.1, length = 10))

```


Our plot demonstrates that lambda is roughly 2, which means that we'll be taking our response variable, Average Rating, to the second power. 

We'll apply this transformation to both the original model as well as the stepwise model. 


```{r boxcox models}

bc_model <- lm((AvgRating)^2 ~., data = new_games)
summary(bc_model)

bc_model2 <- lm((AvgRating)^2 ~. - MfgPlaytime, data = new_games)
summary(bc_model2)


```

The Box-Cox transformation on the stepwise model performed the best with a F-statistic of F(4, 16390) = 1781. 

Looking at our box-cox transformed stepwise model's residual plots show that all conditions are met. 

```{r final model resid analysis}

par(mfrow = c(2,2))
plot(bc_model2)

```




