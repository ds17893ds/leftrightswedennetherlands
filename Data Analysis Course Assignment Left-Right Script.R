                     ############### SETUP #################


## Set working directory (manually) via box clicking

## Install relevant packages

install.packages(c("car", "reshape2", "descr", "psych", "tidyverse",
                   "skimr", "ggplot2", "sfsmisc", "sm", "HistData",
                   "vcd", "memisc", "stargazer", "broom", "MASS", "pscl", "interplot"))

## Create vector of packages

x <- c("car", "reshape2", "descr", "psych", "tidyverse",
       "skimr", "ggplot2", "sfsmisc", "sm", "HistData",
       "vcd", "memisc", "stargazer", "broom", "MASS", "pscl", "interplot")

## Load relevant packages

lapply(x, require, character.only = TRUE)

## Get rid of scientific notation

options(scipen = 1000)

## Load dataset ESS9 (since both Sweden and Nl are included in this round)

ds <- read.csv("ESS9e03_1.csv", header = TRUE)




#################### Hypothesis 1: Swedes are more satisfied with the government 
################################## than Dutch citizens. ########################

######################Looking at stfgov variable ######################

str(ds$stfgov) # Structure is integer
summary(ds$stfgov) # Max is 99, thus need to recode

# Recode stfgov to 'remove' 77, 88, 99

ds$stfgov <- recode(ds$stfgov,
                       "77 = NA; 88 = NA; 99 = NA")
summary(ds$stfgov) # Check


########## Compare stfgov of the Dutch and the Swedes ###########

ds$countryfactor <- recode(ds$cntry,"'SE'='Sweden'; 'NL'='Netherlands'; else=NA")

# Check
table(ds$cntry)
table(ds$countryfactor) # Same numbers


             ############ Check for normality stfgov ##############
ds.nl <- ds %>% filter(cntry == "NL") # Create subset of Dutch observations only
ds.se <- ds %>% filter(cntry == "SE") # Create subset of Swedish observations only
# Check via click on both dataframes

summary(ds.se$stfgov)
summary(ds.nl$stfgov)
table(ds.nl$stfgov)
table(ds.se$stfgov)
summary(ds.se$agea)
summary(ds.nl$agea)
summary(ds.nl$gndr)
summary(ds.se$gndr)


# Visualize normality
ggplot(ds.nl, aes(x = stfgov, y = ..density..)) + geom_histogram(fill = "white",
                                                               color = "black")
ggplot(ds.se, aes(x = stfgov, y = ..density..)) + geom_histogram(fill = "white",
                                                                 color = "black")

ggplot(ds.nl, aes(stfgov)) + geom_density() + stat_function(fun = dnorm, args = 
                                                              list(mean(ds.nl$stfgov),
                                                                   sd = sd(ds.nl$stfgov)),
                                                            color = "blue")
ggplot(ds.se, aes(stfgov)) + geom_density() + stat_function(fun = dnorm, args = 
                                                              list(mean(ds.se$stfgov),
                                                                   sd = sd(ds.se$stfgov)),
                                                            color = "blue")
ggplot(ds.nl, aes(sample = stfgov)) + stat_qq() + stat_qq_line()
ggplot(ds.se, aes(sample = stfgov)) + stat_qq() + stat_qq_line()

# Shapiro-Wilk test for normality
shapiro.test(ds.nl$stfgov) # Not normally distributed
shapiro.test(ds.se$stfgov) # Not normally distributed

# Mann-Whitney U-test

data_se_stfgov <- subset(ds, cntry == "SE")[["stfgov"]]
data_nl_stfgov <- subset(ds, cntry == "NL")[["stfgov"]]
summary(data_se_stfgov) # Check assumption of equal medians, but not 
summary(data_nl_stfgov) # Check assumption of equal medians
# The medians differ one unit, continue with test

whitney_test_score <- wilcox.test(data_se_stfgov, data_nl_stfgov)
whitney_test_score # Statistical significant result: there's a
# difference between groups

stargazer(whitney_test_score, 
          title = "Wilcoxon Rank-Sum Test Results",
          type = "text",
          header = FALSE,
          digits = 3)

print(data_nl_stfgov_clean)

#Visualization difference between group medians
data_nl_stfgov_clean <- na.omit(data_nl_stfgov) # Removes non-numeric values
data_se_stfgov_clean <- na.omit(data_se_stfgov) # Removes non-numeric values

dotchart(c(median(data_nl_stfgov_clean), median(data_se_stfgov_clean)),
         xlim=c(min(data_nl_stfgov_clean, data_se_stfgov_clean)- 1, 
                max(data_nl_stfgov_clean, data_se_stfgov_clean)+ 1), 
         xlab="Median", main="Dotplot of group medians")

######################
med_nl <- median(data_nl_stfgov_clean)
med_se <- median(data_se_stfgov_clean)
ci_nl <- t.test(data_nl_stfgov_clean)$conf.int
ci_se <- t.test(data_se_stfgov_clean)$conf.int

dotchart(c(med_nl, med_se),
         xlim=c(min(data_nl_stfgov_clean, data_se_stfgov_clean) - 1, 
                max(data_nl_stfgov_clean, data_se_stfgov_clean) + 1), 
         xlab="Median", main="Dotplot of group medians")

arrows(x0=c(med_nl, med_se), y0=c(1, 2), 
       x1=c(ci_nl[1], ci_se[1]), x2=c(ci_nl[2], ci_se[2]), 
       angle=90, code=3, length=0.1)
#################

# Calculate effect size: Hodges-Lehmann estimator
effectsize_H1 <- wilcox.test(data_se_stfgov_clean, data_nl_stfgov_clean, conf.int = TRUE)
effectsize_H1

hodges_lehmann_H1 <- median(effectsize_H1$conf.int)
cat("Effect size (Hodges-Lehmann estimator):", hodges_lehmann_H1, "\n")
## This suggests that, on average, people in the Netherlands tend to be more satisfied with their government than people in Sweden.


#################### Hyppthesis 2 ##################

        ############# Prepare data ##################
# Select relevant variables as same as object ds.var
ds.var <- ds %>% dplyr::select(cntry, lrscale, agea, eisced, hinctnta, stfgov) %>% na.omit()
ds.var %>% glimpse() # Check

# Select countries NL and SE and save as object ds.se.nl
ds.se.nl <- ds.var[ds.var$cntry == "NL" | ds.var$cntry == "SE", ]
table(ds.se.nl$cntry)

# Examine variables: format, NA's
summary(ds.se.nl$stfgov) # No weird values
str(ds.se.nl$stfgov) # Integer, needs to be numeric
ds.se.nl$n.stfgov <- as.numeric(ds.se.nl$stfgov)
str(ds.se.nl$n.stfgov) # Check

summary(ds.se.nl$agea) # 999's
ds.se.nl$agea[ds.se.nl$agea == 999] <- NA # 999's as NA
summary(ds.se.nl$agea) # Check: Min age 15, Max age 90, which is possible; 8 NA's
str(ds.se.nl$agea)
brief(ds.se.nl$agea)

summary(ds.se.nl$cntry)
str(ds.se.nl$cntry)

summary(ds.se.nl$lrscale) # 88's
ds.se.nl$lrscale[ds.se.nl$lrscale == 88] <- NA
summary(ds.se.nl$lrscale) ## 77's
ds.se.nl$lrscale[ds.se.nl$lrscale == 77] <- NA
summary(ds.se.nl$lrscale)
str(ds.se.nl$lrscale) # Integer, coerce into numeric
ds.se.nl$n.right <- as.numeric(ds.se.nl$lrscale)
str(ds.se.nl$n.right) # Check
table(ds.se.nl$n.right)

summary(ds.se.nl$eisced) # 88's
ds.se.nl$eisced[ds.se.nl$eisced > 7] <- NA
summary(ds.se.nl$eisced) # Check: max = 7 and 41 NA's; 0 is meaningless, see codebook, but not present in dataset
str(ds.se.nl$eisced) # Integer, coerce into numeric
ds.se.nl$n.eisced <- as.numeric(ds.se.nl$eisced)
str(ds.se.nl$n.eisced) # Check


summary(ds.se.nl$hinctnta) ## 88's
ds.se.nl$hinctnta[ds.se.nl$hinctnta > 10] <- NA
summary(ds.se.nl$hinctnta) # Check: max = 10 and 349 NA's
str(ds.se.nl$hinctnta) # Integer, coerce into numeric
ds.se.nl$n.hinctnta <- as.numeric(ds.se.nl$hinctnta)
str(ds.se.nl$n.hinctnta) # Check

            #################### Examine data #####################

hist(ds.se.nl$n.right) #histogram of left-right

ggplot(ds.se.nl, aes(n.right)) + geom_histogram(bins = 10) +
  xlab("Left-right scale") +
  ylab("Frequency") +
  ggtitle("Histogram of Left-right scale") +
  coord_cartesian(ylim = c(0,725), xlim = c(0, 10)) +
  theme_light() # A more fancy and scientific histogram

plot(density(ds.se.nl$n.right, na.rm=TRUE)) # Density plot

class(ds.se.nl$cntry)
summary(ds.se.nl$cntry)
ds.se.nl$f.cntry <- factor(ds.se.nl$cntry)
table(ds.se.nl$f.cntry) # Check, NL is '1' and Sweden is reference group
ds.se.nl$f.cntry <- factor(ds.se.nl$cntry, levels = c("NL", "SE"), labels = c("Netherlands", "Sweden"))
table(ds.se.nl$f.cntry)
ds.se.nl$f.cntry <- factor(ds.se.nl$cntry, levels = c("SE", "NL"), labels = c("Sweden", "Netherlands"))
table(ds.se.nl$f.cntry) # Check releveling, with Netherlands as reference group
summary(ds.se.nl$f.cntry)

## Visualization of correlation ##

scatterplotMatrix( ~ n.right + n.stfgov + n.eisced + n.hinctnta + f.cntry, data = ds.se.nl)

## Correlation table to quantify the relationship between the variables ##

ds.cor.se.nl <- ds.se.nl %>% dplyr::select(n.right, n.stfgov, n.eisced, n.hinctnta) %>% na.omit()
cor_table <- cor(ds.cor.se.nl) ## I created a subset, without categorical variable f.cntry, since it doesn't make sense to include it in a regression table
print(cor_table)

install.packages("Hmisc")

load(Hmisc)

cor_table_P_Values <- rcorr(as.matrix(ds.cor.se.nl))
print(cor_table_P_Values) # Correlationmatrix with P values. All values are statistically significant (p < 0.05)


######################## Regression model (with interaction) ######################

model1 <- lm(n.stfgov ~ n.right*f.cntry + agea + n.eisced + n.hinctnta, data = ds.se.nl)
summary(model1)
plot(model1) # Check assumptions for multivariate regression analysis

model1vif <- lm(n.stfgov ~ n.right + f.cntry + agea + n.eisced + n.hinctnta, data = ds.se.nl)
vif(model1vif) # Leave out interaction term for calculating VIF

## Assumptions are met and VIF is good, proceed with analysis

stargazer(model1, title = "Multivariate Regression Results", align = TRUE, type = "text")
stargazer(model1, 
          covariate.labels = c("Left/Right", "Country: Netherlands", "Age", "Education", "Income", "Interaction Left/Right and the Netherlands"), 
          type = "text",
          dep.var.labels = "Trust in Government") # Adjust labels in regression table

## Continue with the size of the effects: effect sizes
install.packages("effects")
library(effects)

plot(predictorEffects(model1)) ## All effect plots in 1 image
plot(predictorEffects(model1),1) ## Individual effect plots
plot(predictorEffects(model1),2)
plot(predictorEffects(model1),3)
plot(predictorEffects(model1),4)
plot(predictorEffects(model1),5)


################# Hypothesis 3 ##################

## Install and (manually load) relevant packages to create BNs and infer DAGs
install.packages("BiocManager")
install.packages("Rgraphviz")
install.packages("gRain")
install.packages("lattice")
install.packages("bnlearn")
install.packages("gridExtra")

## Since the DAG (directed acyclic graph) structure is unknown, the DAG should be inferred from the data
## I will use the hill-climbing algorithm to search for the best 
## fitting DAG based on conditional independence tests (which corresponds with the hypothesis). The resulting 
## DAG represents a model of conditional dependencies between variables. This method relies on goodness-of-fit
## statistics (which we covered in the model-fit lab).

              ############# Prepare data ##################

## First, I create a subset of the relevant variables

ds.var.dag <- ds %>% dplyr::select(lrscale, stfeco, stfgov, stfdem, stfedu, stfhlth, cntry) %>% na.omit()
ds.var.dag %>% glimpse() # Check
ds.var.dag.se.nl <- ds.var.dag[ds.var.dag$cntry == "NL" | ds.var.dag$cntry == "SE", ] ## Only Sweden and Netherlands
table(ds.var.dag.se.nl$cntry) # Check

## Check variables

## lrscale
summary(ds.var.dag.se.nl$lrscale) # 88's
ds.var.dag.se.nl$lrscale[ds.var.dag.se.nl$lrscale == 88] <- NA
summary(ds.var.dag.se.nl$lrscale) ## 77's
ds.var.dag.se.nl$lrscale[ds.var.dag.se.nl$lrscale == 77] <- NA
summary(ds.var.dag.se.nl$lrscale)
str(ds.var.dag.se.nl$lrscale) # Integer, coerce into numeric
ds.var.dag.se.nl$n.right <- as.numeric(ds.var.dag.se.nl$lrscale)
str(ds.var.dag.se.nl$n.right) # Check
table(ds.var.dag.se.nl$n.right)

## CREATE VARIABLE LEFT AND RIGHT?

## stfeco
summary(ds.var.dag.se.nl$stfeco) # 88´s
ds.var.dag.se.nl$stfeco[ds.var.dag.se.nl$stfeco == 88] <- NA
summary(ds.var.dag.se.nl$stfeco) ## 77's
ds.var.dag.se.nl$stfeco[ds.var.dag.se.nl$stfeco == 77] <- NA
summary(ds.var.dag.se.nl$stfeco)
str(ds.var.dag.se.nl$stfeco) # Integer, needs to be numeric
ds.var.dag.se.nl$n.stfeco <- as.numeric(ds.var.dag.se.nl$stfeco)
str(ds.var.dag.se.nl$n.stfeco) # Check

## stfgov
summary(ds.var.dag.se.nl$stfgov) # No weird values
str(ds.var.dag.se.nl$stfgov) # Integer, needs to be numeric
ds.var.dag.se.nl$n.stfgov <- as.numeric(ds.var.dag.se.nl$stfgov)
str(ds.var.dag.se.nl$n.stfgov) # Check

## stfdem
summary(ds.var.dag.se.nl$stfdem) # 88´s
ds.var.dag.se.nl$stfdem[ds.var.dag.se.nl$stfdem == 88] <- NA
summary(ds.var.dag.se.nl$stfdem) ## 77's
ds.var.dag.se.nl$stfdem[ds.var.dag.se.nl$stfdem == 77] <- NA
summary(ds.var.dag.se.nl$stfdem)
str(ds.var.dag.se.nl$stfdem) # Integer, needs to be numeric
ds.var.dag.se.nl$n.stfdem <- as.numeric(ds.var.dag.se.nl$stfdem)
str(ds.var.dag.se.nl$n.stfdem) # Check

## stfedu
summary(ds.var.dag.se.nl$stfedu) # 88´s
ds.var.dag.se.nl$stfedu[ds.var.dag.se.nl$stfedu == 88] <- NA
summary(ds.var.dag.se.nl$stfedu)
str(ds.var.dag.se.nl$stfedu) # Integer, needs to be numeric
ds.var.dag.se.nl$n.stfedu <- as.numeric(ds.var.dag.se.nl$stfedu)
str(ds.var.dag.se.nl$n.stfedu) # Check

## stfhlth
summary(ds.var.dag.se.nl$stfhlth) # 88´s
ds.var.dag.se.nl$stfhlth[ds.var.dag.se.nl$stfhlth == 88] <- NA
summary(ds.var.dag.se.nl$stfhlth) ## 77's
str(ds.var.dag.se.nl$stfhlth) # Integer, needs to be numeric
ds.var.dag.se.nl$n.stfhlth <- as.numeric(ds.var.dag.se.nl$stfhlth)
str(ds.var.dag.se.nl$n.stfhlth) # Check

## Create extra subsets: Swedish & Dutch observations only in order to compare
## DAGs later on

ds.var.dag.se <- ds.var.dag.se.nl %>% filter(cntry == "SE") # Subset Sweden
ds.var.dag.nl <- ds.var.dag.se.nl %>% filter(cntry == "NL") # Subset Netherlands


            #################### Examine data #####################

## Visualize linearity

scatterplotMatrix( ~ n.right + n.stfeco + n.stfgov + n.stfdem + n.stfedu + n.stfhlth, data = ds.var.dag.se.nl)
scatterplotMatrix( ~ n.right + n.stfeco + n.stfgov + n.stfdem + n.stfedu + n.stfhlth, data = ds.var.dag.se)
scatterplotMatrix( ~ n.right + n.stfeco + n.stfgov + n.stfdem + n.stfedu + n.stfhlth, data = ds.var.dag.nl)

## Quantify correlations

ds.var.dag.cor.se.nl <- ds.var.dag.se.nl %>% dplyr::select(n.right , n.stfeco , n.stfgov , n.stfdem , n.stfedu , n.stfhlth) %>% na.omit()
cor_table_dag_se_nl <- cor(ds.var.dag.cor.se.nl)
print(cor_table_dag_se_nl)

## CAN BE DONE FOR NL AND SE AS WELL

                           ########## DAG ###########
learned <- hc(ds.var.dag.se.nl) ## error, bc integer variables
ds.sub.dag.se.nl <- ds.var.dag.se.nl %>% dplyr::select(n.right, n.stfgov, n.stfeco, n.stfdem, n.stfedu, n.stfhlth) %>% na.omit()
learned.HL.se.nl <- hc(ds.sub.dag.se.nl) ### Use the hill-climbing algorithm to search for the best fitting DAG
learned ## Check table

## Visualize the network

graph.dag.se.nl.1 <- graphviz.plot(learned.HL.se.nl) ## Basic visualization
graphviz.plot(learned.HL.se.nl) ## Run visualization

graph <- graphviz.plot(learned.HL.se.nl, layout = "dot")
plot(graph)

hlight.1 <- list(nodes = nodes(learned.HL.se.nl), arcs = arcs(learned.HL.se.nl), col = "grey",
               textCol = "grey")
graph.dag.se.nl.2 <- graphviz.plot(learned.HL.se.nl, highlight = hlight.1, render = T)



                   #### Test with Markov Random Field ###

library("ggplot2")
install.packages("bootnet")
library("bootnet")

Network <- estimateNetwork(ds.sub.dag.se.nl, default = "EBICglasso", threshold = TRUE)
plot(Network, layout = "spring", labels = colnames(ds.sub.dag.se.nl))

results <- estimateNetwork(
  ds.sub.dag.se.nl,
  default = "EBICglasso",
  corMethod = "cor")
plot(results)



score(learned, data = ds.sub.dag.se.nl, type = "bic-g")
score(learned, data = ds.sub.dag.se.nl, type = "bge")
## The lower the score, the better, proceed with bic-9
