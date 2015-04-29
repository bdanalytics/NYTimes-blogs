# NYTimes:Blogs:: Popular classification:: hdlpfx7
bdanalytics  

**  **    
**Date: (Tue) May 05, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://kaggle2.blob.core.windows.net/competitions-data/kaggle/4347/NYTimesBlogTrain.csv
    New:        https://kaggle2.blob.core.windows.net/competitions-data/kaggle/4347/NYTimesBlogTest.csv  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

### ![](<filename>.png)

## Potential next steps include:
- Organization:
    - Categorize by chunk
    - Priority criteria:
        0. Ease of change
        1. Impacts report
        2. Cleans innards
        3. Bug report
        
- manage.missing.data chunk:
    - cleaner way to manage re-splitting of training vs. new entity

- extract.features chunk:
    - Add n-grams for glb_txt_vars
        - "RTextTools", "tau", "RWeka", and "textcat" packages
    - Convert user-specified mutate code to config specs
    
- fit.models chunk:
    - Prediction accuracy scatter graph:
    -   Add tiles (raw vs. PCA)
    -   Use shiny for drop-down of "important" features
    -   Use plot.ly for interactive plots ?
    
    - Change .fit suffix of model metrics to .mdl if it's data independent (e.g. AIC, Adj.R.Squared - is it truly data independent ?, etc.)
    - move model_type parameter to myfit_mdl before indep_vars_vctr (keep all model_* together)
    - create a custom model for rpart that has minbucket as a tuning parameter
    - varImp for randomForest crashes in caret version:6.0.41 -> submit bug report

- Probability handling for multinomials vs. desired binomial outcome
-   ROCR currently supports only evaluation of binary classification tasks (version 1.0.7)
-   extensions toward multiclass classification are scheduled for the next release

- Skip trControl.method="cv" for dummy classifier ?
- Add custom model to caret for a dummy (baseline) classifier (binomial & multinomial) that generates proba/outcomes which mimics the freq distribution of glb_rsp_var values; Right now glb_dmy_glm_mdl always generates most frequent outcome in training data
- glm_dmy_mdl should use the same method as glm_sel_mdl until custom dummy classifer is implemented

- fit.all.training chunk:
    - myplot_prediction_classification: displays 'x' instead of '+' when there are no prediction errors 
- Compare glb_sel_mdl vs. glb_fin_mdl:
    - varImp
    - Prediction differences (shd be minimal ?)

- Move glb_analytics_diag_plots to mydsutils.R: (+) Easier to debug (-) Too many glb vars used
- Add print(ggplot.petrinet(glb_analytics_pn) + coord_flip()) at the end of every major chunk
- Parameterize glb_analytics_pn
- Move glb_impute_missing_data to mydsutils.R: (-) Too many glb vars used; glb_<>_df reassigned
- Replicate myfit_mdl_classification features in myfit_mdl_regression
- Do non-glm methods handle interaction terms ?
- f-score computation for classifiers should be summation across outcomes (not just the desired one ?)
- Add accuracy computation to glb_dmy_mdl in predict.data.new chunk
- Why does splitting fit.data.training.all chunk into separate chunks add an overhead of ~30 secs ? It's not rbind b/c other chunks have lower elapsed time. Is it the number of plots ?
- Incorporate code chunks in print_sessionInfo
- Test against 
    - projects in github.com/bdanalytics
    - lectures in jhu-datascience track

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/mydsutils.R")
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
# Gather all package requirements here
#suppressPackageStartupMessages(require())
#packageVersion("snow")

#require(sos); findFn("pinv", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_trnng_url <- "https://kaggle2.blob.core.windows.net/competitions-data/kaggle/4347/NYTimesBlogTrain.csv"
glb_newdt_url <- "https://kaggle2.blob.core.windows.net/competitions-data/kaggle/4347/NYTimesBlogTest.csv"
glb_out_pfx <- "NYTBlogs_hdlpfx7_"

glb_is_separate_newent_dataset <- TRUE    # or TRUE
glb_split_entity_newent_datasets <- TRUE   # or FALSE
glb_split_newdata_method <- "sample"          # "condition" or "sample" or "copy"
glb_split_newdata_condition <- "<col_name> <condition_operator> <value>"    # or NULL
glb_split_newdata_size_ratio <- 0.3               # > 0 & < 1
glb_split_sample.seed <- 123               # or any integer
glb_drop_vars <- c(NULL) # or c("<col_name>")

#glb_max_fitent_obs <- 2238 # NULL # or any integer
glb_max_fitent_obs <- NULL # or any integer                         
glb_is_regression <- FALSE; glb_is_classification <- TRUE; glb_is_binomial <- TRUE

glb_rsp_var_raw <- "Popular"
# for classification, the response variable has to be a factor
glb_rsp_var <- "Popular.fctr"
# if the response factor is based on numbers e.g (0/1 vs. "A"/"B"), 
#   caret predict(..., type="prob") crashes
glb_map_rsp_raw_to_var <- function(raw) {
    relevel(factor(ifelse(raw == 1, "Y", "N")), as.factor(c("Y", "N")), ref="N")
    #as.factor(paste0("B", raw))
    #as.factor(raw)    
}
glb_map_rsp_raw_to_var(c(1, 1, 0, 0, 0))
```

```
## [1] Y Y N N N
## Levels: N Y
```

```r
glb_map_rsp_var_to_raw <- function(var) {
    as.numeric(var) - 1
    #as.numeric(var)
    #levels(var)[as.numeric(var)]
    #c(" <=50K", " >50K")[as.numeric(var)]
}
glb_map_rsp_var_to_raw(glb_map_rsp_raw_to_var(c(1, 1, 0, 0, 0)))
```

```
## [1] 1 1 0 0 0
```

```r
if ((glb_rsp_var != glb_rsp_var_raw) & is.null(glb_map_rsp_raw_to_var))
    stop("glb_map_rsp_raw_to_var function expected")
glb_rsp_var_out <- paste0(glb_rsp_var, ".predict.") # model_id is appended later

# List info gathered for various columns
# <col_name>:   <description>; <notes>
# NewsDesk = the New York Times desk that produced the story (Business, Culture, Foreign, etc.)
# SectionName = the section the article appeared in (Opinion, Arts, Technology, etc.)
# SubsectionName = the subsection the article appeared in (Education, Small Business, Room for Debate, etc.)
# Headline = the title of the article
# Snippet = a small portion of the article text
# Abstract = a summary of the blog article, written by the New York Times
# WordCount = the number of words in the article
#   created WordCount.log

# PubDate = the publication date, in the format "Year-Month-Day Hour:Minute:Second"
glb_date_vars <- c("PubDate")

# UniqueID = a unique identifier for each article
glb_id_vars <- c("UniqueID")

glb_is_textual <- TRUE # vs. glb_is_numerical ???
#Sys.setlocale("LC_ALL", "C") # For english
glb_txt_vars <- c("Headline", "Snippet", "Abstract")   
glb_append_stop_words <- list() # NULL # or c("<freq_word>") 

# Properties:
#   numrows(glb_feats_df) << numrows(glb_fitent_df)
#   Select terms that appear in at least 0.2 * O(FP/FN(glb_OOBent_df))
#       numrows(glb_OOBent_df) = 1.1 * numrows(glb_newent_df)
#glb_sprs_thresholds <- c(0.982, 0.965, 0.965)
glb_sprs_thresholds <- c(0.982, 0.970, 0.970)
names(glb_sprs_thresholds) <- glb_txt_vars

# List transformed vars  
glb_exclude_vars_as_features <- c(NULL) # or c("<var_name>") 
if (glb_is_textual)
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                          glb_txt_vars)
if (glb_rsp_var_raw != glb_rsp_var)
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                            glb_rsp_var_raw)
# List feats that shd be excluded due to known causation by prediction variable
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c(NULL)) # or c("<col_name>")
# List output vars (useful during testing in console)          
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                         grep(glb_rsp_var_out, names(glb_trnent_df), value=TRUE)) 

glb_impute_na_data <- TRUE            # or TRUE
glb_mice_complete.seed <- 144               # or any integer

glb_models_lst <- list(); glb_models_df <- data.frame()
# rpart:  .rnorm messes with the models badly
#         caret creates dummy vars for factor feats which messes up the tuning
#             - better to feed as.numeric(<feat>.fctr) to caret 
# Regression
if (glb_is_regression)
    glb_models_method_vctr <- c("lm", "glm", "rpart", "rf") else
# Classification
    if (glb_is_binomial)
        glb_models_method_vctr <- c("glm", "rpart", "rf") else  
        glb_models_method_vctr <- c("rpart", "rf")

# Baseline prediction model feature(s)
glb_Baseline_mdl_var <- NULL # or c("<col_name>")

glb_model_metric_terms <- NULL # or matrix(c(
#                               0,1,2,3,4,
#                               2,0,1,2,3,
#                               4,2,0,1,2,
#                               6,4,2,0,1,
#                               8,6,4,2,0
#                           ), byrow=TRUE, nrow=5)
glb_model_metric <- NULL # or "<metric_name>"
glb_model_metric_maximize <- NULL # or FALSE (TRUE is not the default for both classification & regression) 
glb_model_metric_smmry <- NULL # or function(data, lev=NULL, model=NULL) {
#     confusion_mtrx <- t(as.matrix(confusionMatrix(data$pred, data$obs)))
#     #print(confusion_mtrx)
#     #print(confusion_mtrx * glb_model_metric_terms)
#     metric <- sum(confusion_mtrx * glb_model_metric_terms) / nrow(data)
#     names(metric) <- glb_model_metric
#     return(metric)
# }

glb_tune_models_df <- 
   rbind(
    #data.frame(parameter="cp", min=0.00005, max=0.00005, by=0.000005),
                            #seq(from=0.01,  to=0.01, by=0.01)
    #data.frame(parameter="mtry", min=2, max=4, by=1),
    data.frame(parameter="dummy", min=2, max=4, by=1)
        ) 
# or NULL
glb_n_cv_folds <- 3 # or NULL

glb_clf_proba_threshold <- NULL # 0.5

# Model selection criteria
if (glb_is_regression)
    glb_model_evl_criteria <- c("min.RMSE.OOB", "max.R.sq.OOB", "max.Adj.R.sq.fit")
if (glb_is_classification) {
    if (glb_is_binomial)
        glb_model_evl_criteria <- 
            c("max.Accuracy.OOB", "max.auc.OOB", "max.Kappa.OOB", "min.aic.fit") else
        glb_model_evl_criteria <- c("max.Accuracy.OOB", "max.Kappa.OOB")
}

glb_sel_mdl_id <- NULL # or "<model_id_prefix>.<model_method>"
glb_fin_mdl_id <- glb_sel_mdl_id # or "Final"

# Depict process
glb_analytics_pn <- petrinet(name="glb_analytics_pn",
                        trans_df=data.frame(id=1:6,
    name=c("data.training.all","data.new",
           "model.selected","model.final",
           "data.training.all.prediction","data.new.prediction"),
    x=c(   -5,-5,-15,-25,-25,-35),
    y=c(   -5, 5,  0,  0, -5,  5)
                        ),
                        places_df=data.frame(id=1:4,
    name=c("bgn","fit.data.training.all","predict.data.new","end"),
    x=c(   -0,   -20,                    -30,               -40),
    y=c(    0,     0,                      0,                 0),
    M0=c(   3,     0,                      0,                 0)
                        ),
                        arcs_df=data.frame(
    begin=c("bgn","bgn","bgn",        
            "data.training.all","model.selected","fit.data.training.all",
            "fit.data.training.all","model.final",    
            "data.new","predict.data.new",
            "data.training.all.prediction","data.new.prediction"),
    end  =c("data.training.all","data.new","model.selected",
            "fit.data.training.all","fit.data.training.all","model.final",
            "data.training.all.prediction","predict.data.new",
            "predict.data.new","data.new.prediction",
            "end","end")
                        ))
#print(ggplot.petrinet(glb_analytics_pn))
print(ggplot.petrinet(glb_analytics_pn) + coord_flip())
```

```
## Loading required package: grid
```

![](NYTBlogs_hdlpfx7_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_script_tm <- proc.time()
glb_script_df <- data.frame(chunk_label="import_data", 
                            chunk_step_major=1, chunk_step_minor=0,
                            elapsed=(proc.time() - glb_script_tm)["elapsed"])
print(tail(glb_script_df, 2))
```

```
##         chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed import_data                1                0   0.002
```

## Step `1`: import data

```r
glb_trnent_df <- myimport_data(url=glb_trnng_url, comment="glb_trnent_df", 
                                force_header=TRUE)
```

```
## [1] "Reading file ./data/NYTimesBlogTrain.csv..."
## [1] "dimensions of data in ./data/NYTimesBlogTrain.csv: 6,532 rows x 10 cols"
##   NewsDesk      SectionName SubsectionName
## 1 Business Crosswords/Games               
## 2  Culture             Arts               
## 3 Business     Business Day       Dealbook
## 4 Business     Business Day       Dealbook
## 5  Science           Health               
## 6  Science           Health               
##                                            Headline
## 1                                  More School Daze
## 2      New 96-Page Murakami Work Coming in December
## 3 Public Pension Funds Stay Mum on Corporate Expats
## 4                             Boot Camp for Bankers
## 5                     Of Little Help to Older Knees
## 6                     A Benefit of Legal Marijuana 
##                                                                                                                                                                                                                           Snippet
## 1                                                                                                                                                                  A puzzle from Ethan Cooper that reminds me that a bill is due.
## 2                                                                            The Strange Library will arrive just three and a half months after Mr. Murakamis latest novel, Colorless Tsukuru Tazaki and His Years of Pilgrimage.
## 3                                      Public pension funds have major stakes in American companies moving overseas to cut their tax bills. But they are saying little about the strategy, which could hurt the nations tax base.
## 4                                                                         As they struggle to find new business to bolster sluggish earnings, banks consider the nations 25 million veterans and service members ideal customers.
## 5                                         Middle-aged and older patients are unlikely to benefit in the long term from surgery to repair tears in the meniscus, pads of cartilage in the knee, a new review of studies has found.
## 6 A new study has found evidence that legal access to marijuana is associated with fewer opioid overdose deaths, but researchers said their findings should not be used as the basis for the wide adoption of legalized cannabis.
##                                                                                                                                                                                                                          Abstract
## 1                                                                                                                                                                  A puzzle from Ethan Cooper that reminds me that a bill is due.
## 2                                                                            The Strange Library will arrive just three and a half months after Mr. Murakamis latest novel, Colorless Tsukuru Tazaki and His Years of Pilgrimage.
## 3                                      Public pension funds have major stakes in American companies moving overseas to cut their tax bills. But they are saying little about the strategy, which could hurt the nations tax base.
## 4                                                                         As they struggle to find new business to bolster sluggish earnings, banks consider the nations 25 million veterans and service members ideal customers.
## 5                                         Middle-aged and older patients are unlikely to benefit in the long term from surgery to repair tears in the meniscus, pads of cartilage in the knee, a new review of studies has found.
## 6 A new study has found evidence that legal access to marijuana is associated with fewer opioid overdose deaths, but researchers said their findings should not be used as the basis for the wide adoption of legalized cannabis.
##   WordCount             PubDate Popular UniqueID
## 1       508 2014-09-01 22:00:09       1        1
## 2       285 2014-09-01 21:14:07       0        2
## 3      1211 2014-09-01 21:05:36       0        3
## 4      1405 2014-09-01 20:43:34       1        4
## 5       181 2014-09-01 18:58:51       1        5
## 6       245 2014-09-01 18:52:22       1        6
##      NewsDesk SectionName SubsectionName
## 226    Styles                           
## 995                                     
## 2124   TStyle                           
## 3326   TStyle                           
## 4752 Business  Technology               
## 6462  Foreign                           
##                                                   Headline
## 226  For Tavi Gevinson, Fashion Takes a Back Seat, for Now
## 995          Reconsidering What to Call an Extremist Group
## 2124          Paris Fashion Week: Kenzo Spring/Summer 2015
## 3326                              The Portable Blue Bottle
## 4752     Monster Moves to Restore a Faded Job Search Brand
## 6462      1889: Priest Questions the Meridian of Greenwich
##                                                                                                                                                                                 Snippet
## 226                                                     Tavi Gevinson, the teenage fashion star turned Broadway actress, wont be much of a player at New York Fashion Week this season.
## 995                                                         Editors have decided to adjust how The Times refer to an Islamic extremist group that controls territory in Syria and Iraq.
## 2124                                                                                                                 Scenes from the Paris Fashion Week photo diary of Nina Westervelt.
## 3326                                                                           The coffee purveyor has teamed up with its fellow Bay Area-based company Timbuk2 to create a travel kit.
## 4752 Monster, which revolutionized online job hunting in the 1990s, is trying to reinvent itself for the era of Twitter and Facebook with new products that capitalize on social media.
## 6462                                                                                From the International Herald Tribune archives: Priest Questions the Meridian of Greenwich in 1889.
##                                                                                                                                                                                Abstract
## 226                                                     Tavi Gevinson, the teenage fashion star turned Broadway actress, wont be much of a player at New York Fashion Week this season.
## 995                                                         Editors have decided to adjust how The Times refer to an Islamic extremist group that controls territory in Syria and Iraq.
## 2124                                                                                                                 Scenes from the Paris Fashion Week photo diary of Nina Westervelt.
## 3326                                                                           The coffee purveyor has teamed up with its fellow Bay Area-based company Timbuk2 to create a travel kit.
## 4752 Monster, which revolutionized online job hunting in the 1990s, is trying to reinvent itself for the era of Twitter and Facebook with new products that capitalize on social media.
## 6462                                                                                From the International Herald Tribune archives: Priest Questions the Meridian of Greenwich in 1889.
##      WordCount             PubDate Popular UniqueID
## 226        459 2014-09-04 16:55:57       0      226
## 995        301 2014-09-15 16:05:13       0      995
## 2124        59 2014-09-28 11:20:02       0     2124
## 3326       248 2014-10-14 14:45:55       0     3326
## 4752       995 2014-11-02 07:00:31       0     4752
## 6462       110 2014-11-27 12:00:34       0     6462
##      NewsDesk SectionName  SubsectionName
## 6527  Foreign                            
## 6528              Opinion Room For Debate
## 6529  Foreign                            
## 6530   TStyle                            
## 6531           Multimedia                
## 6532 Business                            
##                                                                        Headline
## 6527                                     1914: Russians Dominate in East Poland
## 6528                                             Finding a Secretary of Defense
## 6529                         1889: Metropolitan Opera House Reopens in New York
## 6530                         The Daily Gift: Picasso Plates for Creative Dining
## 6531                                          Racing From New York to Barcelona
## 6532 Math Anxiety: Why Hollywood Makes Robots of Alan Turing and Other Geniuses
##                                                                                                                                                                                             Snippet
## 6527                                                                                                      From the International Herald Tribune archives: Russians dominate in East Poland in 1914.
## 6528                                                                                             If Chuck Hagel isn't the right Pentagon chief to respond to an onslaught of global crises, who is?
## 6529                                                                                      From the International Herald Tribune archives: The Metropolitan Opera House reopens in New York in 1889.
## 6530                                                                                                                      Each day until Christmas, the editors of T share a new holiday gift idea.
## 6531                                                      A sailboat race from New York to Barcelona was the setting for a thrilling  and sometimes terrifying  video about this challenging sport.
## 6532 The visionary who stares at formulas written on walls or mirrors  or better yet, thin air  has become a Hollywood trope. So has the depiction of the genius who cant connect with real people.
##                                                                                                                                                                                            Abstract
## 6527                                                                                                      From the International Herald Tribune archives: Russians dominate in East Poland in 1914.
## 6528                                                                                             If Chuck Hagel isn't the right Pentagon chief to respond to an onslaught of global crises, who is?
## 6529                                                                                      From the International Herald Tribune archives: The Metropolitan Opera House reopens in New York in 1889.
## 6530                                                                                                                      Each day until Christmas, the editors of T share a new holiday gift idea.
## 6531                                                      A sailboat race from New York to Barcelona was the setting for a thrilling  and sometimes terrifying  video about this challenging sport.
## 6532 The visionary who stares at formulas written on walls or mirrors  or better yet, thin air  has become a Hollywood trope. So has the depiction of the genius who cant connect with real people.
##      WordCount             PubDate Popular UniqueID
## 6527       176 2014-11-30 13:48:40       0     6527
## 6528      1597 2014-11-30 13:27:23       0     6528
## 6529       214 2014-11-30 09:44:57       0     6529
## 6530        61 2014-11-30 09:00:43       0     6530
## 6531       441 2014-11-30 09:00:22       0     6531
## 6532       921 2014-11-30 07:00:40       0     6532
## 'data.frame':	6532 obs. of  10 variables:
##  $ NewsDesk      : chr  "Business" "Culture" "Business" "Business" ...
##  $ SectionName   : chr  "Crosswords/Games" "Arts" "Business Day" "Business Day" ...
##  $ SubsectionName: chr  "" "" "Dealbook" "Dealbook" ...
##  $ Headline      : chr  "More School Daze" "New 96-Page Murakami Work Coming in December" "Public Pension Funds Stay Mum on Corporate Expats" "Boot Camp for Bankers" ...
##  $ Snippet       : chr  "A puzzle from Ethan Cooper that reminds me that a bill is due." "The Strange Library will arrive just three and a half months after Mr. Murakamis latest novel, Colorless Tsukuru Tazaki and His"| __truncated__ "Public pension funds have major stakes in American companies moving overseas to cut their tax bills. But they are saying little"| __truncated__ "As they struggle to find new business to bolster sluggish earnings, banks consider the nations 25 million veterans and service "| __truncated__ ...
##  $ Abstract      : chr  "A puzzle from Ethan Cooper that reminds me that a bill is due." "The Strange Library will arrive just three and a half months after Mr. Murakamis latest novel, Colorless Tsukuru Tazaki and His"| __truncated__ "Public pension funds have major stakes in American companies moving overseas to cut their tax bills. But they are saying little"| __truncated__ "As they struggle to find new business to bolster sluggish earnings, banks consider the nations 25 million veterans and service "| __truncated__ ...
##  $ WordCount     : int  508 285 1211 1405 181 245 258 893 1077 188 ...
##  $ PubDate       : chr  "2014-09-01 22:00:09" "2014-09-01 21:14:07" "2014-09-01 21:05:36" "2014-09-01 20:43:34" ...
##  $ Popular       : int  1 0 0 1 1 1 0 1 1 0 ...
##  $ UniqueID      : int  1 2 3 4 5 6 7 8 9 10 ...
##  - attr(*, "comment")= chr "glb_trnent_df"
## NULL
```

```r
if (glb_is_separate_newent_dataset) {
    glb_newent_df <- myimport_data(url=glb_newdt_url, comment="glb_newent_df", 
                                   force_header=TRUE)
    
    # To make plots / stats / checks easier in chunk:inspectORexplore.data
    glb_entity_df <- myrbind_df(glb_trnent_df, glb_newent_df); 
    comment(glb_entity_df) <- "glb_entity_df"
} else {
    glb_entity_df <- glb_trnent_df; comment(glb_entity_df) <- "glb_entity_df"
    if (!glb_split_entity_newent_datasets) {
        stop("Not implemented yet") 
        glb_newent_df <- glb_trnent_df[sample(1:nrow(glb_trnent_df),
                                          max(2, nrow(glb_trnent_df) / 1000)),]                    
    } else      if (glb_split_newdata_method == "condition") {
            glb_newent_df <- do.call("subset", 
                list(glb_trnent_df, parse(text=glb_split_newdata_condition)))
            glb_trnent_df <- do.call("subset", 
                list(glb_trnent_df, parse(text=paste0("!(", 
                                                      glb_split_newdata_condition,
                                                      ")"))))
        } else if (glb_split_newdata_method == "sample") {
                require(caTools)
                
                set.seed(glb_split_sample.seed)
                split <- sample.split(glb_trnent_df[, glb_rsp_var_raw], 
                                      SplitRatio=(1-glb_split_newdata_size_ratio))
                glb_newent_df <- glb_trnent_df[!split, ] 
                glb_trnent_df <- glb_trnent_df[split ,]
        } else if (glb_split_newdata_method == "copy") {  
            glb_trnent_df <- glb_entity_df
            comment(glb_trnent_df) <- "glb_trnent_df"
            glb_newent_df <- glb_entity_df
            comment(glb_newent_df) <- "glb_newent_df"
        } else stop("glb_split_newdata_method should be %in% c('condition', 'sample', 'copy')")   

    comment(glb_newent_df) <- "glb_newent_df"
    myprint_df(glb_newent_df)
    str(glb_newent_df)

    if (glb_split_entity_newent_datasets) {
        myprint_df(glb_trnent_df)
        str(glb_trnent_df)        
    }
}         
```

```
## [1] "Reading file ./data/NYTimesBlogTest.csv..."
## [1] "dimensions of data in ./data/NYTimesBlogTest.csv: 1,870 rows x 9 cols"
##   NewsDesk      SectionName SubsectionName
## 1  Culture                                
## 2  Culture             Arts               
## 3 Business Crosswords/Games               
## 4 Business     Business Day       Dealbook
## 5  Science           Health               
## 6  Science           Health               
##                                                             Headline
## 1                                         'Birdman' Tops the Gothams
## 2                     'Sleepy Hollow' Recap: A Not-So-Shocking Death
## 3                                        Drinking Buddy For Falstaff
## 4 Encouraging Public Service, Through Wall Street's 'Revolving Door'
## 5                           Therapy Prevents Repeat Suicide Attempts
## 6                                            Hoping for a Good Death
##                                                                                                                                                 Snippet
## 1                                                    The backstage tale won two awards; Citizenfour, the Edward Snowden documentary, was also a winner.
## 2                                                                      In the fall season finale, a question of where the series has many places to go.
## 3                                                                                                       In which Timothy Polin reveals his potty mouth.
## 4 The debate about pay for Wall Street executives who take government jobs appears to be based more on a populist shakedown than on good public policy.
## 5                                                                Short-term psychotherapy may be an effective way to prevent repeated suicide attempts.
## 6                          What I hadnt considered before my fathers heart attack was the precise meaning of not wanting to live hooked up to machines.
##                                                                                                                                                Abstract
## 1                                                    The backstage tale won two awards; Citizenfour, the Edward Snowden documentary, was also a winner.
## 2                                                                      In the fall season finale, a question of where the series has many places to go.
## 3                                                                                                       In which Timothy Polin reveals his potty mouth.
## 4 The debate about pay for Wall Street executives who take government jobs appears to be based more on a populist shakedown than on good public policy.
## 5                                                                Short-term psychotherapy may be an effective way to prevent repeated suicide attempts.
## 6                          What I hadnt considered before my fathers heart attack was the precise meaning of not wanting to live hooked up to machines.
##   WordCount             PubDate UniqueID
## 1       111 2014-12-01 22:45:24     6533
## 2       558 2014-12-01 22:01:34     6534
## 3       788 2014-12-01 22:00:26     6535
## 4       915 2014-12-01 21:04:13     6536
## 5       213 2014-12-01 19:13:20     6537
## 6       938 2014-12-01 19:05:12     6538
##      NewsDesk      SectionName SubsectionName
## 3    Business Crosswords/Games               
## 725    TStyle                                
## 731  Business     Business Day       Dealbook
## 751    TStyle                                
## 864                                          
## 1376 Business     Business Day Small Business
##                                                                           Headline
## 3                                                      Drinking Buddy For Falstaff
## 725                                              Ansel Elgort Buttons Up in Brioni
## 731                    Didi Dache, a Chinese Ride-Hailing App, Raises $700 Million
## 751        The Daily Gift: A Soft, Colorful Quilt From a Brooklyn Fashion Favorite
## 864                                                              Today in Politics
## 1376 As Health Insurance Evolves, Traditional Brokers Claim They Still Have a Role
##                                                                                                                                                                                            Snippet
## 3                                                                                                                                                  In which Timothy Polin reveals his potty mouth.
## 725                                                                                                   The actor brought a tinge of youthfulness to the classic Italian houses retro-tailored look.
## 731  The Singapore investor Temasek and the Chinese social network operator Tencent are among the leaders of the fund-raising round for a company that says it has 10 times the ridership of Uber.
## 751                                                                                                                      Each day until Christmas, the editors of T share a new holiday gift idea.
## 864                                                     The 113th Congress is concluding with partisan brinksmanship and one last mad scramble for votes to pass a $1.1 trillion spending package.
## 1376                       Its complex picking insurance for yourself and your family, said a health care policy director for a small-business organization. Its even more complex for a business.
##                                                                                                                                                                                           Abstract
## 3                                                                                                                                                  In which Timothy Polin reveals his potty mouth.
## 725                                                                                                   The actor brought a tinge of youthfulness to the classic Italian houses retro-tailored look.
## 731  The Singapore investor Temasek and the Chinese social network operator Tencent are among the leaders of the fund-raising round for a company that says it has 10 times the ridership of Uber.
## 751                                                                                                                      Each day until Christmas, the editors of T share a new holiday gift idea.
## 864                                                     The 113th Congress is concluding with partisan brinksmanship and one last mad scramble for votes to pass a $1.1 trillion spending package.
## 1376                       Its complex picking insurance for yourself and your family, said a health care policy director for a small-business organization. Its even more complex for a business.
##      WordCount             PubDate UniqueID
## 3          788 2014-12-01 22:00:26     6535
## 725         89 2014-12-10 12:30:47     7257
## 731        724 2014-12-10 12:06:32     7263
## 751         85 2014-12-10 09:00:38     7283
## 864       1544 2014-12-11 07:09:25     7396
## 1376      1250 2014-12-18 07:00:05     7908
##      NewsDesk   SectionName SubsectionName
## 1865                                      
## 1866 Business    Technology               
## 1867    Metro N.Y. / Region               
## 1868             Multimedia               
## 1869  Foreign         World   Asia Pacific
## 1870  Science        Health               
##                                                       Headline
## 1865                                         Today in Politics
## 1866                         Uber Suspends Operations in Spain
## 1867                         New York Today: The Year in News 
## 1868                   New Year, Old Memories, in Times Square
## 1869 Hong Kong Police Criticized After 14-Year-Old's Detention
## 1870          The Super-Short Workout and Other Fitness Trends
##                                                                                                                                                                                                                                                   Snippet
## 1865                                                                                                               House Republicans are ending the year on a defensive note over Representative Steve Scalises 2002 speech to a white supremacist group.
## 1866                                                                              In a first in the growing pushback against Ubers global expansion, a judges ruling barred telecommunications operators and banks from supporting the companys services.
## 1867                                                                                                                                                              Wednesday: The most read stories of 2014, teeth-chattering cold, and its New Years Eve.
## 1868                                                                         What happens when you combine Burning Man, Independence Day fireworks, the last day of school and a full-contact Black Friday sale-a-bration? New Years Eve in Times Square.
## 1869 The authorities have been accused of trying to intimidate young pro-democracy protesters and their families after a 14-year-old girl was detained on suspicion of drawing flowers in chalk near government headquarters and sent to a juvenile home.
## 1870                                                                                                                 The big story in exercise science this year was the super-short workout, although many other fitness-related themes emerged in 2014.
##                                                                                                                                                                                                                                                  Abstract
## 1865                                                                                                               House Republicans are ending the year on a defensive note over Representative Steve Scalises 2002 speech to a white supremacist group.
## 1866                                                                              In a first in the growing pushback against Ubers global expansion, a judges ruling barred telecommunications operators and banks from supporting the companys services.
## 1867                                                                                                                                                              Wednesday: The most read stories of 2014, teeth-chattering cold, and its New Years Eve.
## 1868                                                                         What happens when you combine Burning Man, Independence Day fireworks, the last day of school and a full-contact Black Friday sale-a-bration? New Years Eve in Times Square.
## 1869 The authorities have been accused of trying to intimidate young pro-democracy protesters and their families after a 14-year-old girl was detained on suspicion of drawing flowers in chalk near government headquarters and sent to a juvenile home.
## 1870                                                                                                                 The big story in exercise science this year was the super-short workout, although many other fitness-related themes emerged in 2014.
##      WordCount             PubDate UniqueID
## 1865      1616 2014-12-31 07:03:46     8397
## 1866       292 2014-12-31 06:09:32     8398
## 1867      1010 2014-12-31 06:06:58     8399
## 1868       387 2014-12-31 05:00:19     8400
## 1869       717 2014-12-31 04:16:29     8401
## 1870       818 2014-12-31 00:01:10     8402
## 'data.frame':	1870 obs. of  9 variables:
##  $ NewsDesk      : chr  "Culture" "Culture" "Business" "Business" ...
##  $ SectionName   : chr  "" "Arts" "Crosswords/Games" "Business Day" ...
##  $ SubsectionName: chr  "" "" "" "Dealbook" ...
##  $ Headline      : chr  "'Birdman' Tops the Gothams" "'Sleepy Hollow' Recap: A Not-So-Shocking Death" "Drinking Buddy For Falstaff" "Encouraging Public Service, Through Wall Street's 'Revolving Door'" ...
##  $ Snippet       : chr  "The backstage tale won two awards; Citizenfour, the Edward Snowden documentary, was also a winner." "In the fall season finale, a question of where the series has many places to go." "In which Timothy Polin reveals his potty mouth." "The debate about pay for Wall Street executives who take government jobs appears to be based more on a populist shakedown than "| __truncated__ ...
##  $ Abstract      : chr  "The backstage tale won two awards; Citizenfour, the Edward Snowden documentary, was also a winner." "In the fall season finale, a question of where the series has many places to go." "In which Timothy Polin reveals his potty mouth." "The debate about pay for Wall Street executives who take government jobs appears to be based more on a populist shakedown than "| __truncated__ ...
##  $ WordCount     : int  111 558 788 915 213 938 1336 2644 752 99 ...
##  $ PubDate       : chr  "2014-12-01 22:45:24" "2014-12-01 22:01:34" "2014-12-01 22:00:26" "2014-12-01 21:04:13" ...
##  $ UniqueID      : int  6533 6534 6535 6536 6537 6538 6539 6540 6541 6542 ...
##  - attr(*, "comment")= chr "glb_newent_df"
## NULL
```

```r
if (nrow(glb_trnent_df) == nrow(glb_entity_df))
    warning("glb_trnent_df same as glb_entity_df")
if (nrow(glb_newent_df) == nrow(glb_entity_df))
    warning("glb_newent_df same as glb_entity_df")

if (length(glb_drop_vars) > 0) {
    warning("dropping vars: ", paste0(glb_drop_vars, collapse=", "))
    glb_entity_df <- glb_entity_df[, setdiff(names(glb_entity_df), glb_drop_vars)]
    glb_trnent_df <- glb_trnent_df[, setdiff(names(glb_trnent_df), glb_drop_vars)]    
    glb_newent_df <- glb_newent_df[, setdiff(names(glb_newent_df), glb_drop_vars)]    
}

# Check for duplicates in glb_id_vars
if (length(glb_id_vars) == 0) {
    warning("using .rownames as identifiers for observations")
    glb_entity_df$.rownames <- rownames(glb_entity_df)
    glb_id_vars <- ".rownames"
}
if (sum(duplicated(glb_entity_df[, glb_id_vars, FALSE])) > 0)
    stop(glb_id_vars, " duplicated in glb_entity_df")
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_id_vars)

# Combine trnent & newent into glb_entity_df for easier manipulation
glb_trnent_df$.src <- "Train"; glb_newent_df$.src <- "Test"; 
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, ".src")
glb_entity_df <- myrbind_df(glb_trnent_df, glb_newent_df)
comment(glb_entity_df) <- "glb_entity_df"    

glb_script_df <- rbind(glb_script_df,
                   data.frame(chunk_label="inspectORexplore.data", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                    chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed            import_data                1                0   0.002
## elapsed1 inspectORexplore.data                2                0   1.209
```

### Step `2`.`0`: inspect/explore data

```r
#print(str(glb_trnent_df))
#View(glb_trnent_df)

dsp_class_dstrb <- function(var) {
    xtab_df <- mycreate_xtab_df(glb_entity_df, c(".src", var))
    rownames(xtab_df) <- xtab_df$.src
    xtab_df <- subset(xtab_df, select=-.src)
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

dsp_problem_data <- function(df) {
    print(sprintf("numeric data missing in %s: ", 
                  ifelse(!is.null(df_name <- comment(df)), df_name, "")))
    print(sapply(setdiff(names(df), myfind_chr_cols_df(df)), 
                 function(col) sum(is.na(df[, col]))))
    
    print(sprintf("numeric data w/ 0s in %s: ", 
                  ifelse(!is.null(df_name <- comment(df)), df_name, "")))
    print(sapply(setdiff(names(df), myfind_chr_cols_df(df)), 
                 function(col) sum(df[, col] == 0, na.rm=TRUE)))
    
    print(sprintf("numeric data w/ Infs in %s: ", 
                  ifelse(!is.null(df_name <- comment(df)), df_name, "")))
    print(sapply(setdiff(names(df), myfind_chr_cols_df(df)), 
                 function(col) sum(df[, col] == Inf, na.rm=TRUE)))
    
    print(sprintf("numeric data w/ NaNs in %s: ", 
                  ifelse(!is.null(df_name <- comment(df)), df_name, "")))
    print(sapply(setdiff(names(df), myfind_chr_cols_df(df)), 
                 function(col) sum(df[, col] == NaN, na.rm=TRUE)))
    
    print(sprintf("string data missing in %s: ", 
                  ifelse(!is.null(df_name <- comment(df)), df_name, "")))
    print(sapply(setdiff(myfind_chr_cols_df(df), ".src"), 
                        function(col) sum(df[, col] == "")))
}

# Performed repeatedly in other chunks
glb_chk_data <- function() {
    # Histogram of predictor in glb_trnent_df & glb_newent_df
    print(myplot_histogram(glb_entity_df, glb_rsp_var_raw) + facet_wrap(~ .src))
    
    if (glb_is_classification) 
        dsp_class_dstrb(var=ifelse(glb_rsp_var %in% names(glb_entity_df), 
                                   glb_rsp_var, glb_rsp_var_raw))
    dsp_problem_data(glb_entity_df)
}
glb_chk_data()
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## Loading required package: reshape2
```

![](NYTBlogs_hdlpfx7_files/figure-html/inspectORexplore.data-1.png) 

```
##       Popular.0 Popular.1 Popular.NA
## Test         NA        NA       1870
## Train      5439      1093         NA
##       Popular.0 Popular.1 Popular.NA
## Test         NA        NA          1
## Train 0.8326699 0.1673301         NA
## [1] "numeric data missing in glb_entity_df: "
## WordCount   Popular  UniqueID 
##         0      1870         0 
## [1] "numeric data w/ 0s in glb_entity_df: "
## WordCount   Popular  UniqueID 
##       109      5439         0 
## [1] "numeric data w/ Infs in glb_entity_df: "
## WordCount   Popular  UniqueID 
##         0         0         0 
## [1] "numeric data w/ NaNs in glb_entity_df: "
## WordCount   Popular  UniqueID 
##         0         0         0 
## [1] "string data missing in glb_entity_df: "
##       NewsDesk    SectionName SubsectionName       Headline        Snippet 
##           2408           2899           6176              0             13 
##       Abstract        PubDate 
##             17              0
```

```r
# Create new features that help diagnostics
if (!is.null(glb_map_rsp_raw_to_var)) {
    glb_entity_df[, glb_rsp_var] <- 
        glb_map_rsp_raw_to_var(glb_entity_df[, glb_rsp_var_raw])
    mycheck_map_results(mapd_df=glb_entity_df, 
                        from_col_name=glb_rsp_var_raw, to_col_name=glb_rsp_var)
        
    if (glb_is_classification) dsp_class_dstrb(glb_rsp_var)
}
```

```
## Loading required package: sqldf
## Loading required package: gsubfn
## Loading required package: proto
## Loading required package: RSQLite
## Loading required package: DBI
## Loading required package: tcltk
```

```
##   Popular Popular.fctr   .n
## 1       0            N 5439
## 2      NA         <NA> 1870
## 3       1            Y 1093
```

```
## Warning: Removed 1 rows containing missing values (position_stack).
```

![](NYTBlogs_hdlpfx7_files/figure-html/inspectORexplore.data-2.png) 

```
##       Popular.fctr.N Popular.fctr.Y Popular.fctr.NA
## Test              NA             NA            1870
## Train           5439           1093              NA
##       Popular.fctr.N Popular.fctr.Y Popular.fctr.NA
## Test              NA             NA               1
## Train      0.8326699      0.1673301              NA
```

```r
#   Convert dates to numbers 
#       typically, dates come in as chars; 
#           so this must be done before converting chars to factors
myextract_dates_df <- function(df, vars) {
    for (var in vars) {
        dates_df <- data.frame(.date=strptime(df[, var], "%Y-%m-%d %H:%M:%S"))
        dates_df[, paste0(var, ".year")] <- as.numeric(format(dates_df$.date, "%Y"))
        dates_df[, paste0(var, ".month.fctr")] <- as.factor(format(dates_df$.date, "%m")) 
        dates_df[, paste0(var, ".date.fctr")] <- cut(as.numeric(format(dates_df$.date, "%d")), 5) # by week    
        dates_df[, paste0(var, ".wkday.fctr")] <- as.factor(format(dates_df$.date, "%w")) 
        dates_df[, paste0(var, ".hour")] <- as.numeric(format(dates_df$.date, "%H"))
        dates_df[, paste0(var, ".apm.fctr")] <- as.factor(ifelse(dates_df[, paste0(var, ".hour")] < 12, "am", "pm"))                                        
        dates_df[, paste0(var, ".minute")] <- as.numeric(format(dates_df$.date, "%M"))                                                                    
        dates_df[, paste0(var, ".second")] <- as.numeric(format(dates_df$.date, "%S")) 
    }
    #myprint_df(dates_df)
    return(subset(dates_df, select=-.date))
}

if (!is.null(glb_date_vars)) {
    glb_entity_df <- cbind(glb_entity_df, 
                           myextract_dates_df(glb_entity_df, glb_date_vars))
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_date_vars)
}

# check distribution of all numeric data
dsp_numeric_vars_dstrb <- function(vars_lst) {
    for (var in vars_lst) {
        gp <- myplot_box(df=glb_entity_df, ycol_names=var, xcol_name=glb_rsp_var)
        if (inherits(glb_entity_df[, var], "factor"))
            gp <- gp + facet_wrap(reformulate(var))
        print(gp)
    }    
}
dsp_numeric_vars_dstrb(setdiff(names(glb_entity_df), 
                                union(myfind_chr_cols_df(glb_entity_df), 
                                      c(glb_rsp_var_raw, glb_rsp_var))))                                      
```

![](NYTBlogs_hdlpfx7_files/figure-html/inspectORexplore.data-3.png) ![](NYTBlogs_hdlpfx7_files/figure-html/inspectORexplore.data-4.png) ![](NYTBlogs_hdlpfx7_files/figure-html/inspectORexplore.data-5.png) ![](NYTBlogs_hdlpfx7_files/figure-html/inspectORexplore.data-6.png) ![](NYTBlogs_hdlpfx7_files/figure-html/inspectORexplore.data-7.png) ![](NYTBlogs_hdlpfx7_files/figure-html/inspectORexplore.data-8.png) ![](NYTBlogs_hdlpfx7_files/figure-html/inspectORexplore.data-9.png) ![](NYTBlogs_hdlpfx7_files/figure-html/inspectORexplore.data-10.png) ![](NYTBlogs_hdlpfx7_files/figure-html/inspectORexplore.data-11.png) ![](NYTBlogs_hdlpfx7_files/figure-html/inspectORexplore.data-12.png) 

```r
add_new_diag_feats <- function(obs_df, ref_df=glb_entity_df) {
    require(plyr)
    
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>),

#         <col_name>.fctr=factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))), 
#         <col_name>.fctr=relevel(factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))),
#                                   "<ref_val>"), 
#         <col2_name>.fctr=relevel(factor(ifelse(<col1_name> == <val>, "<oth_val>", "<ref_val>")), 
#                               as.factor(c("R", "<ref_val>")),
#                               ref="<ref_val>"),

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)

#         <col_name>.log=log(1 + <col.name>),        
          WordCount.log = log(1 + WordCount),        
#         <col_name>=<table>[as.character(<col2_name>)],
#         <col_name>=as.numeric(<col2_name>),

        .rnorm=rnorm(n=nrow(obs_df))
                        )

    # If levels of a factor are different across obs_df & glb_newent_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    #print(summary(obs_df))
    #print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}
# Add WordCount.log since WordCount is not distributed normally
glb_entity_df <- add_new_diag_feats(glb_entity_df)
```

```
## Loading required package: plyr
```

```r
print("Replacing WordCount with WordCount.log in potential feature set")
```

```
## [1] "Replacing WordCount with WordCount.log in potential feature set"
```

```r
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, "WordCount")

# Remove PubDate.year since all entity data is from 2014
# Remove PubDate.month.fctr since all newent data is from December
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c("PubDate.year", "PubDate.month.fctr"))

# Check distributions of newly transformed / extracted vars
#   Enhancement: remove vars that were displayed ealier
dsp_numeric_vars_dstrb(setdiff(names(glb_entity_df), 
    union(myfind_chr_cols_df(glb_entity_df), 
        union(glb_rsp_var_raw, 
            union(glb_rsp_var, glb_exclude_vars_as_features)))))
```

![](NYTBlogs_hdlpfx7_files/figure-html/inspectORexplore.data-13.png) ![](NYTBlogs_hdlpfx7_files/figure-html/inspectORexplore.data-14.png) ![](NYTBlogs_hdlpfx7_files/figure-html/inspectORexplore.data-15.png) ![](NYTBlogs_hdlpfx7_files/figure-html/inspectORexplore.data-16.png) ![](NYTBlogs_hdlpfx7_files/figure-html/inspectORexplore.data-17.png) ![](NYTBlogs_hdlpfx7_files/figure-html/inspectORexplore.data-18.png) ![](NYTBlogs_hdlpfx7_files/figure-html/inspectORexplore.data-19.png) ![](NYTBlogs_hdlpfx7_files/figure-html/inspectORexplore.data-20.png) 

```r
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

#pairs(subset(glb_trnent_df, select=-c(col_symbol)))
# Check for glb_newent_df & glb_trnent_df features range mismatches

# Other diagnostics:
# print(subset(glb_trnent_df, <col1_name> == max(glb_trnent_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_trnent_df$<col1_name>, na.rm=TRUE)))

# print(glb_trnent_df[which.max(glb_trnent_df$<col_name>),])

# print(<col_name>_freq_glb_trnent_df <- mycreate_tbl_df(glb_trnent_df, "<col_name>"))
# print(which.min(table(glb_trnent_df$<col_name>)))
# print(which.max(table(glb_trnent_df$<col_name>)))
# print(which.max(table(glb_trnent_df$<col1_name>, glb_trnent_df$<col2_name>)[, 2]))
# print(table(glb_trnent_df$<col1_name>, glb_trnent_df$<col2_name>))
# print(table(is.na(glb_trnent_df$<col1_name>), glb_trnent_df$<col2_name>))
# print(table(sign(glb_trnent_df$<col1_name>), glb_trnent_df$<col2_name>))
# print(mycreate_xtab_df(glb_trnent_df, <col1_name>))
# print(mycreate_xtab_df(glb_trnent_df, c(<col1_name>, <col2_name>)))
# print(<col1_name>_<col2_name>_xtab_glb_trnent_df <- 
#   mycreate_xtab_df(glb_trnent_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_trnent_df[is.na(<col1_name>_<col2_name>_xtab_glb_trnent_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_trnent_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_trnent_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_trnent_df$<col1_name>, glb_trnent_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_trnent_df$<col1_name>.NA, glb_trnent_df$<col2_name>, mean, na.rm=TRUE)))

# Other plots:
# print(myplot_box(df=glb_trnent_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_trnent_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_trnent_df, Symbol %in% c("KO", "PG")), 
#                   "Date.my", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.Date("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.Date("1983-01-01")))        
#         )
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", smooth=TRUE))
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", colorcol_name="<Pred.fctr>") + 
#         geom_point(data=subset(glb_entity_df, <condition>), 
#                     mapping=aes(x=<x_var>, y=<y_var>), color="red", shape=4, size=5))

glb_script_df <- rbind(glb_script_df, 
    data.frame(chunk_label="cleanse.data", 
        chunk_step_major=max(glb_script_df$chunk_step_major), 
        chunk_step_minor=glb_script_df[nrow(glb_script_df), "chunk_step_minor"]+1,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                    chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed1 inspectORexplore.data                2                0   1.209
## elapsed2          cleanse.data                2                1  25.148
```

## Step `2`: cleanse data

```r
dsp_problem_data(glb_entity_df)
```

```
## [1] "numeric data missing in : "
##          WordCount            Popular           UniqueID 
##                  0               1870                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##               1870                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "numeric data w/ 0s in : "
##          WordCount            Popular           UniqueID 
##                109               5439                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                378                159 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0               1344                141 
##      WordCount.log             .rnorm 
##                109                  0 
## [1] "numeric data w/ Infs in : "
##          WordCount            Popular           UniqueID 
##                  0                  0                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "numeric data w/ NaNs in : "
##          WordCount            Popular           UniqueID 
##                  0                  0                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "string data missing in : "
##       NewsDesk    SectionName SubsectionName       Headline        Snippet 
##           2408           2899           6176              0             13 
##       Abstract        PubDate 
##             17              0
```

```r
warning("Forcing ", nrow(subset(glb_entity_df, WordCount.log == 0)),
        " obs with WordCount.log 0s to NA")
```

```
## Warning: Forcing 109 obs with WordCount.log 0s to NA
```

```r
glb_entity_df[glb_entity_df$WordCount.log == 0, "WordCount.log"] <- NA

dsp_problem_data(glb_entity_df)
```

```
## [1] "numeric data missing in : "
##          WordCount            Popular           UniqueID 
##                  0               1870                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##               1870                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                109                  0 
## [1] "numeric data w/ 0s in : "
##          WordCount            Popular           UniqueID 
##                109               5439                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                378                159 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0               1344                141 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "numeric data w/ Infs in : "
##          WordCount            Popular           UniqueID 
##                  0                  0                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "numeric data w/ NaNs in : "
##          WordCount            Popular           UniqueID 
##                  0                  0                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "string data missing in : "
##       NewsDesk    SectionName SubsectionName       Headline        Snippet 
##           2408           2899           6176              0             13 
##       Abstract        PubDate 
##             17              0
```

```r
dsp_catgs <- function() {
    print("NewsDesk:")
    print(table(glb_entity_df$NewsDesk))
    print("SectionName:")    
    print(table(glb_entity_df$SectionName))
    print("SubsectionName:")        
    print(table(glb_entity_df$SubsectionName))
}

sel_obs <- function(Popular=NULL, 
                    NewsDesk=NULL, SectionName=NULL, SubsectionName=NULL,
        Headline.contains=NULL, Snippet.contains=NULL, Abstract.contains=NULL,
        Headline.pfx=NULL) {
    tmp_entity_df <- glb_entity_df
    # Does not work for Popular == NAs ???
    if (!is.null(Popular)) {
        if (is.na(Popular))
            tmp_entity_df <- tmp_entity_df[is.na(tmp_entity_df$Popular), ] else   
            tmp_entity_df <- tmp_entity_df[tmp_entity_df$Popular == Popular, ]    
    }    
    if (!is.null(NewsDesk)) 
        tmp_entity_df <- tmp_entity_df[tmp_entity_df$NewsDesk == NewsDesk, ]
    if (!is.null(SectionName)) 
        tmp_entity_df <- tmp_entity_df[tmp_entity_df$SectionName == SectionName, ]
    if (!is.null(SubsectionName)) 
        tmp_entity_df <- tmp_entity_df[tmp_entity_df$SubsectionName == SubsectionName, ]
    if (!is.null(Headline.contains))
        tmp_entity_df <- 
            tmp_entity_df[grep(Headline.contains, tmp_entity_df$Headline), ]
    if (!is.null(Snippet.contains))
        tmp_entity_df <- 
            tmp_entity_df[grep(Snippet.contains, tmp_entity_df$Snippet), ]
    if (!is.null(Abstract.contains))
        tmp_entity_df <- 
            tmp_entity_df[grep(Abstract.contains, tmp_entity_df$Abstract), ]
    if (!is.null(Headline.pfx)) {
        if (length(grep("Headline.pfx", names(tmp_entity_df), fixed=TRUE, value=TRUE))
            > 0) tmp_entity_df <- 
                tmp_entity_df[tmp_entity_df$Headline.pfx == Headline.pfx, ] else
        warning("glb_entity_df does not contain Headline.pfx; ignoring that filter")                    
    }    
    
    return(glb_entity_df$UniqueID %in% tmp_entity_df$UniqueID)
}

dsp_obs <- function(..., all=FALSE) {
    tmp_df <- glb_entity_df[sel_obs(...), ]
    if(all) { print(tmp_df) } else { myprint_df(tmp_df) }
}
#dsp_obs(Popular=1, NewsDesk="", SectionName="", Headline.contains="Boehner")
# dsp_obs(Popular=1, NewsDesk="", SectionName="")
# dsp_obs(Popular=NA, NewsDesk="", SectionName="")

dsp_tbl <- function(...) {
    tmp_entity_df <- glb_entity_df[sel_obs(...), ]
    tmp_tbl <- table(tmp_entity_df$NewsDesk, 
                     tmp_entity_df$SectionName,
                     tmp_entity_df$SubsectionName, 
                     tmp_entity_df$Popular, useNA="ifany")
    #print(names(tmp_tbl))
    #print(dimnames(tmp_tbl))
    print(tmp_tbl)
}
#dsp_tbl(NewsDesk="", SectionName="", Headline.contains="Boehner")
dsp_hdlxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_entity_df[sel_obs(Headline.contains=str), ],
                           c("Headline.pfx", "Headline", glb_rsp_var)))
dsp_hdlxtab("(1914)|(1939)")
```

```
##     "Headline.pfx"
## 1     Headline.pfx
## 2     Headline.pfx
## 3     Headline.pfx
## 4     Headline.pfx
## 5     Headline.pfx
## 6     Headline.pfx
## 7     Headline.pfx
## 8     Headline.pfx
## 9     Headline.pfx
## 10    Headline.pfx
## 11    Headline.pfx
## 12    Headline.pfx
## 13    Headline.pfx
## 14    Headline.pfx
## 15    Headline.pfx
## 16    Headline.pfx
## 17    Headline.pfx
## 18    Headline.pfx
## 19    Headline.pfx
## 20    Headline.pfx
## 21    Headline.pfx
## 22    Headline.pfx
## 23    Headline.pfx
## 24    Headline.pfx
## 25    Headline.pfx
## 26    Headline.pfx
## 27    Headline.pfx
## 28    Headline.pfx
## 29    Headline.pfx
## 30    Headline.pfx
## 31    Headline.pfx
## 32    Headline.pfx
## 33    Headline.pfx
## 34    Headline.pfx
## 35    Headline.pfx
## 36    Headline.pfx
## 37    Headline.pfx
## 38    Headline.pfx
## 39    Headline.pfx
## 40    Headline.pfx
## 41    Headline.pfx
## 42    Headline.pfx
## 43    Headline.pfx
## 44    Headline.pfx
## 45    Headline.pfx
## 46    Headline.pfx
## 47    Headline.pfx
## 48    Headline.pfx
## 49    Headline.pfx
## 50    Headline.pfx
## 51    Headline.pfx
## 52    Headline.pfx
## 53    Headline.pfx
## 54    Headline.pfx
## 55    Headline.pfx
## 56    Headline.pfx
## 57    Headline.pfx
## 58    Headline.pfx
## 59    Headline.pfx
## 60    Headline.pfx
## 61    Headline.pfx
## 62    Headline.pfx
## 63    Headline.pfx
## 64    Headline.pfx
## 65    Headline.pfx
## 66    Headline.pfx
## 67    Headline.pfx
## 68    Headline.pfx
## 69    Headline.pfx
## 70    Headline.pfx
## 71    Headline.pfx
## 72    Headline.pfx
## 73    Headline.pfx
## 74    Headline.pfx
## 75    Headline.pfx
## 76    Headline.pfx
## 77    Headline.pfx
## 78    Headline.pfx
## 79    Headline.pfx
## 80    Headline.pfx
## 81    Headline.pfx
## 82    Headline.pfx
## 83    Headline.pfx
## 84    Headline.pfx
## 85    Headline.pfx
## 86    Headline.pfx
## 87    Headline.pfx
## 88    Headline.pfx
## 89    Headline.pfx
## 90    Headline.pfx
## 91    Headline.pfx
## 92    Headline.pfx
## 93    Headline.pfx
## 94    Headline.pfx
## 95    Headline.pfx
## 96    Headline.pfx
## 97    Headline.pfx
## 98    Headline.pfx
## 99    Headline.pfx
## 100   Headline.pfx
## 101   Headline.pfx
## 102   Headline.pfx
## 103   Headline.pfx
## 104   Headline.pfx
## 105   Headline.pfx
## 106   Headline.pfx
## 107   Headline.pfx
## 108   Headline.pfx
## 109   Headline.pfx
## 110   Headline.pfx
## 111   Headline.pfx
## 112   Headline.pfx
## 113   Headline.pfx
## 114   Headline.pfx
## 115   Headline.pfx
## 116   Headline.pfx
## 117   Headline.pfx
## 118   Headline.pfx
## 119   Headline.pfx
## 120   Headline.pfx
## 121   Headline.pfx
##                                                                           Headline
## 1                                                     1914: Turkish Desire for War
## 2                                                 1939: Letters Reaffirm City Ties
## 3                                                 1939: Nazis' War Production Hurt
## 4                                                  1914: 'City of Light' to Return
## 5                                              1914: 79 Die in Brutal German Raid 
## 6                                             1914: Allies Advance in West Africa 
## 7                                                 1914: Antwerp Is Left in Flames 
## 8                                                     1914: Armored Train Surprise
## 9                                                 1914: Belgians Flood Battlefield
## 10                             1914: Big Guns From Pola to Defend Austrian Capital
## 11                                      1914: Bomb-Dropping Warplane Attacks Paris
## 12                              1914: British Prime Minister Urges Irish to Enlist
## 13                                                 1914: British Take Orange River
## 14                                             1914: British War Funds and Troops 
## 15                                                 1914: Christmas Shows in London
## 16                                                    1914: Christmas at the Front
## 17                             1914: Churches Of Ypres Are Targets For German Guns
## 18                                             1914: City Prepares for War Wounded
## 19                          1914: Despite War, 'New York Herald' to Stay in Paris 
## 20          1914: Fearing Espionage, Officials in London Search Travelers' Luggage
## 21                                     1914: France Silences Enemy Guns Near Arras
## 22                                       1914: France&rsquo;s Champagne Prospects 
## 23                                   1914: French Press Misjudges U.S. Ambassadors
## 24                                                1914: General De Wet Is Captured
## 25                                            1914: German Flags Hung in Invalides
## 26                                       1914: German Offensive Checked in Belgium
## 27                                              1914: Germans Attack Antwerp Forts
## 28                                                     1914: Germans Bring Up Guns
## 29                                      1914: Germans Piloting Turkish Aeroplanes 
## 30                                                  1914: Germans Waste Ammunition
## 31                                                    1914: Germany Attacks Tahiti
## 32                                              1914: Hospital-Ship Lost Off Coast
## 33                                         1914: Hotel Seizes Princess&rsquo;s Art
## 34                                            1914: India's Millions Loyal to Core
## 35                                               1914: Indian Infantry Routs Turks
## 36                                              1914: Is Hairdresser a German Spy?
## 37                                            1914: Italian Mobilization Expected 
## 38                                         1914: Italy's Foreign Minister Is Dead 
## 39                                               1914: King George Addresses Army 
## 40                                             1914: King Opens 'Khaki' Parliament
## 41                        1914: M. Henri Pol Will Go On Caring for Paris Sparrows 
## 42                                             1914: Naval Squadron Shells Belgium
## 43                                                   1914: Paris Auto Trade Stalls
## 44                                                  1914: Paris Theatres to Reopen
## 45    1914: Parisians Flock to Cemeteries on All Souls' Day to Honor Dead Soldiers
## 46                              1914: Prince of Wales Passes Busy Day at the Front
## 47                                              1914: Prisoners Escape Paris Crowd
## 48                                               1914: Republicans Sweep Elections
## 49                   1914: Royal Navy Stages Joint Sea and Air Strike on Cuxhaven 
## 50                                               1914: Russian Army Scores Victory
## 51                                          1914: Russians Dominate in East Poland
## 52                                              1914: Scandinavian Alliance Formed
## 53                           1914: Seizure of Oil Steamer Interests United States 
## 54                                                    1914: Sinking of the Nrnberg
## 55                         1914: Sir Ernest Shackleton Outlines His Polar Projects
## 56                                                 1914: Tipperary Song Is a Hit  
## 57                                                  1914: Turcos Drive Germans Out
## 58                                     1914: War May Rise Price of Hats in America
## 59                                                 1914: Wounded May Go to Riviera
## 60                                                  1914: Zeppelin Danger a Bluff 
## 61                                  1939: "Stanley and Livingstone" Opens in Paris
## 62                                                1939: 'Nazi Spy' Named Best Film
## 63                                                1939: 5 Convicted in Stock Fraud
## 64                            1939: 7,000,000 More Cars Predicted on U.S. Highways
## 65                                                 1939: Advice on Heating Issued 
## 66                                                  1939: Allies Seize Contraband 
## 67                1939: American Ships Hasten to Sail Before New Bill Takes Effect
## 68                                                 1939: Australian Flyers Arrive 
## 69                                            1939: British Unmask Reich Defenses 
## 70                                                  1939: Convict Who Fled Returns
## 71                                                   1939: Crowds Return to Paris 
## 72                                        1939: Eleanor Roosevelt Ready to Testify
## 73                                               1939: Empire Air Accord Is Signed
## 74                                                1939: Fighting on Western Front 
## 75                                            1939: Film Industry Revives in Paris
## 76                                           1939: Finns Resist Soviet Aggression 
## 77          1939: France Aims for &lsquo;Total Peace&rsquo;, Says Finance Minister
## 78                                             1939: France Extends Fortifications
## 79                             1939: France Recalls War Premier Georges Clemenceau
## 80                                               1939: French Army in High Spirits
## 81                                                      1939: French Ban Communism
## 82                               1939: French Join Americans in Thanksgiving Rites
## 83                                                 1939: French Occupy German Soil
## 84                    1939: German Battleship Is Badly Damaged by British Cruisers
## 85                                              1939: German Troops Invade Poland 
## 86                                               1939: Ginsberg Name Change Denied
## 87                                            1939: Greatest Opera Fears Eviction 
## 88                                                     1939: Hitler Appeals to God
## 89                                            1939: Hospital Designated Auxiliary 
## 90                                                 1939: Hungary to Defend Europe 
## 91                                            1939: Jamming of BBC Radio Addressed
## 92                                        1939: Light at Night in Paris criticized
## 93                                              1939: Line of Demarcation Decided 
## 94                                                1939: Louvre Hides Art in Vaults
## 95                                                 1939: Mine Sinks Japanese Liner
## 96                                            1939: More Britons Called to Service
## 97                                                 1939: Nazi Raiders Stir London 
## 98                                              1939: Nazi Squadron Flees British 
## 99                                                  1939: Neither King Nor Soldier
## 100                                           1939: Poles Die Under Sovietization 
## 101                                                1939: Polish Gold Reaches Paris
## 102                                          1939: Pont St. Louis Falls Into Seine
## 103                                          1939: Princess Louise Dies in London 
## 104                                           1939: Radio Play Terrifies Hundreds 
## 105                                                    1939: Radio Station Charged
## 106                                            1939: Rain Quiets the Western Front
## 107                        1939: Reich Maps Show France Partitioned, Says Daladier
## 108 1939: Ribbentrop Will Bear Guilt for Tragedy of War, Neville Chamberlain Says 
## 109                                              1939: Roosevelt Signs Neutrality 
## 110                                             1939: Second Meatless Day Is Named
## 111              1939: Sigmund Freud, Psychoanalyst, Dies Refugee in England at 83
## 112                                                 1939: Sir Thomas Cullinan Dies
## 113                                                   1939: Slovaks Are Terrorized
## 114                                                   1939: Soviets Invade Poland 
## 115                                                 1939: Soviets Push Into China 
## 116                                           1939: Textile Rationing Cards Issued
## 117                                  1939: Turks Sign Mutual Aid Pact With Allies 
## 118                                                 1939: Veterans Miss Fox Hunts 
## 119                                             1939: War Inspires Letter-Writing 
## 120                                                  1939: War on Germany Declared
## 121                                              1939: Women Adopt Military Style 
##     Popular.fctr .n
## 1              N  1
## 2              N  1
## 3              N  1
## 4              N  1
## 5           <NA>  1
## 6              N  1
## 7              N  1
## 8              N  1
## 9              N  1
## 10          <NA>  1
## 11             N  1
## 12             N  1
## 13             N  1
## 14             N  1
## 15          <NA>  1
## 16          <NA>  1
## 17             N  1
## 18             N  1
## 19             N  1
## 20             N  1
## 21          <NA>  1
## 22             N  1
## 23             N  1
## 24          <NA>  1
## 25             N  1
## 26             N  1
## 27             N  1
## 28             N  1
## 29             N  1
## 30             N  1
## 31             N  1
## 32             N  1
## 33          <NA>  1
## 34             N  1
## 35             N  1
## 36             N  1
## 37             N  1
## 38             N  1
## 39          <NA>  1
## 40             N  1
## 41             N  1
## 42             N  1
## 43             N  1
## 44             N  1
## 45             N  1
## 46             N  1
## 47             N  1
## 48             N  1
## 49          <NA>  1
## 50             N  1
## 51             N  1
## 52          <NA>  1
## 53             N  1
## 54          <NA>  1
## 55             N  1
## 56          <NA>  1
## 57             N  1
## 58             N  1
## 59          <NA>  1
## 60             N  1
## 61          <NA>  1
## 62          <NA>  1
## 63          <NA>  1
## 64             N  1
## 65             N  1
## 66             N  1
## 67             N  1
## 68          <NA>  1
## 69             N  1
## 70             N  1
## 71             N  1
## 72             N  1
## 73          <NA>  1
## 74             N  1
## 75             N  1
## 76             N  1
## 77             N  1
## 78          <NA>  1
## 79             N  1
## 80             N  1
## 81             N  1
## 82             N  1
## 83             N  1
## 84          <NA>  1
## 85             N  1
## 86          <NA>  1
## 87          <NA>  1
## 88          <NA>  1
## 89             N  1
## 90             N  1
## 91             N  1
## 92             N  1
## 93             N  1
## 94          <NA>  1
## 95             N  1
## 96             N  1
## 97             N  1
## 98             N  1
## 99          <NA>  1
## 100            N  1
## 101            N  1
## 102         <NA>  1
## 103         <NA>  1
## 104            N  1
## 105            N  1
## 106            N  1
## 107            N  1
## 108            N  1
## 109            N  1
## 110         <NA>  1
## 111            N  1
## 112            N  1
## 113            N  1
## 114            N  1
## 115            N  1
## 116            N  1
## 117            N  1
## 118            N  1
## 119            N  1
## 120            N  1
## 121            N  1
```

```r
dsp_catxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_entity_df[sel_obs(Headline.contains=str), ],
        c("Headline.pfx", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
dsp_catxtab("1914)|(1939)")
```

```
##   "Headline.pfx" NewsDesk SectionName SubsectionName Popular.fctr .n
## 1   Headline.pfx  Foreign                                       N 48
## 2   Headline.pfx  Foreign                                    <NA> 13
## 3   Headline.pfx                                             <NA>  2
```

```r
dsp_catxtab("19(14|39|64):")
```

```
##   "Headline.pfx" NewsDesk SectionName SubsectionName Popular.fctr  .n
## 1   Headline.pfx  Foreign                                       N 138
## 2   Headline.pfx  Foreign                                    <NA>  39
## 3   Headline.pfx                                             <NA>   4
## 4   Headline.pfx                                                N   1
```

```r
dsp_catxtab("19..:")
```

```
##   "Headline.pfx" NewsDesk SectionName SubsectionName Popular.fctr  .n
## 1   Headline.pfx  Foreign                                       N 141
## 2   Headline.pfx  Foreign                                    <NA>  39
## 3   Headline.pfx                                                N   9
## 4   Headline.pfx                                             <NA>   4
```

```r
make_prefix <- function(row_ix) {
    words <- unlist(strsplit(glb_entity_df[row_ix, "Headline"], "\\s+"))
    words <- words[words != ""]
    
    # ^ forces match only at the beginning of ths string; [0-9] matches any number
    # All are matched to NewsDesk=[Foreign|]; SectionName=""; SubsectionName=""
    # None are Popular  
    if (grepl("^19[0-9][0-9]:", words[1])) return("19[0-9][0-9]::")
    
    # Consolidate all ".*Fashion Week::" since all are Popular=0; & 
    #      NewsDesk="TStyle|Styles|.*|Culture|Metro"; 
    #   SectionName=              ".*|Arts   |N.Y. / Region"; SubsectionName="";
    if (grepl("Fashion Week", glb_entity_df[row_ix, "Headline"])) 
        return(".*Fashion Week::")
    
    # Consolidate "Daily Clip Report" & "Daily Report" into "Daily (Clip)*Report::"
    #   None are Popular
    #   "Daily Clip Report" -> ""::""::""
    #   "Daily Report" -> "Business"::"Technology"::""
    if (grepl("Daily (Clip )*Report", glb_entity_df[row_ix, "Headline"])) 
        return("Daily (Clip )*Report::")
    
    # Consolidate 
    if (grepl("Today in (Politics|Small Business)", glb_entity_df[row_ix, "Headline"])) 
        return("Today in (Politics|Small Business)::")
    
    if (grepl("Pictures of the (Day|Year|.)", glb_entity_df[row_ix, "Headline"])) 
        return("Pictures of the (Day|Year|.)::")
    
    if (words[1] %in% c("Verbatim:")) 
        return(paste0(words[1], ":"))
    if (words[1] %in% c("6", "A", "An", "At", "Daily", "First", "For", "From", 
                        "How", "In", 
                        "Morning", "Milan", "New", "Obama", "On",
                        "Paris", "Pictures", "Q.",
                        "Test", "The", "'The", "Today", 
                        "What", "When", "Why", "Word")) {
        words12 <- paste(words[1], words[2], collapse=" ")
        if (words12 %in% c("Morning Agenda:")) 
            return(paste0(words12, ":"))
        if (words12 %in% c("First Draft", "Test Yourself", "What We're")) 
            return(paste0(words12, "::"))
        if (words12 %in% c("Word of")) return(paste0(words12, " the Day::"))
        if (words12 %in% c("6 Q's")) return(paste0(words12, " About the News::"))
        words123 <- paste(words12, words[3], collapse=" ")
        if (words12 %in% c("New York")) {
            if (words[3] == "Today:") return(paste0(words123, ":"))
            return(words123)
        }
        if (words12 %in% c("The Daily")) {
            if (words[3] %in% c("Gift:")) return(paste0(words12, " Gift::"))
            stop("should not happen")                                                         
        }    
        return(words12)
    } 
    return(words[1])    
}
make_prefix(187)
```

```
## [1] "19[0-9][0-9]::"
```

```r
# make_prefix(26)
# make_prefix(91)
glb_entity_df$Headline.pfx <- sapply(1:nrow(glb_entity_df), function(row_ix) make_prefix(row_ix))
#myprint_df(glb_entity_df[, c("Headline", "Headline.pfx")])
headline_pfx_df <- mycreate_sqlxtab_df(glb_entity_df[], c("Headline.pfx", glb_rsp_var))
#print(myplot_histogram(headline_pfx_df, ".n"))
print(myplot_hbar(head(headline_pfx_df, 15), "Headline.pfx", ".n", 
                  colorcol_name=glb_rsp_var))
```

![](NYTBlogs_hdlpfx7_files/figure-html/cleanse.data-1.png) 

```r
#print(head(orderBy(~-.n + Headline.pfx, headline_pfx_df), 20))
print(head(headline_pfx_df, 20))
```

```
##                            Headline.pfx Popular.fctr  .n
## 1                      .*Fashion Week::            N 184
## 2                        19[0-9][0-9]::            N 150
## 3                Daily (Clip )*Report::            N 122
## 4  Today in (Politics|Small Business)::            N 102
## 5                      Morning Agenda::            N  62
## 6                6 Q's About the News::            N  61
## 7                       Test Yourself::            N  61
## 8                     Word of the Day::            N  61
## 9                      New York Today::            N  59
## 10                        First Draft::            N  58
## 11       Pictures of the (Day|Year|.)::            N  53
## 12                         What We're::            N  45
## 13                       19[0-9][0-9]::         <NA>  43
## 14               Daily (Clip )*Report::         <NA>  40
## 15 Today in (Politics|Small Business)::         <NA>  36
## 16                           Verbatim::            N  33
## 17                     The Daily Gift::            N  26
## 18                     The Daily Gift::         <NA>  24
## 19       Pictures of the (Day|Year|.)::         <NA>  23
## 20                     New York Today::         <NA>  21
```

```r
dsp_catxtab("Today in (Politics|Small Business)")
```

```
##                           Headline.pfx NewsDesk  SectionName
## 1 Today in (Politics|Small Business):: Business Business Day
## 2 Today in (Politics|Small Business)::                      
## 3 Today in (Politics|Small Business)::                      
## 4 Today in (Politics|Small Business):: Business Business Day
## 5 Today in (Politics|Small Business)::          Business Day
## 6 Today in (Politics|Small Business):: Business             
##   SubsectionName Popular.fctr .n
## 1 Small Business            N 58
## 2                           N 44
## 3                        <NA> 21
## 4 Small Business         <NA> 13
## 5 Small Business         <NA>  1
## 6                        <NA>  1
```

```r
dsp_obs(Headline.contains="Today in .", all=TRUE)
```

```
##      NewsDesk  SectionName SubsectionName
## 73   Business Business Day Small Business
## 162  Business Business Day Small Business
## 260  Business Business Day Small Business
## 356  Business Business Day Small Business
## 510  Business Business Day Small Business
## 607  Business Business Day Small Business
## 719  Business Business Day Small Business
## 800  Business Business Day Small Business
## 883  Business Business Day Small Business
## 1024 Business Business Day Small Business
## 1111 Business Business Day Small Business
## 1184 Business Business Day Small Business
## 1281 Business Business Day Small Business
## 1379 Business Business Day Small Business
## 1559 Business Business Day Small Business
## 1654                                     
## 1661 Business Business Day Small Business
## 1801 Business Business Day Small Business
## 1829                                     
## 1913 Business Business Day Small Business
## 2051 Business Business Day Small Business
## 2194 Business Business Day Small Business
## 2220                                     
## 2286 Business Business Day Small Business
## 2324                                     
## 2397 Business Business Day Small Business
## 2429                                     
## 2501 Business Business Day Small Business
## 2509                                     
## 2537                                     
## 2605 Business Business Day Small Business
## 2638                                     
## 2744 Business Business Day Small Business
## 2773                                     
## 2850 Business Business Day Small Business
## 2878                                     
## 2939 Business Business Day Small Business
## 2973                                     
## 3028 Business Business Day Small Business
## 3071                                     
## 3110 Business Business Day Small Business
## 3166                                     
## 3251 Business Business Day Small Business
## 3282                                     
## 3351 Business Business Day Small Business
## 3374                                     
## 3469                                     
## 3548 Business Business Day Small Business
## 3569                                     
## 3648 Business Business Day Small Business
## 3666                                     
## 3791 Business Business Day Small Business
## 3804                                     
## 3874 Business Business Day Small Business
## 3905                                     
## 3970 Business Business Day Small Business
## 4003                                     
## 4057 Business Business Day Small Business
## 4107                                     
## 4151 Business Business Day Small Business
## 4203                                     
## 4293 Business Business Day Small Business
## 4326                                     
## 4392 Business Business Day Small Business
## 4418                                     
## 4487 Business Business Day Small Business
## 4520                                     
## 4567 Business Business Day Small Business
## 4616                                     
## 4678 Business Business Day Small Business
## 4708                                     
## 4805 Business Business Day Small Business
## 4829                                     
## 4886 Business Business Day Small Business
## 4915                                     
## 4974 Business Business Day Small Business
## 4999                                     
## 5068 Business Business Day Small Business
## 5107                                     
## 5162 Business Business Day Small Business
## 5195                                     
## 5288 Business Business Day Small Business
## 5315                                     
## 5366 Business Business Day Small Business
## 5396                                     
## 5449 Business Business Day Small Business
## 5488                                     
## 5547 Business Business Day Small Business
## 5582                                     
## 5635 Business Business Day Small Business
## 5676                                     
## 5768 Business Business Day Small Business
## 5803                                     
## 5862 Business Business Day Small Business
## 5890                                     
## 5954 Business Business Day Small Business
## 5985                                     
## 6045 Business Business Day Small Business
## 6083                                     
## 6155 Business Business Day Small Business
## 6186                                     
## 6296                                     
## 6371                                     
## 6431                                     
## 6585 Business Business Day Small Business
## 6617                                     
## 6668 Business Business Day Small Business
## 6705                                     
## 6762 Business Business Day Small Business
## 6797                                     
## 6849 Business Business Day Small Business
## 6889                                     
## 6954 Business Business Day Small Business
## 6986                                     
## 7076 Business Business Day Small Business
## 7104                                     
## 7160 Business Business Day Small Business
## 7191                                     
## 7261 Business Business Day Small Business
## 7294                                     
## 7354 Business Business Day Small Business
## 7396                                     
## 7453          Business Day Small Business
## 7483                                     
## 7563 Business Business Day Small Business
## 7597                                     
## 7667 Business Business Day Small Business
## 7688                                     
## 7787 Business Business Day Small Business
## 7813                                     
## 7880 Business Business Day Small Business
## 7906                                     
## 7973 Business                            
## 8002                                     
## 8090                                     
## 8147                                     
## 8191                                     
## 8309                                     
## 8353                                     
## 8397                                     
##                                                                             Headline
## 73                                       Today in Small Business: Made in the U.S.A.
## 162                  Today in Small Business: The Coolest New Businesses in New York
## 260                       Today in Small Business: Suppose Your Company Name Is Isis
## 356                           Today in Small Business: Target and Starbucks Go Small
## 510                            Today in Small Business: Twitter Tests a 'Buy' Button
## 607        Today in Small Business: Best and Worst Cities for Hispanic Entrepreneurs
## 719                                       Today in Small Business: Internet Slowdown
## 800         Today in Small Business: For New S.B.A. Chief, the Honeymoon May Be Over
## 883                                             Today in Small Business: Dying Malls
## 1024                                  Today in Small Business: 30 Start-Ups to Watch
## 1111                               Today in Small Business: The Case Against Tipping
## 1184                  Today in Small Business: When You Don't Love Your Company Name
## 1281           Today in Small Business: The World's Most Mysterious Nutella Emporium
## 1379                                         Today in Small Business: The Bacon Bowl
## 1559                                     Today in Small Business: How a Store Smells
## 1654             Today in Congressional Instagram: The Majority Leader Finds Bigfoot
## 1661                 Today in Small Business: Why Jewelry Stores Hide the Price Tags
## 1801                   Today in Small Business: A Positive Review on Yelp Goes Viral
## 1829                                                               Today in Politics
## 1913                               Today in Small Business: Mobile Is Not a Priority
## 2051                                     Today in Small Business: Unlimited Vacation
## 2194                       Today in Small Business: Facebook Expands Its Ad Platform
## 2220                                                               Today in Politics
## 2286                                      Today in Small Business: Paper or Plastic?
## 2324                                                               Today in Politics
## 2397      Today in Small Business: 'Bloodletting' at Tony Hsieh's Start-Up Community
## 2429                                                               Today in Politics
## 2501                 Today in Small Business: The Coolest New Businesses in Brooklyn
## 2509                         Today in Political #ThrowBackThursday: Bloomberg on Ice
## 2537                                                               Today in Politics
## 2605                                        Today in Small Business: Hiring Picks Up
## 2638                                                               Today in Politics
## 2744                    Today in Small Business: Is the S.B.A. Going Silicon Valley?
## 2773                                                               Today in Politics
## 2850                                Today in Small Business: A Perfect Yelp Response
## 2878                                                               Today in Politics
## 2939                                         Today in Small Business: The Bacon Boom
## 2973                                                               Today in Politics
## 3028                                 Today in Small Business: When Hashtags Backfire
## 3071                                                               Today in Politics
## 3110                                      Today in Small Business: the Rookie Cookie
## 3166                                                               Today in Politics
## 3251                             Today in Small Business: Why Amazon Must Be Stopped
## 3282                                                               Today in Politics
## 3351                              Today in Small Business: Business Travel and Ebola
## 3374                                                               Today in Politics
## 3469                                                               Today in Politics
## 3548               Today in Small Business: Forget R&eacute;sum&eacute;s. Try Videos
## 3569                                                               Today in Politics
## 3648                 Today in Small Business: Paying Retail Employees $50,000 a Year
## 3666                                                               Today in Politics
## 3791 Today in Small Business: How Hackers Can Stick Businesses With Huge Phone Bills
## 3804                                                               Today in Politics
## 3874                      Today in Small Business: Is Apple Pay the Future of Money?
## 3905                                                               Today in Politics
## 3970                                    Today in Small Business: A Lesson in Pricing
## 4003                                                               Today in Politics
## 4057                          Today in Small Business: 'We're the Uber of Whatever!'
## 4107                                                               Today in Politics
## 4151                    Today in Small Business: Dubious Excuses for Calling in Sick
## 4203                                                               Today in Politics
## 4293                Today in Small Business: When the Ebola Virus Touches a Business
## 4326                                                               Today in Politics
## 4392                        Today in Small Business: Start-Ups With a Social Mission
## 4418                                                               Today in Politics
## 4487                        Today in Small Business: Daring to Close on Thanksgiving
## 4520                                                               Today in Politics
## 4567                      Today in Small Business: Jimmy Kimmel Pitches 'Shark Tank'
## 4616                                                               Today in Politics
## 4678                       Today in Small Business: The Halloween Industrial Complex
## 4708                                                               Today in Politics
## 4805                        Today in Small Business: 'The Yelp of Business Software'
## 4829                                                               Today in Politics
## 4886                             Today in Small Business: Minimum Wage and Marijuana
## 4915                                                               Today in Politics
## 4974                                       Today in Small Business: Election Fallout
## 4999                                                               Today in Politics
## 5068                               Today in Small Business: Veteran-Owned Businesses
## 5107                                                               Today in Politics
## 5162                                        Today in Small Business: Paternity Leave
## 5195                                                               Today in Politics
## 5288                             Today in Small Business: Start-Ups Founded by Women
## 5315                                                               Today in Politics
## 5366                                    Today in Small Business: An S.E.O. Challenge
## 5396                                                               Today in Politics
## 5449                       Today in Small Business: Demise of the Internet Sales Tax
## 5488                                                               Today in Politics
## 5547                              Today in Small Business: Avoiding Bad Yelp Reviews
## 5582                                                               Today in Politics
## 5635            Today in Small Business: 'Next Generation of Lender or Boiler Room?'
## 5676                                                               Today in Politics
## 5768                            Today in Small Business: How Costco Codes Its Prices
## 5803                                                               Today in Politics
## 5862                       Today in Small Business: 'Unrealistic Value Expectations'
## 5890                                                               Today in Politics
## 5954                                Today in Small Business: Pastry, Coffee and Cats
## 5985                                                               Today in Politics
## 6045                        Today in Small Business: Why Typewriters Are Coming Back
## 6083                                                               Today in Politics
## 6155                               Today in Small Business: 'Big Cannabis' Is Coming
## 6186                                                               Today in Politics
## 6296                                                               Today in Politics
## 6371                                                               Today in Politics
## 6431                                                               Today in Politics
## 6585                                     Today in Small Business: 'Mean People Fail'
## 6617                                                               Today in Politics
## 6668                      Today in Small Business: Advance Ticketing for Restaurants
## 6705                                                               Today in Politics
## 6762                               Today in Small Business: Pregnancy Discrimination
## 6797                                                               Today in Politics
## 6849                                                  Today in Small Business: Wages
## 6889                                                               Today in Politics
## 6954                        Today in Small Business: The Best Jobs Numbers in Years?
## 6986                                                               Today in Politics
## 7076                               Today in Small Business: Problems With Apple Pay?
## 7104                                                               Today in Politics
## 7160                                    Today in Small Business: The Mistletoe Drone
## 7191                                                               Today in Politics
## 7261                         Today in Small Business: 'Um, I'm Selling the Business'
## 7294                                                               Today in Politics
## 7354                             Today in Small Business: The Best Start-Ups of 2014
## 7396                                                               Today in Politics
## 7453                                 Today in Small Business: The Future of Payments
## 7483                                                               Today in Politics
## 7563                         Today in Small Business: A Retail Success for Instagram
## 7597                                                               Today in Politics
## 7667                         Today in Small Business: Yelp's Gift to Business Owners
## 7688                                                               Today in Politics
## 7787                             Today in Small Business: The Year's Best Franchises
## 7813                                                               Today in Politics
## 7880                       Today in Small Business: The Best Content Marketing Blogs
## 7906                                                               Today in Politics
## 7973                                  Today in Small Business: Fracking and Gambling
## 8002                                                               Today in Politics
## 8090                                                               Today in Politics
## 8147                                                               Today in Politics
## 8191                                                               Today in Politics
## 8309                                                               Today in Politics
## 8353                                                               Today in Politics
## 8397                                                               Today in Politics
##                                                                                                                                                                                                                                                     Snippet
## 73                                                                                                                    Women are opening businesses in Detroit. More workers are claiming wage theft. And the advantages of being employed by your children.
## 162                                                                                                                  Leadership techniques that do not work. Mistakes that e-commerce sites make. And nine things not to do when trying to sell a business.
## 260                                                                            Gallup reports that new businesses are starved for financing and in decline. How to start a business with a friend (and stay friends). The struggle to find skilled workers.
## 356                                                                                                                                     Job growth slips. Small-business borrowing rises. Dont post pictures of your customers online without reading this.
## 510                                                                                                What happened when one community increased its minimum wage to $15 an hour. How to value a franchising business. The founder of Chick-fil-A died Monday.
## 607                                                                                                                                                                      Why operating cash flow is more important than net profit. Can gas pumps be smart?
## 719                                                                             The big news may turn out to be Apple Pay, which aims to replace the wallet. A small business tries to shake up the insurance industry. And Dennys scores a marketing coup.
## 800                                                                                                                                 California passes a Yelp Bill. Employee health premiums rise only 3 percent. And how scared is the dumb-watch industry?
## 883                                                         Small businesses see only a small increase in health premiums. Small-business loans hit a record high for the third consecutive month. And theres a black market for Olive Garden pasta passes.
## 1024                                       An iconic hardware store closes after nearly 100 years in business. Why Shark Tank doesnt represent the real world of investing. And how Urban Outfitters became the official clothing store of Outrage Twitter.
## 1111                                                                                                       Tech upgrades for small businesses. How to use Instagram to promote products. Should we ban states and cities from offering tax breaks for jobs?
## 1184                                                                                                           How being videotaped changes employee behavior. How RadioShack missed almost every opportunity. And does Urban Outfitters offend on purpose?
## 1281                                                                                                                                 Malcolm Gladwell tells entrepreneurs, Be disagreeable. Yelp settles a lawsuit. And what will happen to whiskey prices?
## 1379                                                                                                      How Alibaba shared the wealth with employees. Rewarding customers for writing negative Yelp reviews. And why its definitely time to take a break!
## 1559                                 How Zulily found its voice and increased its social media presence through humor. Fifteen mistakes to avoid in a content marketing strategy. Should a business owner contact police about a deceitful former employee?
## 1654                                                                                                                                                                                        Representative Kevin McCarthy poses for a picture with Bigfoot.
## 1661                                                                             Ever wonder why jewelry stores hide the price tags? How invoice factoring works for small businesses. And why German companies are on a buying spree in the United States.
## 1801                                                                                                                       Why the fast-food industry is struggling. Why so many job postings are ridiculous. And do older workers impede entrepreneurship?
## 1829                                                                                                                                          Today in Politics: Obama at the U.N., the Republican governors money machine, and Newt Gingrichs C-Span flub.
## 1913                                                                                                                                  The U.S. dollar strengthens. Retailers plan to open even earlier for Thanksgiving. How to protect your business name.
## 2051                                                                                               Ten things you may not know about Google AdWords. The economy grew at a 4.6 percent annualized rate in the second quarter. Inside the dollar-store wars.
## 2194                                                                                                                     You can see the future of restaurants in San Francisco. Ten lessons for entrepreneurs from Derek Jeter. Remember the fiscal cliff?
## 2220                                                                                      The Supreme Court gathers to set its term agenda, Republicans are gaining strength in the polls and a new Lincoln history is coming out in photographs and print.
## 2286                                                                                           Mayor de Blasio plans to sign an executive order expanding New York Citys living wage law. Macys is hiring for the holidays. How to lose $1 million a month.
## 2324                                                                                                                                                    Members of Congress will grill the director of the Secret Service on White House security breaches.
## 2397                                                                                  A guide to co-working in New York. If you want to be a great leader, start acting weirder. Why millions of new blue-collar jobs will open up over the next few years.
## 2429                                                                                                                                               President Obama and Israeli Prime Minister Benjamin Netanyahu will meet at the White House on Wednesday.
## 2501                                                                                                          Uber for Business. How to work from home more effectively. Is it legal for bosses to use social media tools to check backgrounds when hiring?
## 2509                                                                                                                                                                                                        Michael R. Bloomberg and two Disney princesses.
## 2537                                                        A group backed by the billionaire Koch brothers is shifting tactics in several of the most competitive Senate races and will begin more direct and aggressive attacks on Democratic candidates.
## 2605                                                                                                        The marketplace for local services is booming. Good news for owners hoping to sell a small business. And the economic case for paternity leave.
## 2638                                        President Obama is keeping in close touch with health officials in Texas, Mitt Romney has revealed a family man-crush and later, offices will be clearing out early for the start of the Nationals playoff run.
## 2744                                                   The anti-luxury trend. Should employers use credit checks when considering job candidates? An advertising agency devoted exclusively to marketing marijuana wants to re-brand the cannabis industry.
## 2773   With four weeks to go before the midterm elections, the battle for control of the Senate has narrowed to just a handful of races. And the shifting landscape in two of those contests  Iowa and North Carolina  captures how difficult it will be...
## 2850                                                                                                  Business travelers turn to Airbnb. Here are 10 productivity hacks from well known entrepreneurs. And have you considered the spoiled boss hypothesis?
## 2878                                                                                                                                                                                            Have Republicans given up the fight over same-sex marriage?
## 2939                                                                                       The most bountiful holiday season in three years? The most innovative cities in America. Did Comcast retaliate against an unhappy customer by getting him fired?
## 2973                                     Attention is shifting from baseball and back to midterm elections, President Obama and his party continue to drift apart, and Sarah Silverman makes a political ad that we can tell you about but cannot show you.
## 3028                                                                                                                                            Facebook has a new local mobile advertising play. J.C. Penney is in the midst of an e-commerce renaissance.
## 3071                                                       The election has taken a dark turn as conservatives use warnings about Ebola, the Islamic State and terrorism to send a message: The world is a scary place, and the Democrats cant protect you.
## 3110                            A restaurant learns what happens when you tell customers to pay what God wants them to pay, Microsofts chief executive says its O.K. for women to ask for a raise, and new restaurants are making Detroit a culinary oasis.
## 3166                                                  A Supreme Court ruling on voter ID has upset election plans in Wisconsin, President Obama is enjoying a hug from Hollywood and an obsessive artist has captured a new side of Hillary Rodham Clinton.
## 3251                                                                                                      Revolution is sexy. A contrarian developer is looking to buy properties in Atlantic City. A pioneer of hair products for black people dies at 82.
## 3282                                                Its debate night in the marquee Kentucky Senate race featuring one of the countrys shrewdest political tacticians, Mitch McConnell, against a tenacious Democratic challenger, Alison Lundergan Grimes.
## 3351                                                                                                                     How to transfer money through Twitter. How to get the most out of Yelp. Is this the hottest real estate start-up in New York City?
## 3374                                  Democrats struggling to hang on to the Senate, who have been drowning in dreary news, have spotted a hopeful sign: Democratic voters who sat out the 2010 elections are becoming more interested in voting this time.
## 3469                                                                                                    The midterm elections have been maddeningly unpredictable, but now, with three weeks to go, Democrats may be preparing for an electoral apocalypse.
## 3548                                                                                                          What does Wall Street know that the rest of us dont? Are most Airbnb listings illegal? Why are sandwich makers signing noncompete agreements?
## 3569                                                                                                               Weeks after the Ebola epidemic became a global threat, the White House on Wednesday suddenly moved to convey that it was in crisis mode.
## 3648                                                                                                                                                How to create a marketing plan. Apple Pay arrives Monday. And does dressing like a man lead to success?
## 3666                                                                               Bracing for a grim election on Nov. 4, the White House is grappling with what should come afterward  and how quickly to move to overhaul the nations immigration system.
## 3791                                                                                            Why businesses are pushing back against a travel ban. Why young graduates are headed to places like Buffalo and Cleveland. Does Amazon have too much power?
## 3804                                      Democrats running in conservative states worry that the Obama administrations stumbling response to Ebolas arrival in the United States is generating anxiety among voters  and reinforcing a Republican message.
## 3874                                                                                                               How one start-up made its first hires. Why a travel ban is not going to happen. Should you have to pay to make a restaurant reservation?
## 3905                                                                                   President Obama said he believes the Constitution guarantees a right to same-sex marriage in all 50 states, but he supports the Supreme Courts incremental approach.
## 3970                                                                                                                   Is franchising a scam? A start-up gets a valuation of $2 billion. Should car manufacturers be allowed to sell directly to consumers?
## 4003                                                                   President Obama is meeting with his new Ebola czar, while big-name Republicans are barnstorming battleground states. But many in Washington will be remembering a journalism legend.
## 4057                                                                Google is preparing to take on Apple Pay. How one company pampers its workers. Amazon is likely to top $100 billion in revenue next year  but hasnt made a quarterly profit since 2011.
## 4107                                                                          President Obama is turning to scientists for ideas on battling Ebola, Gov. Rick Perry of Texas is making another trip to Iowa, and the White Houses fence remains penetrable.
## 4151                                                                                                                                                                                        Whats the worst excuse for calling in sick you have ever heard?
## 4203                                                                                     If there was one message officials tried to convey in New York on Thursday after the discovery that a doctor contracted Ebola, it was this: We are on top of this.
## 4293                                                                                 A Shark Tank contestant gets hundreds of thousands of downloads. How the Pentagon finally reached its goal for small-business contracts. Is Oakland the next Brooklyn?
## 4326                                                                                           Mitt Romney and Bob Dole are stumping in Kansas, the Bush family is embracing a Jeb Bush run for president and President Obama is honing his Ebola strategy.
## 4392                                                                                                             Five ways to monitor employee social media activities. Where fast-food workers make $20 an hour. And how to spot the rsum of a psychopath.
## 4418                                                                                                                   Races in Colorado, Georgia, Iowa, Kansas and North Carolina will be the most crucial in determining which party controls the Senate.
## 4487                                                                                                                                                                      Apple Pay runs into competition. Does anyone really want a smart washing machine?
## 4520                                                                                                                                                                                          President Obama is starting to think about life after Nov. 4.
## 4567                                                                   Tim Cook says being gay has helped him as chief executive of Apple. Five ways to navigate the sale of your company. And the places where minimum-wage referendums are on the ballot.
## 4616                                                                                                                                                              Election watchers might want to take a moment to savor the uncertainty of these midterms.
## 4678                                                                                                    Why Tim Cooks coming out is so meaningful. What a Republican-controlled Senate might mean for small businesses. How to raise money for a prototype.
## 4708                                                                                                                                                                                 Democrats still have a chance to hold the Senate, but it wont be easy.
## 4805 The rush is on for a new breed of marijuana capitalist in New York. How even a profitable company can run out of cash. Binding referendums in several red states Tuesday would raise the state minimum wage above the $7.25 an hour mandated by the...
## 4829                                                                                                                               Looking back on some of the most memorable moments of this midterm election season less than 24 hours before polls open.
## 4886                                 Why the economy and voter perception of the economy are two different things. A study shows that shorter email subject lines deliver higher open rates. Is it possible to go an entire week using only mobile payment?
## 4915                                                                                                                    After all the money, attack ads, debates, rallies, missteps, surprises and that rare discussion of policy, its time to count votes.
## 4974                                                                 The private sector created 230,000 jobs in October. New York is becoming doughnut city. And how did Taylor Swift sell a million copies of her album in a week? Its called #taylurking.
## 4999                                                                                                                                 Republicans won the Senate, padded their majority in the House and added to their numbers among the nations governors.
## 5068                                                                                                                 More business-related election returns. After 20 years, the banner ad is finally in decline. And theres a Bitcoin-only cafe in Prague.
## 5107                                                                                                                                                                                    What might happen in the final months of a Democratic-led Congress.
## 5162                                                                                                  The economy is producing steady gains on jobs. Oyster farming is booming. And heres why the new soda tax in Berkeley, Calif., may not reduce obesity.
## 5195                                                                                                                        President Obama is having lunch with congressional leaders even as the post-election truce appears to have already broken down.
## 5288                                                                                                       Seven-day postal service. Republicans dont always hate business regulations. Is maple syrup good for anything besides pancakes and French toast?
## 5315                                                                                                                                                                           Will Democrats take advantage of a Senate majority while they still have it?
## 5366                                                                                                    Military-friendly companies. Could a drop in lending be good news for small businesses? Is Igor Pasternak on the verge of revolutionizing shipping?
## 5396                                                                                                                                                                                                 A partisan fight over Internet regulations is brewing.
## 5449                                                                             American retail workers steal more than shoplifters. Pinterest says its male users have doubled in the past year. A company gets $100 million to remake the cash register.
## 5488                                                                                                                                                        Members of Congress are returning for a session that, for many of them, will be a long goodbye.
## 5547                           Did this cereal milk start-up target the wrong market? Should small businesses have to offer health insurance to part-time workers? The ultimate list of start-up competitions, hackathons, business-plan contests and more.
## 5582                                                                      With the last session of the 113th Congress underway, Democrats are looking to expand their leadership ranks, and members of both parties are preparing to pass some legislation.
## 5635                                                                                                  Retail sales rebounded in October. Sliding oil and gas prices are giving Americans more money to spend. And Walmart tries a new kind of Black Friday.
## 5676                                                                                                                                 House Democrats took a beating in the Republican wave on election night, but theyve been doing pretty well ever since.
## 5768                                                                                                                                                Solving marijuanas banking problem. The ugly side of Yelp. Are we running out of chocolate and turkeys?
## 5803                                                                                                                                                                  Its time for the lame-duck Congress to get serious about what it will do  or wont do.
## 5862                                                                       A beloved cookbook destination loses its lease. A start-up that claims better search technology for real estate agents raises $6 million. Is celebrity-branded marijuana coming?
## 5890                                                                                                                                                                               In the Senate, an overhaul of domestic spying operations is on the line.
## 5954                                                                       Vape beats slacktivism. A business owner in Washington state says FEMAs red tape is forcing her out of business. And why businesses need to change the way they accept payments.
## 5985                                                                                                                                                           Republican leaders in the House stuck with an (almost) all-male lineup of committee leaders.
## 6045                                                                                       What the Uber scandal says about start-up culture. How social entrepreneurs are changing the world. An experienced entrepreneur talks about work/life imbalance.
## 6083                                                                                                   Republican options for stopping President Obamas immigration action may be limited because Congress doesnt control the immigration agencys finances.
## 6155    How the presidents immigration plan could help cities like Detroit. A lot of notable investors  and a former head of the Small Business Administration  are investing in a payroll start-up. A seasoned traveler says you can be more productive...
## 6186                                                                                                                                                                         Democrats are worried that President Obama cant close the deal on immigration.
## 6296                                                                                                                                        Republicans are showing no sign of leaving Benghazi behind, and Democrats are balking at a bipartisan tax deal.
## 6371                                                                                       Repercussions of Defense Secretary Chuck Hagels resignation and a grand jury decision over the death of Michael Brown are dominating conversation in Washington.
## 6431                                                                            The potential political costs of the civil unrest in Ferguson, Mo.; speculation about Chuck Hagels replacement; and a look at Speaker John A. Boehners turkey brine recipe.
## 6585                                                           There are pluses and minuses to collapsing oil prices. A California bill would require retailers and restaurants to pay double on holidays. A salad chain manages to attract tech investors.
## 6617                                                                                                                                                                                                   Unfinished business will make December a busy month.
## 6668                                                                              A bakery in Ferguson, Mo., raises $250,000 through crowdfunding. How to avoid making rookie legal mistakes. And French business owners stage protests across the country.
## 6705                                                                                                                                                                                 President Obamas immigration action faces the Congressional spotlight.
## 6762                                                                                                Private-sector hiring fell shy of estimates in November. There is big business in ugly Christmas sweaters. And the value of the worlds largest truffle.
## 6797                                                                                                                                 Republicans are ready to put out the welcome mat for the man President Obama is expected to pick as defense secretary.
## 6849                                                                                     Health care costs grew at the slowest rate in 2013 since 1960. There is $1.99 gas in Oklahoma City. And search engine optimization mistakes that are easy to make.
## 6889                                                                                                                 New Yorks chokehold case has thrown a volatile element into Loretta E. Lynchs confirmation hearings as United States attorney general.
## 6954                      Starbucks plans a new chain of coffee shops for those who thought Starbucks coffee was too inexpensive. Four studies show that the Affordable Care Act is working incredibly well. And here is how to deal with a toxic employee.
## 6986                                                                                                                                                               There is a sense of optimism on Capitol Hill that a package of spending bills will pass.
## 7076                                                                                  A Detroit burger restaurant pays $15 an hour  and makes money. How businesses can manage holiday parties. And how one business owner responded to an antigay request.
## 7104                                                                                                                                                                     The 113th Congress has plenty to do in what most lawmakers hope is its final week.
## 7160                                                                                                                                     Building a better jockstrap. Apple names its top apps of 2014. Could Philadelphia become an East Coast energy hub?
## 7191                                                                                                                                A long-delayed Senate report documenting the C.I.A.s torture of terrorism detainees is set to become public on Tuesday.
## 7261                                                                                                                     Why arent these video stores dead? Amazon introduces a make-an-offer service. And a study puts a price on fake advertising clicks.
## 7294                                                                                                The partisan split over the validity of the report on torture raises questions about how closely the two parties will cooperate on intelligence issues.
## 7354                                                                         Why one business owner thinks Shark Tank is terrible for businesses. There will be no Internet sales tax for another year. And nine reasons business owners hate the holidays.
## 7396                                                                                                             The 113th Congress is concluding with partisan brinksmanship and one last mad scramble for votes to pass a $1.1 trillion spending package.
## 7453                                                                                                      Oil falls below $60 a barrel. Microsoft now accepts Bitcoin. And an Indianapolis gun-store owner offers tips for gun gifting this holiday season.
## 7483                                                                          The near-death experience of a bipartisan, governmentwide funding bill on Thursday highlighted a fundamental problem with Congress at the moment: the lost art of compromise.
## 7563                                 The federal government spending bill quietly includes legislation affecting thousands of small businesses. Ten questions to ask before allowing employees to telecommute. And why retailers keep sending out catalogs.
## 7597                                                                                                                      The lame-duck Senate gets back to business on Monday and is set to begin approving the executive and judicial branch nominations.
## 7667                                                                                             Americans are spending more than ever on fancy dog food. Monday was the biggest shipping day of the year. And more companies are signing up for Apple Pay.
## 7688                                                                           As they plow through backlogged presidential nominations, Senate Democrats are defending a rule change that allowed them to break a nominee filibuster with a majority vote.
## 7787                                                                                                        Can the S.B.A. stay relevant? Will a subscription model work for the movies? Is it acceptable to read and respond to messages during a meeting?
## 7813                                                                                                                                                                                              It has begun. The 2016 presidential campaign is underway.
## 7880                                                                                                           Introducing the BlackBerry Classic. How a Papa Johns franchisee went from driver to owner. Are these the five best business leaders of 2014?
## 7906                                         President Obamas surprise announcement on Wednesday that he is restoring diplomatic relations with Cuba touched off a political storm, and the repercussions will undoubtedly be felt through 2016 and beyond.
## 7973                                                         The minimum wage is set to rise in 21 states by New Years Day. Amazon Prime Now delivered candy to a blogger in 23 minutes. Nebraska and Oklahoma sue Colorado to stop marijuana legalization.
## 8002                                                                                 President Obama will hold a news conference before leaving for a two-week vacation in Hawaii, and the questioning will likely center on plans to renew ties with Cuba.
## 8090                                                                                                                   President Obama is under pressure to address growing racial tensions after the fatal shootings of two New York City police officers.
## 8147                                                                                                                               Mayor Bill de Blasio faces a political crisis over the fatal shooting of two police officers by an African-American man.
## 8191                                     Reince Priebus, chairman of the Republican National Committee, said recently that though the party was overhauling its primary process, it was not solely responsible for Mitt Romneys 2012 general election loss.
## 8309                                                                                                                                 President Obama is halfway through his year-end trip to Hawaii. While he has kept a low profile, hes been fairly busy.
## 8353                                                                                                           Representative Michael G. Grimm of New York insisted last week that he would not resign from his seat. On Monday night, he changed his mind.
## 8397                                                                                                                 House Republicans are ending the year on a defensive note over Representative Steve Scalises 2002 speech to a white supremacist group.
##                                                                                                                                                                                                                                                                                               Abstract
## 73                                                                                                                                                               Women are opening businesses in Detroit. More workers are claiming wage theft. And the advantages of being employed by your children.
## 162                                                                                                                                                             Leadership techniques that do not work. Mistakes that e-commerce sites make. And nine things not to do when trying to sell a business.
## 260                                                                                                                       Gallup reports that new businesses are starved for financing and in decline. How to start a business with a friend (and stay friends). The struggle to find skilled workers.
## 356                                                                                                                                                                                Job growth slips. Small-business borrowing rises. Dont post pictures of your customers online without reading this.
## 510                                                                                                                                           What happened when one community increased its minimum wage to $15 an hour. How to value a franchising business. The founder of Chick-fil-A died Monday.
## 607                                                                                                                                                                                                                 Why operating cash flow is more important than net profit. Can gas pumps be smart?
## 719                                                                                                                        The big news may turn out to be Apple Pay, which aims to replace the wallet. A small business tries to shake up the insurance industry. And Dennys scores a marketing coup.
## 800                                                                                                                                                                            California passes a Yelp Bill. Employee health premiums rise only 3 percent. And how scared is the dumb-watch industry?
## 883                                                                                                    Small businesses see only a small increase in health premiums. Small-business loans hit a record high for the third consecutive month. And theres a black market for Olive Garden pasta passes.
## 1024                                                                                  An iconic hardware store closes after nearly 100 years in business. Why Shark Tank doesnt represent the real world of investing. And how Urban Outfitters became the official clothing store of Outrage Twitter.
## 1111                                                                                                                                                  Tech upgrades for small businesses. How to use Instagram to promote products. Should we ban states and cities from offering tax breaks for jobs?
## 1184                                                                                                                                                      How being videotaped changes employee behavior. How RadioShack missed almost every opportunity. And does Urban Outfitters offend on purpose?
## 1281                                                                                                                                                                            Malcolm Gladwell tells entrepreneurs, Be disagreeable. Yelp settles a lawsuit. And what will happen to whiskey prices?
## 1379                                                                                                                                                 How Alibaba shared the wealth with employees. Rewarding customers for writing negative Yelp reviews. And why its definitely time to take a break!
## 1559                                                                            How Zulily found its voice and increased its social media presence through humor. Fifteen mistakes to avoid in a content marketing strategy. Should a business owner contact police about a deceitful former employee?
## 1654                                                                                                                                                                                                                                   Representative Kevin McCarthy poses for a picture with Bigfoot.
## 1661                                                                                                                        Ever wonder why jewelry stores hide the price tags? How invoice factoring works for small businesses. And why German companies are on a buying spree in the United States.
## 1801                                                                                                                                                                  Why the fast-food industry is struggling. Why so many job postings are ridiculous. And do older workers impede entrepreneurship?
## 1829                                                                                                                                                                                     Today in Politics: Obama at the U.N., the Republican governors money machine, and Newt Gingrichs C-Span flub.
## 1913                                                                                                                                                                             The U.S. dollar strengthens. Retailers plan to open even earlier for Thanksgiving. How to protect your business name.
## 2051                                                                                                                                          Ten things you may not know about Google AdWords. The economy grew at a 4.6 percent annualized rate in the second quarter. Inside the dollar-store wars.
## 2194                                                                                                                                                                You can see the future of restaurants in San Francisco. Ten lessons for entrepreneurs from Derek Jeter. Remember the fiscal cliff?
## 2220                                                                                                                                 The Supreme Court gathers to set its term agenda, Republicans are gaining strength in the polls and a new Lincoln history is coming out in photographs and print.
## 2286                                                                                                                                      Mayor de Blasio plans to sign an executive order expanding New York Citys living wage law. Macys is hiring for the holidays. How to lose $1 million a month.
## 2324                                                                                                                                                                                               Members of Congress will grill the director of the Secret Service on White House security breaches.
## 2397                                                                                                                             A guide to co-working in New York. If you want to be a great leader, start acting weirder. Why millions of new blue-collar jobs will open up over the next few years.
## 2429                                                                                                                                                                                          President Obama and Israeli Prime Minister Benjamin Netanyahu will meet at the White House on Wednesday.
## 2501                                                                                                                                                     Uber for Business. How to work from home more effectively. Is it legal for bosses to use social media tools to check backgrounds when hiring?
## 2509                                                                                                                                                                                                                                                   Michael R. Bloomberg and two Disney princesses.
## 2537                                                                                                   A group backed by the billionaire Koch brothers is shifting tactics in several of the most competitive Senate races and will begin more direct and aggressive attacks on Democratic candidates.
## 2605                                                                                                                                                   The marketplace for local services is booming. Good news for owners hoping to sell a small business. And the economic case for paternity leave.
## 2638                                                                                   President Obama is keeping in close touch with health officials in Texas, Mitt Romney has revealed a family man-crush and later, offices will be clearing out early for the start of the Nationals playoff run.
## 2744                                                                                              The anti-luxury trend. Should employers use credit checks when considering job candidates? An advertising agency devoted exclusively to marketing marijuana wants to re-brand the cannabis industry.
## 2773 With four weeks to go before the midterm elections, the battle for control of the Senate has narrowed to just a handful of races. And the shifting landscape in two of those contests  Iowa and North Carolina  captures how difficult it will be for Democrats to prevent a Republican takeover.
## 2850                                                                                                                                             Business travelers turn to Airbnb. Here are 10 productivity hacks from well known entrepreneurs. And have you considered the spoiled boss hypothesis?
## 2878                                                                                                                                                                                                                                       Have Republicans given up the fight over same-sex marriage?
## 2939                                                                                                                                  The most bountiful holiday season in three years? The most innovative cities in America. Did Comcast retaliate against an unhappy customer by getting him fired?
## 2973                                                                                Attention is shifting from baseball and back to midterm elections, President Obama and his party continue to drift apart, and Sarah Silverman makes a political ad that we can tell you about but cannot show you.
## 3028                                                                                                                                                                                       Facebook has a new local mobile advertising play. J.C. Penney is in the midst of an e-commerce renaissance.
## 3071                                                                                                  The election has taken a dark turn as conservatives use warnings about Ebola, the Islamic State and terrorism to send a message: The world is a scary place, and the Democrats cant protect you.
## 3110                                                                       A restaurant learns what happens when you tell customers to pay what God wants them to pay, Microsofts chief executive says its O.K. for women to ask for a raise, and new restaurants are making Detroit a culinary oasis.
## 3166                                                                                             A Supreme Court ruling on voter ID has upset election plans in Wisconsin, President Obama is enjoying a hug from Hollywood and an obsessive artist has captured a new side of Hillary Rodham Clinton.
## 3251                                                                                                                                                 Revolution is sexy. A contrarian developer is looking to buy properties in Atlantic City. A pioneer of hair products for black people dies at 82.
## 3282                                                                                           Its debate night in the marquee Kentucky Senate race featuring one of the countrys shrewdest political tacticians, Mitch McConnell, against a tenacious Democratic challenger, Alison Lundergan Grimes.
## 3351                                                                                                                                                                How to transfer money through Twitter. How to get the most out of Yelp. Is this the hottest real estate start-up in New York City?
## 3374                                                                             Democrats struggling to hang on to the Senate, who have been drowning in dreary news, have spotted a hopeful sign: Democratic voters who sat out the 2010 elections are becoming more interested in voting this time.
## 3469                                                                                                                                               The midterm elections have been maddeningly unpredictable, but now, with three weeks to go, Democrats may be preparing for an electoral apocalypse.
## 3548                                                                                                                                                     What does Wall Street know that the rest of us dont? Are most Airbnb listings illegal? Why are sandwich makers signing noncompete agreements?
## 3569                                                                                                                                                          Weeks after the Ebola epidemic became a global threat, the White House on Wednesday suddenly moved to convey that it was in crisis mode.
## 3648                                                                                                                                                                                           How to create a marketing plan. Apple Pay arrives Monday. And does dressing like a man lead to success?
## 3666                                                                                                                          Bracing for a grim election on Nov. 4, the White House is grappling with what should come afterward  and how quickly to move to overhaul the nations immigration system.
## 3791                                                                                                                                       Why businesses are pushing back against a travel ban. Why young graduates are headed to places like Buffalo and Cleveland. Does Amazon have too much power?
## 3804                                                                                 Democrats running in conservative states worry that the Obama administrations stumbling response to Ebolas arrival in the United States is generating anxiety among voters  and reinforcing a Republican message.
## 3874                                                                                                                                                          How one start-up made its first hires. Why a travel ban is not going to happen. Should you have to pay to make a restaurant reservation?
## 3905                                                                                                                              President Obama said he believes the Constitution guarantees a right to same-sex marriage in all 50 states, but he supports the Supreme Courts incremental approach.
## 3970                                                                                                                                                              Is franchising a scam? A start-up gets a valuation of $2 billion. Should car manufacturers be allowed to sell directly to consumers?
## 4003                                                                                                              President Obama is meeting with his new Ebola czar, while big-name Republicans are barnstorming battleground states. But many in Washington will be remembering a journalism legend.
## 4057                                                                                                           Google is preparing to take on Apple Pay. How one company pampers its workers. Amazon is likely to top $100 billion in revenue next year  but hasnt made a quarterly profit since 2011.
## 4107                                                                                                                     President Obama is turning to scientists for ideas on battling Ebola, Gov. Rick Perry of Texas is making another trip to Iowa, and the White Houses fence remains penetrable.
## 4151                                                                                                                                                                                                                                   Whats the worst excuse for calling in sick you have ever heard?
## 4203                                                                                                                                If there was one message officials tried to convey in New York on Thursday after the discovery that a doctor contracted Ebola, it was this: We are on top of this.
## 4293                                                                                                                            A Shark Tank contestant gets hundreds of thousands of downloads. How the Pentagon finally reached its goal for small-business contracts. Is Oakland the next Brooklyn?
## 4326                                                                                                                                      Mitt Romney and Bob Dole are stumping in Kansas, the Bush family is embracing a Jeb Bush run for president and President Obama is honing his Ebola strategy.
## 4392                                                                                                                                                        Five ways to monitor employee social media activities. Where fast-food workers make $20 an hour. And how to spot the rsum of a psychopath.
## 4418                                                                                                                                                              Races in Colorado, Georgia, Iowa, Kansas and North Carolina will be the most crucial in determining which party controls the Senate.
## 4487                                                                                                                                                                                                                 Apple Pay runs into competition. Does anyone really want a smart washing machine?
## 4520                                                                                                                                                                                                                                     President Obama is starting to think about life after Nov. 4.
## 4567                                                                                                              Tim Cook says being gay has helped him as chief executive of Apple. Five ways to navigate the sale of your company. And the places where minimum-wage referendums are on the ballot.
## 4616                                                                                                                                                                                                         Election watchers might want to take a moment to savor the uncertainty of these midterms.
## 4678                                                                                                                                               Why Tim Cooks coming out is so meaningful. What a Republican-controlled Senate might mean for small businesses. How to raise money for a prototype.
## 4708                                                                                                                                                                                                                            Democrats still have a chance to hold the Senate, but it wont be easy.
## 4805                           The rush is on for a new breed of marijuana capitalist in New York. How even a profitable company can run out of cash. Binding referendums in several red states Tuesday would raise the state minimum wage above the $7.25 an hour mandated by the federal government.
## 4829                                                                                                                                                                          Looking back on some of the most memorable moments of this midterm election season less than 24 hours before polls open.
## 4886                                                                            Why the economy and voter perception of the economy are two different things. A study shows that shorter email subject lines deliver higher open rates. Is it possible to go an entire week using only mobile payment?
## 4915                                                                                                                                                               After all the money, attack ads, debates, rallies, missteps, surprises and that rare discussion of policy, its time to count votes.
## 4974                                                                                                            The private sector created 230,000 jobs in October. New York is becoming doughnut city. And how did Taylor Swift sell a million copies of her album in a week? Its called #taylurking.
## 4999                                                                                                                                                                            Republicans won the Senate, padded their majority in the House and added to their numbers among the nations governors.
## 5068                                                                                                                                                            More business-related election returns. After 20 years, the banner ad is finally in decline. And theres a Bitcoin-only cafe in Prague.
## 5107                                                                                                                                                                                                                               What might happen in the final months of a Democratic-led Congress.
## 5162                                                                                                                                             The economy is producing steady gains on jobs. Oyster farming is booming. And heres why the new soda tax in Berkeley, Calif., may not reduce obesity.
## 5195                                                                                                                                                                   President Obama is having lunch with congressional leaders even as the post-election truce appears to have already broken down.
## 5288                                                                                                                                                  Seven-day postal service. Republicans dont always hate business regulations. Is maple syrup good for anything besides pancakes and French toast?
## 5315                                                                                                                                                                                                                      Will Democrats take advantage of a Senate majority while they still have it?
## 5366                                                                                                                                               Military-friendly companies. Could a drop in lending be good news for small businesses? Is Igor Pasternak on the verge of revolutionizing shipping?
## 5396                                                                                                                                                                                                                                            A partisan fight over Internet regulations is brewing.
## 5449                                                                                                                        American retail workers steal more than shoplifters. Pinterest says its male users have doubled in the past year. A company gets $100 million to remake the cash register.
## 5488                                                                                                                                                                                                   Members of Congress are returning for a session that, for many of them, will be a long goodbye.
## 5547                                                                      Did this cereal milk start-up target the wrong market? Should small businesses have to offer health insurance to part-time workers? The ultimate list of start-up competitions, hackathons, business-plan contests and more.
## 5582                                                                                                                 With the last session of the 113th Congress underway, Democrats are looking to expand their leadership ranks, and members of both parties are preparing to pass some legislation.
## 5635                                                                                                                                             Retail sales rebounded in October. Sliding oil and gas prices are giving Americans more money to spend. And Walmart tries a new kind of Black Friday.
## 5676                                                                                                                                                                            House Democrats took a beating in the Republican wave on election night, but theyve been doing pretty well ever since.
## 5768                                                                                                                                                                                           Solving marijuanas banking problem. The ugly side of Yelp. Are we running out of chocolate and turkeys?
## 5803                                                                                                                                                                                                             Its time for the lame-duck Congress to get serious about what it will do  or wont do.
## 5862                                                                                                                  A beloved cookbook destination loses its lease. A start-up that claims better search technology for real estate agents raises $6 million. Is celebrity-branded marijuana coming?
## 5890                                                                                                                                                                                                                          In the Senate, an overhaul of domestic spying operations is on the line.
## 5954                                                                                                                  Vape beats slacktivism. A business owner in Washington state says FEMAs red tape is forcing her out of business. And why businesses need to change the way they accept payments.
## 5985                                                                                                                                                                                                      Republican leaders in the House stuck with an (almost) all-male lineup of committee leaders.
## 6045                                                                                                                                  What the Uber scandal says about start-up culture. How social entrepreneurs are changing the world. An experienced entrepreneur talks about work/life imbalance.
## 6083                                                                                                                                              Republican options for stopping President Obamas immigration action may be limited because Congress doesnt control the immigration agencys finances.
## 6155                        How the presidents immigration plan could help cities like Detroit. A lot of notable investors  and a former head of the Small Business Administration  are investing in a payroll start-up. A seasoned traveler says you can be more productive on a plane without Wi-Fi.
## 6186                                                                                                                                                                                                                    Democrats are worried that President Obama cant close the deal on immigration.
## 6296                                                                                                                                                                                   Republicans are showing no sign of leaving Benghazi behind, and Democrats are balking at a bipartisan tax deal.
## 6371                                                                                                                                  Repercussions of Defense Secretary Chuck Hagels resignation and a grand jury decision over the death of Michael Brown are dominating conversation in Washington.
## 6431                                                                                                                       The potential political costs of the civil unrest in Ferguson, Mo.; speculation about Chuck Hagels replacement; and a look at Speaker John A. Boehners turkey brine recipe.
## 6585                                                                                                      There are pluses and minuses to collapsing oil prices. A California bill would require retailers and restaurants to pay double on holidays. A salad chain manages to attract tech investors.
## 6617                                                                                                                                                                                                                                              Unfinished business will make December a busy month.
## 6668                                                                                                                         A bakery in Ferguson, Mo., raises $250,000 through crowdfunding. How to avoid making rookie legal mistakes. And French business owners stage protests across the country.
## 6705                                                                                                                                                                                                                            President Obamas immigration action faces the Congressional spotlight.
## 6762                                                                                                                                           Private-sector hiring fell shy of estimates in November. There is big business in ugly Christmas sweaters. And the value of the worlds largest truffle.
## 6797                                                                                                                                                                            Republicans are ready to put out the welcome mat for the man President Obama is expected to pick as defense secretary.
## 6849                                                                                                                                Health care costs grew at the slowest rate in 2013 since 1960. There is $1.99 gas in Oklahoma City. And search engine optimization mistakes that are easy to make.
## 6889                                                                                                                                                            New Yorks chokehold case has thrown a volatile element into Loretta E. Lynchs confirmation hearings as United States attorney general.
## 6954                                                                 Starbucks plans a new chain of coffee shops for those who thought Starbucks coffee was too inexpensive. Four studies show that the Affordable Care Act is working incredibly well. And here is how to deal with a toxic employee.
## 6986                                                                                                                                                                                                          There is a sense of optimism on Capitol Hill that a package of spending bills will pass.
## 7076                                                                                                                             A Detroit burger restaurant pays $15 an hour  and makes money. How businesses can manage holiday parties. And how one business owner responded to an antigay request.
## 7104                                                                                                                                                                                                                The 113th Congress has plenty to do in what most lawmakers hope is its final week.
## 7160                                                                                                                                                                                Building a better jockstrap. Apple names its top apps of 2014. Could Philadelphia become an East Coast energy hub?
## 7191                                                                                                                                                                           A long-delayed Senate report documenting the C.I.A.s torture of terrorism detainees is set to become public on Tuesday.
## 7261                                                                                                                                                                Why arent these video stores dead? Amazon introduces a make-an-offer service. And a study puts a price on fake advertising clicks.
## 7294                                                                                                                                           The partisan split over the validity of the report on torture raises questions about how closely the two parties will cooperate on intelligence issues.
## 7354                                                                                                                    Why one business owner thinks Shark Tank is terrible for businesses. There will be no Internet sales tax for another year. And nine reasons business owners hate the holidays.
## 7396                                                                                                                                                        The 113th Congress is concluding with partisan brinksmanship and one last mad scramble for votes to pass a $1.1 trillion spending package.
## 7453                                                                                                                                                 Oil falls below $60 a barrel. Microsoft now accepts Bitcoin. And an Indianapolis gun-store owner offers tips for gun gifting this holiday season.
## 7483                                                                                                                     The near-death experience of a bipartisan, governmentwide funding bill on Thursday highlighted a fundamental problem with Congress at the moment: the lost art of compromise.
## 7563                                                                            The federal government spending bill quietly includes legislation affecting thousands of small businesses. Ten questions to ask before allowing employees to telecommute. And why retailers keep sending out catalogs.
## 7597                                                                                                                                                                 The lame-duck Senate gets back to business on Monday and is set to begin approving the executive and judicial branch nominations.
## 7667                                                                                                                                        Americans are spending more than ever on fancy dog food. Monday was the biggest shipping day of the year. And more companies are signing up for Apple Pay.
## 7688                                                                                                                      As they plow through backlogged presidential nominations, Senate Democrats are defending a rule change that allowed them to break a nominee filibuster with a majority vote.
## 7787                                                                                                                                                   Can the S.B.A. stay relevant? Will a subscription model work for the movies? Is it acceptable to read and respond to messages during a meeting?
## 7813                                                                                                                                                                                                                                         It has begun. The 2016 presidential campaign is underway.
## 7880                                                                                                                                                      Introducing the BlackBerry Classic. How a Papa Johns franchisee went from driver to owner. Are these the five best business leaders of 2014?
## 7906                                                                                    President Obamas surprise announcement on Wednesday that he is restoring diplomatic relations with Cuba touched off a political storm, and the repercussions will undoubtedly be felt through 2016 and beyond.
## 7973                                                                                                    The minimum wage is set to rise in 21 states by New Years Day. Amazon Prime Now delivered candy to a blogger in 23 minutes. Nebraska and Oklahoma sue Colorado to stop marijuana legalization.
## 8002                                                                                                                            President Obama will hold a news conference before leaving for a two-week vacation in Hawaii, and the questioning will likely center on plans to renew ties with Cuba.
## 8090                                                                                                                                                              President Obama is under pressure to address growing racial tensions after the fatal shootings of two New York City police officers.
## 8147                                                                                                                                                                          Mayor Bill de Blasio faces a political crisis over the fatal shooting of two police officers by an African-American man.
## 8191                                                                                Reince Priebus, chairman of the Republican National Committee, said recently that though the party was overhauling its primary process, it was not solely responsible for Mitt Romneys 2012 general election loss.
## 8309                                                                                                                                                                            President Obama is halfway through his year-end trip to Hawaii. While he has kept a low profile, hes been fairly busy.
## 8353                                                                                                                                                      Representative Michael G. Grimm of New York insisted last week that he would not resign from his seat. On Monday night, he changed his mind.
## 8397                                                                                                                                                            House Republicans are ending the year on a defensive note over Representative Steve Scalises 2002 speech to a white supremacist group.
##      WordCount             PubDate Popular UniqueID  .src Popular.fctr
## 73         475 2014-09-02 12:40:14       0       73 Train            N
## 162        405 2014-09-03 11:50:36       0      162 Train            N
## 260        416 2014-09-04 12:10:51       0      260 Train            N
## 356        513 2014-09-05 11:32:01       0      356 Train            N
## 510        351 2014-09-08 12:27:07       0      510 Train            N
## 607        545 2014-09-09 12:06:27       0      607 Train            N
## 719        493 2014-09-10 10:59:35       0      719 Train            N
## 800        406 2014-09-11 13:02:48       0      800 Train            N
## 883        462 2014-09-12 11:26:27       0      883 Train            N
## 1024       310 2014-09-15 11:08:29       0     1024 Train            N
## 1111       451 2014-09-16 12:05:13       0     1111 Train            N
## 1184       570 2014-09-17 12:45:59       0     1184 Train            N
## 1281       403 2014-09-18 12:59:36       0     1281 Train            N
## 1379       403 2014-09-19 11:01:27       0     1379 Train            N
## 1559       319 2014-09-22 11:24:48       0     1559 Train            N
## 1654         1 2014-09-23 15:08:19       0     1654 Train            N
## 1661       481 2014-09-23 14:07:19       0     1661 Train            N
## 1801       512 2014-09-24 12:10:05       0     1801 Train            N
## 1829      1132 2014-09-24 07:07:33       0     1829 Train            N
## 1913       432 2014-09-25 12:49:54       0     1913 Train            N
## 2051       380 2014-09-26 10:36:30       0     2051 Train            N
## 2194       398 2014-09-29 11:48:40       0     2194 Train            N
## 2220      1743 2014-09-29 07:37:48       0     2220 Train            N
## 2286       578 2014-09-30 13:02:47       0     2286 Train            N
## 2324      1440 2014-09-30 07:22:29       0     2324 Train            N
## 2397       407 2014-10-01 13:13:26       0     2397 Train            N
## 2429      1615 2014-10-01 07:40:13       0     2429 Train            N
## 2501       517 2014-10-02 12:49:06       0     2501 Train            N
## 2509         4 2014-10-02 12:00:51       0     2509 Train            N
## 2537      1014 2014-10-02 07:24:32       0     2537 Train            N
## 2605       516 2014-10-03 11:18:26       0     2605 Train            N
## 2638      1284 2014-10-03 06:48:36       0     2638 Train            N
## 2744       403 2014-10-06 12:09:58       0     2744 Train            N
## 2773      1409 2014-10-06 07:26:36       0     2773 Train            N
## 2850       492 2014-10-07 11:55:04       0     2850 Train            N
## 2878      1392 2014-10-07 07:27:21       0     2878 Train            N
## 2939       499 2014-10-08 13:06:45       0     2939 Train            N
## 2973      1199 2014-10-08 07:15:47       0     2973 Train            N
## 3028       503 2014-10-09 13:05:15       0     3028 Train            N
## 3071      1260 2014-10-09 07:11:58       0     3071 Train            N
## 3110       555 2014-10-10 15:47:27       0     3110 Train            N
## 3166      1472 2014-10-10 07:17:17       0     3166 Train            N
## 3251       435 2014-10-13 13:12:18       0     3251 Train            N
## 3282       699 2014-10-13 07:20:40       0     3282 Train            N
## 3351       488 2014-10-14 11:03:35       0     3351 Train            N
## 3374      1226 2014-10-14 07:09:49       0     3374 Train            N
## 3469       985 2014-10-15 07:18:33       0     3469 Train            N
## 3548       380 2014-10-16 11:32:34       0     3548 Train            N
## 3569      1385 2014-10-16 07:24:14       0     3569 Train            N
## 3648       510 2014-10-17 09:55:54       0     3648 Train            N
## 3666      1311 2014-10-17 07:18:06       0     3666 Train            N
## 3791       458 2014-10-20 11:00:41       0     3791 Train            N
## 3804      1207 2014-10-20 07:18:52       0     3804 Train            N
## 3874       438 2014-10-21 12:05:44       0     3874 Train            N
## 3905      1271 2014-10-21 07:01:50       0     3905 Train            N
## 3970       455 2014-10-22 12:42:29       0     3970 Train            N
## 4003      1334 2014-10-22 07:04:00       0     4003 Train            N
## 4057       625 2014-10-23 14:34:43       0     4057 Train            N
## 4107      1402 2014-10-23 07:09:31       0     4107 Train            N
## 4151       592 2014-10-24 14:43:38       0     4151 Train            N
## 4203       651 2014-10-24 07:27:29       0     4203 Train            N
## 4293       422 2014-10-27 12:42:01       0     4293 Train            N
## 4326      1519 2014-10-27 07:11:30       0     4326 Train            N
## 4392       471 2014-10-28 11:53:59       0     4392 Train            N
## 4418      1421 2014-10-28 07:19:46       0     4418 Train            N
## 4487       577 2014-10-29 13:07:42       0     4487 Train            N
## 4520      1351 2014-10-29 07:17:13       0     4520 Train            N
## 4567       554 2014-10-30 15:21:15       0     4567 Train            N
## 4616      1392 2014-10-30 07:20:05       0     4616 Train            N
## 4678       447 2014-10-31 12:29:26       0     4678 Train            N
## 4708      1566 2014-10-31 07:17:23       0     4708 Train            N
## 4805       575 2014-11-03 11:52:54       0     4805 Train            N
## 4829      1574 2014-11-03 07:13:00       0     4829 Train            N
## 4886       472 2014-11-04 12:05:27       0     4886 Train            N
## 4915      1561 2014-11-04 07:26:14       0     4915 Train            N
## 4974       484 2014-11-05 13:13:48       0     4974 Train            N
## 4999      1232 2014-11-05 08:18:14       0     4999 Train            N
## 5068       422 2014-11-06 12:54:54       0     5068 Train            N
## 5107       993 2014-11-06 07:07:31       0     5107 Train            N
## 5162       529 2014-11-07 12:26:24       0     5162 Train            N
## 5195      1219 2014-11-07 06:55:51       0     5195 Train            N
## 5288       458 2014-11-10 12:04:30       0     5288 Train            N
## 5315      1237 2014-11-10 06:46:06       0     5315 Train            N
## 5366       514 2014-11-11 12:16:24       0     5366 Train            N
## 5396      1285 2014-11-11 06:59:42       0     5396 Train            N
## 5449       615 2014-11-12 13:16:40       0     5449 Train            N
## 5488      1089 2014-11-12 06:42:58       0     5488 Train            N
## 5547       579 2014-11-13 11:36:02       0     5547 Train            N
## 5582      1543 2014-11-13 07:06:22       0     5582 Train            N
## 5635       470 2014-11-14 13:37:36       0     5635 Train            N
## 5676      1403 2014-11-14 07:04:28       0     5676 Train            N
## 5768       473 2014-11-17 12:32:12       0     5768 Train            N
## 5803      1460 2014-11-17 07:03:26       0     5803 Train            N
## 5862       566 2014-11-18 12:47:34       0     5862 Train            N
## 5890      1360 2014-11-18 07:02:55       0     5890 Train            N
## 5954       593 2014-11-19 13:01:52       0     5954 Train            N
## 5985      1412 2014-11-19 07:11:34       0     5985 Train            N
## 6045       475 2014-11-20 12:39:58       0     6045 Train            N
## 6083      1445 2014-11-20 07:04:06       0     6083 Train            N
## 6155       579 2014-11-21 11:40:41       0     6155 Train            N
## 6186      1378 2014-11-21 07:10:57       0     6186 Train            N
## 6296      1444 2014-11-24 07:16:46       0     6296 Train            N
## 6371      1394 2014-11-25 07:18:08       0     6371 Train            N
## 6431      1482 2014-11-26 07:09:21       0     6431 Train            N
## 6585       427 2014-12-01 13:00:03      NA     6585  Test         <NA>
## 6617      1519 2014-12-01 07:06:20      NA     6617  Test         <NA>
## 6668       394 2014-12-02 14:13:26      NA     6668  Test         <NA>
## 6705      1477 2014-12-02 07:10:55      NA     6705  Test         <NA>
## 6762       532 2014-12-03 13:01:28      NA     6762  Test         <NA>
## 6797      1493 2014-12-03 07:02:58      NA     6797  Test         <NA>
## 6849       614 2014-12-04 13:11:39      NA     6849  Test         <NA>
## 6889      1401 2014-12-04 07:07:08      NA     6889  Test         <NA>
## 6954       453 2014-12-05 12:28:52      NA     6954  Test         <NA>
## 6986      1493 2014-12-05 07:06:29      NA     6986  Test         <NA>
## 7076       496 2014-12-08 12:50:22      NA     7076  Test         <NA>
## 7104      1619 2014-12-08 07:20:44      NA     7104  Test         <NA>
## 7160       482 2014-12-09 12:17:00      NA     7160  Test         <NA>
## 7191      1444 2014-12-09 07:17:00      NA     7191  Test         <NA>
## 7261       549 2014-12-10 12:11:03      NA     7261  Test         <NA>
## 7294      1184 2014-12-10 07:28:16      NA     7294  Test         <NA>
## 7354       667 2014-12-11 12:41:20      NA     7354  Test         <NA>
## 7396      1544 2014-12-11 07:09:25      NA     7396  Test         <NA>
## 7453       447 2014-12-12 12:15:39      NA     7453  Test         <NA>
## 7483      1505 2014-12-12 07:13:51      NA     7483  Test         <NA>
## 7563       415 2014-12-15 13:03:48      NA     7563  Test         <NA>
## 7597      1617 2014-12-15 07:10:53      NA     7597  Test         <NA>
## 7667       531 2014-12-16 11:15:03      NA     7667  Test         <NA>
## 7688      1422 2014-12-16 07:04:56      NA     7688  Test         <NA>
## 7787       505 2014-12-17 11:26:01      NA     7787  Test         <NA>
## 7813      1597 2014-12-17 07:00:55      NA     7813  Test         <NA>
## 7880       515 2014-12-18 11:55:54      NA     7880  Test         <NA>
## 7906      1624 2014-12-18 07:02:55      NA     7906  Test         <NA>
## 7973       473 2014-12-19 12:48:24      NA     7973  Test         <NA>
## 8002      1549 2014-12-19 07:04:20      NA     8002  Test         <NA>
## 8090      1650 2014-12-22 07:12:21      NA     8090  Test         <NA>
## 8147      1653 2014-12-23 07:04:43      NA     8147  Test         <NA>
## 8191      1557 2014-12-24 06:52:14      NA     8191  Test         <NA>
## 8309      1516 2014-12-29 07:03:46      NA     8309  Test         <NA>
## 8353      1582 2014-12-30 07:04:58      NA     8353  Test         <NA>
## 8397      1616 2014-12-31 07:03:46      NA     8397  Test         <NA>
##      PubDate.year PubDate.month.fctr PubDate.date.fctr PubDate.wkday.fctr
## 73           2014                 09          (0.97,7]                  2
## 162          2014                 09          (0.97,7]                  3
## 260          2014                 09          (0.97,7]                  4
## 356          2014                 09          (0.97,7]                  5
## 510          2014                 09            (7,13]                  1
## 607          2014                 09            (7,13]                  2
## 719          2014                 09            (7,13]                  3
## 800          2014                 09            (7,13]                  4
## 883          2014                 09            (7,13]                  5
## 1024         2014                 09           (13,19]                  1
## 1111         2014                 09           (13,19]                  2
## 1184         2014                 09           (13,19]                  3
## 1281         2014                 09           (13,19]                  4
## 1379         2014                 09           (13,19]                  5
## 1559         2014                 09           (19,25]                  1
## 1654         2014                 09           (19,25]                  2
## 1661         2014                 09           (19,25]                  2
## 1801         2014                 09           (19,25]                  3
## 1829         2014                 09           (19,25]                  3
## 1913         2014                 09           (19,25]                  4
## 2051         2014                 09           (25,31]                  5
## 2194         2014                 09           (25,31]                  1
## 2220         2014                 09           (25,31]                  1
## 2286         2014                 09           (25,31]                  2
## 2324         2014                 09           (25,31]                  2
## 2397         2014                 10          (0.97,7]                  3
## 2429         2014                 10          (0.97,7]                  3
## 2501         2014                 10          (0.97,7]                  4
## 2509         2014                 10          (0.97,7]                  4
## 2537         2014                 10          (0.97,7]                  4
## 2605         2014                 10          (0.97,7]                  5
## 2638         2014                 10          (0.97,7]                  5
## 2744         2014                 10          (0.97,7]                  1
## 2773         2014                 10          (0.97,7]                  1
## 2850         2014                 10          (0.97,7]                  2
## 2878         2014                 10          (0.97,7]                  2
## 2939         2014                 10            (7,13]                  3
## 2973         2014                 10            (7,13]                  3
## 3028         2014                 10            (7,13]                  4
## 3071         2014                 10            (7,13]                  4
## 3110         2014                 10            (7,13]                  5
## 3166         2014                 10            (7,13]                  5
## 3251         2014                 10            (7,13]                  1
## 3282         2014                 10            (7,13]                  1
## 3351         2014                 10           (13,19]                  2
## 3374         2014                 10           (13,19]                  2
## 3469         2014                 10           (13,19]                  3
## 3548         2014                 10           (13,19]                  4
## 3569         2014                 10           (13,19]                  4
## 3648         2014                 10           (13,19]                  5
## 3666         2014                 10           (13,19]                  5
## 3791         2014                 10           (19,25]                  1
## 3804         2014                 10           (19,25]                  1
## 3874         2014                 10           (19,25]                  2
## 3905         2014                 10           (19,25]                  2
## 3970         2014                 10           (19,25]                  3
## 4003         2014                 10           (19,25]                  3
## 4057         2014                 10           (19,25]                  4
## 4107         2014                 10           (19,25]                  4
## 4151         2014                 10           (19,25]                  5
## 4203         2014                 10           (19,25]                  5
## 4293         2014                 10           (25,31]                  1
## 4326         2014                 10           (25,31]                  1
## 4392         2014                 10           (25,31]                  2
## 4418         2014                 10           (25,31]                  2
## 4487         2014                 10           (25,31]                  3
## 4520         2014                 10           (25,31]                  3
## 4567         2014                 10           (25,31]                  4
## 4616         2014                 10           (25,31]                  4
## 4678         2014                 10           (25,31]                  5
## 4708         2014                 10           (25,31]                  5
## 4805         2014                 11          (0.97,7]                  1
## 4829         2014                 11          (0.97,7]                  1
## 4886         2014                 11          (0.97,7]                  2
## 4915         2014                 11          (0.97,7]                  2
## 4974         2014                 11          (0.97,7]                  3
## 4999         2014                 11          (0.97,7]                  3
## 5068         2014                 11          (0.97,7]                  4
## 5107         2014                 11          (0.97,7]                  4
## 5162         2014                 11          (0.97,7]                  5
## 5195         2014                 11          (0.97,7]                  5
## 5288         2014                 11            (7,13]                  1
## 5315         2014                 11            (7,13]                  1
## 5366         2014                 11            (7,13]                  2
## 5396         2014                 11            (7,13]                  2
## 5449         2014                 11            (7,13]                  3
## 5488         2014                 11            (7,13]                  3
## 5547         2014                 11            (7,13]                  4
## 5582         2014                 11            (7,13]                  4
## 5635         2014                 11           (13,19]                  5
## 5676         2014                 11           (13,19]                  5
## 5768         2014                 11           (13,19]                  1
## 5803         2014                 11           (13,19]                  1
## 5862         2014                 11           (13,19]                  2
## 5890         2014                 11           (13,19]                  2
## 5954         2014                 11           (13,19]                  3
## 5985         2014                 11           (13,19]                  3
## 6045         2014                 11           (19,25]                  4
## 6083         2014                 11           (19,25]                  4
## 6155         2014                 11           (19,25]                  5
## 6186         2014                 11           (19,25]                  5
## 6296         2014                 11           (19,25]                  1
## 6371         2014                 11           (19,25]                  2
## 6431         2014                 11           (25,31]                  3
## 6585         2014                 12          (0.97,7]                  1
## 6617         2014                 12          (0.97,7]                  1
## 6668         2014                 12          (0.97,7]                  2
## 6705         2014                 12          (0.97,7]                  2
## 6762         2014                 12          (0.97,7]                  3
## 6797         2014                 12          (0.97,7]                  3
## 6849         2014                 12          (0.97,7]                  4
## 6889         2014                 12          (0.97,7]                  4
## 6954         2014                 12          (0.97,7]                  5
## 6986         2014                 12          (0.97,7]                  5
## 7076         2014                 12            (7,13]                  1
## 7104         2014                 12            (7,13]                  1
## 7160         2014                 12            (7,13]                  2
## 7191         2014                 12            (7,13]                  2
## 7261         2014                 12            (7,13]                  3
## 7294         2014                 12            (7,13]                  3
## 7354         2014                 12            (7,13]                  4
## 7396         2014                 12            (7,13]                  4
## 7453         2014                 12            (7,13]                  5
## 7483         2014                 12            (7,13]                  5
## 7563         2014                 12           (13,19]                  1
## 7597         2014                 12           (13,19]                  1
## 7667         2014                 12           (13,19]                  2
## 7688         2014                 12           (13,19]                  2
## 7787         2014                 12           (13,19]                  3
## 7813         2014                 12           (13,19]                  3
## 7880         2014                 12           (13,19]                  4
## 7906         2014                 12           (13,19]                  4
## 7973         2014                 12           (13,19]                  5
## 8002         2014                 12           (13,19]                  5
## 8090         2014                 12           (19,25]                  1
## 8147         2014                 12           (19,25]                  2
## 8191         2014                 12           (19,25]                  3
## 8309         2014                 12           (25,31]                  1
## 8353         2014                 12           (25,31]                  2
## 8397         2014                 12           (25,31]                  3
##      PubDate.hour PubDate.apm.fctr PubDate.minute PubDate.second
## 73             12               pm             40             14
## 162            11               am             50             36
## 260            12               pm             10             51
## 356            11               am             32              1
## 510            12               pm             27              7
## 607            12               pm              6             27
## 719            10               am             59             35
## 800            13               pm              2             48
## 883            11               am             26             27
## 1024           11               am              8             29
## 1111           12               pm              5             13
## 1184           12               pm             45             59
## 1281           12               pm             59             36
## 1379           11               am              1             27
## 1559           11               am             24             48
## 1654           15               pm              8             19
## 1661           14               pm              7             19
## 1801           12               pm             10              5
## 1829            7               am              7             33
## 1913           12               pm             49             54
## 2051           10               am             36             30
## 2194           11               am             48             40
## 2220            7               am             37             48
## 2286           13               pm              2             47
## 2324            7               am             22             29
## 2397           13               pm             13             26
## 2429            7               am             40             13
## 2501           12               pm             49              6
## 2509           12               pm              0             51
## 2537            7               am             24             32
## 2605           11               am             18             26
## 2638            6               am             48             36
## 2744           12               pm              9             58
## 2773            7               am             26             36
## 2850           11               am             55              4
## 2878            7               am             27             21
## 2939           13               pm              6             45
## 2973            7               am             15             47
## 3028           13               pm              5             15
## 3071            7               am             11             58
## 3110           15               pm             47             27
## 3166            7               am             17             17
## 3251           13               pm             12             18
## 3282            7               am             20             40
## 3351           11               am              3             35
## 3374            7               am              9             49
## 3469            7               am             18             33
## 3548           11               am             32             34
## 3569            7               am             24             14
## 3648            9               am             55             54
## 3666            7               am             18              6
## 3791           11               am              0             41
## 3804            7               am             18             52
## 3874           12               pm              5             44
## 3905            7               am              1             50
## 3970           12               pm             42             29
## 4003            7               am              4              0
## 4057           14               pm             34             43
## 4107            7               am              9             31
## 4151           14               pm             43             38
## 4203            7               am             27             29
## 4293           12               pm             42              1
## 4326            7               am             11             30
## 4392           11               am             53             59
## 4418            7               am             19             46
## 4487           13               pm              7             42
## 4520            7               am             17             13
## 4567           15               pm             21             15
## 4616            7               am             20              5
## 4678           12               pm             29             26
## 4708            7               am             17             23
## 4805           11               am             52             54
## 4829            7               am             13              0
## 4886           12               pm              5             27
## 4915            7               am             26             14
## 4974           13               pm             13             48
## 4999            8               am             18             14
## 5068           12               pm             54             54
## 5107            7               am              7             31
## 5162           12               pm             26             24
## 5195            6               am             55             51
## 5288           12               pm              4             30
## 5315            6               am             46              6
## 5366           12               pm             16             24
## 5396            6               am             59             42
## 5449           13               pm             16             40
## 5488            6               am             42             58
## 5547           11               am             36              2
## 5582            7               am              6             22
## 5635           13               pm             37             36
## 5676            7               am              4             28
## 5768           12               pm             32             12
## 5803            7               am              3             26
## 5862           12               pm             47             34
## 5890            7               am              2             55
## 5954           13               pm              1             52
## 5985            7               am             11             34
## 6045           12               pm             39             58
## 6083            7               am              4              6
## 6155           11               am             40             41
## 6186            7               am             10             57
## 6296            7               am             16             46
## 6371            7               am             18              8
## 6431            7               am              9             21
## 6585           13               pm              0              3
## 6617            7               am              6             20
## 6668           14               pm             13             26
## 6705            7               am             10             55
## 6762           13               pm              1             28
## 6797            7               am              2             58
## 6849           13               pm             11             39
## 6889            7               am              7              8
## 6954           12               pm             28             52
## 6986            7               am              6             29
## 7076           12               pm             50             22
## 7104            7               am             20             44
## 7160           12               pm             17              0
## 7191            7               am             17              0
## 7261           12               pm             11              3
## 7294            7               am             28             16
## 7354           12               pm             41             20
## 7396            7               am              9             25
## 7453           12               pm             15             39
## 7483            7               am             13             51
## 7563           13               pm              3             48
## 7597            7               am             10             53
## 7667           11               am             15              3
## 7688            7               am              4             56
## 7787           11               am             26              1
## 7813            7               am              0             55
## 7880           11               am             55             54
## 7906            7               am              2             55
## 7973           12               pm             48             24
## 8002            7               am              4             20
## 8090            7               am             12             21
## 8147            7               am              4             43
## 8191            6               am             52             14
## 8309            7               am              3             46
## 8353            7               am              4             58
## 8397            7               am              3             46
##      WordCount.log       .rnorm                         Headline.pfx
## 73       6.1654179  1.043143552 Today in (Politics|Small Business)::
## 162      6.0063532  0.199920484 Today in (Politics|Small Business)::
## 260      6.0330862 -1.180127174 Today in (Politics|Small Business)::
## 356      6.2422233  0.017860213 Today in (Politics|Small Business)::
## 510      5.8636312 -0.157797990 Today in (Politics|Small Business)::
## 607      6.3026190  0.802890842 Today in (Politics|Small Business)::
## 719      6.2025355  2.112627923 Today in (Politics|Small Business)::
## 800      6.0088132  1.126282928 Today in (Politics|Small Business)::
## 883      6.1377271  0.240304174 Today in (Politics|Small Business)::
## 1024     5.7397929  1.654941797 Today in (Politics|Small Business)::
## 1111     6.1136822 -0.396721691 Today in (Politics|Small Business)::
## 1184     6.3473892  0.489033101 Today in (Politics|Small Business)::
## 1281     6.0014149 -1.803013212 Today in (Politics|Small Business)::
## 1379     6.0014149  0.215797962 Today in (Politics|Small Business)::
## 1559     5.7683210 -1.248315797 Today in (Politics|Small Business)::
## 1654     0.6931472 -1.652377421                             Today in
## 1661     6.1779441  0.317923161 Today in (Politics|Small Business)::
## 1801     6.2402758  0.758747937 Today in (Politics|Small Business)::
## 1829     7.0326243  0.876678068 Today in (Politics|Small Business)::
## 1913     6.0707377  0.635005355 Today in (Politics|Small Business)::
## 2051     5.9427994 -0.448983035 Today in (Politics|Small Business)::
## 2194     5.9889614  0.007676873 Today in (Politics|Small Business)::
## 2220     7.4639366 -0.740468478 Today in (Politics|Small Business)::
## 2286     6.3613025  0.260709150 Today in (Politics|Small Business)::
## 2324     7.2730926  0.697079462 Today in (Politics|Small Business)::
## 2397     6.0112672  0.071634626 Today in (Politics|Small Business)::
## 2429     7.3877092 -1.188139233 Today in (Politics|Small Business)::
## 2501     6.2499752  0.740926096 Today in (Politics|Small Business)::
## 2509     1.6094379 -1.137460119                             Today in
## 2537     6.9226439  0.501806882 Today in (Politics|Small Business)::
## 2605     6.2480429 -1.648142642 Today in (Politics|Small Business)::
## 2638     7.1585140  0.973460427 Today in (Politics|Small Business)::
## 2744     6.0014149  0.270655775 Today in (Politics|Small Business)::
## 2773     7.2513450  0.732951505 Today in (Politics|Small Business)::
## 2850     6.2005092  0.004528593 Today in (Politics|Small Business)::
## 2878     7.2392150  0.258656814 Today in (Politics|Small Business)::
## 2939     6.2146081  1.604939498 Today in (Politics|Small Business)::
## 2973     7.0900768 -1.001472832 Today in (Politics|Small Business)::
## 3028     6.2225763 -0.895045428 Today in (Politics|Small Business)::
## 3071     7.1396603 -1.283628740 Today in (Politics|Small Business)::
## 3110     6.3207683 -1.689644272 Today in (Politics|Small Business)::
## 3166     7.2950564 -0.169058823 Today in (Politics|Small Business)::
## 3251     6.0776422  0.536851245 Today in (Politics|Small Business)::
## 3282     6.5510803 -1.160621742 Today in (Politics|Small Business)::
## 3351     6.1923625  0.210617996 Today in (Politics|Small Business)::
## 3374     7.1123274  0.207742201 Today in (Politics|Small Business)::
## 3469     6.8936564 -2.074391818 Today in (Politics|Small Business)::
## 3548     5.9427994 -1.058705076 Today in (Politics|Small Business)::
## 3569     7.2341772  0.031475591 Today in (Politics|Small Business)::
## 3648     6.2363696  2.327109843 Today in (Politics|Small Business)::
## 3666     7.1793080  1.395728162 Today in (Politics|Small Business)::
## 3791     6.1290502  1.187790357 Today in (Politics|Small Business)::
## 3804     7.0967214  0.141117667 Today in (Politics|Small Business)::
## 3874     6.0844994 -0.615078424 Today in (Politics|Small Business)::
## 3905     7.1483457 -1.236202914 Today in (Politics|Small Business)::
## 3970     6.1224928  0.971309113 Today in (Politics|Small Business)::
## 4003     7.1966866 -0.431638339 Today in (Politics|Small Business)::
## 4057     6.4393504  0.065778176 Today in (Politics|Small Business)::
## 4107     7.2463681 -1.232991226 Today in (Politics|Small Business)::
## 4151     6.3851944  0.680130494 Today in (Politics|Small Business)::
## 4203     6.4800446 -0.547088503 Today in (Politics|Small Business)::
## 4293     6.0473722  0.708371651 Today in (Politics|Small Business)::
## 4326     7.3264656 -0.018661367 Today in (Politics|Small Business)::
## 4392     6.1569790 -0.869485310 Today in (Politics|Small Business)::
## 4418     7.2598196  0.372541044 Today in (Politics|Small Business)::
## 4487     6.3595739  1.282858367 Today in (Politics|Small Business)::
## 4520     7.2093403  0.057658200 Today in (Politics|Small Business)::
## 4567     6.3189681 -1.554663531 Today in (Politics|Small Business)::
## 4616     7.2392150  0.372065991 Today in (Politics|Small Business)::
## 4678     6.1047932 -0.005781868 Today in (Politics|Small Business)::
## 4708     7.3569182  0.156538285 Today in (Politics|Small Business)::
## 4805     6.3561077 -1.186096247 Today in (Politics|Small Business)::
## 4829     7.3620106  1.560756775 Today in (Politics|Small Business)::
## 4886     6.1590954 -0.785504339 Today in (Politics|Small Business)::
## 4915     7.3537223  1.087295141 Today in (Politics|Small Business)::
## 4974     6.1841489 -0.495173878 Today in (Politics|Small Business)::
## 4999     7.1172055 -0.680279809 Today in (Politics|Small Business)::
## 5068     6.0473722 -0.462499112 Today in (Politics|Small Business)::
## 5107     6.9017372 -0.745547304 Today in (Politics|Small Business)::
## 5162     6.2728770  1.786397337 Today in (Politics|Small Business)::
## 5195     7.1066061  0.367367835 Today in (Politics|Small Business)::
## 5288     6.1290502 -0.671069010 Today in (Politics|Small Business)::
## 5315     7.1212525  1.129581517 Today in (Politics|Small Business)::
## 5366     6.2441669  1.400711603 Today in (Politics|Small Business)::
## 5396     7.1592919  0.145282800 Today in (Politics|Small Business)::
## 5449     6.4232470  0.983256026 Today in (Politics|Small Business)::
## 5488     6.9939330  0.025231998 Today in (Politics|Small Business)::
## 5547     6.3630281 -0.273080092 Today in (Politics|Small Business)::
## 5582     7.3421317 -1.590036801 Today in (Politics|Small Business)::
## 5635     6.1548581 -1.926880111 Today in (Politics|Small Business)::
## 5676     7.2470806 -0.915570627 Today in (Politics|Small Business)::
## 5768     6.1612073 -1.275986680 Today in (Politics|Small Business)::
## 5803     7.2868764 -0.520944139 Today in (Politics|Small Business)::
## 5862     6.3403593  1.971549999 Today in (Politics|Small Business)::
## 5890     7.2159750  0.896821166 Today in (Politics|Small Business)::
## 5954     6.3868793  0.294459560 Today in (Politics|Small Business)::
## 5985     7.2534704  0.155416787 Today in (Politics|Small Business)::
## 6045     6.1654179  0.813761008 Today in (Politics|Small Business)::
## 6083     7.2765564 -0.765350316 Today in (Politics|Small Business)::
## 6155     6.3630281 -0.688810220 Today in (Politics|Small Business)::
## 6186     7.2291139  1.129517872 Today in (Politics|Small Business)::
## 6296     7.2758646  1.094700429 Today in (Politics|Small Business)::
## 6371     7.2406497  0.389075807 Today in (Politics|Small Business)::
## 6431     7.3018223  0.375331962 Today in (Politics|Small Business)::
## 6585     6.0591232  0.765466587 Today in (Politics|Small Business)::
## 6617     7.3264656  0.739157863 Today in (Politics|Small Business)::
## 6668     5.9788858 -0.106963023 Today in (Politics|Small Business)::
## 6705     7.2984451 -1.535495408 Today in (Politics|Small Business)::
## 6762     6.2785214 -0.489404822 Today in (Politics|Small Business)::
## 6797     7.3092124 -0.493881884 Today in (Politics|Small Business)::
## 6849     6.4216223 -0.139456823 Today in (Politics|Small Business)::
## 6889     7.2456551  0.677523827 Today in (Politics|Small Business)::
## 6954     6.1180972 -0.855593458 Today in (Politics|Small Business)::
## 6986     7.3092124 -0.084614170 Today in (Politics|Small Business)::
## 7076     6.2085900 -1.854435036 Today in (Politics|Small Business)::
## 7104     7.3901814 -0.322874329 Today in (Politics|Small Business)::
## 7160     6.1800167  1.394658968 Today in (Politics|Small Business)::
## 7191     7.2758646  2.350096339 Today in (Politics|Small Business)::
## 7261     6.3099183 -1.183404167 Today in (Politics|Small Business)::
## 7294     7.0774981 -1.212831232 Today in (Politics|Small Business)::
## 7354     6.5042882  0.006215797 Today in (Politics|Small Business)::
## 7396     7.3427792 -0.930110871 Today in (Politics|Small Business)::
## 7453     6.1047932  1.717225089 Today in (Politics|Small Business)::
## 7483     7.3172124  1.342256732 Today in (Politics|Small Business)::
## 7563     6.0306853  0.300811487 Today in (Politics|Small Business)::
## 7597     7.3889461  0.179473672 Today in (Politics|Small Business)::
## 7667     6.2766435 -0.069669710 Today in (Politics|Small Business)::
## 7688     7.2605226  0.622137931 Today in (Politics|Small Business)::
## 7787     6.2265367  0.087596600 Today in (Politics|Small Business)::
## 7813     7.3765081  1.037583354 Today in (Politics|Small Business)::
## 7880     6.2461068 -1.700337816 Today in (Politics|Small Business)::
## 7906     7.3932631  0.395062529 Today in (Politics|Small Business)::
## 7973     6.1612073  1.831910892 Today in (Politics|Small Business)::
## 8002     7.3460102 -1.268559616 Today in (Politics|Small Business)::
## 8090     7.4091364  1.489842506 Today in (Politics|Small Business)::
## 8147     7.4109519 -0.636843769 Today in (Politics|Small Business)::
## 8191     7.3511582 -0.496235476 Today in (Politics|Small Business)::
## 8309     7.3244900 -0.763449554 Today in (Politics|Small Business)::
## 8353     7.3670771  0.549202768 Today in (Politics|Small Business)::
## 8397     7.3883279  0.639410569 Today in (Politics|Small Business)::
```

```r
#sav_entity_df <- glb_entity_df
#glb_entity_df <- sav_entity_df
glb_entity_df$Headline.pfx <- sapply(1:nrow(glb_entity_df), function(row_ix) 
    ifelse(all(tail(unlist(strsplit(glb_entity_df[row_ix, "Headline.pfx"], "")), 2) 
           == c(":", ":")), glb_entity_df[row_ix, "Headline.pfx"], "myMisc::"))

# unique(sapply(1: #180:190, #:190, #130:250,  #129:256,
#                   nrow(glb_entity_df), 
#               function(row_ix) 
#     ifelse(all(tail(unlist(strsplit(glb_entity_df[row_ix, "Headline.pfx"], "")), 2) 
#            == c(":", ":")), glb_entity_df[row_ix, "Headline.pfx"], "myMisc::")))

#glb_entity_df[c(8399, 8400), c("Headline", "Headline.pfx")]
print(unique(glb_entity_df$Headline.pfx))    
```

```
##  [1] "myMisc::"                            
##  [2] "19[0-9][0-9]::"                      
##  [3] "Daily (Clip )*Report::"              
##  [4] ".*Fashion Week::"                    
##  [5] "What We're::"                        
##  [6] "Pictures of the (Day|Year|.)::"      
##  [7] "Today in (Politics|Small Business)::"
##  [8] "Morning Agenda::"                    
##  [9] "New York Today::"                    
## [10] "6 Q's About the News::"              
## [11] "Test Yourself::"                     
## [12] "Word of the Day::"                   
## [13] "Verbatim::"                          
## [14] "First Draft::"                       
## [15] "The Daily Gift::"
```

```r
# Raw Data
sav_entity_df <- glb_entity_df
dsp_datagrp <- function(..., from=1, to=10, all=FALSE) {
    print((dsp_df <- orderBy(~-.n +Headline.pfx+NewsDesk+SectionName+SubsectionName,
                       mycreate_sqlxtab_df(glb_entity_df[sel_obs(...), ],
        c("Headline.pfx", "NewsDesk", "SectionName", "SubsectionName", 
          glb_rsp_var))))[ifelse(all, 1, from):
                          ifelse(all, nrow(dsp_df), min(to, nrow(dsp_df))), ])
}

### Mine Headlines with myMisc for patterns & create a separate Headline.pfx category
dsp_datagrp(all=TRUE)
```

```
##                             Headline.pfx NewsDesk      SectionName
## 1                               myMisc::                          
## 2                               myMisc:: Business     Business Day
## 3                               myMisc::  Culture             Arts
## 4                               myMisc::   TStyle                 
## 5                               myMisc::     OpEd          Opinion
## 6                               myMisc:: Business     Business Day
## 7                               myMisc::                          
## 8                               myMisc:: Business       Technology
## 9                               myMisc::  Foreign            World
## 10                              myMisc::  Culture             Arts
## 11                              myMisc::                      U.S.
## 12                        19[0-9][0-9]::  Foreign                 
## 13                              myMisc::     OpEd          Opinion
## 14                      .*Fashion Week::   TStyle                 
## 15                              myMisc::    Metro    N.Y. / Region
## 16                              myMisc::  Science           Health
## 17                              myMisc::                          
## 18                              myMisc::   Travel           Travel
## 19                              myMisc::     OpEd          Opinion
## 20                              myMisc:: Business Crosswords/Games
## 21                              myMisc::   Styles             U.S.
## 22                              myMisc:: Business       Technology
## 23                              myMisc:: Business     Business Day
## 24                              myMisc::                Multimedia
## 25                              myMisc::   TStyle                 
## 26                              myMisc::   Styles             U.S.
## 27                              myMisc:: Business     Business Day
## 28                              myMisc::  Science           Health
## 29                              myMisc::  Culture                 
## 30                              myMisc::   Styles                 
## 31                Daily (Clip )*Report::                          
## 32                      Morning Agenda:: Business     Business Day
## 33                6 Q's About the News::                      U.S.
## 36                              myMisc::                   Opinion
## 37                              myMisc::   Styles             U.S.
## 34                       Test Yourself::                      U.S.
## 35                     Word of the Day::                      U.S.
## 38                Daily (Clip )*Report:: Business       Technology
## 39                      New York Today::    Metro    N.Y. / Region
## 40                         First Draft::                          
## 41  Today in (Politics|Small Business):: Business     Business Day
## 42                              myMisc::  Foreign            World
## 43                              myMisc::  Science           Health
## 44        Pictures of the (Day|Year|.)::                Multimedia
## 45                              myMisc:: Business       Technology
## 46                              myMisc::  Culture             Arts
## 47                      .*Fashion Week::   Styles                 
## 48  Today in (Politics|Small Business)::                          
## 49                              myMisc::    Metro    N.Y. / Region
## 50                          What We're::                          
## 51                              myMisc::                      U.S.
## 52                        19[0-9][0-9]::  Foreign                 
## 53                              myMisc:: Business Crosswords/Games
## 54                            Verbatim::                          
## 55                              myMisc:: Magazine         Magazine
## 56                              myMisc::   Travel           Travel
## 57                              myMisc::                Multimedia
## 58                      The Daily Gift::   TStyle                 
## 59                              myMisc:: Business     Business Day
## 60                Daily (Clip )*Report::                          
## 63                              myMisc::  Foreign                 
## 61        Pictures of the (Day|Year|.)::                Multimedia
## 62                      The Daily Gift::   TStyle                 
## 64  Today in (Politics|Small Business)::                          
## 66                              myMisc::                   Opinion
## 65                      New York Today::    Metro    N.Y. / Region
## 67                              myMisc:: Business Crosswords/Games
## 68                Daily (Clip )*Report:: Business       Technology
## 69                              myMisc::     OpEd                 
## 70                       Test Yourself::                      U.S.
## 71                     Word of the Day::                      U.S.
## 72                      Morning Agenda:: Business     Business Day
## 73                              myMisc::                   Opinion
## 74                6 Q's About the News::                      U.S.
## 75                         First Draft::                          
## 76                              myMisc::    Metro    N.Y. / Region
## 77                              myMisc::   Styles                 
## 79                              myMisc::              Business Day
## 78  Today in (Politics|Small Business):: Business     Business Day
## 80                            Verbatim::                          
## 82                              myMisc::                      Arts
## 81                          What We're::                          
## 83                              myMisc::                   Opinion
## 84                        19[0-9][0-9]::                          
## 85                              myMisc::  Foreign            World
## 86                              myMisc::   TStyle                 
## 87                              myMisc::  Foreign                 
## 88                              myMisc::                   Opinion
## 89                              myMisc:: Business     Business Day
## 90                        19[0-9][0-9]::                          
## 92                              myMisc::          Crosswords/Games
## 93                              myMisc::                      Open
## 94                              myMisc::                   Opinion
## 95                              myMisc::                   Opinion
## 96                              myMisc::                    Travel
## 97                              myMisc:: Business                 
## 91                          What We're::     OpEd          Opinion
## 99                              myMisc::              Business Day
## 100                             myMisc::  Foreign            World
## 101                             myMisc:: Magazine         Magazine
## 98                      New York Today::    Metro    N.Y. / Region
## 103                             myMisc::                Multimedia
## 104                             myMisc::             N.Y. / Region
## 105                             myMisc::                      U.S.
## 106                             myMisc:: National                 
## 107                             myMisc:: National             U.S.
## 108                             myMisc::  Science                 
## 109                             myMisc::  Science                 
## 110                             myMisc::   Styles            Style
## 102                     The Daily Gift::                          
## 111                     .*Fashion Week::                          
## 112                     .*Fashion Week::  Culture             Arts
## 113                     .*Fashion Week::    Metro    N.Y. / Region
## 114                     .*Fashion Week::   Styles                 
## 121                             myMisc::              Business Day
## 122                             myMisc::          Crosswords/Games
## 123                             myMisc::                    Health
## 124                             myMisc::                      Open
## 125                             myMisc::                   Opinion
## 126                             myMisc::                   Opinion
## 127                             myMisc::                Technology
## 128                             myMisc::                    Travel
## 129                             myMisc::                      U.S.
## 130                             myMisc::                     World
## 131                             myMisc:: Business                 
## 132                             myMisc::  Culture                 
## 133                             myMisc::  Foreign            World
## 134                             myMisc::     OpEd                 
## 135                             myMisc::   Sports                 
## 136                             myMisc::   Sports           Sports
## 137                             myMisc::   Styles                 
## 138                             myMisc::   Styles           Health
## 139                             myMisc::   Travel           Travel
## 115                     New York Today::             N.Y. / Region
## 116       Pictures of the (Day|Year|.)::    Metro    N.Y. / Region
## 117                     The Daily Gift::                          
## 118 Today in (Politics|Small Business)::              Business Day
## 119 Today in (Politics|Small Business):: Business                 
## 120                         What We're::     OpEd          Opinion
##        SubsectionName Popular.fctr  .n
## 1                                N 920
## 2            Dealbook            N 802
## 3                                N 625
## 4                                N 554
## 5                                Y 407
## 6            Dealbook         <NA> 275
## 7                             <NA> 256
## 8                                N 220
## 9        Asia Pacific            N 200
## 10                            <NA> 162
## 11          Education            N 142
## 12                               N 141
## 13                            <NA> 140
## 14                               N 136
## 15                               N 121
## 16                               Y 119
## 17                               Y 115
## 18                               N 115
## 19                               N 109
## 20                               Y 103
## 21                               Y 100
## 22                            <NA>  93
## 23           Dealbook            Y  88
## 24                               N  86
## 25                            <NA>  83
## 26                               N  77
## 27     Small Business            N  76
## 28                               N  73
## 29                            <NA>  70
## 30                               N  70
## 31                               N  62
## 32           Dealbook            N  62
## 33          Education            N  61
## 36    Room For Debate            N  61
## 37                            <NA>  61
## 34          Education            N  61
## 35          Education            N  61
## 38                               N  60
## 39                               N  59
## 40                               N  58
## 41     Small Business            N  58
## 42       Asia Pacific         <NA>  55
## 43                            <NA>  55
## 44                               N  53
## 45                               Y  50
## 46                               Y  50
## 47                               N  46
## 48                               N  44
## 49                            <NA>  43
## 50                               N  41
## 51          Education         <NA>  40
## 52                            <NA>  39
## 53                            <NA>  38
## 54                               N  33
## 55                               N  31
## 56                            <NA>  31
## 57                            <NA>  30
## 58                               N  25
## 59     Small Business         <NA>  24
## 60                            <NA>  22
## 63                               N  22
## 61                            <NA>  22
## 62                            <NA>  22
## 64                            <NA>  21
## 66    Room For Debate         <NA>  20
## 65                            <NA>  20
## 67                               N  19
## 68                            <NA>  18
## 69                            <NA>  18
## 70          Education         <NA>  17
## 71          Education         <NA>  17
## 72           Dealbook         <NA>  16
## 73  The Public Editor            Y  16
## 74          Education         <NA>  15
## 75                            <NA>  14
## 76                               Y  14
## 77                            <NA>  14
## 79           Dealbook         <NA>  13
## 78     Small Business         <NA>  13
## 80                            <NA>  12
## 82                            <NA>  11
## 81                            <NA>  11
## 83  The Public Editor         <NA>  10
## 84                               N   9
## 85                               N   9
## 86                               Y   9
## 87                            <NA>   7
## 88                            <NA>   5
## 89     Small Business            Y   5
## 90                            <NA>   4
## 92                            <NA>   4
## 93                               N   4
## 94                               N   4
## 95  The Public Editor            N   4
## 96                            <NA>   4
## 97                               N   4
## 91                               N   4
## 99     Small Business         <NA>   3
## 100      Asia Pacific            Y   3
## 101                           <NA>   3
## 98                               Y   3
## 103                              Y   2
## 104                           <NA>   2
## 105                           <NA>   2
## 106                              N   2
## 107          Politics            N   2
## 108                           <NA>   2
## 109                              Y   2
## 110   Fashion & Style            N   2
## 102                           <NA>   2
## 111                              N   1
## 112                           <NA>   1
## 113                              N   1
## 114                           <NA>   1
## 121    Small Business            N   1
## 122                              N   1
## 123                              Y   1
## 124                           <NA>   1
## 125                              Y   1
## 126   Room For Debate            Y   1
## 127                           <NA>   1
## 128                              N   1
## 129                              N   1
## 130      Asia Pacific         <NA>   1
## 131                              Y   1
## 132                              N   1
## 133                           <NA>   1
## 134                              Y   1
## 135                              N   1
## 136                              N   1
## 137                              Y   1
## 138                              N   1
## 139                              Y   1
## 115                           <NA>   1
## 116                           <NA>   1
## 117                              N   1
## 118    Small Business         <NA>   1
## 119                           <NA>   1
## 120                           <NA>   1
```

```r
sav_entity_df <- glb_entity_df
#glb_entity_df <- sav_entity_df
all.equal(sav_entity_df, glb_entity_df)
```

```
## [1] TRUE
```

```r
dsp_datagrp(Headline.pfx="What We're::", all=TRUE)
```

```
##   Headline.pfx NewsDesk SectionName SubsectionName Popular.fctr .n
## 1 What We're::                                                N 41
## 2 What We're::                                             <NA> 11
## 3 What We're::     OpEd     Opinion                           N  4
## 4 What We're::     OpEd     Opinion                        <NA>  1
```

```r
#dsp_obs(Headline.pfx="What We're::")
glb_entity_df[sel_obs(NewsDesk=""), "NewsDesk"] <- 
    glb_entity_df[sel_obs(NewsDesk=""), "Headline.pfx"] 
glb_entity_df[sel_obs(SectionName=""), "SectionName"] <- 
    glb_entity_df[sel_obs(SectionName=""), "NewsDesk"] 
glb_entity_df$SubsectionName <- sapply(1:nrow(glb_entity_df), function(row_ix) 
    ifelse((orig_val <- glb_entity_df[row_ix, "SubsectionName"]) == "",
            paste(glb_entity_df[row_ix, "NewsDesk"], 
                  glb_entity_df[row_ix, "SectionName"], 
        sep=ifelse(grepl("::$", glb_entity_df[row_ix, "NewsDesk"]), "", "::")), 
            orig_val))
dsp_datagrp(Headline.pfx="What We're::", all=TRUE)
```

```
##   Headline.pfx     NewsDesk  SectionName           SubsectionName
## 1 What We're:: What We're:: What We're:: What We're::What We're::
## 2 What We're:: What We're:: What We're:: What We're::What We're::
## 3 What We're::         OpEd      Opinion            OpEd::Opinion
## 4 What We're::         OpEd      Opinion            OpEd::Opinion
##   Popular.fctr .n
## 1            N 41
## 2         <NA> 11
## 3            N  4
## 4         <NA>  1
```

```r
dsp_datagrp(all=TRUE)
```

```
##                             Headline.pfx
## 1                               myMisc::
## 2                               myMisc::
## 3                               myMisc::
## 4                               myMisc::
## 5                               myMisc::
## 6                               myMisc::
## 7                               myMisc::
## 8                               myMisc::
## 9                               myMisc::
## 10                              myMisc::
## 11                              myMisc::
## 12                        19[0-9][0-9]::
## 13                              myMisc::
## 14                      .*Fashion Week::
## 15                              myMisc::
## 16                              myMisc::
## 18                              myMisc::
## 17                              myMisc::
## 19                              myMisc::
## 20                              myMisc::
## 21                              myMisc::
## 22                              myMisc::
## 23                              myMisc::
## 24                              myMisc::
## 25                              myMisc::
## 26                              myMisc::
## 27                              myMisc::
## 28                              myMisc::
## 29                              myMisc::
## 30                              myMisc::
## 31                Daily (Clip )*Report::
## 32                      Morning Agenda::
## 33                6 Q's About the News::
## 37                              myMisc::
## 36                              myMisc::
## 34                       Test Yourself::
## 35                     Word of the Day::
## 38                Daily (Clip )*Report::
## 39                      New York Today::
## 40                         First Draft::
## 41  Today in (Politics|Small Business)::
## 42                              myMisc::
## 43                              myMisc::
## 44        Pictures of the (Day|Year|.)::
## 45                              myMisc::
## 46                              myMisc::
## 47                      .*Fashion Week::
## 48  Today in (Politics|Small Business)::
## 49                              myMisc::
## 50                          What We're::
## 51                              myMisc::
## 52                        19[0-9][0-9]::
## 53                              myMisc::
## 54                            Verbatim::
## 55                              myMisc::
## 56                              myMisc::
## 57                              myMisc::
## 58                      The Daily Gift::
## 59                              myMisc::
## 60                Daily (Clip )*Report::
## 63                              myMisc::
## 61        Pictures of the (Day|Year|.)::
## 62                      The Daily Gift::
## 64  Today in (Politics|Small Business)::
## 66                              myMisc::
## 65                      New York Today::
## 67                              myMisc::
## 68                Daily (Clip )*Report::
## 69                              myMisc::
## 70                       Test Yourself::
## 71                     Word of the Day::
## 72                      Morning Agenda::
## 73                              myMisc::
## 74                6 Q's About the News::
## 75                         First Draft::
## 76                              myMisc::
## 77                              myMisc::
## 79                              myMisc::
## 78  Today in (Politics|Small Business)::
## 80                            Verbatim::
## 82                              myMisc::
## 81                          What We're::
## 83                              myMisc::
## 84                        19[0-9][0-9]::
## 85                              myMisc::
## 86                              myMisc::
## 87                              myMisc::
## 88                              myMisc::
## 89                              myMisc::
## 90                        19[0-9][0-9]::
## 92                              myMisc::
## 93                              myMisc::
## 94                              myMisc::
## 96                              myMisc::
## 95                              myMisc::
## 97                              myMisc::
## 91                          What We're::
## 99                              myMisc::
## 100                             myMisc::
## 101                             myMisc::
## 98                      New York Today::
## 109                             myMisc::
## 110                             myMisc::
## 111                             myMisc::
## 103                             myMisc::
## 104                             myMisc::
## 105                             myMisc::
## 106                             myMisc::
## 107                             myMisc::
## 108                             myMisc::
## 102                     The Daily Gift::
## 112                     .*Fashion Week::
## 113                     .*Fashion Week::
## 114                     .*Fashion Week::
## 115                     .*Fashion Week::
## 122                             myMisc::
## 123                             myMisc::
## 124                             myMisc::
## 129                             myMisc::
## 130                             myMisc::
## 131                             myMisc::
## 132                             myMisc::
## 134                             myMisc::
## 133                             myMisc::
## 135                             myMisc::
## 136                             myMisc::
## 137                             myMisc::
## 138                             myMisc::
## 125                             myMisc::
## 126                             myMisc::
## 127                             myMisc::
## 128                             myMisc::
## 116                     New York Today::
## 117       Pictures of the (Day|Year|.)::
## 118                     The Daily Gift::
## 119 Today in (Politics|Small Business)::
## 120 Today in (Politics|Small Business)::
## 121                         What We're::
##                                 NewsDesk
## 1                               myMisc::
## 2                               Business
## 3                                Culture
## 4                                 TStyle
## 5                                   OpEd
## 6                               Business
## 7                               myMisc::
## 8                               Business
## 9                                Foreign
## 10                               Culture
## 11                              myMisc::
## 12                               Foreign
## 13                                  OpEd
## 14                                TStyle
## 15                                 Metro
## 16                               Science
## 18                              myMisc::
## 17                                Travel
## 19                                  OpEd
## 20                              Business
## 21                                Styles
## 22                              Business
## 23                              Business
## 24                              myMisc::
## 25                                TStyle
## 26                                Styles
## 27                              Business
## 28                               Science
## 29                               Culture
## 30                                Styles
## 31                Daily (Clip )*Report::
## 32                              Business
## 33                6 Q's About the News::
## 37                              myMisc::
## 36                                Styles
## 34                       Test Yourself::
## 35                     Word of the Day::
## 38                              Business
## 39                                 Metro
## 40                         First Draft::
## 41                              Business
## 42                               Foreign
## 43                               Science
## 44        Pictures of the (Day|Year|.)::
## 45                              Business
## 46                               Culture
## 47                                Styles
## 48  Today in (Politics|Small Business)::
## 49                                 Metro
## 50                          What We're::
## 51                              myMisc::
## 52                               Foreign
## 53                              Business
## 54                            Verbatim::
## 55                              Magazine
## 56                                Travel
## 57                              myMisc::
## 58                                TStyle
## 59                              Business
## 60                Daily (Clip )*Report::
## 63                               Foreign
## 61        Pictures of the (Day|Year|.)::
## 62                                TStyle
## 64  Today in (Politics|Small Business)::
## 66                              myMisc::
## 65                                 Metro
## 67                              Business
## 68                              Business
## 69                                  OpEd
## 70                       Test Yourself::
## 71                     Word of the Day::
## 72                              Business
## 73                              myMisc::
## 74                6 Q's About the News::
## 75                         First Draft::
## 76                                 Metro
## 77                                Styles
## 79                              myMisc::
## 78                              Business
## 80                            Verbatim::
## 82                              myMisc::
## 81                          What We're::
## 83                              myMisc::
## 84                        19[0-9][0-9]::
## 85                               Foreign
## 86                                TStyle
## 87                               Foreign
## 88                              Business
## 89                              myMisc::
## 90                        19[0-9][0-9]::
## 92                              Business
## 93                              myMisc::
## 94                              myMisc::
## 96                              myMisc::
## 95                              myMisc::
## 97                              myMisc::
## 91                                  OpEd
## 99                               Foreign
## 100                             Magazine
## 101                             myMisc::
## 98                                 Metro
## 109                             myMisc::
## 110                             myMisc::
## 111                             myMisc::
## 103                             National
## 104                             National
## 105                              Science
## 106                              Science
## 107                               Sports
## 108                               Styles
## 102                     The Daily Gift::
## 112                     .*Fashion Week::
## 113                              Culture
## 114                                Metro
## 115                               Styles
## 122                             Business
## 123                              Culture
## 124                              Foreign
## 129                             myMisc::
## 130                             myMisc::
## 131                             myMisc::
## 132                             myMisc::
## 134                             myMisc::
## 133                             myMisc::
## 135                             myMisc::
## 136                             myMisc::
## 137                             myMisc::
## 138                             myMisc::
## 125                                 OpEd
## 126                               Styles
## 127                               Styles
## 128                               Travel
## 116                     New York Today::
## 117                                Metro
## 118                     The Daily Gift::
## 119                             Business
## 120 Today in (Politics|Small Business)::
## 121                                 OpEd
##                              SectionName
## 1                               myMisc::
## 2                           Business Day
## 3                                   Arts
## 4                                 TStyle
## 5                                Opinion
## 6                           Business Day
## 7                               myMisc::
## 8                             Technology
## 9                                  World
## 10                                  Arts
## 11                                  U.S.
## 12                               Foreign
## 13                               Opinion
## 14                                TStyle
## 15                         N.Y. / Region
## 16                                Health
## 18                              myMisc::
## 17                                Travel
## 19                               Opinion
## 20                      Crosswords/Games
## 21                                  U.S.
## 22                            Technology
## 23                          Business Day
## 24                            Multimedia
## 25                                TStyle
## 26                                  U.S.
## 27                          Business Day
## 28                                Health
## 29                               Culture
## 30                                Styles
## 31                Daily (Clip )*Report::
## 32                          Business Day
## 33                                  U.S.
## 37                               Opinion
## 36                                  U.S.
## 34                                  U.S.
## 35                                  U.S.
## 38                            Technology
## 39                         N.Y. / Region
## 40                         First Draft::
## 41                          Business Day
## 42                                 World
## 43                                Health
## 44                            Multimedia
## 45                            Technology
## 46                                  Arts
## 47                                Styles
## 48  Today in (Politics|Small Business)::
## 49                         N.Y. / Region
## 50                          What We're::
## 51                                  U.S.
## 52                               Foreign
## 53                      Crosswords/Games
## 54                            Verbatim::
## 55                              Magazine
## 56                                Travel
## 57                            Multimedia
## 58                                TStyle
## 59                          Business Day
## 60                Daily (Clip )*Report::
## 63                               Foreign
## 61                            Multimedia
## 62                                TStyle
## 64  Today in (Politics|Small Business)::
## 66                               Opinion
## 65                         N.Y. / Region
## 67                      Crosswords/Games
## 68                            Technology
## 69                                  OpEd
## 70                                  U.S.
## 71                                  U.S.
## 72                          Business Day
## 73                               Opinion
## 74                                  U.S.
## 75                         First Draft::
## 76                         N.Y. / Region
## 77                                Styles
## 79                          Business Day
## 78                          Business Day
## 80                            Verbatim::
## 82                                  Arts
## 81                          What We're::
## 83                               Opinion
## 84                        19[0-9][0-9]::
## 85                                 World
## 86                                TStyle
## 87                               Foreign
## 88                          Business Day
## 89                               Opinion
## 90                        19[0-9][0-9]::
## 92                              Business
## 93                      Crosswords/Games
## 94                                  Open
## 96                               Opinion
## 95                               Opinion
## 97                                Travel
## 91                               Opinion
## 99                                 World
## 100                             Magazine
## 101                         Business Day
## 98                         N.Y. / Region
## 109                           Multimedia
## 110                        N.Y. / Region
## 111                                 U.S.
## 103                             National
## 104                                 U.S.
## 105                              Science
## 106                              Science
## 107                               Sports
## 108                                Style
## 102                     The Daily Gift::
## 112                     .*Fashion Week::
## 113                                 Arts
## 114                        N.Y. / Region
## 115                               Styles
## 122                             Business
## 123                              Culture
## 124                                World
## 129                         Business Day
## 130                     Crosswords/Games
## 131                               Health
## 132                                 Open
## 134                              Opinion
## 133                              Opinion
## 135                           Technology
## 136                               Travel
## 137                                 U.S.
## 138                                World
## 125                                 OpEd
## 126                               Health
## 127                               Styles
## 128                               Travel
## 116                        N.Y. / Region
## 117                        N.Y. / Region
## 118                     The Daily Gift::
## 119                             Business
## 120                         Business Day
## 121                              Opinion
##                                                               SubsectionName
## 1                                                           myMisc::myMisc::
## 2                                                                   Dealbook
## 3                                                              Culture::Arts
## 4                                                             TStyle::TStyle
## 5                                                              OpEd::Opinion
## 6                                                                   Dealbook
## 7                                                           myMisc::myMisc::
## 8                                                       Business::Technology
## 9                                                               Asia Pacific
## 10                                                             Culture::Arts
## 11                                                                 Education
## 12                                                          Foreign::Foreign
## 13                                                             OpEd::Opinion
## 14                                                            TStyle::TStyle
## 15                                                      Metro::N.Y. / Region
## 16                                                           Science::Health
## 18                                                          myMisc::myMisc::
## 17                                                            Travel::Travel
## 19                                                             OpEd::Opinion
## 20                                                Business::Crosswords/Games
## 21                                                              Styles::U.S.
## 22                                                      Business::Technology
## 23                                                                  Dealbook
## 24                                                        myMisc::Multimedia
## 25                                                            TStyle::TStyle
## 26                                                              Styles::U.S.
## 27                                                            Small Business
## 28                                                           Science::Health
## 29                                                          Culture::Culture
## 30                                                            Styles::Styles
## 31                              Daily (Clip )*Report::Daily (Clip )*Report::
## 32                                                                  Dealbook
## 33                                                                 Education
## 37                                                           Room For Debate
## 36                                                              Styles::U.S.
## 34                                                                 Education
## 35                                                                 Education
## 38                                                      Business::Technology
## 39                                                      Metro::N.Y. / Region
## 40                                                First Draft::First Draft::
## 41                                                            Small Business
## 42                                                              Asia Pacific
## 43                                                           Science::Health
## 44                                  Pictures of the (Day|Year|.)::Multimedia
## 45                                                      Business::Technology
## 46                                                             Culture::Arts
## 47                                                            Styles::Styles
## 48  Today in (Politics|Small Business)::Today in (Politics|Small Business)::
## 49                                                      Metro::N.Y. / Region
## 50                                                  What We're::What We're::
## 51                                                                 Education
## 52                                                          Foreign::Foreign
## 53                                                Business::Crosswords/Games
## 54                                                      Verbatim::Verbatim::
## 55                                                        Magazine::Magazine
## 56                                                            Travel::Travel
## 57                                                        myMisc::Multimedia
## 58                                                            TStyle::TStyle
## 59                                                            Small Business
## 60                              Daily (Clip )*Report::Daily (Clip )*Report::
## 63                                                          Foreign::Foreign
## 61                                  Pictures of the (Day|Year|.)::Multimedia
## 62                                                            TStyle::TStyle
## 64  Today in (Politics|Small Business)::Today in (Politics|Small Business)::
## 66                                                           Room For Debate
## 65                                                      Metro::N.Y. / Region
## 67                                                Business::Crosswords/Games
## 68                                                      Business::Technology
## 69                                                                OpEd::OpEd
## 70                                                                 Education
## 71                                                                 Education
## 72                                                                  Dealbook
## 73                                                         The Public Editor
## 74                                                                 Education
## 75                                                First Draft::First Draft::
## 76                                                      Metro::N.Y. / Region
## 77                                                            Styles::Styles
## 79                                                                  Dealbook
## 78                                                            Small Business
## 80                                                      Verbatim::Verbatim::
## 82                                                              myMisc::Arts
## 81                                                  What We're::What We're::
## 83                                                         The Public Editor
## 84                                              19[0-9][0-9]::19[0-9][0-9]::
## 85                                                            Foreign::World
## 86                                                            TStyle::TStyle
## 87                                                          Foreign::Foreign
## 88                                                            Small Business
## 89                                                           myMisc::Opinion
## 90                                              19[0-9][0-9]::19[0-9][0-9]::
## 92                                                        Business::Business
## 93                                                  myMisc::Crosswords/Games
## 94                                                              myMisc::Open
## 96                                                           myMisc::Opinion
## 95                                                         The Public Editor
## 97                                                            myMisc::Travel
## 91                                                             OpEd::Opinion
## 99                                                              Asia Pacific
## 100                                                       Magazine::Magazine
## 101                                                           Small Business
## 98                                                      Metro::N.Y. / Region
## 109                                                       myMisc::Multimedia
## 110                                                    myMisc::N.Y. / Region
## 111                                                             myMisc::U.S.
## 103                                                       National::National
## 104                                                                 Politics
## 105                                                         Science::Science
## 106                                                         Science::Science
## 107                                                           Sports::Sports
## 108                                                          Fashion & Style
## 102                                         The Daily Gift::The Daily Gift::
## 112                                         .*Fashion Week::.*Fashion Week::
## 113                                                            Culture::Arts
## 114                                                     Metro::N.Y. / Region
## 115                                                           Styles::Styles
## 122                                                       Business::Business
## 123                                                         Culture::Culture
## 124                                                           Foreign::World
## 129                                                           Small Business
## 130                                                 myMisc::Crosswords/Games
## 131                                                           myMisc::Health
## 132                                                             myMisc::Open
## 134                                                          myMisc::Opinion
## 133                                                          Room For Debate
## 135                                                       myMisc::Technology
## 136                                                           myMisc::Travel
## 137                                                             myMisc::U.S.
## 138                                                             Asia Pacific
## 125                                                               OpEd::OpEd
## 126                                                           Styles::Health
## 127                                                           Styles::Styles
## 128                                                           Travel::Travel
## 116                                            New York Today::N.Y. / Region
## 117                                                     Metro::N.Y. / Region
## 118                                         The Daily Gift::The Daily Gift::
## 119                                                       Business::Business
## 120                                                           Small Business
## 121                                                            OpEd::Opinion
##     Popular.fctr  .n
## 1              N 920
## 2              N 802
## 3              N 625
## 4              N 554
## 5              Y 407
## 6           <NA> 275
## 7           <NA> 256
## 8              N 220
## 9              N 200
## 10          <NA> 162
## 11             N 142
## 12             N 141
## 13          <NA> 140
## 14             N 136
## 15             N 121
## 16             Y 119
## 18             Y 115
## 17             N 115
## 19             N 109
## 20             Y 103
## 21             Y 100
## 22          <NA>  93
## 23             Y  88
## 24             N  86
## 25          <NA>  83
## 26             N  77
## 27             N  76
## 28             N  73
## 29          <NA>  70
## 30             N  70
## 31             N  62
## 32             N  62
## 33             N  61
## 37             N  61
## 36          <NA>  61
## 34             N  61
## 35             N  61
## 38             N  60
## 39             N  59
## 40             N  58
## 41             N  58
## 42          <NA>  55
## 43          <NA>  55
## 44             N  53
## 45             Y  50
## 46             Y  50
## 47             N  46
## 48             N  44
## 49          <NA>  43
## 50             N  41
## 51          <NA>  40
## 52          <NA>  39
## 53          <NA>  38
## 54             N  33
## 55             N  31
## 56          <NA>  31
## 57          <NA>  30
## 58             N  25
## 59          <NA>  24
## 60          <NA>  22
## 63             N  22
## 61          <NA>  22
## 62          <NA>  22
## 64          <NA>  21
## 66          <NA>  20
## 65          <NA>  20
## 67             N  19
## 68          <NA>  18
## 69          <NA>  18
## 70          <NA>  17
## 71          <NA>  17
## 72          <NA>  16
## 73             Y  16
## 74          <NA>  15
## 75          <NA>  14
## 76             Y  14
## 77          <NA>  14
## 79          <NA>  13
## 78          <NA>  13
## 80          <NA>  12
## 82          <NA>  11
## 81          <NA>  11
## 83          <NA>  10
## 84             N   9
## 85             N   9
## 86             Y   9
## 87          <NA>   7
## 88             Y   5
## 89          <NA>   5
## 90          <NA>   4
## 92             N   4
## 93          <NA>   4
## 94             N   4
## 96             N   4
## 95             N   4
## 97          <NA>   4
## 91             N   4
## 99             Y   3
## 100         <NA>   3
## 101         <NA>   3
## 98             Y   3
## 109            Y   2
## 110         <NA>   2
## 111         <NA>   2
## 103            N   2
## 104            N   2
## 105         <NA>   2
## 106            Y   2
## 107            N   2
## 108            N   2
## 102         <NA>   2
## 112            N   1
## 113         <NA>   1
## 114            N   1
## 115         <NA>   1
## 122            Y   1
## 123            N   1
## 124         <NA>   1
## 129            N   1
## 130            N   1
## 131            Y   1
## 132         <NA>   1
## 134            Y   1
## 133            Y   1
## 135         <NA>   1
## 136            N   1
## 137            N   1
## 138         <NA>   1
## 125            Y   1
## 126            N   1
## 127            Y   1
## 128            Y   1
## 116         <NA>   1
## 117         <NA>   1
## 118            N   1
## 119         <NA>   1
## 120         <NA>   1
## 121         <NA>   1
```

```r
# ## May we group all 1914::Foreign into Education(history) for SubsectionName ?
# ### Will it will clash with other entries in Education for SubsectionName ?
# print(orderBy(~ Headline.pfx+NewsDesk+SectionName+SubsectionName,
#         mycreate_sqlxtab_df(glb_entity_df[sel_obs(SubsectionName="Education"), ],
#     c(glb_rsp_var, "Headline.pfx", "NewsDesk", "SectionName", "SubsectionName"))))
# #### No all other Education entries have U.S. as SectionName
# ### May we group all 1914::Foreign into World for SectionName ?
# print(orderBy(~ Headline.pfx+NewsDesk+SectionName+SubsectionName,
#         mycreate_sqlxtab_df(glb_entity_df[sel_obs(SectionName="World"), ],
#     c(glb_rsp_var, "Headline.pfx", "NewsDesk", "SectionName", "SubsectionName"))))
# #### Yes, all other World entries don't have Education in SubsectionName    
# print(head(orderBy(~ Headline.pfx+NewsDesk+SectionName+SubsectionName,
#                    mycreate_sqlxtab_df(glb_entity_df[sel_obs(), ],
#     c(glb_rsp_var, "Headline.pfx", "NewsDesk", "SectionName", "SubsectionName"))),
#     10))
# glb_entity_df[glb_entity_df$Headline.pfx == "1914::", "SectionName"] <- "World"
# glb_entity_df[glb_entity_df$Headline.pfx == "1914::", "SubsectionName"] <- "Education"
# 
# ## What are the headlines for 1939::blank::blank::blank ?
# dsp_obs(Headline.pfx="1939::", NewsDesk="")
# ### It is Foreign stuff
# glb_entity_df[glb_entity_df$Headline.pfx == "1939::", "NewsDesk"] <- "Foreign"
# glb_entity_df[glb_entity_df$Headline.pfx == "1939::", "SectionName"] <- "World"
# glb_entity_df[glb_entity_df$Headline.pfx == "1939::", "SubsectionName"] <- "Education"
# 
# ## What are the headlines for 1964::blank::blank::blank ?
# dsp_obs(Headline.pfx="1964::", NewsDesk="")
# ### It is Foreign stuff
# glb_entity_df[glb_entity_df$Headline.pfx == "1964::", "NewsDesk"] <- "Foreign"
# glb_entity_df[glb_entity_df$Headline.pfx == "1964::", "SectionName"] <- "World"
# glb_entity_df[glb_entity_df$Headline.pfx == "1964::", "SubsectionName"] <- "Education"
# 
# ## Grouping 6 Q's About the News:: into NewsDesk=myEducation - A new category
# glb_entity_df[glb_entity_df$Headline.pfx == "6 Q's About the News::", "NewsDesk"] <-
#     "myEducation"
# dsp_datagrp(7, 17)
# 
# ## Sample headlines from Daily Clip Report:: ?
# dsp_obs(Headline.pfx="Daily Clip Report::")
# ## Grouping Daily Clip Report:: into NewsDesk=myCollection - A new category
# ###                                 SectionName=myCollection - A new category
# ###                                 SubsectionName=myCollection - A new category
# glb_entity_df[glb_entity_df$Headline.pfx == "Daily Clip Report::", "NewsDesk"] <- "myCollection"
# glb_entity_df[glb_entity_df$Headline.pfx == "Daily Clip Report::", "SectionName"] <- "myCollection"
# glb_entity_df[glb_entity_df$Headline.pfx == "Daily Clip Report::", "SubsectionName"] <- "myCollection"
# 
# ## Sample headlines from Daily Report:: ?
# dsp_obs(Headline.pfx="Daily Report::")
# ### What are the SubsectionNames for <>::Business::Technology ?
# print(orderBy(~ Headline.pfx+NewsDesk+SectionName+SubsectionName,
#         mycreate_sqlxtab_df(glb_entity_df[sel_obs(
#             NewsDesk="Business", SectionName="Technology"), ],
#     c(glb_rsp_var, "Headline.pfx", "NewsDesk", "SectionName", "SubsectionName"))))
# 
# dsp_obs(Headline.pfx="Daily Report::")
# ## Grouping Daily Report:: into SubsectionName=myBus::Tech - A new category
# glb_entity_df[glb_entity_df$Headline.pfx == "Daily Report::", "SubsectionName"] <- "myBus::Tech"
# dsp_datagrp(7, 17)
# 
# ## Sample headlines from First Draft:: ?
# dsp_obs(Headline.pfx="First Draft::")
# ## Grouping First Draft:: into NewsDesk=myCollection - A new category
# ###                                 SectionName=myCollection - A new category
# ###                                 SubsectionName=myCollection - A new category
# glb_entity_df[glb_entity_df$Headline.pfx == "First Draft::", "NewsDesk"] <- "myCollection"
# glb_entity_df[glb_entity_df$Headline.pfx == "First Draft::", "SectionName"] <- "myCollection"
# glb_entity_df[glb_entity_df$Headline.pfx == "First Draft::", "SubsectionName"] <- "myCollection"
# dsp_datagrp(1, 20)
# 
# ## How are the Milan Fashion Week:: blogs categorized ?
# print(orderBy(~ Headline.pfx+NewsDesk+SectionName+SubsectionName,
#         mycreate_sqlxtab_df(glb_entity_df[sel_obs(
#             Headline.contains="Fashion Week"), ],
#     c(glb_rsp_var, "Headline.pfx", "NewsDesk", "SectionName", "SubsectionName"))))
# print(orderBy(~ Headline.pfx+NewsDesk+SectionName+SubsectionName,
#         mycreate_sqlxtab_df(glb_entity_df[sel_obs(
#             NewsDesk="Styles"), ],
#     c(glb_rsp_var, "Headline.pfx", "NewsDesk", "SectionName", "SubsectionName"))))
# dsp_obs(Popular=1, NewsDesk="Styles")
# 
# print(orderBy(~ Headline.pfx+NewsDesk+SectionName+SubsectionName,
#         mycreate_sqlxtab_df(glb_entity_df[sel_obs(
#             NewsDesk="TStyle"), ],
#     c(glb_rsp_var, "Headline.pfx", "NewsDesk", "SectionName", "SubsectionName"))))
# #dsp_xtab("Fashion Week")
# 
# ## Sample headlines from myMisc:: ?
# dsp_obs(Popular=1, Headline.pfx="myMisc::")
# dsp_obs(Headline.contains="Saturday Morning Music") # only 1 obs
# dsp_obs(Headline.pfx="myMisc::", Headline.contains=":")
# dsp_obs(Headline.contains="Charities That Inspire Kids")

dsp_chisq.test <- function(...) {
    sel_df <- glb_entity_df[sel_obs(...) & 
                            !is.na(glb_entity_df$Popular), ]
    sel_df$.marker <- 1
    ref_df <- glb_entity_df[!is.na(glb_entity_df$Popular), ]
    mrg_df <- merge(ref_df[, c(glb_id_vars, "Popular")],
                    sel_df[, c(glb_id_vars, ".marker")], all.x=TRUE)
    mrg_df[is.na(mrg_df)] <- 0
    print(mrg_tbl <- table(mrg_df$.marker, mrg_df$Popular))
    print("Rows:Selected; Cols:Popular")
    #print(mrg_tbl)
    print(chisq.test(mrg_tbl))
}
# dsp_chisq.test(Headline.contains="[Ee]bola")
# dsp_chisq.test(Snippet.contains="[Ee]bola")
# dsp_chisq.test(Abstract.contains="[Ee]bola")

# print(mycreate_sqlxtab_df(glb_entity_df[sel_obs(Headline.contains="[Ee]bola"), ], 
#                           c(glb_rsp_var, "NewsDesk", "SectionName", "SubsectionName")))

#dsp_NewsDesk_SectionName_obs("", "U.S.")

# print(table(glb_entity_df$NewsDesk, glb_entity_df$SectionName))
# print(table(glb_entity_df$SectionName, glb_entity_df$SubsectionName))
# print(table(glb_entity_df$NewsDesk, glb_entity_df$SectionName, glb_entity_df$SubsectionName))

# Copy Headline into Snipper & Abstract if they are empty
print(glb_entity_df[nchar(glb_entity_df[, "Snippet"]) == 0, c("Headline", "Snippet")])
```

```
##                                                            Headline
## 2838            First Draft Focus: Off to Raise Money for Democrats
## 3728                      Verbatim: Obama as Supreme Court Justice?
## 4904                                   Election 2014: Live Coverage
## 4994                                   Election 2014: Live Coverage
## 5029                        First Draft Focus: Perry's Day in Court
## 5065                   First Draft Focus: Honoring a Civil War Hero
## 5160                 Supreme Court to Hear New Health Law Challenge
## 5254                                 Verbatim: Will Rick Perry Run?
## 5472                        First Draft Focus: A Red Carpet Welcome
## 7129                                 First Draft Focus: Pass a Bill
## 7164 Does Torture Work? C.I.A.'s Claims vs. Senate Panel's Findings
## 7364                              First Draft Focus: Three Wise Men
## 7368                              Verbatim: The People's Priorities
##      Snippet
## 2838        
## 3728        
## 4904        
## 4994        
## 5029        
## 5065        
## 5160        
## 5254        
## 5472        
## 7129        
## 7164        
## 7364        
## 7368
```

```r
print(glb_entity_df[glb_entity_df$Headline == glb_entity_df$Snippet, 
                    c("UniqueID", "Headline", "Snippet")])
```

```
## [1] UniqueID Headline Snippet 
## <0 rows> (or 0-length row.names)
```

```r
glb_entity_df[nchar(glb_entity_df[, "Snippet"]) == 0, "Snippet"] <- 
    glb_entity_df[nchar(glb_entity_df[, "Snippet"]) == 0, "Headline"]

print(glb_entity_df[nchar(glb_entity_df[, "Abstract"]) == 0, c("Headline", "Abstract")])
```

```
##                                                            Headline
## 2838            First Draft Focus: Off to Raise Money for Democrats
## 3728                      Verbatim: Obama as Supreme Court Justice?
## 4904                                   Election 2014: Live Coverage
## 4994                                   Election 2014: Live Coverage
## 5029                        First Draft Focus: Perry's Day in Court
## 5065                   First Draft Focus: Honoring a Civil War Hero
## 5160                 Supreme Court to Hear New Health Law Challenge
## 5254                                 Verbatim: Will Rick Perry Run?
## 5472                        First Draft Focus: A Red Carpet Welcome
## 7129                                 First Draft Focus: Pass a Bill
## 7164 Does Torture Work? C.I.A.'s Claims vs. Senate Panel's Findings
## 7309             Spending Bill Passes House With Democratic Support
## 7310                   Funding Bill Hangs in Balance as House Votes
## 7315              House Democrats Vent Frustration With White House
## 7329                Obama Works the Phones to Get Funding Deal Done
## 7364                              First Draft Focus: Three Wise Men
## 7368                              Verbatim: The People's Priorities
##      Abstract
## 2838         
## 3728         
## 4904         
## 4994         
## 5029         
## 5065         
## 5160         
## 5254         
## 5472         
## 7129         
## 7164         
## 7309         
## 7310         
## 7315         
## 7329         
## 7364         
## 7368
```

```r
print(glb_entity_df[glb_entity_df$Headline == glb_entity_df$Abstract, 
                    c("UniqueID", "Headline", "Abstract")])
```

```
## [1] UniqueID Headline Abstract
## <0 rows> (or 0-length row.names)
```

```r
glb_entity_df[nchar(glb_entity_df[, "Abstract"]) == 0, "Abstract"] <- 
    glb_entity_df[nchar(glb_entity_df[, "Abstract"]) == 0, "Headline"]

# WordCount_0_df <- subset(glb_entity_df, WordCount == 0)
# table(WordCount_0_df$Popular, WordCount_0_df$WordCount, useNA="ifany")
# myprint_df(WordCount_0_df[, 
#                 c("UniqueID", "Popular", "WordCount", "Headline")])

glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="manage.missing.data", 
                              chunk_step_major=max(glb_script_df$chunk_step_major), 
                              chunk_step_minor=1,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                  chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed2        cleanse.data                2                1  25.148
## elapsed3 manage.missing.data                2                1  36.598
```

### Step `2`.`1`: manage missing data

```r
# print(sapply(names(glb_trnent_df), function(col) sum(is.na(glb_trnent_df[, col]))))
# print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))
# glb_trnent_df <- na.omit(glb_trnent_df)
# glb_newent_df <- na.omit(glb_newent_df)
# df[is.na(df)] <- 0

dsp_problem_data(glb_entity_df)
```

```
## [1] "numeric data missing in : "
##          WordCount            Popular           UniqueID 
##                  0               1870                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##               1870                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                109                  0 
## [1] "numeric data w/ 0s in : "
##          WordCount            Popular           UniqueID 
##                109               5439                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                378                159 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0               1344                141 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "numeric data w/ Infs in : "
##          WordCount            Popular           UniqueID 
##                  0                  0                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "numeric data w/ NaNs in : "
##          WordCount            Popular           UniqueID 
##                  0                  0                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "string data missing in : "
##       NewsDesk    SectionName SubsectionName       Headline        Snippet 
##              0              0              0              0              0 
##       Abstract        PubDate   Headline.pfx 
##              0              0              0
```

```r
# Not refactored into mydsutils.R since glb_*_df might be reassigned
glb_impute_missing_data <- function() {
    
    require(mice)
    set.seed(glb_mice_complete.seed)
    inp_impent_df <- glb_entity_df[, setdiff(names(glb_entity_df), 
                                union(glb_exclude_vars_as_features, glb_rsp_var))]
    print("Summary before imputation: ")
    print(summary(inp_impent_df))
    out_impent_df <- complete(mice(inp_impent_df))
    print(summary(out_impent_df))
    
    return(out_impent_df[, "WordCount.log"])
}

if (glb_impute_na_data) 
    glb_entity_df[, "WordCount.log"] <- glb_impute_missing_data()
```

```
## Loading required package: mice
## Loading required package: Rcpp
## Loading required package: lattice
## mice 2.22 2014-06-10
```

```
## [1] "Summary before imputation: "
##    NewsDesk         SectionName        SubsectionName    
##  Length:8402        Length:8402        Length:8402       
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##  PubDate.date.fctr PubDate.wkday.fctr  PubDate.hour   PubDate.apm.fctr
##  (0.97,7]:1981     0: 378             Min.   : 0.00   am:3636         
##  (7,13]  :1757     1:1605             1st Qu.: 9.00   pm:4766         
##  (13,19] :1808     2:1559             Median :12.00                   
##  (19,25] :1650     3:1614             Mean   :12.22                   
##  (25,31] :1206     4:1539             3rd Qu.:16.00                   
##                    5:1470             Max.   :23.00                   
##                    6: 237                                             
##  PubDate.minute  PubDate.second  WordCount.log        .rnorm         
##  Min.   : 0.00   Min.   : 0.00   Min.   :0.6932   Min.   :-3.881663  
##  1st Qu.: 5.00   1st Qu.:14.00   1st Qu.:5.2679   1st Qu.:-0.665043  
##  Median :24.00   Median :30.00   Median :5.9480   Median :-0.004510  
##  Mean   :24.11   Mean   :29.49   Mean   :5.8263   Mean   :-0.006807  
##  3rd Qu.:40.00   3rd Qu.:44.00   3rd Qu.:6.6067   3rd Qu.: 0.664125  
##  Max.   :59.00   Max.   :59.00   Max.   :9.2977   Max.   : 3.356092  
##                                  NA's   :109                         
##  Headline.pfx      
##  Length:8402       
##  Class :character  
##  Mode  :character  
##                    
##                    
##                    
##                    
## 
##  iter imp variable
##   1   1  WordCount.log
##   1   2  WordCount.log
##   1   3  WordCount.log
##   1   4  WordCount.log
##   1   5  WordCount.log
##   2   1  WordCount.log
##   2   2  WordCount.log
##   2   3  WordCount.log
##   2   4  WordCount.log
##   2   5  WordCount.log
##   3   1  WordCount.log
##   3   2  WordCount.log
##   3   3  WordCount.log
##   3   4  WordCount.log
##   3   5  WordCount.log
##   4   1  WordCount.log
##   4   2  WordCount.log
##   4   3  WordCount.log
##   4   4  WordCount.log
##   4   5  WordCount.log
##   5   1  WordCount.log
##   5   2  WordCount.log
##   5   3  WordCount.log
##   5   4  WordCount.log
##   5   5  WordCount.log
##    NewsDesk         SectionName        SubsectionName    
##  Length:8402        Length:8402        Length:8402       
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##  PubDate.date.fctr PubDate.wkday.fctr  PubDate.hour   PubDate.apm.fctr
##  (0.97,7]:1981     0: 378             Min.   : 0.00   am:3636         
##  (7,13]  :1757     1:1605             1st Qu.: 9.00   pm:4766         
##  (13,19] :1808     2:1559             Median :12.00                   
##  (19,25] :1650     3:1614             Mean   :12.22                   
##  (25,31] :1206     4:1539             3rd Qu.:16.00                   
##                    5:1470             Max.   :23.00                   
##                    6: 237                                             
##  PubDate.minute  PubDate.second  WordCount.log        .rnorm         
##  Min.   : 0.00   Min.   : 0.00   Min.   :0.6931   Min.   :-3.881663  
##  1st Qu.: 5.00   1st Qu.:14.00   1st Qu.:5.2730   1st Qu.:-0.665043  
##  Median :24.00   Median :30.00   Median :5.9467   Median :-0.004510  
##  Mean   :24.11   Mean   :29.49   Mean   :5.8263   Mean   :-0.006807  
##  3rd Qu.:40.00   3rd Qu.:44.00   3rd Qu.:6.6067   3rd Qu.: 0.664125  
##  Max.   :59.00   Max.   :59.00   Max.   :9.2977   Max.   : 3.356092  
##                                                                      
##  Headline.pfx      
##  Length:8402       
##  Class :character  
##  Mode  :character  
##                    
##                    
##                    
## 
```

```r
dsp_problem_data(glb_entity_df)
```

```
## [1] "numeric data missing in : "
##          WordCount            Popular           UniqueID 
##                  0               1870                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##               1870                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "numeric data w/ 0s in : "
##          WordCount            Popular           UniqueID 
##                109               5439                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                378                159 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0               1344                141 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "numeric data w/ Infs in : "
##          WordCount            Popular           UniqueID 
##                  0                  0                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "numeric data w/ NaNs in : "
##          WordCount            Popular           UniqueID 
##                  0                  0                  0 
##       Popular.fctr       PubDate.year PubDate.month.fctr 
##                  0                  0                  0 
##  PubDate.date.fctr PubDate.wkday.fctr       PubDate.hour 
##                  0                  0                  0 
##   PubDate.apm.fctr     PubDate.minute     PubDate.second 
##                  0                  0                  0 
##      WordCount.log             .rnorm 
##                  0                  0 
## [1] "string data missing in : "
##       NewsDesk    SectionName SubsectionName       Headline        Snippet 
##              0              0              0              0              0 
##       Abstract        PubDate   Headline.pfx 
##              0              0              0
```

```r
glb_script_df <- rbind(glb_script_df, 
    data.frame(chunk_label="encodeORretype.data", 
        chunk_step_major=max(glb_script_df$chunk_step_major), 
        chunk_step_minor=glb_script_df[nrow(glb_script_df), "chunk_step_minor"]+1,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                  chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed3 manage.missing.data                2                1  36.598
## elapsed4 encodeORretype.data                2                2  40.549
```

### Step `2`.`2`: encode/retype data

```r
# map_<col_name>_df <- myimport_data(
#     url="<map_url>", 
#     comment="map_<col_name>_df", print_diagn=TRUE)
# map_<col_name>_df <- read.csv(paste0(getwd(), "/data/<file_name>.csv"), strip.white=TRUE)

# glb_trnent_df <- mymap_codes(glb_trnent_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
# glb_newent_df <- mymap_codes(glb_newent_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
    					
# glb_trnent_df$<col_name>.fctr <- factor(glb_trnent_df$<col_name>, 
#                     as.factor(union(glb_trnent_df$<col_name>, glb_newent_df$<col_name>)))
# glb_newent_df$<col_name>.fctr <- factor(glb_newent_df$<col_name>, 
#                     as.factor(union(glb_trnent_df$<col_name>, glb_newent_df$<col_name>)))

glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="extract.features", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                  chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed4 encodeORretype.data                2                2  40.549
## elapsed5    extract.features                3                0  40.615
```

## Step `3`: extract features

```r
#```{r extract_features, cache=FALSE, eval=glb_is_textual}
# Create new features that help prediction
# <col_name>.lag.2 <- lag(zoo(glb_trnent_df$<col_name>), -2, na.pad=TRUE)
# glb_trnent_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_newent_df$<col_name>), -2, na.pad=TRUE)
# glb_newent_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_newent_df[1, "<col_name>.lag.2"] <- glb_trnent_df[nrow(glb_trnent_df) - 1, 
#                                                    "<col_name>"]
# glb_newent_df[2, "<col_name>.lag.2"] <- glb_trnent_df[nrow(glb_trnent_df), 
#                                                    "<col_name>"]
                                                   
# glb_entity_df <- mutate(glb_entity_df,
#     A.has.http=ifelse(grepl("http",Added,fixed=TRUE), 1, 0)
#                     )
# 
# glb_trnent_df <- mutate(glb_trnent_df,
#                     )
# 
# glb_newent_df <- mutate(glb_newent_df,
#                     )

#   Create factors of string variables
str_vars <- sapply(names(glb_entity_df), function(var)  
                    ifelse(class(glb_entity_df[, var]) == "character", var, ""))
print(str_vars <- str_vars[str_vars != ""])
```

```
##         NewsDesk      SectionName   SubsectionName         Headline 
##       "NewsDesk"    "SectionName" "SubsectionName"       "Headline" 
##          Snippet         Abstract          PubDate             .src 
##        "Snippet"       "Abstract"        "PubDate"           ".src" 
##     Headline.pfx 
##   "Headline.pfx"
```

```r
if (length(str_vars <- setdiff(str_vars, 
                               glb_exclude_vars_as_features)) > 0) {
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, str_vars)
    for (var in str_vars) {
        warning("Creating factors of string variable: ", var, 
                ": # of unique values: ", length(unique(glb_entity_df[, var])))
        glb_entity_df[, paste0(var, ".fctr")] <- factor(glb_entity_df[, var], 
                        as.factor(unique(glb_entity_df[, var])))
#         glb_trnent_df[, paste0(var, ".fctr")] <- factor(glb_trnent_df[, var], 
#                         as.factor(unique(glb_entity_df[, var])))
#         glb_newent_df[, paste0(var, ".fctr")] <- factor(glb_newent_df[, var], 
#                         as.factor(unique(glb_entity_df[, var])))
    }
}
```

```
## Warning: Creating factors of string variable: NewsDesk: # of unique
## values: 26
```

```
## Warning: Creating factors of string variable: SectionName: # of unique
## values: 32
```

```
## Warning: Creating factors of string variable: SubsectionName: # of unique
## values: 49
```

```
## Warning: Creating factors of string variable: Headline.pfx: # of unique
## values: 15
```

```r
if (glb_is_textual) {
    require(tm)
    
    glb_corpus_lst <- list(); glb_full_DTM_lst <- list(); glb_sprs_DTM_lst <- list();
    for (txt_var in glb_txt_vars) {
        print(sprintf("Building corpus for %s...", txt_var))
        
        # Combine "new york" to "newyork"
        #   shd be created as a tm_map::content_transformer
        txt_df <- glb_entity_df[, txt_var]
        txt_df <- gsub("[Nn]ew [Dd]elhi",   "newdelhi",   txt_df)                        
        txt_df <- gsub("[Nn]ew [Gg]uinea",  "newguinea",   txt_df)                                
        txt_df <- gsub("[Nn]ew [Jj]ersey",  "newjersey",  txt_df)                
        txt_df <- gsub("[Nn]ew [Oo]rleans", "neworleans",  txt_df)                
        txt_df <- gsub("[Nn]ew [Yy]ear",    "newyear",    txt_df)        
        txt_df <- gsub("[Nn]ew [Yy]ork",    "newyork",    txt_df)
        txt_df <- gsub("[Nn]ew [Zz]ealand", "newzealand", txt_df)
        
        if (txt_var == "Headline") {
            dsp_chisq.test(Headline.contains="[Nn]ew ")
            print(head(txt_df[grep("[Nn]ew ", txt_df)]))
            print(tail(txt_df[grep("[Nn]ew ", txt_df)]))
            print(sample(txt_df[grep("[Nn]ew ", txt_df)], 5))
            print(length(txt_df[grep("[Nn]ew ", txt_df)]))
            print(txt_df[grep("[Nn]ew ", txt_df)][01:20])
            print(txt_df[grep("[Nn]ew ", txt_df)][21:40])        
            print(txt_df[grep("[Nn]ew ", txt_df)][41:60])                
            print(txt_df[grep("[Nn]ew ", txt_df)][61:80])                       
            print(txt_df[grep("[Nn]ew ", txt_df)][81:100])                               
            #print(length(txt_df[grep("[Nn]ew [Zz]ealand", txt_df)]))
            
            dsp_chisq.test(Headline.contains="[Nn]ew [Yy]ork")
            dsp_chisq.test(Headline.contains="[Re]eport")
            dsp_chisq.test(Snippet.contains="[Re]eport")
            
            dsp_chisq.test(Headline.contains="[Ww]eek")
            dsp_chisq.test(Headline.contains="[Dd]ay")
            dsp_chisq.test(Headline.contains="[Ff]ashion")
            dsp_chisq.test(Headline.contains="[Tt]oday")
            dsp_chisq.test(Headline.contains="[Dd]ail")
            dsp_chisq.test(Headline.contains="2014")
            dsp_chisq.test(Headline.contains="2015")            
            glb_append_stop_words[["Headline"]] <- c(NULL)
        }

        if (txt_var == "Snippet") {
            dsp_chisq.test(Snippet.contains="[Nn]ew ")
            print(head(txt_df[grep("[Nn]ew ", txt_df)]))
            print(tail(txt_df[grep("[Nn]ew ", txt_df)]))
            print(sample(txt_df[grep("[Nn]ew ", txt_df)], 5))
            print(length(txt_df[grep("[Nn]ew ", txt_df)]))
            print(txt_df[grep("[Nn]ew ", txt_df)][11:20])
            print(txt_df[grep("[Nn]ew ", txt_df)][21:30])        
            print(txt_df[grep("[Nn]ew ", txt_df)][31:40])                
            print(txt_df[grep("[Nn]ew ", txt_df)][41:50])                       
            print(txt_df[grep("[Nn]ew ", txt_df)][51:60])                               
            #print(length(txt_df[grep("[Nn]ew [Zz]ealand", txt_df)]))
    
            dsp_chisq.test(Snippet.contains="[Ww]ill")
            dsp_chisq.test(Snippet.contains="[Tt]ime")
            dsp_chisq.test(Snippet.contains="[Ww]eek")
            dsp_chisq.test(Snippet.contains="[Yy]ear")        
            dsp_chisq.test(Snippet.contains="[Ne]w [Yy]ork")
            dsp_chisq.test(Snippet.contains="[Cc]ompan")
            dsp_chisq.test(Snippet.contains="[Oo]ne")
            dsp_chisq.test(Snippet.contains="[Rr]eport")
            dsp_chisq.test(Snippet.contains="[Pp]resid")
            dsp_chisq.test(Snippet.contains="[Ss]aid")
            dsp_chisq.test(Snippet.contains="[Cc]an")
            dsp_chisq.test(Snippet.contains="[Dd]ay")
            
            glb_append_stop_words[["Snippet"]] <- c(NULL)
                #c("can")
        }

        if (txt_var == "Abstract") {
            dsp_chisq.test(Abstract.contains="[Nn]ew ")
            print(head(txt_df[grep("[Nn]ew ", txt_df)]))
            print(tail(txt_df[grep("[Nn]ew ", txt_df)]))
            print(sample(txt_df[grep("[Nn]ew ", txt_df)], 5))
            print(length(txt_df[grep("[Nn]ew ", txt_df)]))
            print(txt_df[grep("[Nn]ew ", txt_df)][11:20])
            print(txt_df[grep("[Nn]ew ", txt_df)][21:30])        
            print(txt_df[grep("[Nn]ew ", txt_df)][31:40])                
            print(txt_df[grep("[Nn]ew ", txt_df)][41:50])                       
            print(txt_df[grep("[Nn]ew ", txt_df)][51:60])                               
            #print(length(txt_df[grep("[Nn]ew [Zz]ealand", txt_df)]))
    
            dsp_chisq.test(Abstract.contains="[Ww]ill")
            dsp_chisq.test(Abstract.contains="[Tt]ime")
            dsp_chisq.test(Abstract.contains="[Ww]eek")
            dsp_chisq.test(Abstract.contains="[Yy]ear")        
            dsp_chisq.test(Abstract.contains="[Ne]w [Yy]ork")
            dsp_chisq.test(Abstract.contains="[Cc]ompan")
            dsp_chisq.test(Abstract.contains="[Oo]ne")
            dsp_chisq.test(Abstract.contains="[Rr]eport")
            dsp_chisq.test(Abstract.contains="[Pp]resid")
            
            dsp_chisq.test(Abstract.contains="[Ss]aid")
            dsp_chisq.test(Abstract.contains="[Cc]an")
            dsp_chisq.test(Abstract.contains="[Dd]ay")
            dsp_chisq.test(Abstract.contains="[Ss]tate")            
            dsp_chisq.test(Abstract.contains="[Mm]ake")            
            dsp_chisq.test(Abstract.contains="[Bb]ank")                        
            
            glb_append_stop_words[["Abstract"]] <- 
                c("fashion", "first", "intern", "make", "newyork", "report", 
                  "said", "share", "show", "state", "week", "year")
        }

        txt_corpus <- Corpus(VectorSource(txt_df))
        txt_corpus <- tm_map(txt_corpus, tolower)
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation)
#         txt-corpus <- tm_map(txt_corpus, content_transformer(function(x, pattern) gsub(pattern, "", x))        
        txt_corpus <- tm_map(txt_corpus, removeWords, 
                             c(glb_append_stop_words[[txt_var]], 
                               stopwords("english")))
        txt_corpus <- tm_map(txt_corpus, stemDocument)
        
        full_freqs_DTM <- DocumentTermMatrix(txt_corpus)
        print("   Full freqs:"); print(full_freqs_DTM)
        full_freqs_vctr <- colSums(as.matrix(full_freqs_DTM))
        names(full_freqs_vctr) <- dimnames(full_freqs_DTM)[[2]]
        full_freqs_df <- as.data.frame(full_freqs_vctr)
        names(full_freqs_df) <- "freq.full"
        full_freqs_df$term <- rownames(full_freqs_df)
        full_freqs_df <- orderBy(~ -freq.full, full_freqs_df)
        
        sprs_freqs_DTM <- removeSparseTerms(full_freqs_DTM, 
                                            glb_sprs_thresholds[txt_var])
        print("   Sparse freqs:"); print(sprs_freqs_DTM)
        sprs_freqs_vctr <- colSums(as.matrix(sprs_freqs_DTM))
        names(sprs_freqs_vctr) <- dimnames(sprs_freqs_DTM)[[2]]
        sprs_freqs_df <- as.data.frame(sprs_freqs_vctr)
        names(sprs_freqs_df) <- "freq.sprs"
        sprs_freqs_df$term <- rownames(sprs_freqs_df)
        sprs_freqs_df <- orderBy(~ -freq.sprs, sprs_freqs_df)
        
        terms_freqs_df <- merge(full_freqs_df, sprs_freqs_df, all.x=TRUE)
        melt_freqs_df <- orderBy(~ -value, melt(terms_freqs_df, id.var="term"))
        print(ggplot(melt_freqs_df, aes(value, color=variable)) + stat_ecdf() + 
                  geom_hline(yintercept=glb_sprs_thresholds[txt_var], 
                             linetype = "dotted"))
        melt_freqs_df <- orderBy(~ -value, 
                        melt(subset(terms_freqs_df, !is.na(freq.sprs)), id.var="term"))
        print(myplot_hbar(melt_freqs_df, "term", "value", 
                          colorcol_name="variable"))
        melt_freqs_df <- orderBy(~ -value, 
                        melt(subset(terms_freqs_df, is.na(freq.sprs)), id.var="term"))
        print(myplot_hbar(head(melt_freqs_df, 10), "term", "value", 
                          colorcol_name="variable"))
        
        glb_corpus_lst[[txt_var]] <- txt_corpus
        glb_full_DTM_lst[[txt_var]] <- full_freqs_DTM
        glb_sprs_DTM_lst[[txt_var]] <- sprs_freqs_DTM
    }

    # Create txt features
    if ((length(glb_txt_vars) > 1) &&
        (length(unique(pfxs <- sapply(glb_txt_vars, 
                    function(txt) toupper(substr(txt, 1, 1))))) < length(glb_txt_vars)))
            stop("Prefixes for corpus freq terms not unique: ", pfxs)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DTM for %s...", txt_var))
        txt_X_df <- as.data.frame(as.matrix(glb_sprs_DTM_lst[[txt_var]]))
        colnames(txt_X_df) <- paste(toupper(substr(txt_var, 1, 1)), ".",
                                    make.names(colnames(txt_X_df)), sep="")
        rownames(txt_X_df) <- rownames(glb_entity_df) # warning otherwise
        glb_entity_df <- cbind(glb_entity_df, txt_X_df)
        
        # Create <txt_var>.has.http
        glb_entity_df[, paste(toupper(substr(txt_var, 1, 1)), ".has.http", sep="")] <- 
            sapply(1:nrow(glb_entity_df), 
    function(row_ix) ifelse(grepl("http", glb_entity_df[row_ix, txt_var], fixed=TRUE), 
                            1, 0))
    
        # Create user-specified term vectors 
        #   UniqueID == 4020, H.has.ebola
        dsp_chisq.test(Headline.contains="[Ee]bola")                            
        dsp_chisq.test( Snippet.contains="[Ee]bola")
        dsp_chisq.test(Abstract.contains="[Ee]bola")
        if (txt_var == "Headline") {
        glb_entity_df[, paste(toupper(substr(txt_var, 1, 1)), ".has.ebola", sep="")] <- 
            sapply(1:nrow(glb_entity_df), 
    function(row_ix) ifelse(grepl("[Ee]bola", glb_entity_df[row_ix, txt_var]), 
                            1, 0))            
        }
    
        # Create <txt_var>.num.chars
        glb_entity_df[, paste(toupper(substr(txt_var, 1, 1)), ".num.chars", sep="")] <- 
            sapply(1:nrow(glb_entity_df), 
                    function(row_ix) nchar(glb_entity_df[row_ix, txt_var]))

        # Create <txt_var>.num.words & .num.words.unq
        glb_entity_df[, paste(toupper(substr(txt_var, 1, 1)), ".num.words", sep="")] <- 
            rowSums(as.matrix(glb_full_DTM_lst[[txt_var]]))
        glb_entity_df[, paste(toupper(substr(txt_var, 1, 1)), ".num.words.unq", sep="")] <- 
            rowSums(as.matrix(glb_full_DTM_lst[[txt_var]]) != 0)
    
        for (feat in paste(toupper(substr(txt_var, 1, 1)), 
                            c(".num.chars", ".num.words", ".num.words.unq"), sep="")) {
            glb_entity_df[, paste0(feat, ".log")] <- log(1 + glb_entity_df[, feat])
            print(myplot_box(glb_entity_df, paste0(feat, ".log"), glb_rsp_var))
            glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features,
                                                  feat)
        }            
    }        

    # Generate summaries
#     print(summary(glb_entity_df))
#     print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
#     print(summary(glb_trnent_df))
#     print(sapply(names(glb_trnent_df), function(col) sum(is.na(glb_trnent_df[, col]))))
#     print(summary(glb_newent_df))
#     print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))
}
```

```
## Loading required package: tm
## Loading required package: NLP
## 
## Attaching package: 'NLP'
## 
## The following object is masked from 'package:ggplot2':
## 
##     annotate
```

```
## [1] "Building corpus for Headline..."
##    
##        0    1
##   0 4990 1064
##   1  449   29
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 41.2888, df = 1, p-value = 1.313e-10
## 
## [1] "New 96-Page Murakami Work Coming in December"                                         
## [2] "New Nick Cave Sculptures, a History of High Heels and More from the Cultural Calendar"
## [3] "Fabio Luisi Has a New Gig"                                                            
## [4] "Sean MacPherson on His New Hotel, the Ludlow"                                         
## [5] "The New Unofficial Scent of newyork City"                                             
## [6] "New Life for Some Pulp Fiction by Gore Vidal"                                         
## [1] "A New Way to Tour Taiwan"                                          
## [2] "A Conversation With Glenn Lurie, AT&T Mobility's New Chief"        
## [3] "1889: New Names of Paris Streets"                                  
## [4] "More Than 60 Years Later, a New Edition of Cartier-Bresson Classic"
## [5] "Art Basel, Eyes to the East, Names New Director for Asia"          
## [6] "A New Mortgage Trap"                                               
## [1] "A Sensual Music Video from a Dynamic New Duo"               
## [2] "Puma Explains Its New Creative Director: Rihanna"           
## [3] "Why It Took Us So Long to Finish Our New Website"           
## [4] "New Jonathan Franzen Novel, 'Purity,' Coming in September"  
## [5] "Sam Waksal's New Biotech Venture Tests Investor Forgiveness"
## [1] 306
##  [1] "New 96-Page Murakami Work Coming in December"                                         
##  [2] "New Nick Cave Sculptures, a History of High Heels and More from the Cultural Calendar"
##  [3] "Fabio Luisi Has a New Gig"                                                            
##  [4] "Sean MacPherson on His New Hotel, the Ludlow"                                         
##  [5] "The New Unofficial Scent of newyork City"                                             
##  [6] "New Life for Some Pulp Fiction by Gore Vidal"                                         
##  [7] "The New Neutral"                                                                      
##  [8] "The New Look of Smokers' Litter"                                                      
##  [9] "A Montage of Iconic New Wave Movies, Via the newyork Fashion Film Festival"           
## [10] "Calico, Google's Anti-Aging Company, Announces New Research Facility "                
## [11] "Court Challenge to New Inversion Rules Would Face Long Odds"                          
## [12] "Today in Small Business: The Coolest New Businesses in newyork"                       
## [13] "Apple Says It Will Add New iCloud Security Measures After Celebrity Hack"             
## [14] "Microsoft Introduces Three New Smartphones"                                           
## [15] "'Pretesting' and Other New Ideas to Help Kids Learn"                                  
## [16] "Braun's Old Look Is New Again"                                                        
## [17] "Chicago Symphony Association Names a New President"                                   
## [18] "A New Food Tour in Prague"                                                            
## [19] "Will the New Autocorrect Steal Your Soul?"                                            
## [20] "Hanks and Tomlin Among New Kennedy Center Honorees"                                   
##  [1] "Joan Rivers's Funeral Instructions, Scarlett Johansson's New Arrival and Karl Lagerfeld's Mini-Me"
##  [2] "During a Season for New Devices, a Tough Market for Smartphone Makers"                            
##  [3] "Chef Nick Anderer on Rome, Pizza and His New Restaurant, Marta"                                   
##  [4] "New Answers About Carbs and Fat"                                                                  
##  [5] "New Margaret Atwood Work Coming in  2114?"                                                        
##  [6] "Koons Creating Sculptures for New Philanthropy Project"                                           
##  [7] "A New Study Clarifies Treatment Needs for Water from Fracked Gas and Oil Wells"                   
##  [8] "Ty Segall, Garage-Rock Whiz, Debuts a Clever New Video"                                           
##  [9] "Fed Proposes New Rule, and Wall St. Banks Feel the Pressure"                                      
## [10] "U2 Releases New Album for Free on Apple's iTunes"                                                 
## [11] "Making New Friends: In Praise of Name Tags"                                                       
## [12] "The New Dealer"                                                                                   
## [13] "Back to School, to New Marching Orders"                                                           
## [14] "2 Senators Introduce New Anti-Inversion Bill"                                                     
## [15] "What New Technologies or Tech Toys Are You Most Excited About?"                                   
## [16] "Test Yourself | A New Dinosaur"                                                                   
## [17] "Introducing a New Crowdsourced Feature: 'NYTLNreads'"                                             
## [18] "Today in Small Business: For New S.B.A. Chief, the Honeymoon May Be Over"                         
## [19] "The New Look"                                                                                     
## [20] "New Contemporary Label Teams Composers With Artists"                                              
##  [1] "Online Orders for New iPhones Exceed 4 Million"                                              
##  [2] "The Juan MacLean Triumphs Over Tragedy With a Sparkling New Dance Album"                     
##  [3] "Royal newzealand Ballet Names New Director"                                                  
##  [4] "In a Season of Deadly Rains in India, Does the New Prime Minister Believe in Climate Change?"
##  [5] "Stream the New Album From Japan's Original Indie Rock Star, Shintaro Sakamoto"               
##  [6] "On New Measurements of Aging"                                                                
##  [7] "New North American Tour for the Former Cat Stevens"                                          
##  [8] "New Broker Seeks Role in Secondary Sales of Private Stock"                                   
##  [9] "Daily Report: New iPhones' Strongest Feature Is iOS 8 Operating System"                      
## [10] "New Lease on Life"                                                                           
## [11] "Women's Project Theater to Present New Laura Eason Play"                                     
## [12] "To Mark I.P.O., Alibaba to Make Gift to Its New Market Home"                                 
## [13] "Paul Taylor's American Modern Dance Sets New Season"                                         
## [14] "Push for New Climate Change Pact as U.N. Summit Approaches"                                  
## [15] "Q&A With Rodolfo Paglialunga, the New Designer of Jil Sander"                                
## [16] "A New Home Page Feature"                                                                     
## [17] "A New High Mark for 'Lion King'"                                                             
## [18] "New 'Super PAC' Aims for Better Beer"                                                        
## [19] "At a New Amsterdam Pop-Up, Dining Among Rembrandts in the Sky"                               
## [20] "New Rules Make Inversions Less Lucrative, Experts Say"                                       
##  [1] "New Fordham Building Opens at Lincoln Center"                                       
##  [2] "Tiger Global, Big Investor in Tech Start-Ups, Said to Plan New $1.5 Billion Fund"   
##  [3] "Syria Strikes Win Obama New Supporters: Republicans"                                
##  [4] "New CO2 Emissions Report Shows China's Central Role in Shaping World's Climate Path"
##  [5] "New Kids on the Block"                                                              
##  [6] "The Best New Stands at the Paul Bert Serpette Antiques Market in Paris"             
##  [7] "New British Center Seeks to Offer Ideas on Preventing Financial Crises"             
##  [8] "Thanks to Video Monitors, Parents are the New Big Brother"                          
##  [9] "Ichabod Crane Rides Again in New Edition"                                           
## [10] "BHP Billiton Considers New Listing in London for Spinoff"                           
## [11] "Frieze Art Fair Founders Step Aside for a New Director"                             
## [12] "A New Guide to Hiking in California"                                                
## [13] "Sam Waksal's New Biotech Venture Tests Investor Forgiveness"                        
## [14] "The New Classical Clique"                                                           
## [15] "Janus Capital, Bill Gross's New Firm, Has a Troubled Past"                          
## [16] "Prince Celebrating New Albums With a Paisley Park Livestream"                       
## [17] "'Waterfall,' a New Musical With Thai Inspiration, Aiming for Broadway"              
## [18] "Roundabout to Produce New Stephen Karam Play"                                       
## [19] "New Nations, Living in Limbo"                                                       
## [20] "Secret Service Hearing Yields New Information on White House Security Breach"       
##  [1] "Daily Report: Netflix Aims at Hollywood's New Releases"            
##  [2] "Goldman Leads Creation of New Messaging Service"                   
##  [3] "Laura Raicovich to Be New President of Queens Museum"              
##  [4] "A Chic New Hotel Opens in Fez, Morocco"                            
##  [5] "Relational Investors, an Activist Firm, Is Said to Plan a New Fund"
##  [6] "Goldman's New Conflict Rules Go Only So Far"                       
##  [7] "A New Book Celebrates the Downtown Icon Cookie Mueller"            
##  [8] "New Apple Tool Checks iPhones for 'Kill Switch' Security"          
##  [9] "New Ad in Kansas Features a Familiar Face"                         
## [10] "Is Hong Kongs Umbrella Revolution a New Tiananmen?"                
## [11] "Guggenheim Plans New Building in newyork City"                     
## [12] "With New App, Groupon Goes After Grocery Deals "                   
## [13] "Today in Small Business: The Coolest New Businesses in Brooklyn"   
## [14] "Miranda July's Got a Brand-New Bag "                               
## [15] "In L.A., a New Outpost for European Fashion and Art "              
## [16] "Choreographer and Composers Win New Dance Prize"                   
## [17] "A New Culture-Focused Tour of Haiti"                               
## [18] "New Image of the Hong Kong Protests: 'Umbrella Man'"               
## [19] "Breaking Up Is the New Thing to Do in Silicon Valley"              
## [20] "3 New Campaign Ads Feature Jeb Bush, in Spanish"                   
##    
##        0    1
##   0 5218 1080
##   1  221   13
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 20.9379, df = 1, p-value = 4.744e-06
## 
##    
##        0    1
##   0 5238 1086
##   1  201    7
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 26.5716, df = 1, p-value = 2.54e-07
## 
##    
##        0    1
##   0 5372 1092
##   1   67    1
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 10.408, df = 1, p-value = 0.001255
## 
##    
##        0    1
##   0 5169 1082
##   1  270   11
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 33.6731, df = 1, p-value = 6.52e-09
## 
##    
##        0    1
##   0 4989 1071
##   1  450   22
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 52.2842, df = 1, p-value = 4.802e-13
## 
##    
##        0    1
##   0 5212 1092
##   1  227    1
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 43.8153, df = 1, p-value = 3.609e-11
## 
##    
##        0    1
##   0 5270 1089
##   1  169    4
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 25.4715, df = 1, p-value = 4.49e-07
## 
##    
##        0    1
##   0 5287 1093
##   1  152    0
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 30.0556, df = 1, p-value = 4.198e-08
## 
##    
##        0    1
##   0 5341 1090
##   1   98    3
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 12.9608, df = 1, p-value = 0.0003181
## 
##    
##        0    1
##   0 5298 1093
##   1  141    0
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 27.7455, df = 1, p-value = 1.384e-07
## 
## [1] "   Full freqs:"
## <<DocumentTermMatrix (documents: 8402, terms: 9205)>>
## Non-/sparse entries: 44361/77296049
## Sparsity           : 100%
## Maximal term length: 31
## Weighting          : term frequency (tf)
## [1] "   Sparse freqs:"
## <<DocumentTermMatrix (documents: 8402, terms: 10)>>
## Non-/sparse entries: 2407/81613
## Sparsity           : 97%
## Maximal term length: 7
## Weighting          : term frequency (tf)
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

![](NYTBlogs_hdlpfx7_files/figure-html/extract.features-1.png) ![](NYTBlogs_hdlpfx7_files/figure-html/extract.features-2.png) ![](NYTBlogs_hdlpfx7_files/figure-html/extract.features-3.png) 

```
## [1] "Building corpus for Snippet..."
##    
##        0    1
##   0 4538  981
##   1  901  112
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 27.2496, df = 1, p-value = 1.788e-07
## 
## [1] "As they struggle to find new business to bolster sluggish earnings, banks consider the nations 25 million veterans and service members ideal customers."                                                                        
## [2] "Middle-aged and older patients are unlikely to benefit in the long term from surgery to repair tears in the meniscus, pads of cartilage in the knee, a new review of studies has found."                                        
## [3] "A new study has found evidence that legal access to marijuana is associated with fewer opioid overdose deaths, but researchers said their findings should not be used as the basis for the wide adoption of legalized cannabis."
## [4] "African-Americans born at low birth weight are at an increased risk for Type 2 diabetes later in life, a new study has found."                                                                                                  
## [5] "Thanks in part to attention brought in by Michael Lewiss book Flash Boys, IEX has raised $75 million in a new round of financing."                                                                                              
## [6] "Just in time for a new school year and the spring/summer 2015 fashion shows, these black bags are the ultimate in chic-yet-practical carryalls."                                                                                
## [1] "The Decisive Moment, a book of photographs by Henri Cartier-Bresson, has been republished in a new edition."                                                                                                 
## [2] "A new festival in Telluride, Colo., features fire barrels and sculptures."                                                                                                                                   
## [3] "A new survey by the Pew Research Center finds that e-mail is very important to three-fifths of American workers, while social media is a blip on the charts."                                                
## [4] "These 10 pieces reveal the new landscape of design, from unusual places to live to a crop of young ceramicists redefining their field."                                                                      
## [5] "A new round of financing raised $1.1 billion for the company, which has become one of the worlds biggest smartphone makers by offering cheap, high-quality phones through clever online marketing campaigns."
## [6] "newyears Eve can ring in healthful resolutions and new beginnings, but it can also be a risk factor for accidents, death and other dangers."                                                                 
## [1] "New accounts of events from 2008 are casting Lehmans collapse in a different light. | EBay to spin off PayPal. | The dangers of fighting bad press. | A.I.G. trial begins in Washington."  
## [2] "The model Lara Stone whiles away the afternoon in the new oversize mens coats."                                                                                                            
## [3] "The Blackstone Group, the private equity giant run by Stephen A. Schwarzman, is almost finished raising a new fund for energy investments that is expected to exceed $4 billion in assets."
## [4] "Karen Dawisha discusses Putins Kleptocracy, and Stephen Kotkin talks about his new biography of Stalin."                                                                                   
## [5] "The Guggenheim Museum has narrowed the list of architecture firms that might be hired to build a new museum in Helsinki, Finland, to six."                                                 
## [1] 759
##  [1] "Thieves Fall Out, written by Gore Vidal under the name Cameron Kay and published in 1953, will come out in April in a new edition."                                                                                                          
##  [2] "Soon after arriving in newyork in the mid-1960s, Jean-Pierre Laffont discovered that  unlike anywhere else  his new neighbors respected him and his profession."                                                                             
##  [3] "In this new series, Chris Labzda and Bon Duke, the co-founders of the newyork Fashion Film Festival, curate a short film each week for T. This weeks installment: A collage of classics overlaid with sound from Lauren Wolksteins Social..."
##  [4] "The celebrated artist Cai Guo-Qiang discusses his art and a new exhibit, Cai Guo-Qiang: The Ninth Wave."                                                                                                                                     
##  [5] "Flight Tonight, a new digital application, provides travelers with a search service to find the most affordable flight leaving from a nearby airport in the next 24 hours."                                                                  
##  [6] "A Google-backed biotech company plans to build a new Bay Area-based facility to research diseases that afflict the elderly, such as neurodegeneration and cancer."                                                                           
##  [7] "Consumers appear to be unimpressed by smartwatches. But tech companies continue to make new ones in the hope that one of them will catch on."                                                                                                
##  [8] "As the government considers new regulations to curb inversions, a review of Tax Court cases shows the rules would most likely withstand any legal challenge."                                                                                
##  [9] "It will be the first new play by a woman to make it to Broadway since 2013."                                                                                                                                                                 
## [10] "Mr. OBriens The Body of an American received the award for outstanding new American play, while Father Comes Home From the Wars (Parts 1, 2, & 3) by Ms. Parks was selected for promising new American play."                                
##  [1] "The company is rushing to announce new large-format phones in advance of Apples big  and possibly big-screen  announcement next week."                                                                                            
##  [2] "Four new books, each with a distinctive view of style, will help wile away the coming season."                                                                                                                                    
##  [3] "A new start-up called DWNLD is promising to make it cheaper and easier for publishers to convert their sites into attractive apps, which now dominate how people use their mobile phones."                                        
##  [4] "Vanessa Friedman, the fashion director and chief fashion critic for The Times and The International newyork Times begins a month-long season of covering the new collections."                                                    
##  [5] "New research in learning offers new techniques for students: Play teacher. Take a break. Walk around. Get some sleep."                                                                                                            
##  [6] "As an alternative to bronzer, a new golden beige lipstick mimicked the natural flush of the models cheeks."                                                                                                                       
##  [7] "A musical adaptation of Doctor Zhivago is expected to begin performances at the Broadway Theater in the spring of 2015, with another new musical, King Kong, in discussions to move into that theater after the Zhivago run ends."
##  [8] "Gallup reports that new businesses are starved for financing and in decline. How to start a business with a friend (and stay friends). The struggle to find skilled workers."                                                     
##  [9] "The new class of inductees also includes Al Green, Sting and Patricia McBride."                                                                                                                                                   
## [10] "Cartiers spectacular new Tank Louis Sapphire Skeleton features blued steel hands, a sapphire crystal and case back and a movement with 159 individual parts."                                                                     
##  [1] "Metropolitan Diary: A sleepless resident noticed that the new red strobe at the top of 1 World Trade Center pulsated more quickly, urgently, than its predecessor."            
##  [2] "A new study has found that low-carb diets may be better for cardiovascular health than low-fat ones. But one doctor says focusing on low anything is the wrong approach."      
##  [3] "A new website and book feature the work of North African photographers whose projects show aspects of life overshadowed by the regions tumult."                                
##  [4] "Motorolas new device is the rare smartwatch that looks like a fashion accessory. Though it has some shortcomings, it might the best option available, for now."                
##  [5] "Plus, Bill de Blasio attempts to show off his fashion cred, Gisele Bndchen keeps adding new campaigns and more from the week in style."                                        
##  [6] "The artists new show at Envoy Enterprises NYC includes a years worth of lunch and dinner napkins and sugar sculptures of body parts."                                          
##  [7] "At a consumer electronics show in Berlin, major smartphone makers introduced new devices, features or pricing in attempts to distinguish themselves within a crowded industry."
##  [8] "Ben Ratliff and Jon Caramanica discuss new projects by Ms. Grande and Ms. Bush."                                                                                               
##  [9] "The Marriott Chicago OHare has a new vending machine in its lobby, with healthy selections like salads and yogurt."                                                            
## [10] "An annual summer pantry clean-out inspires three new grain salads, and Israel couscous pasta and an eggplant and tomato gratin."                                               
##  [1] "An inside look at the two-year process the two brands underwent to produce a new limited-edition collection, which began with planting and harvesting an organic crop in Alabama."                           
##  [2] "George Martin, author of the Song of Ice and Fire series, will be promoting his new book at the 92nd St. Y."                                                                                                 
##  [3] "Family Dollar said on Friday that it had formally rejected a revised $9.1 billion offer from its larger rival, arguing that new measures meant to provide assurances about antitrust risk were insufficient."
##  [4] "Promising new talents split the morning, including Wes Gordon at 11 a.m. and the LVMH Prize-nominated Chris Gelinas, whose CG presentation begins bright and early at 9:30 a.m."                             
##  [5] "The watch, which is expected to include fitness tracking and wireless payment, will be the first new product unveiled under Tim Cook, who replaced Steve Jobs in 2011."                                      
##  [6] "In a new Insider feature, The newyork Times Book Review extends the By the Book column to Times editors and writers."                                                                                        
##  [7] "The well-known New Atheist makes a case for the value of spirituality, which he bases on his experiences in meditation."                                                                                     
##  [8] "Cokes assurance of support for a new compensation plan failed to account for opposition by three of the companys six largest shareholders."                                                                  
##  [9] "The fashion designer and the stand-up comedian tackle furry shoes, fancy toothpaste and a new film about Nick Cave."                                                                                         
## [10] "To celebrate his show today and the opening of his new NoHo store, the designer lists a few of his essentials."                                                                                              
##  [1] "Children born to women who had gestational diabetes have a substantially higher likelihood of developing diabetes as teenagers, a new study shows."                                                                 
##  [2] "The most reliable workers are those who get seven to eight hours of sleep each night, a new study shows."                                                                                                           
##  [3] "Today is the 350th anniversary of the surrender of Dutch New Amsterdam to the British and the settlements renaming as newyork; no official commemoration is planned."                                               
##  [4] "A new analysis of water from fracked wells around the country clarifies treatment needs."                                                                                                                           
##  [5] "Just as the iPods appeal relied in part on Apples music industry relationships, the future of its new wrist device may rest on courting health companies."                                                          
##  [6] "In the late 1960s at the height of his career, Dennis Hopper left Hollywood for artistic bohemia in New Mexico. His daughter looks back on a man in search of free expression and a more contemplative way of life."
##  [7] "Ms. Hannahs The Monogram Murders is the first new Poirot story since Christies death in 1976."                                                                                                                      
##  [8] "The long-awaited, much-anticipated Apple Watch. Dont call it an iWatch; this is a new era, people. OMG! OMG! OMG! And all that."                                                                                    
##  [9] "We try to answer your pressing questions about Apples new line of products. It doesnt look good for left-handed watches."                                                                                           
## [10] "While the company makes its much-anticipated announcements about its new products, it seems to be having a lot of trouble with the live-streaming of the event."                                                    
##    
##        0    1
##   0 4882 1035
##   1  557   58
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 25.4065, df = 1, p-value = 4.644e-07
## 
##    
##        0    1
##   0 4925 1035
##   1  514   58
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 19.0433, df = 1, p-value = 1.278e-05
## 
##    
##        0    1
##   0 4937 1068
##   1  502   25
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 58.2069, df = 1, p-value = 2.36e-14
## 
##    
##        0    1
##   0 5060 1042
##   1  379   51
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 7.4736, df = 1, p-value = 0.006261
## 
##    
##        0    1
##   0 5060 1061
##   1  379   32
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 24.5185, df = 1, p-value = 7.36e-07
## 
##    
##        0    1
##   0 5099 1060
##   1  340   33
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 17.0608, df = 1, p-value = 3.62e-05
## 
##    
##        0    1
##   0 4871  973
##   1  568  120
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 0.2234, df = 1, p-value = 0.6365
## 
##    
##        0    1
##   0 5195 1073
##   1  244   20
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 15.88, df = 1, p-value = 6.749e-05
## 
##    
##        0    1
##   0 5192 1059
##   1  247   34
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 4.1835, df = 1, p-value = 0.04082
## 
##    
##        0    1
##   0 5227 1051
##   1  212   42
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 0, df = 1, p-value = 0.9997
## 
##    
##        0    1
##   0 4837  951
##   1  602  142
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 3.1486, df = 1, p-value = 0.07599
## 
##    
##        0    1
##   0 4672 1024
##   1  767   69
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 48.7775, df = 1, p-value = 2.867e-12
## 
## [1] "   Full freqs:"
## <<DocumentTermMatrix (documents: 8402, terms: 13822)>>
## Non-/sparse entries: 105519/116026925
## Sparsity           : 100%
## Maximal term length: 25
## Weighting          : term frequency (tf)
## [1] "   Sparse freqs:"
## <<DocumentTermMatrix (documents: 8402, terms: 22)>>
## Non-/sparse entries: 8657/176187
## Sparsity           : 95%
## Maximal term length: 7
## Weighting          : term frequency (tf)
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

![](NYTBlogs_hdlpfx7_files/figure-html/extract.features-4.png) ![](NYTBlogs_hdlpfx7_files/figure-html/extract.features-5.png) ![](NYTBlogs_hdlpfx7_files/figure-html/extract.features-6.png) 

```
## [1] "Building corpus for Abstract..."
##    
##        0    1
##   0 4536  981
##   1  903  112
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 27.5262, df = 1, p-value = 1.55e-07
## 
## [1] "As they struggle to find new business to bolster sluggish earnings, banks consider the nations 25 million veterans and service members ideal customers."                                                                        
## [2] "Middle-aged and older patients are unlikely to benefit in the long term from surgery to repair tears in the meniscus, pads of cartilage in the knee, a new review of studies has found."                                        
## [3] "A new study has found evidence that legal access to marijuana is associated with fewer opioid overdose deaths, but researchers said their findings should not be used as the basis for the wide adoption of legalized cannabis."
## [4] "African-Americans born at low birth weight are at an increased risk for Type 2 diabetes later in life, a new study has found."                                                                                                  
## [5] "Thanks in part to attention brought in by Michael Lewiss book Flash Boys, IEX has raised $75 million in a new round of financing."                                                                                              
## [6] "Just in time for a new school year and the spring/summer 2015 fashion shows, these black bags are the ultimate in chic-yet-practical carryalls."                                                                                
## [1] "The Decisive Moment, a book of photographs by Henri Cartier-Bresson, has been republished in a new edition."                                                                                                 
## [2] "A new festival in Telluride, Colo., features fire barrels and sculptures."                                                                                                                                   
## [3] "A new survey by the Pew Research Center finds that e-mail is very important to three-fifths of American workers, while social media is a blip on the charts."                                                
## [4] "These 10 pieces reveal the new landscape of design, from unusual places to live to a crop of young ceramicists redefining their field."                                                                      
## [5] "A new round of financing raised $1.1 billion for the company, which has become one of the worlds biggest smartphone makers by offering cheap, high-quality phones through clever online marketing campaigns."
## [6] "newyears Eve can ring in healthful resolutions and new beginnings, but it can also be a risk factor for accidents, death and other dangers."                                                                 
## [1] "Two new books look at the phenomenon of MOOCs, online courses from prestigious universities, most of them free and enrolling throngs of students. An essay in Education Life excerpts one of them."                                                                   
## [2] "On the occasion of the release of their new albums this month, the offspring of Jada Pinkett Smith and Will Smith sat down with T for their first-ever joint interview."                                                                                              
## [3] "Each day until Christmas, the editors of T share a new holiday gift idea."                                                                                                                                                                                            
## [4] "Now is the moment to prepare children for Thanksgiving: show pictures of past gatherings, remind them of the names that go with faces and talk about things like interrupting, playing with younger cousins, new foods and appreciating gifts but not expecting them."
## [5] "Why all the fuss about Ello, a new social network that is some amalgam of Twitter, Facebook and Medium, the online publishing start-up, rolled into one?"                                                                                                             
## [1] 761
##  [1] "Thieves Fall Out, written by Gore Vidal under the name Cameron Kay and published in 1953, will come out in April in a new edition."                                                                                                                  
##  [2] "Soon after arriving in newyork in the mid-1960s, Jean-Pierre Laffont discovered that  unlike anywhere else  his new neighbors respected him and his profession."                                                                                     
##  [3] "In this new series, Chris Labzda and Bon Duke, the co-founders of the newyork Fashion Film Festival, curate a short film each week for T. This weeks installment: A collage of classics overlaid with sound from Lauren Wolksteins Social Butterfly."
##  [4] "The celebrated artist Cai Guo-Qiang discusses his art and a new exhibit, Cai Guo-Qiang: The Ninth Wave."                                                                                                                                             
##  [5] "Flight Tonight, a new digital application, provides travelers with a search service to find the most affordable flight leaving from a nearby airport in the next 24 hours."                                                                          
##  [6] "A Google-backed biotech company plans to build a new Bay Area-based facility to research diseases that afflict the elderly, such as neurodegeneration and cancer."                                                                                   
##  [7] "Consumers appear to be unimpressed by smartwatches. But tech companies continue to make new ones in the hope that one of them will catch on."                                                                                                        
##  [8] "As the government considers new regulations to curb inversions, a review of Tax Court cases shows the rules would most likely withstand any legal challenge."                                                                                        
##  [9] "It will be the first new play by a woman to make it to Broadway since 2013."                                                                                                                                                                         
## [10] "Mr. OBriens The Body of an American received the award for outstanding new American play, while Father Comes Home From the Wars (Parts 1, 2, & 3) by Ms. Parks was selected for promising new American play."                                        
##  [1] "The company is rushing to announce new large-format phones in advance of Apples big  and possibly big-screen  announcement next week."                                                                                            
##  [2] "Four new books, each with a distinctive view of style, will help wile away the coming season."                                                                                                                                    
##  [3] "A new start-up called DWNLD is promising to make it cheaper and easier for publishers to convert their sites into attractive apps, which now dominate how people use their mobile phones."                                        
##  [4] "Vanessa Friedman, the fashion director and chief fashion critic for The Times and The International newyork Times begins a month-long season of covering the new collections."                                                    
##  [5] "New research in learning offers new techniques for students: Play teacher. Take a break. Walk around. Get some sleep."                                                                                                            
##  [6] "As an alternative to bronzer, a new golden beige lipstick mimicked the natural flush of the models cheeks."                                                                                                                       
##  [7] "A musical adaptation of Doctor Zhivago is expected to begin performances at the Broadway Theater in the spring of 2015, with another new musical, King Kong, in discussions to move into that theater after the Zhivago run ends."
##  [8] "Gallup reports that new businesses are starved for financing and in decline. How to start a business with a friend (and stay friends). The struggle to find skilled workers."                                                     
##  [9] "The new class of inductees also includes Al Green, Sting and Patricia McBride."                                                                                                                                                   
## [10] "Cartiers spectacular new Tank Louis Sapphire Skeleton features blued steel hands, a sapphire crystal and case back and a movement with 159 individual parts."                                                                     
##  [1] "Metropolitan Diary: A sleepless resident noticed that the new red strobe at the top of 1 World Trade Center pulsated more quickly, urgently, than its predecessor."            
##  [2] "A new study has found that low-carb diets may be better for cardiovascular health than low-fat ones. But one doctor says focusing on low anything is the wrong approach."      
##  [3] "A new website and book feature the work of North African photographers whose projects show aspects of life overshadowed by the regions tumult."                                
##  [4] "Motorolas new device is the rare smartwatch that looks like a fashion accessory. Though it has some shortcomings, it might the best option available, for now."                
##  [5] "Plus, Bill de Blasio attempts to show off his fashion cred, Gisele Bndchen keeps adding new campaigns and more from the week in style."                                        
##  [6] "The artists new show at Envoy Enterprises NYC includes a years worth of lunch and dinner napkins and sugar sculptures of body parts."                                          
##  [7] "At a consumer electronics show in Berlin, major smartphone makers introduced new devices, features or pricing in attempts to distinguish themselves within a crowded industry."
##  [8] "Ben Ratliff and Jon Caramanica discuss new projects by Ms. Grande and Ms. Bush."                                                                                               
##  [9] "The Marriott Chicago OHare has a new vending machine in its lobby, with healthy selections like salads and yogurt."                                                            
## [10] "An annual summer pantry clean-out inspires three new grain salads, and Israel couscous pasta and an eggplant and tomato gratin."                                               
##  [1] "An inside look at the two-year process the two brands underwent to produce a new limited-edition collection, which began with planting and harvesting an organic crop in Alabama."                           
##  [2] "George Martin, author of the Song of Ice and Fire series, will be promoting his new book at the 92nd St. Y."                                                                                                 
##  [3] "Family Dollar said on Friday that it had formally rejected a revised $9.1 billion offer from its larger rival, arguing that new measures meant to provide assurances about antitrust risk were insufficient."
##  [4] "Promising new talents split the morning, including Wes Gordon at 11 a.m. and the LVMH Prize-nominated Chris Gelinas, whose CG presentation begins bright and early at 9:30 a.m."                             
##  [5] "The watch, which is expected to include fitness tracking and wireless payment, will be the first new product unveiled under Tim Cook, who replaced Steve Jobs in 2011."                                      
##  [6] "In a new Insider feature, The newyork Times Book Review extends the By the Book column to Times editors and writers."                                                                                        
##  [7] "The well-known New Atheist makes a case for the value of spirituality, which he bases on his experiences in meditation."                                                                                     
##  [8] "Cokes assurance of support for a new compensation plan failed to account for opposition by three of the companys six largest shareholders."                                                                  
##  [9] "The fashion designer and the stand-up comedian tackle furry shoes, fancy toothpaste and a new film about Nick Cave."                                                                                         
## [10] "To celebrate his show today and the opening of his new NoHo store, the designer lists a few of his essentials."                                                                                              
##  [1] "Children born to women who had gestational diabetes have a substantially higher likelihood of developing diabetes as teenagers, a new study shows."                                                                 
##  [2] "The most reliable workers are those who get seven to eight hours of sleep each night, a new study shows."                                                                                                           
##  [3] "Today is the 350th anniversary of the surrender of Dutch New Amsterdam to the British and the settlements renaming as newyork; no official commemoration is planned."                                               
##  [4] "A new analysis of water from fracked wells around the country clarifies treatment needs."                                                                                                                           
##  [5] "Just as the iPods appeal relied in part on Apples music industry relationships, the future of its new wrist device may rest on courting health companies."                                                          
##  [6] "In the late 1960s at the height of his career, Dennis Hopper left Hollywood for artistic bohemia in New Mexico. His daughter looks back on a man in search of free expression and a more contemplative way of life."
##  [7] "Ms. Hannahs The Monogram Murders is the first new Poirot story since Christies death in 1976."                                                                                                                      
##  [8] "The long-awaited, much-anticipated Apple Watch. Dont call it an iWatch; this is a new era, people. OMG! OMG! OMG! And all that."                                                                                    
##  [9] "We try to answer your pressing questions about Apples new line of products. It doesnt look good for left-handed watches."                                                                                           
## [10] "While the company makes its much-anticipated announcements about its new products, it seems to be having a lot of trouble with the live-streaming of the event."                                                    
##    
##        0    1
##   0 4880 1035
##   1  559   58
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 25.716, df = 1, p-value = 3.955e-07
## 
##    
##        0    1
##   0 4920 1035
##   1  519   58
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 19.7532, df = 1, p-value = 8.811e-06
## 
##    
##        0    1
##   0 4937 1068
##   1  502   25
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 58.2069, df = 1, p-value = 2.36e-14
## 
##    
##        0    1
##   0 5060 1042
##   1  379   51
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 7.4736, df = 1, p-value = 0.006261
## 
##    
##        0    1
##   0 5060 1061
##   1  379   32
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 24.5185, df = 1, p-value = 7.36e-07
## 
##    
##        0    1
##   0 5099 1060
##   1  340   33
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 17.0608, df = 1, p-value = 3.62e-05
## 
##    
##        0    1
##   0 4868  973
##   1  571  120
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 0.1744, df = 1, p-value = 0.6762
## 
##    
##        0    1
##   0 5195 1073
##   1  244   20
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 15.88, df = 1, p-value = 6.749e-05
## 
##    
##        0    1
##   0 5192 1059
##   1  247   34
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 4.1835, df = 1, p-value = 0.04082
## 
##    
##        0    1
##   0 5227 1051
##   1  212   42
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 0, df = 1, p-value = 0.9997
## 
##    
##        0    1
##   0 4835  951
##   1  604  142
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 3.0189, df = 1, p-value = 0.0823
## 
##    
##        0    1
##   0 4669 1024
##   1  770   69
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 49.3248, df = 1, p-value = 2.169e-12
## 
##    
##        0    1
##   0 5205 1048
##   1  234   45
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 0.0377, df = 1, p-value = 0.846
## 
##    
##        0    1
##   0 5219 1049
##   1  220   44
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 0, df = 1, p-value = 1
## 
##    
##        0    1
##   0 5224 1068
##   1  215   25
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 6.6715, df = 1, p-value = 0.009797
## 
## [1] "   Full freqs:"
## <<DocumentTermMatrix (documents: 8402, terms: 13864)>>
## Non-/sparse entries: 103163/116382165
## Sparsity           : 100%
## Maximal term length: 112
## Weighting          : term frequency (tf)
## [1] "   Sparse freqs:"
## <<DocumentTermMatrix (documents: 8402, terms: 11)>>
## Non-/sparse entries: 4787/87635
## Sparsity           : 95%
## Maximal term length: 7
## Weighting          : term frequency (tf)
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

![](NYTBlogs_hdlpfx7_files/figure-html/extract.features-7.png) ![](NYTBlogs_hdlpfx7_files/figure-html/extract.features-8.png) ![](NYTBlogs_hdlpfx7_files/figure-html/extract.features-9.png) 

```
## [1] "Binding DTM for Headline..."
##    
##        0    1
##   0 5373 1071
##   1   66   22
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 3.7947, df = 1, p-value = 0.05142
## 
##    
##        0    1
##   0 5368 1075
##   1   71   18
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 0.5559, df = 1, p-value = 0.4559
## 
##    
##        0    1
##   0 5367 1075
##   1   72   18
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 0.4815, df = 1, p-value = 0.4877
```

![](NYTBlogs_hdlpfx7_files/figure-html/extract.features-10.png) ![](NYTBlogs_hdlpfx7_files/figure-html/extract.features-11.png) ![](NYTBlogs_hdlpfx7_files/figure-html/extract.features-12.png) 

```
## [1] "Binding DTM for Snippet..."
##    
##        0    1
##   0 5373 1071
##   1   66   22
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 3.7947, df = 1, p-value = 0.05142
## 
##    
##        0    1
##   0 5368 1075
##   1   71   18
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 0.5559, df = 1, p-value = 0.4559
## 
##    
##        0    1
##   0 5367 1075
##   1   72   18
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 0.4815, df = 1, p-value = 0.4877
```

![](NYTBlogs_hdlpfx7_files/figure-html/extract.features-13.png) ![](NYTBlogs_hdlpfx7_files/figure-html/extract.features-14.png) ![](NYTBlogs_hdlpfx7_files/figure-html/extract.features-15.png) 

```
## [1] "Binding DTM for Abstract..."
##    
##        0    1
##   0 5373 1071
##   1   66   22
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 3.7947, df = 1, p-value = 0.05142
## 
##    
##        0    1
##   0 5368 1075
##   1   71   18
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 0.5559, df = 1, p-value = 0.4559
## 
##    
##        0    1
##   0 5367 1075
##   1   72   18
## [1] "Rows:Selected; Cols:Popular"
## 
## 	Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  mrg_tbl
## X-squared = 0.4815, df = 1, p-value = 0.4877
```

![](NYTBlogs_hdlpfx7_files/figure-html/extract.features-16.png) ![](NYTBlogs_hdlpfx7_files/figure-html/extract.features-17.png) ![](NYTBlogs_hdlpfx7_files/figure-html/extract.features-18.png) 

```r
# Re-partition
glb_trnent_df <- subset(glb_entity_df, .src == "Train")
glb_newent_df <- subset(glb_entity_df, .src == "Test")

# print(sapply(names(glb_trnent_df), function(col) sum(is.na(glb_trnent_df[, col]))))
# print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))

# print(myplot_scatter(glb_trnent_df, "<col1_name>", "<col2_name>", smooth=TRUE))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all","data.new")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0
```

![](NYTBlogs_hdlpfx7_files/figure-html/extract.features-19.png) 

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="select.features", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##               chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed5 extract.features                3                0  40.615
## elapsed6  select.features                4                0 139.703
```

## Step `4`: select features

```r
print(glb_feats_df <- myselect_features(entity_df=glb_trnent_df, 
                       exclude_vars_as_features=glb_exclude_vars_as_features, 
                       rsp_var=glb_rsp_var))
```

```
## Warning in cor(data.matrix(entity_df[, sel_feats]), y =
## as.numeric(entity_df[, : the standard deviation is zero
```

```
##                                      id        cor.y exclude.as.feat
## Popular                         Popular  1.000000000               1
## WordCount.log             WordCount.log  0.265952699               0
## WordCount                     WordCount  0.257526549               1
## S.num.words.unq.log S.num.words.unq.log -0.250796919               0
## S.num.words.log         S.num.words.log -0.245354135               0
## A.num.words.unq.log A.num.words.unq.log -0.226995906               0
## S.num.chars.log         S.num.chars.log -0.224692967               0
## A.num.chars.log         A.num.chars.log -0.224548821               0
## A.num.words.log         A.num.words.log -0.221953007               0
## S.num.words.unq         S.num.words.unq -0.212102717               1
## S.num.words                 S.num.words -0.206385049               1
## H.num.words.unq.log H.num.words.unq.log -0.204496360               0
## H.num.words.log         H.num.words.log -0.200686356               0
## A.num.words.unq         A.num.words.unq -0.192819225               1
## H.num.words.unq         H.num.words.unq -0.189702157               1
## A.num.words                 A.num.words -0.187423227               1
## H.num.words                 H.num.words -0.186036895               1
## S.num.chars                 S.num.chars -0.179331806               1
## A.num.chars                 A.num.chars -0.177037425               1
## H.num.chars.log         H.num.chars.log -0.171062360               0
## SubsectionName.fctr SubsectionName.fctr -0.168723053               0
## SectionName.fctr       SectionName.fctr -0.165854115               0
## NewsDesk.fctr             NewsDesk.fctr -0.161611606               0
## PubDate.hour               PubDate.hour  0.159167673               0
## Headline.pfx.fctr     Headline.pfx.fctr -0.156829778               0
## H.num.chars                 H.num.chars -0.147211183               1
## PubDate.apm.fctr       PubDate.apm.fctr  0.101472715               0
## S.fashion                     S.fashion -0.086446251               0
## S.week                           S.week -0.084814939               0
## H.fashion                     H.fashion -0.081708612               0
## H.week                           H.week -0.075105216               0
## H.daili                         H.daili -0.069192975               0
## S.intern                       S.intern -0.068485701               0
## A.intern                       A.intern -0.068485701               0
## H.X2015                         H.X2015 -0.066584892               0
## H.report                       H.report -0.064948102               0
## H.today                         H.today -0.063723058               0
## S.newyork                     S.newyork -0.062117105               0
## H.day                             H.day -0.061669687               0
## A.will                           A.will -0.061025004               0
## S.will                           S.will -0.060575493               0
## S.articl                       S.articl -0.059520554               0
## A.articl                       A.articl -0.059520554               0
## H.newyork                     H.newyork -0.057970095               0
## A.time                           A.time -0.057790617               0
## S.time                           S.time -0.057595102               0
## S.first                         S.first -0.053388178               0
## H.new                             H.new -0.053121542               0
## A.compani                     A.compani -0.053099633               0
## S.compani                     S.compani -0.053012962               0
## S.year                           S.year -0.051146178               0
## S.share                         S.share -0.050329686               0
## S.report                       S.report -0.050211524               0
## S.show                           S.show -0.048801740               0
## H.X2014                         H.X2014 -0.046206380               0
## A.day                             A.day -0.045909684               0
## S.day                             S.day -0.045649185               0
## PubDate.wkday.fctr   PubDate.wkday.fctr -0.039801288               0
## A.new                             A.new -0.035359447               0
## S.new                             S.new -0.034948520               0
## A.can                             A.can  0.031498867               0
## PubDate.minute           PubDate.minute -0.031469083               0
## S.can                             S.can  0.029999780               0
## A.take                           A.take -0.026086108               0
## H.has.ebola                 H.has.ebola  0.025881397               0
## S.take                           S.take -0.025762398               0
## S.make                           S.make  0.023138853               0
## S.presid                       S.presid -0.019828826               0
## A.presid                       A.presid -0.019828826               0
## PubDate.month.fctr   PubDate.month.fctr  0.019148739               1
## A.has.http                   A.has.http -0.013592603               0
## PubDate.second           PubDate.second -0.012253600               0
## UniqueID                       UniqueID  0.011824920               1
## PubDate.date.fctr     PubDate.date.fctr -0.011647558               0
## .rnorm                           .rnorm -0.008703337               0
## S.one                             S.one  0.006342094               0
## S.state                         S.state  0.006069626               0
## A.one                             A.one  0.005696039               0
## S.said                           S.said  0.001363226               0
## PubDate.year               PubDate.year           NA               1
## H.has.http                   H.has.http           NA               0
## S.has.http                   S.has.http           NA               0
##                       cor.y.abs
## Popular             1.000000000
## WordCount.log       0.265952699
## WordCount           0.257526549
## S.num.words.unq.log 0.250796919
## S.num.words.log     0.245354135
## A.num.words.unq.log 0.226995906
## S.num.chars.log     0.224692967
## A.num.chars.log     0.224548821
## A.num.words.log     0.221953007
## S.num.words.unq     0.212102717
## S.num.words         0.206385049
## H.num.words.unq.log 0.204496360
## H.num.words.log     0.200686356
## A.num.words.unq     0.192819225
## H.num.words.unq     0.189702157
## A.num.words         0.187423227
## H.num.words         0.186036895
## S.num.chars         0.179331806
## A.num.chars         0.177037425
## H.num.chars.log     0.171062360
## SubsectionName.fctr 0.168723053
## SectionName.fctr    0.165854115
## NewsDesk.fctr       0.161611606
## PubDate.hour        0.159167673
## Headline.pfx.fctr   0.156829778
## H.num.chars         0.147211183
## PubDate.apm.fctr    0.101472715
## S.fashion           0.086446251
## S.week              0.084814939
## H.fashion           0.081708612
## H.week              0.075105216
## H.daili             0.069192975
## S.intern            0.068485701
## A.intern            0.068485701
## H.X2015             0.066584892
## H.report            0.064948102
## H.today             0.063723058
## S.newyork           0.062117105
## H.day               0.061669687
## A.will              0.061025004
## S.will              0.060575493
## S.articl            0.059520554
## A.articl            0.059520554
## H.newyork           0.057970095
## A.time              0.057790617
## S.time              0.057595102
## S.first             0.053388178
## H.new               0.053121542
## A.compani           0.053099633
## S.compani           0.053012962
## S.year              0.051146178
## S.share             0.050329686
## S.report            0.050211524
## S.show              0.048801740
## H.X2014             0.046206380
## A.day               0.045909684
## S.day               0.045649185
## PubDate.wkday.fctr  0.039801288
## A.new               0.035359447
## S.new               0.034948520
## A.can               0.031498867
## PubDate.minute      0.031469083
## S.can               0.029999780
## A.take              0.026086108
## H.has.ebola         0.025881397
## S.take              0.025762398
## S.make              0.023138853
## S.presid            0.019828826
## A.presid            0.019828826
## PubDate.month.fctr  0.019148739
## A.has.http          0.013592603
## PubDate.second      0.012253600
## UniqueID            0.011824920
## PubDate.date.fctr   0.011647558
## .rnorm              0.008703337
## S.one               0.006342094
## S.state             0.006069626
## A.one               0.005696039
## S.said              0.001363226
## PubDate.year                 NA
## H.has.http                   NA
## S.has.http                   NA
```

```r
glb_script_df <- rbind(glb_script_df, 
    data.frame(chunk_label="remove.correlated.features", 
        chunk_step_major=max(glb_script_df$chunk_step_major),
        chunk_step_minor=glb_script_df[nrow(glb_script_df), "chunk_step_minor"]+1,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))        
print(tail(glb_script_df, 2))
```

```
##                         chunk_label chunk_step_major chunk_step_minor
## elapsed6            select.features                4                0
## elapsed7 remove.correlated.features                4                1
##          elapsed
## elapsed6 139.703
## elapsed7 142.664
```

### Step `4`.`1`: remove correlated features

```r
print(glb_feats_df <- orderBy(~-cor.y, 
          myfind_cor_features(feats_df=glb_feats_df, entity_df=glb_trnent_df, 
                              rsp_var=glb_rsp_var, 
                            checkConditionalX=(glb_is_classification && glb_is_binomial))))
```

```
## Loading required package: caret
## 
## Attaching package: 'caret'
## 
## The following object is masked from 'package:survival':
## 
##     cluster
```

```
## [1] "cor(A.articl, S.articl)=1.0000"
## [1] "cor(Popular.fctr, A.articl)=-0.0595"
## [1] "cor(Popular.fctr, S.articl)=-0.0595"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.articl as highly correlated with A.articl
```

```
## [1] "cor(A.intern, S.intern)=1.0000"
## [1] "cor(Popular.fctr, A.intern)=-0.0685"
## [1] "cor(Popular.fctr, S.intern)=-0.0685"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.intern as highly correlated with A.intern
```

```
## [1] "cor(A.presid, S.presid)=1.0000"
## [1] "cor(Popular.fctr, A.presid)=-0.0198"
## [1] "cor(Popular.fctr, S.presid)=-0.0198"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.presid as highly correlated with A.presid
```

```
## [1] "cor(A.time, S.time)=0.9991"
## [1] "cor(Popular.fctr, A.time)=-0.0578"
## [1] "cor(Popular.fctr, S.time)=-0.0576"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.time as highly correlated with A.time
```

```
## [1] "cor(A.compani, S.compani)=0.9988"
## [1] "cor(Popular.fctr, A.compani)=-0.0531"
## [1] "cor(Popular.fctr, S.compani)=-0.0530"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.compani as highly correlated with A.compani
```

```
## [1] "cor(A.num.chars.log, S.num.chars.log)=0.9986"
## [1] "cor(Popular.fctr, A.num.chars.log)=-0.2245"
## [1] "cor(Popular.fctr, S.num.chars.log)=-0.2247"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.num.chars.log as highly correlated with
## S.num.chars.log
```

```
## [1] "cor(A.new, S.new)=0.9983"
## [1] "cor(Popular.fctr, A.new)=-0.0354"
## [1] "cor(Popular.fctr, S.new)=-0.0349"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.new as highly correlated with A.new
```

```
## [1] "cor(A.can, S.can)=0.9982"
## [1] "cor(Popular.fctr, A.can)=0.0315"
## [1] "cor(Popular.fctr, S.can)=0.0300"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.can as highly correlated with A.can
```

```
## [1] "cor(A.day, S.day)=0.9981"
## [1] "cor(Popular.fctr, A.day)=-0.0459"
## [1] "cor(Popular.fctr, S.day)=-0.0456"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.day as highly correlated with A.day
```

```
## [1] "cor(A.take, S.take)=0.9976"
## [1] "cor(Popular.fctr, A.take)=-0.0261"
## [1] "cor(Popular.fctr, S.take)=-0.0258"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.take as highly correlated with A.take
```

```
## [1] "cor(A.will, S.will)=0.9976"
## [1] "cor(Popular.fctr, A.will)=-0.0610"
## [1] "cor(Popular.fctr, S.will)=-0.0606"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.will as highly correlated with A.will
```

```
## [1] "cor(H.num.words.log, H.num.words.unq.log)=0.9967"
## [1] "cor(Popular.fctr, H.num.words.log)=-0.2007"
## [1] "cor(Popular.fctr, H.num.words.unq.log)=-0.2045"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.num.words.log as highly correlated with
## H.num.words.unq.log
```

```
## [1] "cor(A.num.words.log, A.num.words.unq.log)=0.9957"
## [1] "cor(Popular.fctr, A.num.words.log)=-0.2220"
## [1] "cor(Popular.fctr, A.num.words.unq.log)=-0.2270"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.num.words.log as highly correlated with
## A.num.words.unq.log
```

```
## [1] "cor(S.num.words.log, S.num.words.unq.log)=0.9954"
## [1] "cor(Popular.fctr, S.num.words.log)=-0.2454"
## [1] "cor(Popular.fctr, S.num.words.unq.log)=-0.2508"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.num.words.log as highly correlated with
## S.num.words.unq.log
```

```
## [1] "cor(A.num.words.unq.log, S.num.words.unq.log)=0.9872"
## [1] "cor(Popular.fctr, A.num.words.unq.log)=-0.2270"
## [1] "cor(Popular.fctr, S.num.words.unq.log)=-0.2508"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.num.words.unq.log as highly correlated with
## S.num.words.unq.log
```

```
## [1] "cor(S.num.chars.log, S.num.words.unq.log)=0.9543"
## [1] "cor(Popular.fctr, S.num.chars.log)=-0.2247"
## [1] "cor(Popular.fctr, S.num.words.unq.log)=-0.2508"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.num.chars.log as highly correlated with
## S.num.words.unq.log
```

```
## [1] "cor(SectionName.fctr, SubsectionName.fctr)=0.9089"
## [1] "cor(Popular.fctr, SectionName.fctr)=-0.1659"
## [1] "cor(Popular.fctr, SubsectionName.fctr)=-0.1687"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified SectionName.fctr as highly correlated with
## SubsectionName.fctr
```

```
## [1] "cor(H.num.chars.log, H.num.words.unq.log)=0.8881"
## [1] "cor(Popular.fctr, H.num.chars.log)=-0.1711"
## [1] "cor(Popular.fctr, H.num.words.unq.log)=-0.2045"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.num.chars.log as highly correlated with
## H.num.words.unq.log
```

```
## [1] "cor(PubDate.apm.fctr, PubDate.hour)=0.8156"
## [1] "cor(Popular.fctr, PubDate.apm.fctr)=0.1015"
## [1] "cor(Popular.fctr, PubDate.hour)=0.1592"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified PubDate.apm.fctr as highly correlated with
## PubDate.hour
```

```
## [1] "cor(H.fashion, H.week)=0.7616"
## [1] "cor(Popular.fctr, H.fashion)=-0.0817"
## [1] "cor(Popular.fctr, H.week)=-0.0751"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.week as highly correlated with H.fashion
```

```
## [1] "cor(NewsDesk.fctr, SubsectionName.fctr)=0.7223"
## [1] "cor(Popular.fctr, NewsDesk.fctr)=-0.1616"
## [1] "cor(Popular.fctr, SubsectionName.fctr)=-0.1687"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified NewsDesk.fctr as highly correlated with
## SubsectionName.fctr
```

```
##                                      id        cor.y exclude.as.feat
## Popular                         Popular  1.000000000               1
## WordCount.log             WordCount.log  0.265952699               0
## WordCount                     WordCount  0.257526549               1
## PubDate.hour               PubDate.hour  0.159167673               0
## PubDate.apm.fctr       PubDate.apm.fctr  0.101472715               0
## A.can                             A.can  0.031498867               0
## S.can                             S.can  0.029999780               0
## H.has.ebola                 H.has.ebola  0.025881397               0
## S.make                           S.make  0.023138853               0
## PubDate.month.fctr   PubDate.month.fctr  0.019148739               1
## UniqueID                       UniqueID  0.011824920               1
## S.one                             S.one  0.006342094               0
## S.state                         S.state  0.006069626               0
## A.one                             A.one  0.005696039               0
## S.said                           S.said  0.001363226               0
## .rnorm                           .rnorm -0.008703337               0
## PubDate.date.fctr     PubDate.date.fctr -0.011647558               0
## PubDate.second           PubDate.second -0.012253600               0
## A.has.http                   A.has.http -0.013592603               0
## S.presid                       S.presid -0.019828826               0
## A.presid                       A.presid -0.019828826               0
## S.take                           S.take -0.025762398               0
## A.take                           A.take -0.026086108               0
## PubDate.minute           PubDate.minute -0.031469083               0
## S.new                             S.new -0.034948520               0
## A.new                             A.new -0.035359447               0
## PubDate.wkday.fctr   PubDate.wkday.fctr -0.039801288               0
## S.day                             S.day -0.045649185               0
## A.day                             A.day -0.045909684               0
## H.X2014                         H.X2014 -0.046206380               0
## S.show                           S.show -0.048801740               0
## S.report                       S.report -0.050211524               0
## S.share                         S.share -0.050329686               0
## S.year                           S.year -0.051146178               0
## S.compani                     S.compani -0.053012962               0
## A.compani                     A.compani -0.053099633               0
## H.new                             H.new -0.053121542               0
## S.first                         S.first -0.053388178               0
## S.time                           S.time -0.057595102               0
## A.time                           A.time -0.057790617               0
## H.newyork                     H.newyork -0.057970095               0
## S.articl                       S.articl -0.059520554               0
## A.articl                       A.articl -0.059520554               0
## S.will                           S.will -0.060575493               0
## A.will                           A.will -0.061025004               0
## H.day                             H.day -0.061669687               0
## S.newyork                     S.newyork -0.062117105               0
## H.today                         H.today -0.063723058               0
## H.report                       H.report -0.064948102               0
## H.X2015                         H.X2015 -0.066584892               0
## S.intern                       S.intern -0.068485701               0
## A.intern                       A.intern -0.068485701               0
## H.daili                         H.daili -0.069192975               0
## H.week                           H.week -0.075105216               0
## H.fashion                     H.fashion -0.081708612               0
## S.week                           S.week -0.084814939               0
## S.fashion                     S.fashion -0.086446251               0
## H.num.chars                 H.num.chars -0.147211183               1
## Headline.pfx.fctr     Headline.pfx.fctr -0.156829778               0
## NewsDesk.fctr             NewsDesk.fctr -0.161611606               0
## SectionName.fctr       SectionName.fctr -0.165854115               0
## SubsectionName.fctr SubsectionName.fctr -0.168723053               0
## H.num.chars.log         H.num.chars.log -0.171062360               0
## A.num.chars                 A.num.chars -0.177037425               1
## S.num.chars                 S.num.chars -0.179331806               1
## H.num.words                 H.num.words -0.186036895               1
## A.num.words                 A.num.words -0.187423227               1
## H.num.words.unq         H.num.words.unq -0.189702157               1
## A.num.words.unq         A.num.words.unq -0.192819225               1
## H.num.words.log         H.num.words.log -0.200686356               0
## H.num.words.unq.log H.num.words.unq.log -0.204496360               0
## S.num.words                 S.num.words -0.206385049               1
## S.num.words.unq         S.num.words.unq -0.212102717               1
## A.num.words.log         A.num.words.log -0.221953007               0
## A.num.chars.log         A.num.chars.log -0.224548821               0
## S.num.chars.log         S.num.chars.log -0.224692967               0
## A.num.words.unq.log A.num.words.unq.log -0.226995906               0
## S.num.words.log         S.num.words.log -0.245354135               0
## S.num.words.unq.log S.num.words.unq.log -0.250796919               0
## PubDate.year               PubDate.year           NA               1
## H.has.http                   H.has.http           NA               0
## S.has.http                   S.has.http           NA               0
##                       cor.y.abs       cor.high.X is.ConditionalX.y
## Popular             1.000000000             <NA>                NA
## WordCount.log       0.265952699             <NA>              TRUE
## WordCount           0.257526549             <NA>                NA
## PubDate.hour        0.159167673 PubDate.apm.fctr              TRUE
## PubDate.apm.fctr    0.101472715             <NA>              TRUE
## A.can               0.031498867            S.can              TRUE
## S.can               0.029999780             <NA>              TRUE
## H.has.ebola         0.025881397             <NA>              TRUE
## S.make              0.023138853             <NA>              TRUE
## PubDate.month.fctr  0.019148739             <NA>                NA
## UniqueID            0.011824920             <NA>                NA
## S.one               0.006342094             <NA>              TRUE
## S.state             0.006069626             <NA>              TRUE
## A.one               0.005696039             <NA>              TRUE
## S.said              0.001363226             <NA>              TRUE
## .rnorm              0.008703337             <NA>              TRUE
## PubDate.date.fctr   0.011647558             <NA>              TRUE
## PubDate.second      0.012253600             <NA>              TRUE
## A.has.http          0.013592603             <NA>             FALSE
## S.presid            0.019828826             <NA>              TRUE
## A.presid            0.019828826         S.presid              TRUE
## S.take              0.025762398             <NA>              TRUE
## A.take              0.026086108           S.take              TRUE
## PubDate.minute      0.031469083             <NA>              TRUE
## S.new               0.034948520             <NA>              TRUE
## A.new               0.035359447            S.new              TRUE
## PubDate.wkday.fctr  0.039801288             <NA>              TRUE
## S.day               0.045649185             <NA>              TRUE
## A.day               0.045909684            S.day              TRUE
## H.X2014             0.046206380             <NA>              TRUE
## S.show              0.048801740             <NA>              TRUE
## S.report            0.050211524             <NA>              TRUE
## S.share             0.050329686             <NA>              TRUE
## S.year              0.051146178             <NA>              TRUE
## S.compani           0.053012962             <NA>              TRUE
## A.compani           0.053099633        S.compani              TRUE
## H.new               0.053121542             <NA>              TRUE
## S.first             0.053388178             <NA>              TRUE
## S.time              0.057595102             <NA>              TRUE
## A.time              0.057790617           S.time              TRUE
## H.newyork           0.057970095             <NA>              TRUE
## S.articl            0.059520554             <NA>              TRUE
## A.articl            0.059520554         S.articl              TRUE
## S.will              0.060575493             <NA>              TRUE
## A.will              0.061025004           S.will              TRUE
## H.day               0.061669687             <NA>              TRUE
## S.newyork           0.062117105             <NA>              TRUE
## H.today             0.063723058             <NA>              TRUE
## H.report            0.064948102             <NA>              TRUE
## H.X2015             0.066584892             <NA>             FALSE
## S.intern            0.068485701             <NA>              TRUE
## A.intern            0.068485701         S.intern              TRUE
## H.daili             0.069192975             <NA>             FALSE
## H.week              0.075105216             <NA>              TRUE
## H.fashion           0.081708612           H.week              TRUE
## S.week              0.084814939             <NA>              TRUE
## S.fashion           0.086446251             <NA>              TRUE
## H.num.chars         0.147211183             <NA>                NA
## Headline.pfx.fctr   0.156829778             <NA>              TRUE
## NewsDesk.fctr       0.161611606             <NA>              TRUE
## SectionName.fctr    0.165854115             <NA>              TRUE
## SubsectionName.fctr 0.168723053    NewsDesk.fctr              TRUE
## H.num.chars.log     0.171062360             <NA>              TRUE
## A.num.chars         0.177037425             <NA>                NA
## S.num.chars         0.179331806             <NA>                NA
## H.num.words         0.186036895             <NA>                NA
## A.num.words         0.187423227             <NA>                NA
## H.num.words.unq     0.189702157             <NA>                NA
## A.num.words.unq     0.192819225             <NA>                NA
## H.num.words.log     0.200686356             <NA>              TRUE
## H.num.words.unq.log 0.204496360  H.num.chars.log              TRUE
## S.num.words         0.206385049             <NA>                NA
## S.num.words.unq     0.212102717             <NA>                NA
## A.num.words.log     0.221953007             <NA>              TRUE
## A.num.chars.log     0.224548821             <NA>              TRUE
## S.num.chars.log     0.224692967  A.num.chars.log              TRUE
## A.num.words.unq.log 0.226995906  A.num.words.log              TRUE
## S.num.words.log     0.245354135             <NA>              TRUE
## S.num.words.unq.log 0.250796919  S.num.chars.log              TRUE
## PubDate.year                 NA             <NA>                NA
## H.has.http                   NA             <NA>             FALSE
## S.has.http                   NA             <NA>             FALSE
##                     is.cor.y.abs.low
## Popular                        FALSE
## WordCount.log                  FALSE
## WordCount                      FALSE
## PubDate.hour                   FALSE
## PubDate.apm.fctr               FALSE
## A.can                          FALSE
## S.can                          FALSE
## H.has.ebola                    FALSE
## S.make                         FALSE
## PubDate.month.fctr             FALSE
## UniqueID                       FALSE
## S.one                           TRUE
## S.state                         TRUE
## A.one                           TRUE
## S.said                          TRUE
## .rnorm                         FALSE
## PubDate.date.fctr              FALSE
## PubDate.second                 FALSE
## A.has.http                     FALSE
## S.presid                       FALSE
## A.presid                       FALSE
## S.take                         FALSE
## A.take                         FALSE
## PubDate.minute                 FALSE
## S.new                          FALSE
## A.new                          FALSE
## PubDate.wkday.fctr             FALSE
## S.day                          FALSE
## A.day                          FALSE
## H.X2014                        FALSE
## S.show                         FALSE
## S.report                       FALSE
## S.share                        FALSE
## S.year                         FALSE
## S.compani                      FALSE
## A.compani                      FALSE
## H.new                          FALSE
## S.first                        FALSE
## S.time                         FALSE
## A.time                         FALSE
## H.newyork                      FALSE
## S.articl                       FALSE
## A.articl                       FALSE
## S.will                         FALSE
## A.will                         FALSE
## H.day                          FALSE
## S.newyork                      FALSE
## H.today                        FALSE
## H.report                       FALSE
## H.X2015                        FALSE
## S.intern                       FALSE
## A.intern                       FALSE
## H.daili                        FALSE
## H.week                         FALSE
## H.fashion                      FALSE
## S.week                         FALSE
## S.fashion                      FALSE
## H.num.chars                    FALSE
## Headline.pfx.fctr              FALSE
## NewsDesk.fctr                  FALSE
## SectionName.fctr               FALSE
## SubsectionName.fctr            FALSE
## H.num.chars.log                FALSE
## A.num.chars                    FALSE
## S.num.chars                    FALSE
## H.num.words                    FALSE
## A.num.words                    FALSE
## H.num.words.unq                FALSE
## A.num.words.unq                FALSE
## H.num.words.log                FALSE
## H.num.words.unq.log            FALSE
## S.num.words                    FALSE
## S.num.words.unq                FALSE
## A.num.words.log                FALSE
## A.num.chars.log                FALSE
## S.num.chars.log                FALSE
## A.num.words.unq.log            FALSE
## S.num.words.log                FALSE
## S.num.words.unq.log            FALSE
## PubDate.year                      NA
## H.has.http                        NA
## S.has.http                        NA
```

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="partition.data.training", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                         chunk_label chunk_step_major chunk_step_minor
## elapsed7 remove.correlated.features                4                1
## elapsed8    partition.data.training                5                0
##          elapsed
## elapsed7 142.664
## elapsed8 146.153
```

## Step `5`: partition training data into fit & OOB

```r
if (all(is.na(glb_newent_df[, glb_rsp_var]))) {
    require(caTools)
    
    set.seed(glb_split_sample.seed)
    split <- sample.split(glb_trnent_df[, glb_rsp_var_raw], 
        SplitRatio=1 - (nrow(glb_newent_df) * 1.1 / nrow(glb_trnent_df)))
    glb_fitent_df <- glb_trnent_df[split, ] 
    glb_OOBent_df <- glb_trnent_df[!split ,]    
} else {
    print(sprintf("Newdata contains non-NA data for %s; setting OOB to Newdata", 
                  glb_rsp_var))
    glb_fitent_df <- glb_trnent_df; glb_OOBent_df <- glb_newent_df
}
```

```
## Loading required package: caTools
```

```r
if (!is.null(glb_max_fitent_obs) && (nrow(glb_fitent_df) > glb_max_fitent_obs)) {
    warning("glb_fitent_df restricted to glb_max_fitent_obs: ", 
            format(glb_max_fitent_obs, big.mark=","))
    org_fitent_df <- glb_fitent_df
    glb_fitent_df <- 
        org_fitent_df[split <- sample.split(org_fitent_df[, glb_rsp_var_raw], 
                                            SplitRatio=glb_max_fitent_obs), ]
    org_fitent_df <- NULL
}

# Run this line by line
print("glb_feats_df:");   print(dim(glb_feats_df))
```

```
## [1] "glb_feats_df:"
```

```
## [1] 82  7
```

```r
sav_feats_df <- glb_feats_df
glb_feats_df <- sav_feats_df

glb_feats_df[, "rsp_var_raw"] <- FALSE
glb_feats_df[glb_feats_df$id == glb_rsp_var_raw, "rsp_var_raw"] <- TRUE 
glb_feats_df$exclude.as.feat <- (glb_feats_df$exclude.as.feat == 1)
if (!is.null(glb_id_vars) && glb_id_vars != ".rownames")
    glb_feats_df[glb_feats_df$id %in% glb_id_vars, "id_var"] <- TRUE 
add_feats_df <- data.frame(id=glb_rsp_var, exclude.as.feat=TRUE, rsp_var=TRUE)
row.names(add_feats_df) <- add_feats_df$id; print(add_feats_df)
```

```
##                        id exclude.as.feat rsp_var
## Popular.fctr Popular.fctr            TRUE    TRUE
```

```r
glb_feats_df <- myrbind_df(glb_feats_df, add_feats_df)
print(subset(glb_feats_df, rsp_var_raw | rsp_var | id_var))
```

```
##                        id      cor.y exclude.as.feat  cor.y.abs cor.high.X
## Popular           Popular 1.00000000            TRUE 1.00000000       <NA>
## UniqueID         UniqueID 0.01182492            TRUE 0.01182492       <NA>
## Popular.fctr Popular.fctr         NA            TRUE         NA       <NA>
##              is.ConditionalX.y is.cor.y.abs.low rsp_var_raw id_var rsp_var
## Popular                     NA            FALSE        TRUE     NA      NA
## UniqueID                    NA            FALSE       FALSE   TRUE      NA
## Popular.fctr                NA               NA          NA     NA    TRUE
```

```r
print("glb_feats_df vs. glb_entity_df: "); 
```

```
## [1] "glb_feats_df vs. glb_entity_df: "
```

```r
print(setdiff(glb_feats_df$id, names(glb_entity_df)))
```

```
## character(0)
```

```r
print("glb_entity_df vs. glb_feats_df: "); 
```

```
## [1] "glb_entity_df vs. glb_feats_df: "
```

```r
# Ensure these are only chr vars
print(setdiff(setdiff(names(glb_entity_df), glb_feats_df$id), 
                myfind_chr_cols_df(glb_entity_df)))
```

```
## character(0)
```

```r
#print(setdiff(setdiff(names(glb_entity_df), glb_exclude_vars_as_features), 
#                glb_feats_df$id))

print("glb_entity_df: "); print(dim(glb_entity_df))
```

```
## [1] "glb_entity_df: "
```

```
## [1] 8402   92
```

```r
print("glb_trnent_df: "); print(dim(glb_trnent_df))
```

```
## [1] "glb_trnent_df: "
```

```
## [1] 6532   92
```

```r
print("glb_fitent_df: "); print(dim(glb_fitent_df))
```

```
## [1] "glb_fitent_df: "
```

```
## [1] 4475   92
```

```r
print("glb_OOBent_df: "); print(dim(glb_OOBent_df))
```

```
## [1] "glb_OOBent_df: "
```

```
## [1] 2057   92
```

```r
print("glb_newent_df: "); print(dim(glb_newent_df))
```

```
## [1] "glb_newent_df: "
```

```
## [1] 1870   92
```

```r
# sav_entity_df <- glb_entity_df
# glb_entity_df <- sav_entity_df
# # Does not handle NULL or length(glb_id_vars) > 1
# glb_entity_df$.src.trn <- 0
# glb_entity_df[glb_entity_df[, glb_id_vars] %in% glb_trnent_df[, glb_id_vars], 
#                 ".src.trn"] <- 1 
# glb_entity_df$.src.fit <- 0
# glb_entity_df[glb_entity_df[, glb_id_vars] %in% glb_fitent_df[, glb_id_vars], 
#                 ".src.fit"] <- 1 
# glb_entity_df$.src.OOB <- 0
# glb_entity_df[glb_entity_df[, glb_id_vars] %in% glb_OOBent_df[, glb_id_vars], 
#                 ".src.OOB"] <- 1 
# glb_entity_df$.src.new <- 0
# glb_entity_df[glb_entity_df[, glb_id_vars] %in% glb_newent_df[, glb_id_vars], 
#                 ".src.new"] <- 1 
# #print(unique(glb_entity_df[, ".src.trn"]))
# write_cols <- c(glb_feats_df$id, 
#                 ".src.trn", ".src.fit", ".src.OOB", ".src.new")
# glb_entity_df <- glb_entity_df[, write_cols]
# 
# tmp_feats_df <- glb_feats_df
# tmp_entity_df <- glb_entity_df

save(glb_feats_df, 
     glb_entity_df, glb_trnent_df, glb_fitent_df, glb_OOBent_df, glb_newent_df,
     file=paste0(glb_out_pfx, "blddfs_dsk.RData"))
# load(paste0(glb_out_pfx, "blddfs_dsk.RData"))

# if (!all.equal(tmp_feats_df, glb_feats_df))
#     stop("glb_feats_df r/w not working")
# if (!all.equal(tmp_entity_df, glb_entity_df))
#     stop("glb_entity_df r/w not working")
          
glb_script_df <- rbind(glb_script_df, 
    data.frame(chunk_label="fit.models", 
        chunk_step_major=max(glb_script_df$chunk_step_major)+1,
        chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))        
print(tail(glb_script_df, 2))
```

```
##                      chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed8 partition.data.training                5                0 146.153
## elapsed9              fit.models                6                0 148.494
```

## Step `6`: fit models

```r
# load(paste0(glb_out_pfx, "dsk.RData"))
# keep_cols <- setdiff(names(glb_entity_df), 
#                      grep("^.src", names(glb_entity_df), value=TRUE))
# glb_trnent_df <- glb_entity_df[glb_entity_df$.src.trn == 1, keep_cols]
# glb_fitent_df <- glb_entity_df[glb_entity_df$.src.fit == 1, keep_cols]
# glb_OOBent_df <- glb_entity_df[glb_entity_df$.src.OOB == 1, keep_cols]
# glb_newent_df <- glb_entity_df[glb_entity_df$.src.new == 1, keep_cols]
# 
# glb_models_lst <- list(); glb_models_df <- data.frame()
# 
if (glb_is_classification && glb_is_binomial && 
        (length(unique(glb_fitent_df[, glb_rsp_var])) < 2))
    stop("glb_fitent_df$", glb_rsp_var, ": contains less than 2 unique values: ",
         paste0(unique(glb_fitent_df[, glb_rsp_var]), collapse=", "))

max_cor_y_x_var <- orderBy(~ -cor.y.abs, 
        subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low))[1, "id"]
if (!is.null(glb_Baseline_mdl_var)) {
    if ((max_cor_y_x_var != glb_Baseline_mdl_var) & 
        (glb_feats_df[max_cor_y_x_var, "cor.y.abs"] > 
         glb_feats_df[glb_Baseline_mdl_var, "cor.y.abs"]))
        stop(max_cor_y_x_var, " has a lower correlation with ", glb_rsp_var, 
             " than the Baseline var: ", glb_Baseline_mdl_var)
}

glb_model_type <- ifelse(glb_is_regression, "regression", "classification")
    
# Baseline
if (!is.null(glb_Baseline_mdl_var)) 
    ret_lst <- myfit_mdl_fn(model_id="Baseline", model_method="mybaseln_classfr",
                            indep_vars_vctr=glb_Baseline_mdl_var,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitent_df, OOB_df=glb_OOBent_df)

# Most Frequent Outcome "MFO" model: mean(y) for regression
#   Not using caret's nullModel since model stats not avl
#   Cannot use rpart for multinomial classification since it predicts non-MFO
ret_lst <- myfit_mdl(model_id="MFO", 
                     model_method=ifelse(glb_is_regression, "lm", "myMFO_classfr"), 
                     model_type=glb_model_type,
                        indep_vars_vctr=".rnorm",
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitent_df, OOB_df=glb_OOBent_df)
```

```
## [1] "fitting model: MFO.myMFO_classfr"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
## [1] "in MFO.Classifier$fit"
## [1] "unique.vals:"
## [1] N Y
## Levels: N Y
## [1] "unique.prob:"
## y
##         N         Y 
## 0.8326257 0.1673743 
## [1] "MFO.val:"
## [1] "N"
##             Length Class      Mode     
## unique.vals 2      factor     numeric  
## unique.prob 2      -none-     numeric  
## MFO.val     1      -none-     character
## x.names     1      -none-     character
## xNames      1      -none-     character
## problemType 1      -none-     character
## tuneValue   1      data.frame list     
## obsLevels   2      -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

```
## Loading required package: ROCR
## Loading required package: gplots
## 
## Attaching package: 'gplots'
## 
## The following object is masked from 'package:stats':
## 
##     lowess
```

```
## [1] "entr MFO.Classifier$predict"
## [1] "exit MFO.Classifier$predict"
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.8326257 0.1673743
## 2 0.8326257 0.1673743
## 3 0.8326257 0.1673743
## 4 0.8326257 0.1673743
## 5 0.8326257 0.1673743
## 6 0.8326257 0.1673743
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.MFO.myMFO_classfr.N
## 1            N                                     3726
## 2            Y                                      749
##          Prediction
## Reference    N    Y
##         N 3726    0
##         Y  749    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.326257e-01   0.000000e+00   8.213602e-01   8.434553e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   5.097571e-01  1.800616e-164 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "entr MFO.Classifier$predict"
## [1] "exit MFO.Classifier$predict"
## [1] "in MFO.Classifier$prob"
##           N         Y
## 1 0.8326257 0.1673743
## 2 0.8326257 0.1673743
## 3 0.8326257 0.1673743
## 4 0.8326257 0.1673743
## 5 0.8326257 0.1673743
## 6 0.8326257 0.1673743
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.MFO.myMFO_classfr.N
## 1            N                                     1713
## 2            Y                                      344
##          Prediction
## Reference    N    Y
##         N 1713    0
##         Y  344    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.327662e-01   0.000000e+00   8.159247e-01   8.486533e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   5.143944e-01   2.337097e-76 
##            model_id  model_method  feats max.nTuningRuns
## 1 MFO.myMFO_classfr myMFO_classfr .rnorm               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.985                 0.003         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.8326257
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8213602             0.8434553             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8327662
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8159247             0.8486533             0
```

```r
if (glb_is_classification)
    # "random" model - only for classification; 
    #   none needed for regression since it is same as MFO
    ret_lst <- myfit_mdl(model_id="Random", model_method="myrandom_classfr",
                            model_type=glb_model_type,                         
                            indep_vars_vctr=".rnorm",
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitent_df, OOB_df=glb_OOBent_df)
```

```
## [1] "fitting model: Random.myrandom_classfr"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
##             Length Class      Mode     
## unique.vals 2      factor     numeric  
## unique.prob 2      table      numeric  
## xNames      1      -none-     character
## problemType 1      -none-     character
## tuneValue   1      data.frame list     
## obsLevels   2      -none-     character
## [1] "    calling mypredict_mdl for fit:"
## [1] "in Random.Classifier$prob"
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-1.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.2867534
## 3        0.2 0.1683938
## 4        0.3 0.1683938
## 5        0.4 0.1683938
## 6        0.5 0.1683938
## 7        0.6 0.1683938
## 8        0.7 0.1683938
## 9        0.8 0.1683938
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-2.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.Y
## 1            N                                           3726
## 2            Y                                            749
##          Prediction
## Reference    N    Y
##         N    0 3726
##         Y    0  749
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.1673743      0.0000000      0.1565447      0.1786398      0.8326257 
## AccuracyPValue  McnemarPValue 
##      1.0000000      0.0000000 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "in Random.Classifier$prob"
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-3.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.2865473
## 3        0.2 0.1404011
## 4        0.3 0.1404011
## 5        0.4 0.1404011
## 6        0.5 0.1404011
## 7        0.6 0.1404011
## 8        0.7 0.1404011
## 9        0.8 0.1404011
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-4.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.Y
## 1            N                                           1713
## 2            Y                                            344
##          Prediction
## Reference    N    Y
##         N    0 1713
##         Y    0  344
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      0.1672338      0.0000000      0.1513467      0.1840753      0.8327662 
## AccuracyPValue  McnemarPValue 
##      1.0000000      0.0000000 
##                  model_id     model_method  feats max.nTuningRuns
## 1 Random.myrandom_classfr myrandom_classfr .rnorm               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.349                 0.001   0.4975446
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.1       0.2867534        0.1673743
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.1565447             0.1786398             0   0.4821958
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.1       0.2865473        0.1672338
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.1513467             0.1840753             0
```

```r
# Any models that have tuning parameters has "better" results with cross-validation
#   (except rf) & "different" results for different outcome metrics

# Max.cor.Y
#   Check impact of cv
#       rpart is not a good candidate since caret does not optimize cp (only tuning parameter of rpart) well
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_var,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitent_df, OOB_df=glb_OOBent_df)
```

```
## [1] "fitting model: Max.cor.Y.cv.0.rpart"
## [1] "    indep_vars: WordCount.log"
```

```
## Loading required package: rpart
```

```
## Fitting cp = 0.00223 on full training set
```

```
## Loading required package: rpart.plot
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-5.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 4475 
## 
##            CP nsplit rel error
## 1 0.002225189      0         1
## 
## Node number 1: 4475 observations
##   predicted class=N  expected loss=0.1673743  P(node) =1
##     class counts:  3726   749
##    probabilities: 0.833 0.167 
## 
## n= 4475 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 4475 749 N (0.8326257 0.1673743) *
## [1] "    calling mypredict_mdl for fit:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.rpart.N
## 1            N                                        3726
## 2            Y                                         749
##          Prediction
## Reference    N    Y
##         N 3726    0
##         Y  749    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.326257e-01   0.000000e+00   8.213602e-01   8.434553e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   5.097571e-01  1.800616e-164 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.rpart.N
## 1            N                                        1713
## 2            Y                                         344
##          Prediction
## Reference    N    Y
##         N 1713    0
##         Y  344    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.327662e-01   0.000000e+00   8.159247e-01   8.486533e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   5.143944e-01   2.337097e-76 
##               model_id model_method         feats max.nTuningRuns
## 1 Max.cor.Y.cv.0.rpart        rpart WordCount.log               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.769                 0.069         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.8326257
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8213602             0.8434553             0         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8327662
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8159247             0.8486533             0
```

```r
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0.cp.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_var,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitent_df, OOB_df=glb_OOBent_df,
                        n_cv_folds=0, 
            tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
```

```
## [1] "fitting model: Max.cor.Y.cv.0.cp.0.rpart"
## [1] "    indep_vars: WordCount.log"
## Fitting cp = 0 on full training set
```

```
## Warning: labs do not fit even at cex 0.15, there may be some overplotting
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-6.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 4475 
## 
##             CP nsplit rel error
## 1 0.0022251891      0 1.0000000
## 2 0.0020026702     13 0.9666222
## 3 0.0013351135     19 0.9519359
## 4 0.0008900757     39 0.9158879
## 5 0.0005340454     50 0.9052069
## 6 0.0002225189     55 0.9025367
## 7 0.0000000000     61 0.9012016
## 
## Variable importance
## WordCount.log 
##           100 
## 
## Node number 1: 4475 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.1673743  P(node) =1
##     class counts:  3726   749
##    probabilities: 0.833 0.167 
##   left son=2 (3276 obs) right son=3 (1199 obs)
##   Primary splits:
##       WordCount.log < 6.528688 to the left,  improve=109.5997, (0 missing)
## 
## Node number 2: 3276 observations
##   predicted class=N  expected loss=0.1004274  P(node) =0.732067
##     class counts:  2947   329
##    probabilities: 0.900 0.100 
## 
## Node number 3: 1199 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.3502919  P(node) =0.267933
##     class counts:   779   420
##    probabilities: 0.650 0.350 
##   left son=6 (193 obs) right son=7 (1006 obs)
##   Primary splits:
##       WordCount.log < 6.663771 to the left,  improve=3.008125, (0 missing)
## 
## Node number 6: 193 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.2694301  P(node) =0.04312849
##     class counts:   141    52
##    probabilities: 0.731 0.269 
##   left son=12 (62 obs) right son=13 (131 obs)
##   Primary splits:
##       WordCount.log < 6.631343 to the right, improve=2.136379, (0 missing)
## 
## Node number 7: 1006 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.3658052  P(node) =0.2248045
##     class counts:   638   368
##    probabilities: 0.634 0.366 
##   left son=14 (85 obs) right son=15 (921 obs)
##   Primary splits:
##       WordCount.log < 7.57327  to the right, improve=3.162874, (0 missing)
## 
## Node number 12: 62 observations
##   predicted class=N  expected loss=0.1612903  P(node) =0.01385475
##     class counts:    52    10
##    probabilities: 0.839 0.161 
## 
## Node number 13: 131 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.3206107  P(node) =0.02927374
##     class counts:    89    42
##    probabilities: 0.679 0.321 
##   left son=26 (121 obs) right son=27 (10 obs)
##   Primary splits:
##       WordCount.log < 6.535966 to the right, improve=0.6968015, (0 missing)
## 
## Node number 14: 85 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.2352941  P(node) =0.01899441
##     class counts:    65    20
##    probabilities: 0.765 0.235 
##   left son=28 (77 obs) right son=29 (8 obs)
##   Primary splits:
##       WordCount.log < 8.229096 to the left,  improve=4.679144, (0 missing)
## 
## Node number 15: 921 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.3778502  P(node) =0.2058101
##     class counts:   573   348
##    probabilities: 0.622 0.378 
##   left son=30 (734 obs) right son=31 (187 obs)
##   Primary splits:
##       WordCount.log < 6.775937 to the right, improve=1.435366, (0 missing)
## 
## Node number 26: 121 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.3057851  P(node) =0.02703911
##     class counts:    84    37
##    probabilities: 0.694 0.306 
##   left son=52 (15 obs) right son=53 (106 obs)
##   Primary splits:
##       WordCount.log < 6.548935 to the left,  improve=1.018442, (0 missing)
## 
## Node number 27: 10 observations
##   predicted class=N  expected loss=0.5  P(node) =0.002234637
##     class counts:     5     5
##    probabilities: 0.500 0.500 
## 
## Node number 28: 77 observations
##   predicted class=N  expected loss=0.1818182  P(node) =0.0172067
##     class counts:    63    14
##    probabilities: 0.818 0.182 
## 
## Node number 29: 8 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.001787709
##     class counts:     2     6
##    probabilities: 0.250 0.750 
## 
## Node number 30: 734 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.3637602  P(node) =0.1640223
##     class counts:   467   267
##    probabilities: 0.636 0.364 
##   left son=60 (11 obs) right son=61 (723 obs)
##   Primary splits:
##       WordCount.log < 6.782759 to the left,  improve=2.955363, (0 missing)
## 
## Node number 31: 187 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.4331551  P(node) =0.04178771
##     class counts:   106    81
##    probabilities: 0.567 0.433 
##   left son=62 (177 obs) right son=63 (10 obs)
##   Primary splits:
##       WordCount.log < 6.771362 to the left,  improve=2.843566, (0 missing)
## 
## Node number 52: 15 observations
##   predicted class=N  expected loss=0.1333333  P(node) =0.003351955
##     class counts:    13     2
##    probabilities: 0.867 0.133 
## 
## Node number 53: 106 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.3301887  P(node) =0.02368715
##     class counts:    71    35
##    probabilities: 0.670 0.330 
##   left son=106 (87 obs) right son=107 (19 obs)
##   Primary splits:
##       WordCount.log < 6.566671 to the right, improve=1.780924, (0 missing)
## 
## Node number 60: 11 observations
##   predicted class=N  expected loss=0  P(node) =0.002458101
##     class counts:    11     0
##    probabilities: 1.000 0.000 
## 
## Node number 61: 723 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.3692946  P(node) =0.1615642
##     class counts:   456   267
##    probabilities: 0.631 0.369 
##   left son=122 (515 obs) right son=123 (208 obs)
##   Primary splits:
##       WordCount.log < 7.162785 to the left,  improve=1.689287, (0 missing)
## 
## Node number 62: 177 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.4124294  P(node) =0.03955307
##     class counts:   104    73
##    probabilities: 0.588 0.412 
##   left son=124 (125 obs) right son=125 (52 obs)
##   Primary splits:
##       WordCount.log < 6.736373 to the left,  improve=0.6877723, (0 missing)
## 
## Node number 63: 10 observations
##   predicted class=Y  expected loss=0.2  P(node) =0.002234637
##     class counts:     2     8
##    probabilities: 0.200 0.800 
## 
## Node number 106: 87 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.2873563  P(node) =0.01944134
##     class counts:    62    25
##    probabilities: 0.713 0.287 
##   left son=212 (41 obs) right son=213 (46 obs)
##   Primary splits:
##       WordCount.log < 6.597826 to the left,  improve=1.319353, (0 missing)
## 
## Node number 107: 19 observations
##   predicted class=Y  expected loss=0.4736842  P(node) =0.00424581
##     class counts:     9    10
##    probabilities: 0.474 0.526 
## 
## Node number 122: 515 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3475728  P(node) =0.1150838
##     class counts:   336   179
##    probabilities: 0.652 0.348 
##   left son=244 (190 obs) right son=245 (325 obs)
##   Primary splits:
##       WordCount.log < 6.982399 to the right, improve=2.835815, (0 missing)
## 
## Node number 123: 208 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.4230769  P(node) =0.04648045
##     class counts:   120    88
##    probabilities: 0.577 0.423 
##   left son=246 (199 obs) right son=247 (9 obs)
##   Primary splits:
##       WordCount.log < 7.17434  to the right, improve=2.367049, (0 missing)
## 
## Node number 124: 125 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.384  P(node) =0.02793296
##     class counts:    77    48
##    probabilities: 0.616 0.384 
##   left son=248 (40 obs) right son=249 (85 obs)
##   Primary splits:
##       WordCount.log < 6.713563 to the right, improve=1.397765, (0 missing)
## 
## Node number 125: 52 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.4807692  P(node) =0.01162011
##     class counts:    27    25
##    probabilities: 0.519 0.481 
##   left son=250 (40 obs) right son=251 (12 obs)
##   Primary splits:
##       WordCount.log < 6.745823 to the right, improve=2.261538, (0 missing)
## 
## Node number 212: 41 observations
##   predicted class=N  expected loss=0.195122  P(node) =0.009162011
##     class counts:    33     8
##    probabilities: 0.805 0.195 
## 
## Node number 213: 46 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.3695652  P(node) =0.01027933
##     class counts:    29    17
##    probabilities: 0.630 0.370 
##   left son=426 (33 obs) right son=427 (13 obs)
##   Primary splits:
##       WordCount.log < 6.605974 to the right, improve=2.190027, (0 missing)
## 
## Node number 244: 190 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.2789474  P(node) =0.0424581
##     class counts:   137    53
##    probabilities: 0.721 0.279 
##   left son=488 (8 obs) right son=489 (182 obs)
##   Primary splits:
##       WordCount.log < 7.158125 to the right, improve=1.299711, (0 missing)
## 
## Node number 245: 325 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3876923  P(node) =0.0726257
##     class counts:   199   126
##    probabilities: 0.612 0.388 
##   left son=490 (231 obs) right son=491 (94 obs)
##   Primary splits:
##       WordCount.log < 6.912741 to the left,  improve=0.9243624, (0 missing)
## 
## Node number 246: 199 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.4070352  P(node) =0.04446927
##     class counts:   118    81
##    probabilities: 0.593 0.407 
##   left son=492 (114 obs) right son=493 (85 obs)
##   Primary splits:
##       WordCount.log < 7.275172 to the right, improve=0.7959052, (0 missing)
## 
## Node number 247: 9 observations
##   predicted class=Y  expected loss=0.2222222  P(node) =0.002011173
##     class counts:     2     7
##    probabilities: 0.222 0.778 
## 
## Node number 248: 40 observations
##   predicted class=N  expected loss=0.275  P(node) =0.008938547
##     class counts:    29    11
##    probabilities: 0.725 0.275 
## 
## Node number 249: 85 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.4352941  P(node) =0.01899441
##     class counts:    48    37
##    probabilities: 0.565 0.435 
##   left son=498 (56 obs) right son=499 (29 obs)
##   Primary splits:
##       WordCount.log < 6.698884 to the left,  improve=1.193408, (0 missing)
## 
## Node number 250: 40 observations
##   predicted class=N  expected loss=0.4  P(node) =0.008938547
##     class counts:    24    16
##    probabilities: 0.600 0.400 
## 
## Node number 251: 12 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.002681564
##     class counts:     3     9
##    probabilities: 0.250 0.750 
## 
## Node number 426: 33 observations
##   predicted class=N  expected loss=0.2727273  P(node) =0.007374302
##     class counts:    24     9
##    probabilities: 0.727 0.273 
## 
## Node number 427: 13 observations
##   predicted class=Y  expected loss=0.3846154  P(node) =0.002905028
##     class counts:     5     8
##    probabilities: 0.385 0.615 
## 
## Node number 488: 8 observations
##   predicted class=N  expected loss=0  P(node) =0.001787709
##     class counts:     8     0
##    probabilities: 1.000 0.000 
## 
## Node number 489: 182 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.2912088  P(node) =0.04067039
##     class counts:   129    53
##    probabilities: 0.709 0.291 
##   left son=978 (127 obs) right son=979 (55 obs)
##   Primary splits:
##       WordCount.log < 7.088408 to the left,  improve=1.865726, (0 missing)
## 
## Node number 490: 231 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3636364  P(node) =0.05162011
##     class counts:   147    84
##    probabilities: 0.636 0.364 
##   left son=980 (36 obs) right son=981 (195 obs)
##   Primary splits:
##       WordCount.log < 6.892134 to the right, improve=1.705672, (0 missing)
## 
## Node number 491: 94 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.4468085  P(node) =0.02100559
##     class counts:    52    42
##    probabilities: 0.553 0.447 
##   left son=982 (63 obs) right son=983 (31 obs)
##   Primary splits:
##       WordCount.log < 6.935857 to the right, improve=0.9545162, (0 missing)
## 
## Node number 492: 114 observations,    complexity param=0.0005340454
##   predicted class=N  expected loss=0.3684211  P(node) =0.02547486
##     class counts:    72    42
##    probabilities: 0.632 0.368 
##   left son=984 (35 obs) right son=985 (79 obs)
##   Primary splits:
##       WordCount.log < 7.345687 to the left,  improve=1.250823, (0 missing)
## 
## Node number 493: 85 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.4588235  P(node) =0.01899441
##     class counts:    46    39
##    probabilities: 0.541 0.459 
##   left son=986 (69 obs) right son=987 (16 obs)
##   Primary splits:
##       WordCount.log < 7.257355 to the left,  improve=1.088576, (0 missing)
## 
## Node number 498: 56 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.375  P(node) =0.01251397
##     class counts:    35    21
##    probabilities: 0.625 0.375 
##   left son=996 (12 obs) right son=997 (44 obs)
##   Primary splits:
##       WordCount.log < 6.691463 to the right, improve=1.325758, (0 missing)
## 
## Node number 499: 29 observations
##   predicted class=Y  expected loss=0.4482759  P(node) =0.006480447
##     class counts:    13    16
##    probabilities: 0.448 0.552 
## 
## Node number 978: 127 observations,    complexity param=0.0002225189
##   predicted class=N  expected loss=0.2440945  P(node) =0.02837989
##     class counts:    96    31
##    probabilities: 0.756 0.244 
##   left son=1956 (9 obs) right son=1957 (118 obs)
##   Primary splits:
##       WordCount.log < 7.080026 to the right, improve=1.154277, (0 missing)
## 
## Node number 979: 55 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.4  P(node) =0.0122905
##     class counts:    33    22
##    probabilities: 0.600 0.400 
##   left son=1958 (43 obs) right son=1959 (12 obs)
##   Primary splits:
##       WordCount.log < 7.100027 to the right, improve=1.031783, (0 missing)
## 
## Node number 980: 36 observations
##   predicted class=N  expected loss=0.2222222  P(node) =0.008044693
##     class counts:    28     8
##    probabilities: 0.778 0.222 
## 
## Node number 981: 195 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3897436  P(node) =0.04357542
##     class counts:   119    76
##    probabilities: 0.610 0.390 
##   left son=1962 (185 obs) right son=1963 (10 obs)
##   Primary splits:
##       WordCount.log < 6.884486 to the left,  improve=0.9319473, (0 missing)
## 
## Node number 982: 63 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3968254  P(node) =0.01407821
##     class counts:    38    25
##    probabilities: 0.603 0.397 
##   left son=1964 (10 obs) right son=1965 (53 obs)
##   Primary splits:
##       WordCount.log < 6.942156 to the left,  improve=2.094579, (0 missing)
## 
## Node number 983: 31 observations,    complexity param=0.001335113
##   predicted class=Y  expected loss=0.4516129  P(node) =0.006927374
##     class counts:    14    17
##    probabilities: 0.452 0.548 
##   left son=1966 (24 obs) right son=1967 (7 obs)
##   Primary splits:
##       WordCount.log < 6.928048 to the left,  improve=0.4976959, (0 missing)
## 
## Node number 984: 35 observations
##   predicted class=N  expected loss=0.2571429  P(node) =0.007821229
##     class counts:    26     9
##    probabilities: 0.743 0.257 
## 
## Node number 985: 79 observations,    complexity param=0.0005340454
##   predicted class=N  expected loss=0.4177215  P(node) =0.01765363
##     class counts:    46    33
##    probabilities: 0.582 0.418 
##   left son=1970 (67 obs) right son=1971 (12 obs)
##   Primary splits:
##       WordCount.log < 7.543801 to the left,  improve=0.1915738, (0 missing)
## 
## Node number 986: 69 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.4202899  P(node) =0.01541899
##     class counts:    40    29
##    probabilities: 0.580 0.420 
##   left son=1972 (14 obs) right son=1973 (55 obs)
##   Primary splits:
##       WordCount.log < 7.191805 to the left,  improve=0.6361754, (0 missing)
## 
## Node number 987: 16 observations
##   predicted class=Y  expected loss=0.375  P(node) =0.003575419
##     class counts:     6    10
##    probabilities: 0.375 0.625 
## 
## Node number 996: 12 observations
##   predicted class=N  expected loss=0.1666667  P(node) =0.002681564
##     class counts:    10     2
##    probabilities: 0.833 0.167 
## 
## Node number 997: 44 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.4318182  P(node) =0.009832402
##     class counts:    25    19
##    probabilities: 0.568 0.432 
##   left son=1994 (35 obs) right son=1995 (9 obs)
##   Primary splits:
##       WordCount.log < 6.685236 to the left,  improve=2.708369, (0 missing)
## 
## Node number 1956: 9 observations
##   predicted class=N  expected loss=0  P(node) =0.002011173
##     class counts:     9     0
##    probabilities: 1.000 0.000 
## 
## Node number 1957: 118 observations,    complexity param=0.0002225189
##   predicted class=N  expected loss=0.2627119  P(node) =0.02636872
##     class counts:    87    31
##    probabilities: 0.737 0.263 
##   left son=3914 (98 obs) right son=3915 (20 obs)
##   Primary splits:
##       WordCount.log < 7.060476 to the left,  improve=0.9077828, (0 missing)
## 
## Node number 1958: 43 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.3488372  P(node) =0.009608939
##     class counts:    28    15
##    probabilities: 0.651 0.349 
##   left son=3916 (11 obs) right son=3917 (32 obs)
##   Primary splits:
##       WordCount.log < 7.11192  to the left,  improve=0.8246564, (0 missing)
## 
## Node number 1959: 12 observations
##   predicted class=Y  expected loss=0.4166667  P(node) =0.002681564
##     class counts:     5     7
##    probabilities: 0.417 0.583 
## 
## Node number 1962: 185 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3783784  P(node) =0.04134078
##     class counts:   115    70
##    probabilities: 0.622 0.378 
##   left son=3924 (164 obs) right son=3925 (21 obs)
##   Primary splits:
##       WordCount.log < 6.790659 to the right, improve=1.002056, (0 missing)
## 
## Node number 1963: 10 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.002234637
##     class counts:     4     6
##    probabilities: 0.400 0.600 
## 
## Node number 1964: 10 observations
##   predicted class=N  expected loss=0.1  P(node) =0.002234637
##     class counts:     9     1
##    probabilities: 0.900 0.100 
## 
## Node number 1965: 53 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.4528302  P(node) =0.01184358
##     class counts:    29    24
##    probabilities: 0.547 0.453 
##   left son=3930 (21 obs) right son=3931 (32 obs)
##   Primary splits:
##       WordCount.log < 6.956069 to the left,  improve=0.359389, (0 missing)
## 
## Node number 1966: 24 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.5  P(node) =0.005363128
##     class counts:    12    12
##    probabilities: 0.500 0.500 
##   left son=3932 (9 obs) right son=3933 (15 obs)
##   Primary splits:
##       WordCount.log < 6.920178 to the right, improve=0.8, (0 missing)
## 
## Node number 1967: 7 observations
##   predicted class=Y  expected loss=0.2857143  P(node) =0.001564246
##     class counts:     2     5
##    probabilities: 0.286 0.714 
## 
## Node number 1970: 67 observations,    complexity param=0.0005340454
##   predicted class=N  expected loss=0.4029851  P(node) =0.01497207
##     class counts:    40    27
##    probabilities: 0.597 0.403 
##   left son=3940 (8 obs) right son=3941 (59 obs)
##   Primary splits:
##       WordCount.log < 7.506042 to the right, improve=0.4252466, (0 missing)
## 
## Node number 1971: 12 observations
##   predicted class=N  expected loss=0.5  P(node) =0.002681564
##     class counts:     6     6
##    probabilities: 0.500 0.500 
## 
## Node number 1972: 14 observations
##   predicted class=N  expected loss=0.2857143  P(node) =0.003128492
##     class counts:    10     4
##    probabilities: 0.714 0.286 
## 
## Node number 1973: 55 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.4545455  P(node) =0.0122905
##     class counts:    30    25
##    probabilities: 0.545 0.455 
##   left son=3946 (40 obs) right son=3947 (15 obs)
##   Primary splits:
##       WordCount.log < 7.20897  to the right, improve=0.8727273, (0 missing)
## 
## Node number 1994: 35 observations
##   predicted class=N  expected loss=0.3428571  P(node) =0.007821229
##     class counts:    23    12
##    probabilities: 0.657 0.343 
## 
## Node number 1995: 9 observations
##   predicted class=Y  expected loss=0.2222222  P(node) =0.002011173
##     class counts:     2     7
##    probabilities: 0.222 0.778 
## 
## Node number 3914: 98 observations,    complexity param=0.0002225189
##   predicted class=N  expected loss=0.2346939  P(node) =0.02189944
##     class counts:    75    23
##    probabilities: 0.765 0.235 
##   left son=7828 (27 obs) right son=7829 (71 obs)
##   Primary splits:
##       WordCount.log < 7.044469 to the right, improve=0.5582809, (0 missing)
## 
## Node number 3915: 20 observations
##   predicted class=N  expected loss=0.4  P(node) =0.004469274
##     class counts:    12     8
##    probabilities: 0.600 0.400 
## 
## Node number 3916: 11 observations
##   predicted class=N  expected loss=0.1818182  P(node) =0.002458101
##     class counts:     9     2
##    probabilities: 0.818 0.182 
## 
## Node number 3917: 32 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.40625  P(node) =0.007150838
##     class counts:    19    13
##    probabilities: 0.594 0.406 
##   left son=7834 (22 obs) right son=7835 (10 obs)
##   Primary splits:
##       WordCount.log < 7.127693 to the right, improve=1.092045, (0 missing)
## 
## Node number 3924: 164 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3597561  P(node) =0.03664804
##     class counts:   105    59
##    probabilities: 0.640 0.360 
##   left son=7848 (18 obs) right son=7849 (146 obs)
##   Primary splits:
##       WordCount.log < 6.873163 to the right, improve=0.7649144, (0 missing)
## 
## Node number 3925: 21 observations
##   predicted class=Y  expected loss=0.4761905  P(node) =0.004692737
##     class counts:    10    11
##    probabilities: 0.476 0.524 
## 
## Node number 3930: 21 observations
##   predicted class=N  expected loss=0.3809524  P(node) =0.004692737
##     class counts:    13     8
##    probabilities: 0.619 0.381 
## 
## Node number 3931: 32 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.5  P(node) =0.007150838
##     class counts:    16    16
##    probabilities: 0.500 0.500 
##   left son=7862 (23 obs) right son=7863 (9 obs)
##   Primary splits:
##       WordCount.log < 6.96319  to the right, improve=1.932367, (0 missing)
## 
## Node number 3932: 9 observations
##   predicted class=N  expected loss=0.3333333  P(node) =0.002011173
##     class counts:     6     3
##    probabilities: 0.667 0.333 
## 
## Node number 3933: 15 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.003351955
##     class counts:     6     9
##    probabilities: 0.400 0.600 
## 
## Node number 3940: 8 observations
##   predicted class=N  expected loss=0.25  P(node) =0.001787709
##     class counts:     6     2
##    probabilities: 0.750 0.250 
## 
## Node number 3941: 59 observations,    complexity param=0.0005340454
##   predicted class=N  expected loss=0.4237288  P(node) =0.01318436
##     class counts:    34    25
##    probabilities: 0.576 0.424 
##   left son=7882 (27 obs) right son=7883 (32 obs)
##   Primary splits:
##       WordCount.log < 7.405491 to the left,  improve=0.2834667, (0 missing)
## 
## Node number 3946: 40 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.4  P(node) =0.008938547
##     class counts:    24    16
##    probabilities: 0.600 0.400 
##   left son=7892 (7 obs) right son=7893 (33 obs)
##   Primary splits:
##       WordCount.log < 7.220371 to the left,  improve=1.122078, (0 missing)
## 
## Node number 3947: 15 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.003351955
##     class counts:     6     9
##    probabilities: 0.400 0.600 
## 
## Node number 7828: 27 observations
##   predicted class=N  expected loss=0.1481481  P(node) =0.00603352
##     class counts:    23     4
##    probabilities: 0.852 0.148 
## 
## Node number 7829: 71 observations,    complexity param=0.0002225189
##   predicted class=N  expected loss=0.2676056  P(node) =0.01586592
##     class counts:    52    19
##    probabilities: 0.732 0.268 
##   left son=15658 (52 obs) right son=15659 (19 obs)
##   Primary splits:
##       WordCount.log < 7.025094 to the left,  improve=0.5273422, (0 missing)
## 
## Node number 7834: 22 observations
##   predicted class=N  expected loss=0.3181818  P(node) =0.004916201
##     class counts:    15     7
##    probabilities: 0.682 0.318 
## 
## Node number 7835: 10 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.002234637
##     class counts:     4     6
##    probabilities: 0.400 0.600 
## 
## Node number 7848: 18 observations
##   predicted class=N  expected loss=0.2222222  P(node) =0.004022346
##     class counts:    14     4
##    probabilities: 0.778 0.222 
## 
## Node number 7849: 146 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3767123  P(node) =0.0326257
##     class counts:    91    55
##    probabilities: 0.623 0.377 
##   left son=15698 (130 obs) right son=15699 (16 obs)
##   Primary splits:
##       WordCount.log < 6.862757 to the left,  improve=2.21549, (0 missing)
## 
## Node number 7862: 23 observations
##   predicted class=N  expected loss=0.3913043  P(node) =0.005139665
##     class counts:    14     9
##    probabilities: 0.609 0.391 
## 
## Node number 7863: 9 observations
##   predicted class=Y  expected loss=0.2222222  P(node) =0.002011173
##     class counts:     2     7
##    probabilities: 0.222 0.778 
## 
## Node number 7882: 27 observations
##   predicted class=N  expected loss=0.3703704  P(node) =0.00603352
##     class counts:    17    10
##    probabilities: 0.630 0.370 
## 
## Node number 7883: 32 observations,    complexity param=0.0005340454
##   predicted class=N  expected loss=0.46875  P(node) =0.007150838
##     class counts:    17    15
##    probabilities: 0.531 0.469 
##   left son=15766 (16 obs) right son=15767 (16 obs)
##   Primary splits:
##       WordCount.log < 7.444526 to the right, improve=0.5625, (0 missing)
## 
## Node number 7892: 7 observations
##   predicted class=N  expected loss=0.1428571  P(node) =0.001564246
##     class counts:     6     1
##    probabilities: 0.857 0.143 
## 
## Node number 7893: 33 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.4545455  P(node) =0.007374302
##     class counts:    18    15
##    probabilities: 0.545 0.455 
##   left son=15786 (15 obs) right son=15787 (18 obs)
##   Primary splits:
##       WordCount.log < 7.238497 to the right, improve=0.8080808, (0 missing)
## 
## Node number 15658: 52 observations,    complexity param=0.0002225189
##   predicted class=N  expected loss=0.2307692  P(node) =0.01162011
##     class counts:    40    12
##    probabilities: 0.769 0.231 
##   left son=31316 (11 obs) right son=31317 (41 obs)
##   Primary splits:
##       WordCount.log < 7.013015 to the right, improve=1.485929, (0 missing)
## 
## Node number 15659: 19 observations
##   predicted class=N  expected loss=0.3684211  P(node) =0.00424581
##     class counts:    12     7
##    probabilities: 0.632 0.368 
## 
## Node number 15698: 130 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3461538  P(node) =0.02905028
##     class counts:    85    45
##    probabilities: 0.654 0.346 
##   left son=31396 (32 obs) right son=31397 (98 obs)
##   Primary splits:
##       WordCount.log < 6.843217 to the right, improve=0.7849294, (0 missing)
## 
## Node number 15699: 16 observations
##   predicted class=Y  expected loss=0.375  P(node) =0.003575419
##     class counts:     6    10
##    probabilities: 0.375 0.625 
## 
## Node number 15766: 16 observations
##   predicted class=N  expected loss=0.375  P(node) =0.003575419
##     class counts:    10     6
##    probabilities: 0.625 0.375 
## 
## Node number 15767: 16 observations
##   predicted class=Y  expected loss=0.4375  P(node) =0.003575419
##     class counts:     7     9
##    probabilities: 0.437 0.562 
## 
## Node number 15786: 15 observations
##   predicted class=N  expected loss=0.3333333  P(node) =0.003351955
##     class counts:    10     5
##    probabilities: 0.667 0.333 
## 
## Node number 15787: 18 observations
##   predicted class=Y  expected loss=0.4444444  P(node) =0.004022346
##     class counts:     8    10
##    probabilities: 0.444 0.556 
## 
## Node number 31316: 11 observations
##   predicted class=N  expected loss=0  P(node) =0.002458101
##     class counts:    11     0
##    probabilities: 1.000 0.000 
## 
## Node number 31317: 41 observations,    complexity param=0.0002225189
##   predicted class=N  expected loss=0.2926829  P(node) =0.009162011
##     class counts:    29    12
##    probabilities: 0.707 0.293 
##   left son=62634 (30 obs) right son=62635 (11 obs)
##   Primary splits:
##       WordCount.log < 7.004882 to the left,  improve=1.921064, (0 missing)
## 
## Node number 31396: 32 observations
##   predicted class=N  expected loss=0.25  P(node) =0.007150838
##     class counts:    24     8
##    probabilities: 0.750 0.250 
## 
## Node number 31397: 98 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.377551  P(node) =0.02189944
##     class counts:    61    37
##    probabilities: 0.622 0.378 
##   left son=62794 (83 obs) right son=62795 (15 obs)
##   Primary splits:
##       WordCount.log < 6.836796 to the left,  improve=1.752791, (0 missing)
## 
## Node number 62634: 30 observations
##   predicted class=N  expected loss=0.2  P(node) =0.006703911
##     class counts:    24     6
##    probabilities: 0.800 0.200 
## 
## Node number 62635: 11 observations
##   predicted class=Y  expected loss=0.4545455  P(node) =0.002458101
##     class counts:     5     6
##    probabilities: 0.455 0.545 
## 
## Node number 62794: 83 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3373494  P(node) =0.01854749
##     class counts:    55    28
##    probabilities: 0.663 0.337 
##   left son=125588 (11 obs) right son=125589 (72 obs)
##   Primary splits:
##       WordCount.log < 6.829253 to the right, improve=0.6134842, (0 missing)
## 
## Node number 62795: 15 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.003351955
##     class counts:     6     9
##    probabilities: 0.400 0.600 
## 
## Node number 125588: 11 observations
##   predicted class=N  expected loss=0.1818182  P(node) =0.002458101
##     class counts:     9     2
##    probabilities: 0.818 0.182 
## 
## Node number 125589: 72 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3611111  P(node) =0.01608939
##     class counts:    46    26
##    probabilities: 0.639 0.361 
##   left son=251178 (29 obs) right son=251179 (43 obs)
##   Primary splits:
##       WordCount.log < 6.807382 to the left,  improve=0.7057828, (0 missing)
## 
## Node number 251178: 29 observations
##   predicted class=N  expected loss=0.2758621  P(node) =0.006480447
##     class counts:    21     8
##    probabilities: 0.724 0.276 
## 
## Node number 251179: 43 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.4186047  P(node) =0.009608939
##     class counts:    25    18
##    probabilities: 0.581 0.419 
##   left son=502358 (35 obs) right son=502359 (8 obs)
##   Primary splits:
##       WordCount.log < 6.810693 to the right, improve=2.158804, (0 missing)
## 
## Node number 502358: 35 observations
##   predicted class=N  expected loss=0.3428571  P(node) =0.007821229
##     class counts:    23    12
##    probabilities: 0.657 0.343 
## 
## Node number 502359: 8 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.001787709
##     class counts:     2     6
##    probabilities: 0.250 0.750 
## 
## n= 4475 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##      1) root 4475 749 N (0.8326257 0.1673743)  
##        2) WordCount.log< 6.528688 3276 329 N (0.8995726 0.1004274) *
##        3) WordCount.log>=6.528688 1199 420 N (0.6497081 0.3502919)  
##          6) WordCount.log< 6.663771 193  52 N (0.7305699 0.2694301)  
##           12) WordCount.log>=6.631343 62  10 N (0.8387097 0.1612903) *
##           13) WordCount.log< 6.631343 131  42 N (0.6793893 0.3206107)  
##             26) WordCount.log>=6.535966 121  37 N (0.6942149 0.3057851)  
##               52) WordCount.log< 6.548935 15   2 N (0.8666667 0.1333333) *
##               53) WordCount.log>=6.548935 106  35 N (0.6698113 0.3301887)  
##                106) WordCount.log>=6.566671 87  25 N (0.7126437 0.2873563)  
##                  212) WordCount.log< 6.597826 41   8 N (0.8048780 0.1951220) *
##                  213) WordCount.log>=6.597826 46  17 N (0.6304348 0.3695652)  
##                    426) WordCount.log>=6.605974 33   9 N (0.7272727 0.2727273) *
##                    427) WordCount.log< 6.605974 13   5 Y (0.3846154 0.6153846) *
##                107) WordCount.log< 6.566671 19   9 Y (0.4736842 0.5263158) *
##             27) WordCount.log< 6.535966 10   5 N (0.5000000 0.5000000) *
##          7) WordCount.log>=6.663771 1006 368 N (0.6341948 0.3658052)  
##           14) WordCount.log>=7.57327 85  20 N (0.7647059 0.2352941)  
##             28) WordCount.log< 8.229096 77  14 N (0.8181818 0.1818182) *
##             29) WordCount.log>=8.229096 8   2 Y (0.2500000 0.7500000) *
##           15) WordCount.log< 7.57327 921 348 N (0.6221498 0.3778502)  
##             30) WordCount.log>=6.775937 734 267 N (0.6362398 0.3637602)  
##               60) WordCount.log< 6.782759 11   0 N (1.0000000 0.0000000) *
##               61) WordCount.log>=6.782759 723 267 N (0.6307054 0.3692946)  
##                122) WordCount.log< 7.162785 515 179 N (0.6524272 0.3475728)  
##                  244) WordCount.log>=6.982399 190  53 N (0.7210526 0.2789474)  
##                    488) WordCount.log>=7.158125 8   0 N (1.0000000 0.0000000) *
##                    489) WordCount.log< 7.158125 182  53 N (0.7087912 0.2912088)  
##                      978) WordCount.log< 7.088408 127  31 N (0.7559055 0.2440945)  
##                       1956) WordCount.log>=7.080026 9   0 N (1.0000000 0.0000000) *
##                       1957) WordCount.log< 7.080026 118  31 N (0.7372881 0.2627119)  
##                         3914) WordCount.log< 7.060476 98  23 N (0.7653061 0.2346939)  
##                           7828) WordCount.log>=7.044469 27   4 N (0.8518519 0.1481481) *
##                           7829) WordCount.log< 7.044469 71  19 N (0.7323944 0.2676056)  
##                            15658) WordCount.log< 7.025094 52  12 N (0.7692308 0.2307692)  
##                              31316) WordCount.log>=7.013015 11   0 N (1.0000000 0.0000000) *
##                              31317) WordCount.log< 7.013015 41  12 N (0.7073171 0.2926829)  
##                                62634) WordCount.log< 7.004882 30   6 N (0.8000000 0.2000000) *
##                                62635) WordCount.log>=7.004882 11   5 Y (0.4545455 0.5454545) *
##                            15659) WordCount.log>=7.025094 19   7 N (0.6315789 0.3684211) *
##                         3915) WordCount.log>=7.060476 20   8 N (0.6000000 0.4000000) *
##                      979) WordCount.log>=7.088408 55  22 N (0.6000000 0.4000000)  
##                       1958) WordCount.log>=7.100027 43  15 N (0.6511628 0.3488372)  
##                         3916) WordCount.log< 7.11192 11   2 N (0.8181818 0.1818182) *
##                         3917) WordCount.log>=7.11192 32  13 N (0.5937500 0.4062500)  
##                           7834) WordCount.log>=7.127693 22   7 N (0.6818182 0.3181818) *
##                           7835) WordCount.log< 7.127693 10   4 Y (0.4000000 0.6000000) *
##                       1959) WordCount.log< 7.100027 12   5 Y (0.4166667 0.5833333) *
##                  245) WordCount.log< 6.982399 325 126 N (0.6123077 0.3876923)  
##                    490) WordCount.log< 6.912741 231  84 N (0.6363636 0.3636364)  
##                      980) WordCount.log>=6.892134 36   8 N (0.7777778 0.2222222) *
##                      981) WordCount.log< 6.892134 195  76 N (0.6102564 0.3897436)  
##                       1962) WordCount.log< 6.884486 185  70 N (0.6216216 0.3783784)  
##                         3924) WordCount.log>=6.790659 164  59 N (0.6402439 0.3597561)  
##                           7848) WordCount.log>=6.873163 18   4 N (0.7777778 0.2222222) *
##                           7849) WordCount.log< 6.873163 146  55 N (0.6232877 0.3767123)  
##                            15698) WordCount.log< 6.862757 130  45 N (0.6538462 0.3461538)  
##                              31396) WordCount.log>=6.843217 32   8 N (0.7500000 0.2500000) *
##                              31397) WordCount.log< 6.843217 98  37 N (0.6224490 0.3775510)  
##                                62794) WordCount.log< 6.836796 83  28 N (0.6626506 0.3373494)  
##                                 125588) WordCount.log>=6.829253 11   2 N (0.8181818 0.1818182) *
##                                 125589) WordCount.log< 6.829253 72  26 N (0.6388889 0.3611111)  
##                                   251178) WordCount.log< 6.807382 29   8 N (0.7241379 0.2758621) *
##                                   251179) WordCount.log>=6.807382 43  18 N (0.5813953 0.4186047)  
##                                     502358) WordCount.log>=6.810693 35  12 N (0.6571429 0.3428571) *
##                                     502359) WordCount.log< 6.810693 8   2 Y (0.2500000 0.7500000) *
##                                62795) WordCount.log>=6.836796 15   6 Y (0.4000000 0.6000000) *
##                            15699) WordCount.log>=6.862757 16   6 Y (0.3750000 0.6250000) *
##                         3925) WordCount.log< 6.790659 21  10 Y (0.4761905 0.5238095) *
##                       1963) WordCount.log>=6.884486 10   4 Y (0.4000000 0.6000000) *
##                    491) WordCount.log>=6.912741 94  42 N (0.5531915 0.4468085)  
##                      982) WordCount.log>=6.935857 63  25 N (0.6031746 0.3968254)  
##                       1964) WordCount.log< 6.942156 10   1 N (0.9000000 0.1000000) *
##                       1965) WordCount.log>=6.942156 53  24 N (0.5471698 0.4528302)  
##                         3930) WordCount.log< 6.956069 21   8 N (0.6190476 0.3809524) *
##                         3931) WordCount.log>=6.956069 32  16 N (0.5000000 0.5000000)  
##                           7862) WordCount.log>=6.96319 23   9 N (0.6086957 0.3913043) *
##                           7863) WordCount.log< 6.96319 9   2 Y (0.2222222 0.7777778) *
##                      983) WordCount.log< 6.935857 31  14 Y (0.4516129 0.5483871)  
##                       1966) WordCount.log< 6.928048 24  12 N (0.5000000 0.5000000)  
##                         3932) WordCount.log>=6.920178 9   3 N (0.6666667 0.3333333) *
##                         3933) WordCount.log< 6.920178 15   6 Y (0.4000000 0.6000000) *
##                       1967) WordCount.log>=6.928048 7   2 Y (0.2857143 0.7142857) *
##                123) WordCount.log>=7.162785 208  88 N (0.5769231 0.4230769)  
##                  246) WordCount.log>=7.17434 199  81 N (0.5929648 0.4070352)  
##                    492) WordCount.log>=7.275172 114  42 N (0.6315789 0.3684211)  
##                      984) WordCount.log< 7.345687 35   9 N (0.7428571 0.2571429) *
##                      985) WordCount.log>=7.345687 79  33 N (0.5822785 0.4177215)  
##                       1970) WordCount.log< 7.543801 67  27 N (0.5970149 0.4029851)  
##                         3940) WordCount.log>=7.506042 8   2 N (0.7500000 0.2500000) *
##                         3941) WordCount.log< 7.506042 59  25 N (0.5762712 0.4237288)  
##                           7882) WordCount.log< 7.405491 27  10 N (0.6296296 0.3703704) *
##                           7883) WordCount.log>=7.405491 32  15 N (0.5312500 0.4687500)  
##                            15766) WordCount.log>=7.444526 16   6 N (0.6250000 0.3750000) *
##                            15767) WordCount.log< 7.444526 16   7 Y (0.4375000 0.5625000) *
##                       1971) WordCount.log>=7.543801 12   6 N (0.5000000 0.5000000) *
##                    493) WordCount.log< 7.275172 85  39 N (0.5411765 0.4588235)  
##                      986) WordCount.log< 7.257355 69  29 N (0.5797101 0.4202899)  
##                       1972) WordCount.log< 7.191805 14   4 N (0.7142857 0.2857143) *
##                       1973) WordCount.log>=7.191805 55  25 N (0.5454545 0.4545455)  
##                         3946) WordCount.log>=7.20897 40  16 N (0.6000000 0.4000000)  
##                           7892) WordCount.log< 7.220371 7   1 N (0.8571429 0.1428571) *
##                           7893) WordCount.log>=7.220371 33  15 N (0.5454545 0.4545455)  
##                            15786) WordCount.log>=7.238497 15   5 N (0.6666667 0.3333333) *
##                            15787) WordCount.log< 7.238497 18   8 Y (0.4444444 0.5555556) *
##                         3947) WordCount.log< 7.20897 15   6 Y (0.4000000 0.6000000) *
##                      987) WordCount.log>=7.257355 16   6 Y (0.3750000 0.6250000) *
##                  247) WordCount.log< 7.17434 9   2 Y (0.2222222 0.7777778) *
##             31) WordCount.log< 6.775937 187  81 N (0.5668449 0.4331551)  
##               62) WordCount.log< 6.771362 177  73 N (0.5875706 0.4124294)  
##                124) WordCount.log< 6.736373 125  48 N (0.6160000 0.3840000)  
##                  248) WordCount.log>=6.713563 40  11 N (0.7250000 0.2750000) *
##                  249) WordCount.log< 6.713563 85  37 N (0.5647059 0.4352941)  
##                    498) WordCount.log< 6.698884 56  21 N (0.6250000 0.3750000)  
##                      996) WordCount.log>=6.691463 12   2 N (0.8333333 0.1666667) *
##                      997) WordCount.log< 6.691463 44  19 N (0.5681818 0.4318182)  
##                       1994) WordCount.log< 6.685236 35  12 N (0.6571429 0.3428571) *
##                       1995) WordCount.log>=6.685236 9   2 Y (0.2222222 0.7777778) *
##                    499) WordCount.log>=6.698884 29  13 Y (0.4482759 0.5517241) *
##                125) WordCount.log>=6.736373 52  25 N (0.5192308 0.4807692)  
##                  250) WordCount.log>=6.745823 40  16 N (0.6000000 0.4000000) *
##                  251) WordCount.log< 6.745823 12   3 Y (0.2500000 0.7500000) *
##               63) WordCount.log>=6.771362 10   2 Y (0.2000000 0.8000000) *
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-7.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.2890821
## 3        0.2 0.4572127
## 4        0.3 0.4481999
## 5        0.4 0.3744208
## 6        0.5 0.3744208
## 7        0.6 0.2414929
## 8        0.7 0.1339829
## 9        0.8 0.0000000
## 10       0.9 0.0000000
## 11       1.0 0.0000000
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-8.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             3213
## 2            Y                                              375
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                              513
## 2                                              374
##          Prediction
## Reference    N    Y
##         N 3213  513
##         Y  375  374
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.015642e-01   3.368571e-01   7.895723e-01   8.131613e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   4.277569e-06 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-9.png) 

```
##    threshold    f.score
## 1        0.0 0.28654727
## 2        0.1 0.28307434
## 3        0.2 0.37994723
## 4        0.3 0.32692308
## 5        0.4 0.19793814
## 6        0.5 0.19793814
## 7        0.6 0.13365155
## 8        0.7 0.07894737
## 9        0.8 0.00000000
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-10.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             1443
## 2            Y                                              200
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                              270
## 2                                              144
##          Prediction
## Reference    N    Y
##         N 1443  270
##         Y  200  144
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##    0.771511911    0.241360856    0.752744918    0.789501902    0.832766164 
## AccuracyPValue  McnemarPValue 
##    1.000000000    0.001458922 
##                    model_id model_method         feats max.nTuningRuns
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart WordCount.log               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.633                 0.062   0.7074274
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.2       0.4572127        0.8015642
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7895723             0.8131613     0.3368571   0.6504263
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.3799472        0.7715119
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7527449             0.7895019     0.2413609
```

```r
if (glb_is_regression || glb_is_binomial) # For multinomials this model will be run next by default
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_var,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitent_df, OOB_df=glb_OOBent_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.rpart"
## [1] "    indep_vars: WordCount.log"
## + Fold1: cp=0.001335 
## - Fold1: cp=0.001335 
## + Fold2: cp=0.001335 
## - Fold2: cp=0.001335 
## + Fold3: cp=0.001335 
## - Fold3: cp=0.001335 
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.00223 on full training set
```

```
## Warning in myfit_mdl(model_id = "Max.cor.Y", model_method = "rpart",
## model_type = glb_model_type, : model's bestTune found at an extreme of
## tuneGrid for parameter: cp
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-11.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-12.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 4475 
## 
##            CP nsplit rel error
## 1 0.002225189      0         1
## 
## Node number 1: 4475 observations
##   predicted class=N  expected loss=0.1673743  P(node) =1
##     class counts:  3726   749
##    probabilities: 0.833 0.167 
## 
## n= 4475 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
## 1) root 4475 749 N (0.8326257 0.1673743) *
## [1] "    calling mypredict_mdl for fit:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.rpart.N
## 1            N                                   3726
## 2            Y                                    749
##          Prediction
## Reference    N    Y
##         N 3726    0
##         Y  749    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.326257e-01   0.000000e+00   8.213602e-01   8.434553e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   5.097571e-01  1.800616e-164 
## [1] "    calling mypredict_mdl for OOB:"
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.rpart.N
## 1            N                                   1713
## 2            Y                                    344
##          Prediction
## Reference    N    Y
##         N 1713    0
##         Y  344    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.327662e-01   0.000000e+00   8.159247e-01   8.486533e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   5.143944e-01   2.337097e-76 
##          model_id model_method         feats max.nTuningRuns
## 1 Max.cor.Y.rpart        rpart WordCount.log               3
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.353                 0.071         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.8174308
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8213602             0.8434553    0.06210715         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8327662
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8159247             0.8486533             0
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.002468564      0.01888548
```

```r
# Used to compare vs. Interactions.High.cor.Y 
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_var,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitent_df, OOB_df=glb_OOBent_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.glm"
## [1] "    indep_vars: WordCount.log"
## + Fold1: parameter=none 
## - Fold1: parameter=none 
## + Fold2: parameter=none 
## - Fold2: parameter=none 
## + Fold3: parameter=none 
## - Fold3: parameter=none 
## Aggregating results
## Fitting final model on full training set
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-13.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-14.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-15.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-16.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.4353  -0.6490  -0.4807  -0.2734   3.2543  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -7.02059    0.33309  -21.08   <2e-16 ***
## WordCount.log  0.88928    0.05231   17.00   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 3670.9  on 4473  degrees of freedom
## AIC: 3674.9
## 
## Number of Fisher Scoring iterations: 5
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-17.png) 

```
##    threshold     f.score
## 1        0.0 0.286753446
## 2        0.1 0.353155973
## 3        0.2 0.422222222
## 4        0.3 0.287937743
## 5        0.4 0.080367394
## 6        0.5 0.023225806
## 7        0.6 0.013227513
## 8        0.7 0.005326232
## 9        0.8 0.000000000
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-18.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 2700
## 2            Y                                  274
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                 1026
## 2                                  475
##          Prediction
## Reference    N    Y
##         N 2700 1026
##         Y  274  475
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.094972e-01   2.560981e-01   6.959502e-01   7.227703e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   2.363819e-96 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-19.png) 

```
##    threshold     f.score
## 1        0.0 0.286547272
## 2        0.1 0.352486188
## 3        0.2 0.396172249
## 4        0.3 0.333868379
## 5        0.4 0.118483412
## 6        0.5 0.016713092
## 7        0.6 0.011527378
## 8        0.7 0.005797101
## 9        0.8 0.000000000
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-20.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 1219
## 2            Y                                  137
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                  494
## 2                                  207
##          Prediction
## Reference    N    Y
##         N 1219  494
##         Y  137  207
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   6.932426e-01   2.215049e-01   6.728061e-01   7.131270e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   1.362906e-45 
##        model_id model_method         feats max.nTuningRuns
## 1 Max.cor.Y.glm          glm WordCount.log               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.211                  0.08   0.7301738
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.2       0.4222222        0.8306149
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.6959502             0.7227703    0.01169498   0.7342331
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.3961722        0.6932426
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.6728061              0.713127     0.2215049    3674.923
##   max.AccuracySD.fit max.KappaSD.fit
## 1          0.0016274     0.007870559
```

```r
# Interactions.High.cor.Y
if (length(int_feats <- setdiff(unique(glb_feats_df$cor.high.X), NA)) > 0) {
    # lm & glm handle interaction terms; rpart & rf do not
    if (glb_is_regression || glb_is_binomial) {
        indep_vars_vctr <- 
            c(max_cor_y_x_var, paste(max_cor_y_x_var, int_feats, sep=":"))       
    } else { indep_vars_vctr <- union(max_cor_y_x_var, int_feats) }
    
    ret_lst <- myfit_mdl(model_id="Interact.High.cor.Y", 
                            model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                         model_type=glb_model_type,
                            indep_vars_vctr,
                            glb_rsp_var, glb_rsp_var_out,
                            fit_df=glb_fitent_df, OOB_df=glb_OOBent_df,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)                        
}    
```

```
## [1] "fitting model: Interact.High.cor.Y.glm"
## [1] "    indep_vars: WordCount.log, WordCount.log:PubDate.apm.fctr, WordCount.log:S.can, WordCount.log:S.presid, WordCount.log:S.take, WordCount.log:S.new, WordCount.log:S.day, WordCount.log:S.compani, WordCount.log:S.time, WordCount.log:S.articl, WordCount.log:S.will, WordCount.log:S.intern, WordCount.log:H.week, WordCount.log:NewsDesk.fctr, WordCount.log:H.num.chars.log, WordCount.log:A.num.chars.log, WordCount.log:A.num.words.log, WordCount.log:S.num.chars.log"
## + Fold1: parameter=none
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## - Fold1: parameter=none 
## + Fold2: parameter=none
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## - Fold2: parameter=none 
## + Fold3: parameter=none
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## - Fold3: parameter=none 
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning: not plotting observations with leverage one:
##   137, 297
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-21.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-22.png) 

```
## Warning: not plotting observations with leverage one:
##   137, 297
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-23.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -2.79811  -0.45943  -0.26492  -0.06943   3.07161  
## 
## Coefficients: (2 not defined because of singularities)
##                                                                     Estimate
## (Intercept)                                                        -7.001805
## WordCount.log                                                       2.072665
## `WordCount.log:PubDate.apm.fctrpm`                                  0.021959
## `WordCount.log:S.can`                                              -0.030862
## `WordCount.log:S.presid`                                            0.091648
## `WordCount.log:S.take`                                             -0.008695
## `WordCount.log:S.new`                                              -0.027775
## `WordCount.log:S.day`                                               0.001517
## `WordCount.log:S.compani`                                          -0.079091
## `WordCount.log:S.time`                                              0.008838
## `WordCount.log:S.articl`                                            0.078013
## `WordCount.log:S.will`                                             -0.058889
## `WordCount.log:S.intern`                                           -0.091176
## `WordCount.log:H.week`                                             -0.247893
## `WordCount.log:NewsDesk.fctrCulture`                               -0.080132
## `WordCount.log:NewsDesk.fctrScience`                                0.343534
## `WordCount.log:NewsDesk.fctrOpEd`                                   0.412458
## `WordCount.log:NewsDesk.fctrmyMisc::`                              -0.115605
## `WordCount.log:NewsDesk.fctrForeign`                               -0.414548
## `WordCount.log:NewsDesk.fctrStyles`                                 0.217986
## `WordCount.log:NewsDesk.fctrTStyle`                                -0.320142
## `WordCount.log:NewsDesk.fctrMagazine`                              -3.399759
## `WordCount.log:NewsDesk.fctrWhat We're::`                          -2.881435
## `WordCount.log:NewsDesk.fctrPictures of the (Day|Year|.)::`        -5.316649
## `WordCount.log:NewsDesk.fctrTravel`                                -0.405769
## `WordCount.log:NewsDesk.fctrMetro`                                 -0.154774
## `WordCount.log:NewsDesk.fctrDaily (Clip )*Report::`                -3.244963
## `WordCount.log:NewsDesk.fctr6 Q's About the News::`                -3.134782
## `WordCount.log:NewsDesk.fctrTest Yourself::`                       -3.845744
## `WordCount.log:NewsDesk.fctrWord of the Day::`                     -3.244240
## `WordCount.log:NewsDesk.fctr.*Fashion Week::`                      -2.140666
## `WordCount.log:NewsDesk.fctr19[0-9][0-9]::`                        -3.134307
## `WordCount.log:NewsDesk.fctrNational`                              -3.050953
## `WordCount.log:NewsDesk.fctrSports`                                -2.319565
## `WordCount.log:NewsDesk.fctrVerbatim::`                            -4.129767
## `WordCount.log:NewsDesk.fctrFirst Draft::`                         -4.434603
## `WordCount.log:NewsDesk.fctrToday in (Politics|Small Business)::`  -2.629184
## `WordCount.log:NewsDesk.fctrThe Daily Gift::`                             NA
## `WordCount.log:NewsDesk.fctrNew York Today::`                             NA
## `WordCount.log:H.num.chars.log`                                    -0.139378
## `WordCount.log:A.num.chars.log`                                    -1.516228
## `WordCount.log:A.num.words.log`                                    -0.018277
## `WordCount.log:S.num.chars.log`                                     1.385950
##                                                                   Std. Error
## (Intercept)                                                         0.444089
## WordCount.log                                                       0.179051
## `WordCount.log:PubDate.apm.fctrpm`                                  0.017629
## `WordCount.log:S.can`                                               0.036079
## `WordCount.log:S.presid`                                            0.046012
## `WordCount.log:S.take`                                              0.048263
## `WordCount.log:S.new`                                               0.029540
## `WordCount.log:S.day`                                               0.047704
## `WordCount.log:S.compani`                                           0.038260
## `WordCount.log:S.time`                                              0.031902
## `WordCount.log:S.articl`                                            0.078063
## `WordCount.log:S.will`                                              0.031722
## `WordCount.log:S.intern`                                            0.110567
## `WordCount.log:H.week`                                              0.071795
## `WordCount.log:NewsDesk.fctrCulture`                                0.031974
## `WordCount.log:NewsDesk.fctrScience`                                0.037203
## `WordCount.log:NewsDesk.fctrOpEd`                                   0.031690
## `WordCount.log:NewsDesk.fctrmyMisc::`                               0.024338
## `WordCount.log:NewsDesk.fctrForeign`                                0.113095
## `WordCount.log:NewsDesk.fctrStyles`                                 0.030657
## `WordCount.log:NewsDesk.fctrTStyle`                                 0.056502
## `WordCount.log:NewsDesk.fctrMagazine`                             255.148360
## `WordCount.log:NewsDesk.fctrWhat We're::`                         211.632204
## `WordCount.log:NewsDesk.fctrPictures of the (Day|Year|.)::`       371.628816
## `WordCount.log:NewsDesk.fctrTravel`                                 0.175015
## `WordCount.log:NewsDesk.fctrMetro`                                  0.055074
## `WordCount.log:NewsDesk.fctrDaily (Clip )*Report::`               185.754776
## `WordCount.log:NewsDesk.fctr6 Q's About the News::`               202.631243
## `WordCount.log:NewsDesk.fctrTest Yourself::`                      258.586370
## `WordCount.log:NewsDesk.fctrWord of the Day::`                    183.451214
## `WordCount.log:NewsDesk.fctr.*Fashion Week::`                     962.052184
## `WordCount.log:NewsDesk.fctr19[0-9][0-9]::`                       451.361342
## `WordCount.log:NewsDesk.fctrNational`                             574.508686
## `WordCount.log:NewsDesk.fctrSports`                               885.376731
## `WordCount.log:NewsDesk.fctrVerbatim::`                           368.103709
## `WordCount.log:NewsDesk.fctrFirst Draft::`                        236.141771
## `WordCount.log:NewsDesk.fctrToday in (Politics|Small Business)::` 160.299281
## `WordCount.log:NewsDesk.fctrThe Daily Gift::`                             NA
## `WordCount.log:NewsDesk.fctrNew York Today::`                             NA
## `WordCount.log:H.num.chars.log`                                     0.022574
## `WordCount.log:A.num.chars.log`                                     0.967289
## `WordCount.log:A.num.words.log`                                     0.069003
## `WordCount.log:S.num.chars.log`                                     0.966685
##                                                                   z value
## (Intercept)                                                       -15.767
## WordCount.log                                                      11.576
## `WordCount.log:PubDate.apm.fctrpm`                                  1.246
## `WordCount.log:S.can`                                              -0.855
## `WordCount.log:S.presid`                                            1.992
## `WordCount.log:S.take`                                             -0.180
## `WordCount.log:S.new`                                              -0.940
## `WordCount.log:S.day`                                               0.032
## `WordCount.log:S.compani`                                          -2.067
## `WordCount.log:S.time`                                              0.277
## `WordCount.log:S.articl`                                            0.999
## `WordCount.log:S.will`                                             -1.856
## `WordCount.log:S.intern`                                           -0.825
## `WordCount.log:H.week`                                             -3.453
## `WordCount.log:NewsDesk.fctrCulture`                               -2.506
## `WordCount.log:NewsDesk.fctrScience`                                9.234
## `WordCount.log:NewsDesk.fctrOpEd`                                  13.016
## `WordCount.log:NewsDesk.fctrmyMisc::`                              -4.750
## `WordCount.log:NewsDesk.fctrForeign`                               -3.665
## `WordCount.log:NewsDesk.fctrStyles`                                 7.111
## `WordCount.log:NewsDesk.fctrTStyle`                                -5.666
## `WordCount.log:NewsDesk.fctrMagazine`                              -0.013
## `WordCount.log:NewsDesk.fctrWhat We're::`                          -0.014
## `WordCount.log:NewsDesk.fctrPictures of the (Day|Year|.)::`        -0.014
## `WordCount.log:NewsDesk.fctrTravel`                                -2.318
## `WordCount.log:NewsDesk.fctrMetro`                                 -2.810
## `WordCount.log:NewsDesk.fctrDaily (Clip )*Report::`                -0.017
## `WordCount.log:NewsDesk.fctr6 Q's About the News::`                -0.015
## `WordCount.log:NewsDesk.fctrTest Yourself::`                       -0.015
## `WordCount.log:NewsDesk.fctrWord of the Day::`                     -0.018
## `WordCount.log:NewsDesk.fctr.*Fashion Week::`                      -0.002
## `WordCount.log:NewsDesk.fctr19[0-9][0-9]::`                        -0.007
## `WordCount.log:NewsDesk.fctrNational`                              -0.005
## `WordCount.log:NewsDesk.fctrSports`                                -0.003
## `WordCount.log:NewsDesk.fctrVerbatim::`                            -0.011
## `WordCount.log:NewsDesk.fctrFirst Draft::`                         -0.019
## `WordCount.log:NewsDesk.fctrToday in (Politics|Small Business)::`  -0.016
## `WordCount.log:NewsDesk.fctrThe Daily Gift::`                          NA
## `WordCount.log:NewsDesk.fctrNew York Today::`                          NA
## `WordCount.log:H.num.chars.log`                                    -6.174
## `WordCount.log:A.num.chars.log`                                    -1.568
## `WordCount.log:A.num.words.log`                                    -0.265
## `WordCount.log:S.num.chars.log`                                     1.434
##                                                                   Pr(>|z|)
## (Intercept)                                                        < 2e-16
## WordCount.log                                                      < 2e-16
## `WordCount.log:PubDate.apm.fctrpm`                                0.212906
## `WordCount.log:S.can`                                             0.392340
## `WordCount.log:S.presid`                                          0.046388
## `WordCount.log:S.take`                                            0.857030
## `WordCount.log:S.new`                                             0.347098
## `WordCount.log:S.day`                                             0.974629
## `WordCount.log:S.compani`                                         0.038718
## `WordCount.log:S.time`                                            0.781746
## `WordCount.log:S.articl`                                          0.317624
## `WordCount.log:S.will`                                            0.063392
## `WordCount.log:S.intern`                                          0.409584
## `WordCount.log:H.week`                                            0.000555
## `WordCount.log:NewsDesk.fctrCulture`                              0.012205
## `WordCount.log:NewsDesk.fctrScience`                               < 2e-16
## `WordCount.log:NewsDesk.fctrOpEd`                                  < 2e-16
## `WordCount.log:NewsDesk.fctrmyMisc::`                             2.03e-06
## `WordCount.log:NewsDesk.fctrForeign`                              0.000247
## `WordCount.log:NewsDesk.fctrStyles`                               1.16e-12
## `WordCount.log:NewsDesk.fctrTStyle`                               1.46e-08
## `WordCount.log:NewsDesk.fctrMagazine`                             0.989369
## `WordCount.log:NewsDesk.fctrWhat We're::`                         0.989137
## `WordCount.log:NewsDesk.fctrPictures of the (Day|Year|.)::`       0.988586
## `WordCount.log:NewsDesk.fctrTravel`                               0.020424
## `WordCount.log:NewsDesk.fctrMetro`                                0.004950
## `WordCount.log:NewsDesk.fctrDaily (Clip )*Report::`               0.986062
## `WordCount.log:NewsDesk.fctr6 Q's About the News::`               0.987657
## `WordCount.log:NewsDesk.fctrTest Yourself::`                      0.988134
## `WordCount.log:NewsDesk.fctrWord of the Day::`                    0.985891
## `WordCount.log:NewsDesk.fctr.*Fashion Week::`                     0.998225
## `WordCount.log:NewsDesk.fctr19[0-9][0-9]::`                       0.994459
## `WordCount.log:NewsDesk.fctrNational`                             0.995763
## `WordCount.log:NewsDesk.fctrSports`                               0.997910
## `WordCount.log:NewsDesk.fctrVerbatim::`                           0.991049
## `WordCount.log:NewsDesk.fctrFirst Draft::`                        0.985017
## `WordCount.log:NewsDesk.fctrToday in (Politics|Small Business)::` 0.986914
## `WordCount.log:NewsDesk.fctrThe Daily Gift::`                           NA
## `WordCount.log:NewsDesk.fctrNew York Today::`                           NA
## `WordCount.log:H.num.chars.log`                                   6.65e-10
## `WordCount.log:A.num.chars.log`                                   0.116997
## `WordCount.log:A.num.words.log`                                   0.791111
## `WordCount.log:S.num.chars.log`                                   0.151654
##                                                                      
## (Intercept)                                                       ***
## WordCount.log                                                     ***
## `WordCount.log:PubDate.apm.fctrpm`                                   
## `WordCount.log:S.can`                                                
## `WordCount.log:S.presid`                                          *  
## `WordCount.log:S.take`                                               
## `WordCount.log:S.new`                                                
## `WordCount.log:S.day`                                                
## `WordCount.log:S.compani`                                         *  
## `WordCount.log:S.time`                                               
## `WordCount.log:S.articl`                                             
## `WordCount.log:S.will`                                            .  
## `WordCount.log:S.intern`                                             
## `WordCount.log:H.week`                                            ***
## `WordCount.log:NewsDesk.fctrCulture`                              *  
## `WordCount.log:NewsDesk.fctrScience`                              ***
## `WordCount.log:NewsDesk.fctrOpEd`                                 ***
## `WordCount.log:NewsDesk.fctrmyMisc::`                             ***
## `WordCount.log:NewsDesk.fctrForeign`                              ***
## `WordCount.log:NewsDesk.fctrStyles`                               ***
## `WordCount.log:NewsDesk.fctrTStyle`                               ***
## `WordCount.log:NewsDesk.fctrMagazine`                                
## `WordCount.log:NewsDesk.fctrWhat We're::`                            
## `WordCount.log:NewsDesk.fctrPictures of the (Day|Year|.)::`          
## `WordCount.log:NewsDesk.fctrTravel`                               *  
## `WordCount.log:NewsDesk.fctrMetro`                                ** 
## `WordCount.log:NewsDesk.fctrDaily (Clip )*Report::`                  
## `WordCount.log:NewsDesk.fctr6 Q's About the News::`                  
## `WordCount.log:NewsDesk.fctrTest Yourself::`                         
## `WordCount.log:NewsDesk.fctrWord of the Day::`                       
## `WordCount.log:NewsDesk.fctr.*Fashion Week::`                        
## `WordCount.log:NewsDesk.fctr19[0-9][0-9]::`                          
## `WordCount.log:NewsDesk.fctrNational`                                
## `WordCount.log:NewsDesk.fctrSports`                                  
## `WordCount.log:NewsDesk.fctrVerbatim::`                              
## `WordCount.log:NewsDesk.fctrFirst Draft::`                           
## `WordCount.log:NewsDesk.fctrToday in (Politics|Small Business)::`    
## `WordCount.log:NewsDesk.fctrThe Daily Gift::`                        
## `WordCount.log:NewsDesk.fctrNew York Today::`                        
## `WordCount.log:H.num.chars.log`                                   ***
## `WordCount.log:A.num.chars.log`                                      
## `WordCount.log:A.num.words.log`                                      
## `WordCount.log:S.num.chars.log`                                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 2453.6  on 4434  degrees of freedom
## AIC: 2535.6
## 
## Number of Fisher Scoring iterations: 17
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-24.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-25.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.5242031
## 3        0.2 0.6469585
## 4        0.3 0.6483957
## 5        0.4 0.6449275
## 6        0.5 0.6014320
## 7        0.6 0.5724198
## 8        0.7 0.5032741
## 9        0.8 0.3880289
## 10       0.9 0.2440137
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           3464
## 2            Y                                            264
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                            262
## 2                                            485
##          Prediction
## Reference    N    Y
##         N 3464  262
##         Y  264  485
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.824581e-01   5.778297e-01   8.726598e-01   8.917541e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   6.094197e-21   9.652216e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-26.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-27.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.5267490
## 3        0.2 0.6314526
## 4        0.3 0.6610879
## 5        0.4 0.6829268
## 6        0.5 0.6589404
## 7        0.6 0.6347518
## 8        0.7 0.5725191
## 9        0.8 0.4458874
## 10       0.9 0.2871046
## 11       1.0 0.0000000
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-28.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           1625
## 2            Y                                            120
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                             88
## 2                                            224
##          Prediction
## Reference    N    Y
##         N 1625   88
##         Y  120  224
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.988819e-01   6.229467e-01   8.850365e-01   9.115781e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   8.234641e-18   3.159770e-02 
##                  model_id model_method
## 1 Interact.High.cor.Y.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                            feats
## 1 WordCount.log, WordCount.log:PubDate.apm.fctr, WordCount.log:S.can, WordCount.log:S.presid, WordCount.log:S.take, WordCount.log:S.new, WordCount.log:S.day, WordCount.log:S.compani, WordCount.log:S.time, WordCount.log:S.articl, WordCount.log:S.will, WordCount.log:S.intern, WordCount.log:H.week, WordCount.log:NewsDesk.fctr, WordCount.log:H.num.chars.log, WordCount.log:A.num.chars.log, WordCount.log:A.num.words.log, WordCount.log:S.num.chars.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      3.703                  0.88
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9020505                    0.3       0.6483957        0.8862589
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8726598             0.8917541     0.5338094   0.9123444
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.6829268        0.8988819
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.8850365             0.9115781     0.6229467    2535.551
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.007433434      0.03381263
```

```r
# Low.cor.X
if (glb_is_classification && glb_is_binomial)
    indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & 
                                            is.ConditionalX.y & 
                                            (exclude.as.feat != 1))[, "id"] else
    indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & 
                                            (exclude.as.feat != 1))[, "id"]  
ret_lst <- myfit_mdl(model_id="Low.cor.X", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                        indep_vars_vctr=indep_vars_vctr,
                        model_type=glb_model_type,                     
                        glb_rsp_var, glb_rsp_var_out,
                        fit_df=glb_fitent_df, OOB_df=glb_OOBent_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Low.cor.X.glm"
## [1] "    indep_vars: WordCount.log, PubDate.apm.fctr, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, .rnorm, PubDate.date.fctr, PubDate.second, S.presid, S.take, PubDate.minute, S.new, PubDate.wkday.fctr, S.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, H.new, S.first, S.time, H.newyork, S.articl, S.will, H.day, S.newyork, H.today, H.report, S.intern, H.week, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, H.num.chars.log, H.num.words.log, A.num.words.log, A.num.chars.log, S.num.words.log"
## + Fold1: parameter=none
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## - Fold1: parameter=none 
## + Fold2: parameter=none
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## - Fold2: parameter=none 
## + Fold3: parameter=none
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## - Fold3: parameter=none 
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning: not plotting observations with leverage one:
##   137, 3778
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-29.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-30.png) 

```
## Warning: not plotting observations with leverage one:
##   137, 3778
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-31.png) 

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## Warning in sqrt(crit * p * (1 - hh)/hh): NaNs produced
```

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.8139  -0.3707  -0.1768  -0.0001   3.2329  
## 
## Coefficients: (25 not defined because of singularities)
##                                                           Estimate
## (Intercept)                                             -1.919e+00
## WordCount.log                                            1.119e+00
## PubDate.apm.fctrpm                                      -2.909e-01
## S.can                                                   -4.459e-01
## H.has.ebola                                             -5.976e-01
## S.make                                                  -2.062e-01
## S.one                                                    1.436e+01
## S.state                                                  1.253e-01
## A.one                                                   -1.436e+01
## S.said                                                   5.342e-01
## .rnorm                                                  -4.058e-02
## `PubDate.date.fctr(7,13]`                                1.485e-01
## `PubDate.date.fctr(13,19]`                              -1.458e-01
## `PubDate.date.fctr(19,25]`                              -2.037e-01
## `PubDate.date.fctr(25,31]`                               7.680e-02
## PubDate.second                                          -6.697e-05
## S.presid                                                 2.288e-01
## S.take                                                  -3.498e-02
## PubDate.minute                                          -1.254e-03
## S.new                                                   -1.709e-01
## PubDate.wkday.fctr1                                     -8.066e-01
## PubDate.wkday.fctr2                                     -1.135e+00
## PubDate.wkday.fctr3                                     -9.746e-01
## PubDate.wkday.fctr4                                     -1.130e+00
## PubDate.wkday.fctr5                                     -1.174e+00
## PubDate.wkday.fctr6                                     -9.346e-01
## S.day                                                   -2.591e-02
## H.X2014                                                 -6.415e-01
## S.show                                                  -2.759e-01
## S.report                                                -1.740e-01
## S.share                                                 -4.062e-01
## S.year                                                  -4.595e-01
## S.compani                                               -2.870e-01
## H.new                                                   -5.058e-01
## S.first                                                 -1.761e-01
## S.time                                                  -1.027e-01
## H.newyork                                                1.548e-01
## S.articl                                                 7.687e-01
## S.will                                                  -2.850e-01
## H.day                                                   -7.359e-03
## S.newyork                                               -2.466e-02
## H.today                                                 -1.694e+01
## H.report                                                -7.846e-01
## S.intern                                                 2.024e-02
## H.week                                                  -5.751e-01
## S.week                                                   1.319e-01
## S.fashion                                               -1.073e+00
## `Headline.pfx.fctr19[0-9][0-9]::`                        1.023e+00
## `Headline.pfx.fctrDaily (Clip )*Report::`               -1.601e+01
## `Headline.pfx.fctr.*Fashion Week::`                     -1.315e+01
## `Headline.pfx.fctrWhat We're::`                         -2.161e+01
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`       -3.095e+01
## `Headline.pfx.fctrToday in (Politics|Small Business)::`  1.973e-01
## `Headline.pfx.fctrMorning Agenda::`                     -1.746e+01
## `Headline.pfx.fctrNew York Today::`                      1.426e+01
## `Headline.pfx.fctr6 Q's About the News::`               -3.370e+01
## `Headline.pfx.fctrTest Yourself::`                      -3.218e+01
## `Headline.pfx.fctrWord of the Day::`                    -3.467e+01
## `Headline.pfx.fctrVerbatim::`                           -1.806e+01
## `Headline.pfx.fctrFirst Draft::`                        -1.994e+01
## `Headline.pfx.fctrThe Daily Gift::`                     -1.424e+01
## NewsDesk.fctrCulture                                    -3.262e+00
## NewsDesk.fctrScience                                    -1.674e+01
## NewsDesk.fctrOpEd                                       -1.521e+01
## `NewsDesk.fctrmyMisc::`                                 -1.977e+01
## NewsDesk.fctrForeign                                    -5.655e+00
## NewsDesk.fctrStyles                                     -1.418e+01
## NewsDesk.fctrTStyle                                     -4.560e+00
## NewsDesk.fctrMagazine                                   -2.076e+01
## `NewsDesk.fctrWhat We're::`                              1.259e+00
## `NewsDesk.fctrPictures of the (Day|Year|.)::`                   NA
## NewsDesk.fctrTravel                                     -4.614e+00
## NewsDesk.fctrMetro                                      -2.449e+00
## `NewsDesk.fctrDaily (Clip )*Report::`                   -3.524e+00
## `NewsDesk.fctr6 Q's About the News::`                           NA
## `NewsDesk.fctrTest Yourself::`                                  NA
## `NewsDesk.fctrWord of the Day::`                                NA
## `NewsDesk.fctr.*Fashion Week::`                         -4.538e+00
## `NewsDesk.fctr19[0-9][0-9]::`                           -2.078e+01
## NewsDesk.fctrNational                                   -3.442e+01
## NewsDesk.fctrSports                                     -2.063e+01
## `NewsDesk.fctrVerbatim::`                                       NA
## `NewsDesk.fctrFirst Draft::`                                    NA
## `NewsDesk.fctrToday in (Politics|Small Business)::`     -5.451e+00
## `NewsDesk.fctrThe Daily Gift::`                                 NA
## `NewsDesk.fctrNew York Today::`                                 NA
## SectionName.fctrArts                                            NA
## `SectionName.fctrBusiness Day`                          -3.429e+00
## SectionName.fctrHealth                                   1.639e+01
## SectionName.fctrOpinion                                  1.553e+01
## SectionName.fctrForeign                                 -1.496e+01
## SectionName.fctrStyle                                   -6.626e+00
## SectionName.fctrTStyle                                          NA
## SectionName.fctrWorld                                           NA
## SectionName.fctrTechnology                              -2.372e+00
## SectionName.fctrMagazine                                        NA
## SectionName.fctrMultimedia                               1.417e+01
## `SectionName.fctrmyMisc::`                               1.693e+01
## `SectionName.fctrWhat We're::`                                  NA
## SectionName.fctrTravel                                          NA
## SectionName.fctrU.S.                                     1.384e+01
## `SectionName.fctrN.Y. / Region`                                 NA
## `SectionName.fctrDaily (Clip )*Report::`                        NA
## SectionName.fctrStyles                                  -5.766e+00
## SectionName.fctrOpen                                    -1.399e+00
## `SectionName.fctr.*Fashion Week::`                              NA
## `SectionName.fctr19[0-9][0-9]::`                                NA
## SectionName.fctrSports                                          NA
## SectionName.fctrNational                                 1.496e+01
## `SectionName.fctrVerbatim::`                                    NA
## `SectionName.fctrFirst Draft::`                                 NA
## `SectionName.fctrToday in (Politics|Small Business)::`          NA
## SectionName.fctrScience                                  3.452e+01
## SectionName.fctrCulture                                         NA
## SectionName.fctrBusiness                                -2.765e+00
## `SectionName.fctrThe Daily Gift::`                              NA
## SectionName.fctrOpEd                                            NA
## H.num.chars.log                                          1.525e-01
## H.num.words.log                                         -5.800e-01
## A.num.words.log                                         -8.123e-01
## A.num.chars.log                                         -1.620e-01
## S.num.words.log                                          2.300e-01
##                                                         Std. Error z value
## (Intercept)                                              1.236e+00  -1.552
## WordCount.log                                            8.297e-02  13.490
## PubDate.apm.fctrpm                                       1.268e-01  -2.295
## S.can                                                    2.734e-01  -1.631
## H.has.ebola                                              4.781e-01  -1.250
## S.make                                                   2.969e-01  -0.695
## S.one                                                    6.883e+03   0.002
## S.state                                                  3.278e-01   0.382
## A.one                                                    6.883e+03  -0.002
## S.said                                                   3.115e-01   1.715
## .rnorm                                                   5.805e-02  -0.699
## `PubDate.date.fctr(7,13]`                                1.811e-01   0.820
## `PubDate.date.fctr(13,19]`                               1.805e-01  -0.807
## `PubDate.date.fctr(19,25]`                               1.773e-01  -1.149
## `PubDate.date.fctr(25,31]`                               1.886e-01   0.407
## PubDate.second                                           3.376e-03  -0.020
## S.presid                                                 2.965e-01   0.772
## S.take                                                   3.510e-01  -0.100
## PubDate.minute                                           3.310e-03  -0.379
## S.new                                                    2.048e-01  -0.834
## PubDate.wkday.fctr1                                      2.667e-01  -3.024
## PubDate.wkday.fctr2                                      2.739e-01  -4.145
## PubDate.wkday.fctr3                                      2.698e-01  -3.613
## PubDate.wkday.fctr4                                      2.738e-01  -4.127
## PubDate.wkday.fctr5                                      2.756e-01  -4.259
## PubDate.wkday.fctr6                                      4.366e-01  -2.141
## S.day                                                    3.878e-01  -0.067
## H.X2014                                                  9.539e-01  -0.672
## S.show                                                   3.914e-01  -0.705
## S.report                                                 3.391e-01  -0.513
## S.share                                                  4.065e-01  -0.999
## S.year                                                   3.038e-01  -1.513
## S.compani                                                2.693e-01  -1.066
## H.new                                                    4.104e-01  -1.232
## S.first                                                  4.448e-01  -0.396
## S.time                                                   2.602e-01  -0.394
## H.newyork                                                6.765e-01   0.229
## S.articl                                                 5.462e-01   1.408
## S.will                                                   2.190e-01  -1.301
## H.day                                                    5.531e-01  -0.013
## S.newyork                                                3.883e-01  -0.064
## H.today                                                  3.306e+03  -0.005
## H.report                                                 6.217e-01  -1.262
## S.intern                                                 6.987e-01   0.029
## H.week                                                   5.736e-01  -1.003
## S.week                                                   3.141e-01   0.420
## S.fashion                                                1.212e+00  -0.885
## `Headline.pfx.fctr19[0-9][0-9]::`                        2.912e+03   0.000
## `Headline.pfx.fctrDaily (Clip )*Report::`                1.660e+03  -0.010
## `Headline.pfx.fctr.*Fashion Week::`                      8.650e+02  -0.015
## `Headline.pfx.fctrWhat We're::`                          6.070e+03  -0.004
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`        6.077e+03  -0.005
## `Headline.pfx.fctrToday in (Politics|Small Business)::`  3.692e+03   0.000
## `Headline.pfx.fctrMorning Agenda::`                      1.588e+03  -0.011
## `Headline.pfx.fctrNew York Today::`                      3.306e+03   0.004
## `Headline.pfx.fctr6 Q's About the News::`                6.095e+03  -0.006
## `Headline.pfx.fctrTest Yourself::`                       6.085e+03  -0.005
## `Headline.pfx.fctrWord of the Day::`                     6.077e+03  -0.006
## `Headline.pfx.fctrVerbatim::`                            2.235e+03  -0.008
## `Headline.pfx.fctrFirst Draft::`                         1.434e+03  -0.014
## `Headline.pfx.fctrThe Daily Gift::`                      2.417e+03  -0.006
## NewsDesk.fctrCulture                                     4.143e-01  -7.873
## NewsDesk.fctrScience                                     5.854e+03  -0.003
## NewsDesk.fctrOpEd                                        5.854e+03  -0.003
## `NewsDesk.fctrmyMisc::`                                  5.854e+03  -0.003
## NewsDesk.fctrForeign                                     8.235e-01  -6.867
## NewsDesk.fctrStyles                                      5.854e+03  -0.002
## NewsDesk.fctrTStyle                                      5.211e-01  -8.751
## NewsDesk.fctrMagazine                                    2.132e+03  -0.010
## `NewsDesk.fctrWhat We're::`                              6.436e+03   0.000
## `NewsDesk.fctrPictures of the (Day|Year|.)::`                   NA      NA
## NewsDesk.fctrTravel                                      1.067e+00  -4.324
## NewsDesk.fctrMetro                                       5.154e-01  -4.752
## `NewsDesk.fctrDaily (Clip )*Report::`                    2.317e+03  -0.002
## `NewsDesk.fctr6 Q's About the News::`                           NA      NA
## `NewsDesk.fctrTest Yourself::`                                  NA      NA
## `NewsDesk.fctrWord of the Day::`                                NA      NA
## `NewsDesk.fctr.*Fashion Week::`                          1.079e+04   0.000
## `NewsDesk.fctr19[0-9][0-9]::`                            4.924e+03  -0.004
## NewsDesk.fctrNational                                    9.582e+03  -0.004
## NewsDesk.fctrSports                                      1.075e+04  -0.002
## `NewsDesk.fctrVerbatim::`                                       NA      NA
## `NewsDesk.fctrFirst Draft::`                                    NA      NA
## `NewsDesk.fctrToday in (Politics|Small Business)::`      2.539e+03  -0.002
## `NewsDesk.fctrThe Daily Gift::`                                 NA      NA
## `NewsDesk.fctrNew York Today::`                                 NA      NA
## SectionName.fctrArts                                            NA      NA
## `SectionName.fctrBusiness Day`                           4.140e-01  -8.283
## SectionName.fctrHealth                                   5.854e+03   0.003
## SectionName.fctrOpinion                                  5.854e+03   0.003
## SectionName.fctrForeign                                  2.720e+03  -0.005
## SectionName.fctrStyle                                    9.454e+03  -0.001
## SectionName.fctrTStyle                                          NA      NA
## SectionName.fctrWorld                                           NA      NA
## SectionName.fctrTechnology                               4.304e-01  -5.511
## SectionName.fctrMagazine                                        NA      NA
## SectionName.fctrMultimedia                               5.854e+03   0.002
## `SectionName.fctrmyMisc::`                               5.854e+03   0.003
## `SectionName.fctrWhat We're::`                                  NA      NA
## SectionName.fctrTravel                                          NA      NA
## SectionName.fctrU.S.                                     5.854e+03   0.002
## `SectionName.fctrN.Y. / Region`                                 NA      NA
## `SectionName.fctrDaily (Clip )*Report::`                        NA      NA
## SectionName.fctrStyles                                   5.962e+03  -0.001
## SectionName.fctrOpen                                     9.594e+03   0.000
## `SectionName.fctr.*Fashion Week::`                              NA      NA
## `SectionName.fctr19[0-9][0-9]::`                                NA      NA
## SectionName.fctrSports                                          NA      NA
## SectionName.fctrNational                                 1.221e+04   0.001
## `SectionName.fctrVerbatim::`                                    NA      NA
## `SectionName.fctrFirst Draft::`                                 NA      NA
## `SectionName.fctrToday in (Politics|Small Business)::`          NA      NA
## SectionName.fctrScience                                  1.224e+04   0.003
## SectionName.fctrCulture                                         NA      NA
## SectionName.fctrBusiness                                 1.305e+00  -2.118
## `SectionName.fctrThe Daily Gift::`                              NA      NA
## SectionName.fctrOpEd                                            NA      NA
## H.num.chars.log                                          3.348e-01   0.455
## H.num.words.log                                          3.942e-01  -1.471
## A.num.words.log                                          2.198e+00  -0.370
## A.num.chars.log                                          4.614e-01  -0.351
## S.num.words.log                                          2.246e+00   0.102
##                                                         Pr(>|z|)    
## (Intercept)                                             0.120608    
## WordCount.log                                            < 2e-16 ***
## PubDate.apm.fctrpm                                      0.021751 *  
## S.can                                                   0.102942    
## H.has.ebola                                             0.211370    
## S.make                                                  0.487266    
## S.one                                                   0.998336    
## S.state                                                 0.702297    
## A.one                                                   0.998335    
## S.said                                                  0.086352 .  
## .rnorm                                                  0.484525    
## `PubDate.date.fctr(7,13]`                               0.412241    
## `PubDate.date.fctr(13,19]`                              0.419379    
## `PubDate.date.fctr(19,25]`                              0.250570    
## `PubDate.date.fctr(25,31]`                              0.683884    
## PubDate.second                                          0.984172    
## S.presid                                                0.440218    
## S.take                                                  0.920635    
## PubDate.minute                                          0.704668    
## S.new                                                   0.404061    
## PubDate.wkday.fctr1                                     0.002493 ** 
## PubDate.wkday.fctr2                                     3.39e-05 ***
## PubDate.wkday.fctr3                                     0.000303 ***
## PubDate.wkday.fctr4                                     3.68e-05 ***
## PubDate.wkday.fctr5                                     2.06e-05 ***
## PubDate.wkday.fctr6                                     0.032311 *  
## S.day                                                   0.946738    
## H.X2014                                                 0.501279    
## S.show                                                  0.480899    
## S.report                                                0.607927    
## S.share                                                 0.317690    
## S.year                                                  0.130356    
## S.compani                                               0.286605    
## H.new                                                   0.217803    
## S.first                                                 0.692078    
## S.time                                                  0.693238    
## H.newyork                                               0.818972    
## S.articl                                                0.159269    
## S.will                                                  0.193109    
## H.day                                                   0.989384    
## S.newyork                                               0.949368    
## H.today                                                 0.995912    
## H.report                                                0.206977    
## S.intern                                                0.976891    
## H.week                                                  0.316031    
## S.week                                                  0.674454    
## S.fashion                                               0.376096    
## `Headline.pfx.fctr19[0-9][0-9]::`                       0.999720    
## `Headline.pfx.fctrDaily (Clip )*Report::`               0.992308    
## `Headline.pfx.fctr.*Fashion Week::`                     0.987868    
## `Headline.pfx.fctrWhat We're::`                         0.997159    
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`       0.995937    
## `Headline.pfx.fctrToday in (Politics|Small Business)::` 0.999957    
## `Headline.pfx.fctrMorning Agenda::`                     0.991226    
## `Headline.pfx.fctrNew York Today::`                     0.996558    
## `Headline.pfx.fctr6 Q's About the News::`               0.995588    
## `Headline.pfx.fctrTest Yourself::`                      0.995781    
## `Headline.pfx.fctrWord of the Day::`                    0.995448    
## `Headline.pfx.fctrVerbatim::`                           0.993553    
## `Headline.pfx.fctrFirst Draft::`                        0.988908    
## `Headline.pfx.fctrThe Daily Gift::`                     0.995298    
## NewsDesk.fctrCulture                                    3.46e-15 ***
## NewsDesk.fctrScience                                    0.997719    
## NewsDesk.fctrOpEd                                       0.997927    
## `NewsDesk.fctrmyMisc::`                                 0.997305    
## NewsDesk.fctrForeign                                    6.55e-12 ***
## NewsDesk.fctrStyles                                     0.998067    
## NewsDesk.fctrTStyle                                      < 2e-16 ***
## NewsDesk.fctrMagazine                                   0.992231    
## `NewsDesk.fctrWhat We're::`                             0.999844    
## `NewsDesk.fctrPictures of the (Day|Year|.)::`                 NA    
## NewsDesk.fctrTravel                                     1.53e-05 ***
## NewsDesk.fctrMetro                                      2.02e-06 ***
## `NewsDesk.fctrDaily (Clip )*Report::`                   0.998786    
## `NewsDesk.fctr6 Q's About the News::`                         NA    
## `NewsDesk.fctrTest Yourself::`                                NA    
## `NewsDesk.fctrWord of the Day::`                              NA    
## `NewsDesk.fctr.*Fashion Week::`                         0.999664    
## `NewsDesk.fctr19[0-9][0-9]::`                           0.996633    
## NewsDesk.fctrNational                                   0.997134    
## NewsDesk.fctrSports                                     0.998470    
## `NewsDesk.fctrVerbatim::`                                     NA    
## `NewsDesk.fctrFirst Draft::`                                  NA    
## `NewsDesk.fctrToday in (Politics|Small Business)::`     0.998287    
## `NewsDesk.fctrThe Daily Gift::`                               NA    
## `NewsDesk.fctrNew York Today::`                               NA    
## SectionName.fctrArts                                          NA    
## `SectionName.fctrBusiness Day`                           < 2e-16 ***
## SectionName.fctrHealth                                  0.997767    
## SectionName.fctrOpinion                                 0.997883    
## SectionName.fctrForeign                                 0.995612    
## SectionName.fctrStyle                                   0.999441    
## SectionName.fctrTStyle                                        NA    
## SectionName.fctrWorld                                         NA    
## SectionName.fctrTechnology                              3.56e-08 ***
## SectionName.fctrMagazine                                      NA    
## SectionName.fctrMultimedia                              0.998069    
## `SectionName.fctrmyMisc::`                              0.997693    
## `SectionName.fctrWhat We're::`                                NA    
## SectionName.fctrTravel                                        NA    
## SectionName.fctrU.S.                                    0.998114    
## `SectionName.fctrN.Y. / Region`                               NA    
## `SectionName.fctrDaily (Clip )*Report::`                      NA    
## SectionName.fctrStyles                                  0.999228    
## SectionName.fctrOpen                                    0.999884    
## `SectionName.fctr.*Fashion Week::`                            NA    
## `SectionName.fctr19[0-9][0-9]::`                              NA    
## SectionName.fctrSports                                        NA    
## SectionName.fctrNational                                0.999022    
## `SectionName.fctrVerbatim::`                                  NA    
## `SectionName.fctrFirst Draft::`                               NA    
## `SectionName.fctrToday in (Politics|Small Business)::`        NA    
## SectionName.fctrScience                                 0.997751    
## SectionName.fctrCulture                                       NA    
## SectionName.fctrBusiness                                0.034143 *  
## `SectionName.fctrThe Daily Gift::`                            NA    
## SectionName.fctrOpEd                                          NA    
## H.num.chars.log                                         0.648788    
## H.num.words.log                                         0.141175    
## A.num.words.log                                         0.711722    
## A.num.chars.log                                         0.725458    
## S.num.words.log                                         0.918446    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 2032.8  on 4378  degrees of freedom
## AIC: 2226.8
## 
## Number of Fisher Scoring iterations: 18
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-32.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-33.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.6252791
## 3        0.2 0.6998265
## 4        0.3 0.7170543
## 5        0.4 0.7060459
## 6        0.5 0.6942393
## 7        0.6 0.6677019
## 8        0.7 0.6340249
## 9        0.8 0.5283721
## 10       0.9 0.3150985
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 3482
## 2            Y                                  194
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  244
## 2                                  555
##          Prediction
## Reference    N    Y
##         N 3482  244
##         Y  194  555
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.021229e-01   6.579557e-01   8.930408e-01   9.106763e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   1.391306e-40   1.921617e-02 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-34.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-35.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.6214908
## 3        0.2 0.6816479
## 4        0.3 0.6963788
## 5        0.4 0.7079646
## 6        0.5 0.7051482
## 7        0.6 0.6926829
## 8        0.7 0.6597222
## 9        0.8 0.5454545
## 10       0.9 0.3277910
## 11       1.0 0.0000000
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_0-36.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 1619
## 2            Y                                  104
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                   94
## 2                                  240
##          Prediction
## Reference    N    Y
##         N 1619   94
##         Y  104  240
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.037433e-01   6.503545e-01   8.901728e-01   9.161500e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   1.831382e-20   5.224313e-01 
##        model_id model_method
## 1 Low.cor.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      feats
## 1 WordCount.log, PubDate.apm.fctr, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, .rnorm, PubDate.date.fctr, PubDate.second, S.presid, S.take, PubDate.minute, S.new, PubDate.wkday.fctr, S.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, H.new, S.first, S.time, H.newyork, S.articl, S.will, H.day, S.newyork, H.today, H.report, S.intern, H.week, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, H.num.chars.log, H.num.words.log, A.num.words.log, A.num.chars.log, S.num.words.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                     10.977                 3.218
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9396716                    0.3       0.7170543        0.9016767
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8930408             0.9106763     0.6182928   0.9301172
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7079646        0.9037433
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.8901728               0.91615     0.6503545    2226.837
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.004828153      0.01963972
```

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="fit.models", 
    chunk_step_major=glb_script_df[nrow(glb_script_df), "chunk_step_major"], 
    chunk_step_minor=glb_script_df[nrow(glb_script_df), "chunk_step_minor"]+1,                              
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##           chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed9   fit.models                6                0 148.494
## elapsed10  fit.models                6                1 199.203
```


```r
# All X that is not user excluded
if (glb_is_classification && glb_is_binomial) {
    model_id_pfx <- "Conditional.X"
# indep_vars_vctr <- setdiff(names(glb_fitent_df), union(glb_rsp_var, glb_exclude_vars_as_features))
    indep_vars_vctr <- subset(glb_feats_df, is.ConditionalX.y & 
                                            (exclude.as.feat != 1))[, "id"]
} else {
    model_id_pfx <- "All.X"
    indep_vars_vctr <- subset(glb_feats_df, 
                                            (exclude.as.feat != 1))[, "id"]
}
for (method in glb_models_method_vctr) {
    ret_lst <- myfit_mdl(model_id=paste0(model_id_pfx, ""), model_method=method,
                            indep_vars_vctr=indep_vars_vctr,
                            model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitent_df, OOB_df=glb_OOBent_df,
                n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    
    # Since caret does not optimize rpart well
    if (method == "rpart")
        ret_lst <- myfit_mdl(model_id=paste0(model_id_pfx, ".cp.0"), model_method=method,
                                indep_vars_vctr=indep_vars_vctr,
                                model_type=glb_model_type,
                                rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                                fit_df=glb_fitent_df, OOB_df=glb_OOBent_df,        
            n_cv_folds=0, tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
    
    # Compare how rf performs w/i & w/o .rnorm
    if (method == "rf")
        ret_lst <- myfit_mdl(model_id=paste0(model_id_pfx, ".no.rnorm"), model_method=method,
                                indep_vars_vctr=setdiff(indep_vars_vctr, c(".rnorm")),
                                model_type=glb_model_type,
                                rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                                fit_df=glb_fitent_df, OOB_df=glb_OOBent_df,
                    n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
}
```

```
## [1] "fitting model: Conditional.X.glm"
## [1] "    indep_vars: WordCount.log, PubDate.hour, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, .rnorm, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, A.compani, H.new, S.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, SubsectionName.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.words.log, A.num.chars.log, S.num.chars.log, A.num.words.unq.log, S.num.words.log, S.num.words.unq.log"
## + Fold1: parameter=none
```

```
## Warning: glm.fit: algorithm did not converge
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## - Fold1: parameter=none 
## + Fold2: parameter=none
```

```
## Warning: glm.fit: algorithm did not converge
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## - Fold2: parameter=none 
## + Fold3: parameter=none
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## - Fold3: parameter=none 
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: glm.fit: algorithm did not converge
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```
## Warning: not plotting observations with leverage one:
##   137, 297, 1693, 3382, 3596, 3778, 3794, 3800, 4156, 4229, 4338, 4394, 4459
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-1.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-2.png) 

```
## Warning: not plotting observations with leverage one:
##   137, 297, 1693, 3382, 3596, 3778, 3794, 3800, 4156, 4229, 4338, 4394, 4459
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-3.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
##  -8.49    0.00    0.00    0.00    8.49  
## 
## Coefficients: (71 not defined because of singularities)
##                                                                                                 Estimate
## (Intercept)                                                                                   -3.927e+15
## WordCount.log                                                                                  7.964e+14
## PubDate.hour                                                                                   3.591e+13
## PubDate.apm.fctrpm                                                                            -4.164e+14
## A.can                                                                                          5.371e+15
## S.can                                                                                         -5.585e+15
## H.has.ebola                                                                                   -6.034e+14
## S.make                                                                                        -1.687e+14
## S.one                                                                                         -5.165e+14
## S.state                                                                                       -1.109e+14
## A.one                                                                                          5.851e+14
## S.said                                                                                         3.403e+14
## .rnorm                                                                                         8.151e+12
## `PubDate.date.fctr(7,13]`                                                                     -1.423e+13
## `PubDate.date.fctr(13,19]`                                                                    -3.688e+13
## `PubDate.date.fctr(19,25]`                                                                    -1.556e+14
## `PubDate.date.fctr(25,31]`                                                                     1.695e+14
## PubDate.second                                                                                -5.051e+11
## S.presid                                                                                       7.360e+13
## A.presid                                                                                              NA
## S.take                                                                                        -3.426e+15
## A.take                                                                                         3.522e+15
## PubDate.minute                                                                                -4.131e+11
## S.new                                                                                         -2.033e+15
## A.new                                                                                          2.049e+15
## PubDate.wkday.fctr1                                                                           -1.165e+14
## PubDate.wkday.fctr2                                                                           -4.779e+14
## PubDate.wkday.fctr3                                                                           -2.670e+14
## PubDate.wkday.fctr4                                                                           -4.867e+14
## PubDate.wkday.fctr5                                                                           -3.918e+14
## PubDate.wkday.fctr6                                                                           -7.918e+13
## S.day                                                                                          1.910e+15
## A.day                                                                                         -1.969e+15
## H.X2014                                                                                       -3.497e+14
## S.show                                                                                        -2.764e+14
## S.report                                                                                       1.933e+14
## S.share                                                                                       -5.677e+13
## S.year                                                                                        -2.326e+14
## S.compani                                                                                     -1.081e+14
## A.compani                                                                                             NA
## H.new                                                                                         -3.730e+14
## S.first                                                                                        1.595e+14
## S.time                                                                                        -6.327e+14
## A.time                                                                                         3.738e+14
## H.newyork                                                                                     -3.487e+14
## S.articl                                                                                      -4.334e+13
## A.articl                                                                                              NA
## S.will                                                                                        -5.183e+14
## A.will                                                                                         3.917e+14
## H.day                                                                                          1.313e+14
## S.newyork                                                                                      2.918e+14
## H.today                                                                                       -2.897e+15
## H.report                                                                                      -1.781e+14
## S.intern                                                                                       7.160e+13
## A.intern                                                                                              NA
## H.week                                                                                         2.510e+14
## H.fashion                                                                                      1.910e+15
## S.week                                                                                         1.709e+14
## S.fashion                                                                                     -9.242e+14
## `Headline.pfx.fctr19[0-9][0-9]::`                                                              6.347e+14
## `Headline.pfx.fctrDaily (Clip )*Report::`                                                     -2.867e+15
## `Headline.pfx.fctr.*Fashion Week::`                                                           -1.443e+15
## `Headline.pfx.fctrWhat We're::`                                                               -5.357e+15
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                                              5.274e+15
## `Headline.pfx.fctrToday in (Politics|Small Business)::`                                        2.862e+15
## `Headline.pfx.fctrMorning Agenda::`                                                           -1.499e+15
## `Headline.pfx.fctrNew York Today::`                                                            2.216e+15
## `Headline.pfx.fctr6 Q's About the News::`                                                     -1.974e+14
## `Headline.pfx.fctrTest Yourself::`                                                             3.385e+15
## `Headline.pfx.fctrWord of the Day::`                                                          -1.691e+14
## `Headline.pfx.fctrVerbatim::`                                                                 -2.655e+15
## `Headline.pfx.fctrFirst Draft::`                                                              -3.230e+15
## `Headline.pfx.fctrThe Daily Gift::`                                                           -1.032e+15
## NewsDesk.fctrCulture                                                                          -1.332e+15
## NewsDesk.fctrScience                                                                          -2.444e+15
## NewsDesk.fctrOpEd                                                                              2.134e+15
## `NewsDesk.fctrmyMisc::`                                                                        1.522e+15
## NewsDesk.fctrForeign                                                                          -5.210e+15
## NewsDesk.fctrStyles                                                                           -4.798e+15
## NewsDesk.fctrTStyle                                                                           -2.218e+15
## NewsDesk.fctrMagazine                                                                         -4.196e+15
## `NewsDesk.fctrWhat We're::`                                                                    5.003e+14
## `NewsDesk.fctrPictures of the (Day|Year|.)::`                                                         NA
## NewsDesk.fctrTravel                                                                           -3.414e+15
## NewsDesk.fctrMetro                                                                            -5.334e+14
## `NewsDesk.fctrDaily (Clip )*Report::`                                                         -5.431e+14
## `NewsDesk.fctr6 Q's About the News::`                                                                 NA
## `NewsDesk.fctrTest Yourself::`                                                                        NA
## `NewsDesk.fctrWord of the Day::`                                                                      NA
## `NewsDesk.fctr.*Fashion Week::`                                                               -6.200e+15
## `NewsDesk.fctr19[0-9][0-9]::`                                                                 -4.328e+15
## NewsDesk.fctrNational                                                                         -9.304e+15
## NewsDesk.fctrSports                                                                           -5.015e+15
## `NewsDesk.fctrVerbatim::`                                                                             NA
## `NewsDesk.fctrFirst Draft::`                                                                          NA
## `NewsDesk.fctrToday in (Politics|Small Business)::`                                           -5.432e+15
## `NewsDesk.fctrThe Daily Gift::`                                                                       NA
## `NewsDesk.fctrNew York Today::`                                                                       NA
## SectionName.fctrArts                                                                                  NA
## `SectionName.fctrBusiness Day`                                                                -1.500e+15
## SectionName.fctrHealth                                                                         2.752e+15
## SectionName.fctrOpinion                                                                       -1.633e+15
## SectionName.fctrForeign                                                                        9.364e+14
## SectionName.fctrStyle                                                                          4.756e+14
## SectionName.fctrTStyle                                                                                NA
## SectionName.fctrWorld                                                                                 NA
## SectionName.fctrTechnology                                                                    -3.608e+14
## SectionName.fctrMagazine                                                                              NA
## SectionName.fctrMultimedia                                                                    -4.749e+15
## `SectionName.fctrmyMisc::`                                                                    -2.422e+15
## `SectionName.fctrWhat We're::`                                                                        NA
## SectionName.fctrTravel                                                                                NA
## SectionName.fctrU.S.                                                                           4.824e+15
## `SectionName.fctrN.Y. / Region`                                                                       NA
## `SectionName.fctrDaily (Clip )*Report::`                                                              NA
## SectionName.fctrStyles                                                                         1.782e+15
## SectionName.fctrOpen                                                                          -6.581e+15
## `SectionName.fctr.*Fashion Week::`                                                                    NA
## `SectionName.fctr19[0-9][0-9]::`                                                                      NA
## SectionName.fctrSports                                                                                NA
## SectionName.fctrNational                                                                       5.586e+15
## `SectionName.fctrVerbatim::`                                                                          NA
## `SectionName.fctrFirst Draft::`                                                                       NA
## `SectionName.fctrToday in (Politics|Small Business)::`                                                NA
## SectionName.fctrScience                                                                        6.583e+15
## SectionName.fctrCulture                                                                               NA
## SectionName.fctrBusiness                                                                      -2.634e+15
## `SectionName.fctrThe Daily Gift::`                                                                    NA
## SectionName.fctrOpEd                                                                                  NA
## `SubsectionName.fctrCulture::Arts`                                                                    NA
## SubsectionName.fctrDealbook                                                                    8.134e+14
## `SubsectionName.fctrScience::Health`                                                                  NA
## `SubsectionName.fctrOpEd::Opinion`                                                                    NA
## `SubsectionName.fctrRoom For Debate`                                                          -4.688e+15
## `SubsectionName.fctrForeign::Foreign`                                                                 NA
## `SubsectionName.fctrFashion & Style`                                                                  NA
## `SubsectionName.fctrTStyle::TStyle`                                                                   NA
## `SubsectionName.fctrAsia Pacific`                                                              1.155e+15
## `SubsectionName.fctrBusiness::Technology`                                                             NA
## `SubsectionName.fctrMagazine::Magazine`                                                               NA
## `SubsectionName.fctrmyMisc::Multimedia`                                                               NA
## `SubsectionName.fctrmyMisc::myMisc::`                                                                 NA
## `SubsectionName.fctrWhat We're::What We're::`                                                         NA
## `SubsectionName.fctrPictures of the (Day|Year|.)::Multimedia`                                         NA
## `SubsectionName.fctrTravel::Travel`                                                                   NA
## SubsectionName.fctrEducation                                                                  -8.423e+15
## `SubsectionName.fctrThe Public Editor`                                                         6.337e+14
## `SubsectionName.fctrStyles::U.S.`                                                                     NA
## `SubsectionName.fctrSmall Business`                                                                   NA
## `SubsectionName.fctrMetro::N.Y. / Region`                                                             NA
## `SubsectionName.fctrDaily (Clip )*Report::Daily (Clip )*Report::`                                     NA
## `SubsectionName.fctrStyles::Styles`                                                                   NA
## `SubsectionName.fctrmyMisc::Open`                                                                     NA
## `SubsectionName.fctr.*Fashion Week::.*Fashion Week::`                                                 NA
## `SubsectionName.fctrForeign::World`                                                                   NA
## `SubsectionName.fctr19[0-9][0-9]::19[0-9][0-9]::`                                                     NA
## SubsectionName.fctrPolitics                                                                           NA
## `SubsectionName.fctrSports::Sports`                                                                   NA
## `SubsectionName.fctrmyMisc::Opinion`                                                                  NA
## `SubsectionName.fctrNational::National`                                                               NA
## `SubsectionName.fctrVerbatim::Verbatim::`                                                             NA
## `SubsectionName.fctrFirst Draft::First Draft::`                                                       NA
## `SubsectionName.fctrToday in (Politics|Small Business)::Today in (Politics|Small Business)::`         NA
## `SubsectionName.fctrScience::Science`                                                                 NA
## `SubsectionName.fctrCulture::Culture`                                                                 NA
## `SubsectionName.fctrStyles::Health`                                                                   NA
## `SubsectionName.fctrBusiness::Business`                                                               NA
## `SubsectionName.fctrmyMisc::U.S.`                                                                     NA
## `SubsectionName.fctrmyMisc::Health`                                                                   NA
## `SubsectionName.fctrThe Daily Gift::The Daily Gift::`                                                 NA
## `SubsectionName.fctrmyMisc::Travel`                                                                   NA
## `SubsectionName.fctrOpEd::OpEd`                                                                       NA
## `SubsectionName.fctrmyMisc::Crosswords/Games`                                                 -1.856e+15
## `SubsectionName.fctrmyMisc::Arts`                                                                     NA
## `SubsectionName.fctrmyMisc::Technology`                                                               NA
## `SubsectionName.fctrmyMisc::N.Y. / Region`                                                            NA
## `SubsectionName.fctrNew York Today::N.Y. / Region`                                                    NA
## H.num.chars.log                                                                                3.682e+14
## H.num.words.log                                                                                6.902e+13
## H.num.words.unq.log                                                                           -7.799e+14
## A.num.words.log                                                                               -1.267e+16
## A.num.chars.log                                                                               -5.724e+15
## S.num.chars.log                                                                                5.878e+15
## A.num.words.unq.log                                                                            1.315e+16
## S.num.words.log                                                                                1.347e+16
## S.num.words.unq.log                                                                           -1.462e+16
##                                                                                               Std. Error
## (Intercept)                                                                                    2.416e+07
## WordCount.log                                                                                  1.255e+06
## PubDate.hour                                                                                   3.902e+05
## PubDate.apm.fctrpm                                                                             3.663e+06
## A.can                                                                                          6.960e+07
## S.can                                                                                          7.024e+07
## H.has.ebola                                                                                    9.031e+06
## S.make                                                                                         5.542e+06
## S.one                                                                                          1.056e+08
## S.state                                                                                        5.729e+06
## A.one                                                                                          1.057e+08
## S.said                                                                                         5.855e+06
## .rnorm                                                                                         1.009e+06
## `PubDate.date.fctr(7,13]`                                                                      3.149e+06
## `PubDate.date.fctr(13,19]`                                                                     3.118e+06
## `PubDate.date.fctr(19,25]`                                                                     3.033e+06
## `PubDate.date.fctr(25,31]`                                                                     3.383e+06
## PubDate.second                                                                                 5.844e+04
## S.presid                                                                                       5.120e+06
## A.presid                                                                                              NA
## S.take                                                                                         9.714e+07
## A.take                                                                                         9.701e+07
## PubDate.minute                                                                                 5.844e+04
## S.new                                                                                          5.088e+07
## A.new                                                                                          5.076e+07
## PubDate.wkday.fctr1                                                                            5.341e+06
## PubDate.wkday.fctr2                                                                            5.356e+06
## PubDate.wkday.fctr3                                                                            5.340e+06
## PubDate.wkday.fctr4                                                                            5.337e+06
## PubDate.wkday.fctr5                                                                            5.358e+06
## PubDate.wkday.fctr6                                                                            7.946e+06
## S.day                                                                                          6.976e+07
## A.day                                                                                          6.949e+07
## H.X2014                                                                                        9.566e+06
## S.show                                                                                         5.610e+06
## S.report                                                                                       6.093e+06
## S.share                                                                                        6.237e+06
## S.year                                                                                         4.901e+06
## S.compani                                                                                      4.484e+06
## A.compani                                                                                             NA
## H.new                                                                                          5.584e+06
## S.first                                                                                        6.331e+06
## S.time                                                                                         6.812e+07
## A.time                                                                                         6.798e+07
## H.newyork                                                                                      7.706e+06
## S.articl                                                                                       9.932e+06
## A.articl                                                                                              NA
## S.will                                                                                         7.820e+07
## A.will                                                                                         7.815e+07
## H.day                                                                                          8.677e+06
## S.newyork                                                                                      5.710e+06
## H.today                                                                                        2.766e+07
## H.report                                                                                       9.792e+06
## S.intern                                                                                       1.089e+07
## A.intern                                                                                              NA
## H.week                                                                                         1.105e+07
## H.fashion                                                                                      1.448e+07
## S.week                                                                                         5.236e+06
## S.fashion                                                                                      7.965e+06
## `Headline.pfx.fctr19[0-9][0-9]::`                                                              1.927e+07
## `Headline.pfx.fctrDaily (Clip )*Report::`                                                      1.548e+07
## `Headline.pfx.fctr.*Fashion Week::`                                                            1.926e+07
## `Headline.pfx.fctrWhat We're::`                                                                3.923e+07
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                                              7.010e+07
## `Headline.pfx.fctrToday in (Politics|Small Business)::`                                        3.110e+07
## `Headline.pfx.fctrMorning Agenda::`                                                            1.143e+07
## `Headline.pfx.fctrNew York Today::`                                                            3.084e+07
## `Headline.pfx.fctr6 Q's About the News::`                                                      6.922e+07
## `Headline.pfx.fctrTest Yourself::`                                                             6.947e+07
## `Headline.pfx.fctrWord of the Day::`                                                           7.090e+07
## `Headline.pfx.fctrVerbatim::`                                                                  1.687e+07
## `Headline.pfx.fctrFirst Draft::`                                                               1.378e+07
## `Headline.pfx.fctrThe Daily Gift::`                                                            1.851e+07
## NewsDesk.fctrCulture                                                                           8.927e+06
## NewsDesk.fctrScience                                                                           9.589e+07
## NewsDesk.fctrOpEd                                                                              7.454e+07
## `NewsDesk.fctrmyMisc::`                                                                        6.798e+07
## NewsDesk.fctrForeign                                                                           3.979e+07
## NewsDesk.fctrStyles                                                                            1.199e+08
## NewsDesk.fctrTStyle                                                                            8.993e+06
## NewsDesk.fctrMagazine                                                                          1.715e+07
## `NewsDesk.fctrWhat We're::`                                                                    4.269e+07
## `NewsDesk.fctrPictures of the (Day|Year|.)::`                                                         NA
## NewsDesk.fctrTravel                                                                            1.094e+07
## NewsDesk.fctrMetro                                                                             1.080e+07
## `NewsDesk.fctrDaily (Clip )*Report::`                                                          2.215e+07
## `NewsDesk.fctr6 Q's About the News::`                                                                 NA
## `NewsDesk.fctrTest Yourself::`                                                                        NA
## `NewsDesk.fctrWord of the Day::`                                                                      NA
## `NewsDesk.fctr.*Fashion Week::`                                                                7.285e+07
## `NewsDesk.fctr19[0-9][0-9]::`                                                                  3.386e+07
## NewsDesk.fctrNational                                                                          1.292e+08
## NewsDesk.fctrSports                                                                            6.986e+07
## `NewsDesk.fctrVerbatim::`                                                                             NA
## `NewsDesk.fctrFirst Draft::`                                                                          NA
## `NewsDesk.fctrToday in (Politics|Small Business)::`                                            2.052e+07
## `NewsDesk.fctrThe Daily Gift::`                                                                       NA
## `NewsDesk.fctrNew York Today::`                                                                       NA
## SectionName.fctrArts                                                                                  NA
## `SectionName.fctrBusiness Day`                                                                 1.253e+07
## SectionName.fctrHealth                                                                         9.602e+07
## SectionName.fctrOpinion                                                                        7.488e+07
## SectionName.fctrForeign                                                                        4.410e+07
## SectionName.fctrStyle                                                                          1.293e+08
## SectionName.fctrTStyle                                                                                NA
## SectionName.fctrWorld                                                                                 NA
## SectionName.fctrTechnology                                                                     9.969e+06
## SectionName.fctrMagazine                                                                              NA
## SectionName.fctrMultimedia                                                                     6.899e+07
## `SectionName.fctrmyMisc::`                                                                     6.847e+07
## `SectionName.fctrWhat We're::`                                                                        NA
## SectionName.fctrTravel                                                                                NA
## SectionName.fctrU.S.                                                                           1.203e+08
## `SectionName.fctrN.Y. / Region`                                                                       NA
## `SectionName.fctrDaily (Clip )*Report::`                                                              NA
## SectionName.fctrStyles                                                                         1.206e+08
## SectionName.fctrOpen                                                                           8.347e+07
## `SectionName.fctr.*Fashion Week::`                                                                    NA
## `SectionName.fctr19[0-9][0-9]::`                                                                      NA
## SectionName.fctrSports                                                                                NA
## SectionName.fctrNational                                                                       1.381e+08
## `SectionName.fctrVerbatim::`                                                                          NA
## `SectionName.fctrFirst Draft::`                                                                       NA
## `SectionName.fctrToday in (Politics|Small Business)::`                                                NA
## SectionName.fctrScience                                                                        1.174e+08
## SectionName.fctrCulture                                                                               NA
## SectionName.fctrBusiness                                                                       3.481e+07
## `SectionName.fctrThe Daily Gift::`                                                                    NA
## SectionName.fctrOpEd                                                                                  NA
## `SubsectionName.fctrCulture::Arts`                                                                    NA
## SubsectionName.fctrDealbook                                                                    9.795e+06
## `SubsectionName.fctrScience::Health`                                                                  NA
## `SubsectionName.fctrOpEd::Opinion`                                                                    NA
## `SubsectionName.fctrRoom For Debate`                                                           3.210e+07
## `SubsectionName.fctrForeign::Foreign`                                                                 NA
## `SubsectionName.fctrFashion & Style`                                                                  NA
## `SubsectionName.fctrTStyle::TStyle`                                                                   NA
## `SubsectionName.fctrAsia Pacific`                                                              3.928e+07
## `SubsectionName.fctrBusiness::Technology`                                                             NA
## `SubsectionName.fctrMagazine::Magazine`                                                               NA
## `SubsectionName.fctrmyMisc::Multimedia`                                                               NA
## `SubsectionName.fctrmyMisc::myMisc::`                                                                 NA
## `SubsectionName.fctrWhat We're::What We're::`                                                         NA
## `SubsectionName.fctrPictures of the (Day|Year|.)::Multimedia`                                         NA
## `SubsectionName.fctrTravel::Travel`                                                                   NA
## SubsectionName.fctrEducation                                                                   9.934e+07
## `SubsectionName.fctrThe Public Editor`                                                         3.521e+07
## `SubsectionName.fctrStyles::U.S.`                                                                     NA
## `SubsectionName.fctrSmall Business`                                                                   NA
## `SubsectionName.fctrMetro::N.Y. / Region`                                                             NA
## `SubsectionName.fctrDaily (Clip )*Report::Daily (Clip )*Report::`                                     NA
## `SubsectionName.fctrStyles::Styles`                                                                   NA
## `SubsectionName.fctrmyMisc::Open`                                                                     NA
## `SubsectionName.fctr.*Fashion Week::.*Fashion Week::`                                                 NA
## `SubsectionName.fctrForeign::World`                                                                   NA
## `SubsectionName.fctr19[0-9][0-9]::19[0-9][0-9]::`                                                     NA
## SubsectionName.fctrPolitics                                                                           NA
## `SubsectionName.fctrSports::Sports`                                                                   NA
## `SubsectionName.fctrmyMisc::Opinion`                                                                  NA
## `SubsectionName.fctrNational::National`                                                               NA
## `SubsectionName.fctrVerbatim::Verbatim::`                                                             NA
## `SubsectionName.fctrFirst Draft::First Draft::`                                                       NA
## `SubsectionName.fctrToday in (Politics|Small Business)::Today in (Politics|Small Business)::`         NA
## `SubsectionName.fctrScience::Science`                                                                 NA
## `SubsectionName.fctrCulture::Culture`                                                                 NA
## `SubsectionName.fctrStyles::Health`                                                                   NA
## `SubsectionName.fctrBusiness::Business`                                                               NA
## `SubsectionName.fctrmyMisc::U.S.`                                                                     NA
## `SubsectionName.fctrmyMisc::Health`                                                                   NA
## `SubsectionName.fctrThe Daily Gift::The Daily Gift::`                                                 NA
## `SubsectionName.fctrmyMisc::Travel`                                                                   NA
## `SubsectionName.fctrOpEd::OpEd`                                                                       NA
## `SubsectionName.fctrmyMisc::Crosswords/Games`                                                  9.604e+07
## `SubsectionName.fctrmyMisc::Arts`                                                                     NA
## `SubsectionName.fctrmyMisc::Technology`                                                               NA
## `SubsectionName.fctrmyMisc::N.Y. / Region`                                                            NA
## `SubsectionName.fctrNew York Today::N.Y. / Region`                                                    NA
## H.num.chars.log                                                                                6.484e+06
## H.num.words.log                                                                                3.800e+07
## H.num.words.unq.log                                                                            3.755e+07
## A.num.words.log                                                                                1.527e+08
## A.num.chars.log                                                                                6.312e+07
## S.num.chars.log                                                                                6.375e+07
## A.num.words.unq.log                                                                            1.475e+08
## S.num.words.log                                                                                1.525e+08
## S.num.words.unq.log                                                                            1.466e+08
##                                                                                                  z value
## (Intercept)                                                                                   -162524772
## WordCount.log                                                                                  634680542
## PubDate.hour                                                                                    92017626
## PubDate.apm.fctrpm                                                                            -113684840
## A.can                                                                                           77163549
## S.can                                                                                          -79512319
## H.has.ebola                                                                                    -66819425
## S.make                                                                                         -30449148
## S.one                                                                                           -4890890
## S.state                                                                                        -19358045
## A.one                                                                                            5534750
## S.said                                                                                          58116286
## .rnorm                                                                                           8075062
## `PubDate.date.fctr(7,13]`                                                                       -4520105
## `PubDate.date.fctr(13,19]`                                                                     -11827159
## `PubDate.date.fctr(19,25]`                                                                     -51309712
## `PubDate.date.fctr(25,31]`                                                                      50113609
## PubDate.second                                                                                  -8642631
## S.presid                                                                                        14374205
## A.presid                                                                                              NA
## S.take                                                                                         -35267564
## A.take                                                                                          36304698
## PubDate.minute                                                                                  -7069707
## S.new                                                                                          -39948619
## A.new                                                                                           40354923
## PubDate.wkday.fctr1                                                                            -21812472
## PubDate.wkday.fctr2                                                                            -89210197
## PubDate.wkday.fctr3                                                                            -49999647
## PubDate.wkday.fctr4                                                                            -91207907
## PubDate.wkday.fctr5                                                                            -73114634
## PubDate.wkday.fctr6                                                                             -9963664
## S.day                                                                                           27382720
## A.day                                                                                          -28327997
## H.X2014                                                                                        -36555153
## S.show                                                                                         -49266566
## S.report                                                                                        31732590
## S.share                                                                                         -9102052
## S.year                                                                                         -47471831
## S.compani                                                                                      -24116640
## A.compani                                                                                             NA
## H.new                                                                                          -66798781
## S.first                                                                                         25185940
## S.time                                                                                          -9287440
## A.time                                                                                           5498717
## H.newyork                                                                                      -45252579
## S.articl                                                                                        -4363557
## A.articl                                                                                              NA
## S.will                                                                                          -6627671
## A.will                                                                                           5012139
## H.day                                                                                           15135893
## S.newyork                                                                                       51107291
## H.today                                                                                       -104707415
## H.report                                                                                       -18189209
## S.intern                                                                                         6575474
## A.intern                                                                                              NA
## H.week                                                                                          22713039
## H.fashion                                                                                      131885859
## S.week                                                                                          32637812
## S.fashion                                                                                     -116036409
## `Headline.pfx.fctr19[0-9][0-9]::`                                                               32938349
## `Headline.pfx.fctrDaily (Clip )*Report::`                                                     -185220509
## `Headline.pfx.fctr.*Fashion Week::`                                                            -74928961
## `Headline.pfx.fctrWhat We're::`                                                               -136569940
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                                               75235149
## `Headline.pfx.fctrToday in (Politics|Small Business)::`                                         92004375
## `Headline.pfx.fctrMorning Agenda::`                                                           -131140706
## `Headline.pfx.fctrNew York Today::`                                                             71860994
## `Headline.pfx.fctr6 Q's About the News::`                                                       -2851832
## `Headline.pfx.fctrTest Yourself::`                                                              48723765
## `Headline.pfx.fctrWord of the Day::`                                                            -2385030
## `Headline.pfx.fctrVerbatim::`                                                                 -157332298
## `Headline.pfx.fctrFirst Draft::`                                                              -234323019
## `Headline.pfx.fctrThe Daily Gift::`                                                            -55717918
## NewsDesk.fctrCulture                                                                          -149170121
## NewsDesk.fctrScience                                                                           -25491515
## NewsDesk.fctrOpEd                                                                               28632614
## `NewsDesk.fctrmyMisc::`                                                                         22395992
## NewsDesk.fctrForeign                                                                          -130949739
## NewsDesk.fctrStyles                                                                            -40011390
## NewsDesk.fctrTStyle                                                                           -246588684
## NewsDesk.fctrMagazine                                                                         -244690286
## `NewsDesk.fctrWhat We're::`                                                                     11719976
## `NewsDesk.fctrPictures of the (Day|Year|.)::`                                                         NA
## NewsDesk.fctrTravel                                                                           -311952956
## NewsDesk.fctrMetro                                                                             -49394861
## `NewsDesk.fctrDaily (Clip )*Report::`                                                          -24519833
## `NewsDesk.fctr6 Q's About the News::`                                                                 NA
## `NewsDesk.fctrTest Yourself::`                                                                        NA
## `NewsDesk.fctrWord of the Day::`                                                                      NA
## `NewsDesk.fctr.*Fashion Week::`                                                                -85106696
## `NewsDesk.fctr19[0-9][0-9]::`                                                                 -127802745
## NewsDesk.fctrNational                                                                          -71993337
## NewsDesk.fctrSports                                                                            -71790666
## `NewsDesk.fctrVerbatim::`                                                                             NA
## `NewsDesk.fctrFirst Draft::`                                                                          NA
## `NewsDesk.fctrToday in (Politics|Small Business)::`                                           -264709796
## `NewsDesk.fctrThe Daily Gift::`                                                                       NA
## `NewsDesk.fctrNew York Today::`                                                                       NA
## SectionName.fctrArts                                                                                  NA
## `SectionName.fctrBusiness Day`                                                                -119665096
## SectionName.fctrHealth                                                                          28662356
## SectionName.fctrOpinion                                                                        -21806662
## SectionName.fctrForeign                                                                         21233833
## SectionName.fctrStyle                                                                            3676940
## SectionName.fctrTStyle                                                                                NA
## SectionName.fctrWorld                                                                                 NA
## SectionName.fctrTechnology                                                                     -36190446
## SectionName.fctrMagazine                                                                              NA
## SectionName.fctrMultimedia                                                                     -68838300
## `SectionName.fctrmyMisc::`                                                                     -35369502
## `SectionName.fctrWhat We're::`                                                                        NA
## SectionName.fctrTravel                                                                                NA
## SectionName.fctrU.S.                                                                            40114583
## `SectionName.fctrN.Y. / Region`                                                                       NA
## `SectionName.fctrDaily (Clip )*Report::`                                                              NA
## SectionName.fctrStyles                                                                          14772771
## SectionName.fctrOpen                                                                           -78849786
## `SectionName.fctr.*Fashion Week::`                                                                    NA
## `SectionName.fctr19[0-9][0-9]::`                                                                      NA
## SectionName.fctrSports                                                                                NA
## SectionName.fctrNational                                                                        40450385
## `SectionName.fctrVerbatim::`                                                                          NA
## `SectionName.fctrFirst Draft::`                                                                       NA
## `SectionName.fctrToday in (Politics|Small Business)::`                                                NA
## SectionName.fctrScience                                                                         56056622
## SectionName.fctrCulture                                                                               NA
## SectionName.fctrBusiness                                                                       -75671583
## `SectionName.fctrThe Daily Gift::`                                                                    NA
## SectionName.fctrOpEd                                                                                  NA
## `SubsectionName.fctrCulture::Arts`                                                                    NA
## SubsectionName.fctrDealbook                                                                     83046338
## `SubsectionName.fctrScience::Health`                                                                  NA
## `SubsectionName.fctrOpEd::Opinion`                                                                    NA
## `SubsectionName.fctrRoom For Debate`                                                          -146058999
## `SubsectionName.fctrForeign::Foreign`                                                                 NA
## `SubsectionName.fctrFashion & Style`                                                                  NA
## `SubsectionName.fctrTStyle::TStyle`                                                                   NA
## `SubsectionName.fctrAsia Pacific`                                                               29397337
## `SubsectionName.fctrBusiness::Technology`                                                             NA
## `SubsectionName.fctrMagazine::Magazine`                                                               NA
## `SubsectionName.fctrmyMisc::Multimedia`                                                               NA
## `SubsectionName.fctrmyMisc::myMisc::`                                                                 NA
## `SubsectionName.fctrWhat We're::What We're::`                                                         NA
## `SubsectionName.fctrPictures of the (Day|Year|.)::Multimedia`                                         NA
## `SubsectionName.fctrTravel::Travel`                                                                   NA
## SubsectionName.fctrEducation                                                                   -84794483
## `SubsectionName.fctrThe Public Editor`                                                          17997078
## `SubsectionName.fctrStyles::U.S.`                                                                     NA
## `SubsectionName.fctrSmall Business`                                                                   NA
## `SubsectionName.fctrMetro::N.Y. / Region`                                                             NA
## `SubsectionName.fctrDaily (Clip )*Report::Daily (Clip )*Report::`                                     NA
## `SubsectionName.fctrStyles::Styles`                                                                   NA
## `SubsectionName.fctrmyMisc::Open`                                                                     NA
## `SubsectionName.fctr.*Fashion Week::.*Fashion Week::`                                                 NA
## `SubsectionName.fctrForeign::World`                                                                   NA
## `SubsectionName.fctr19[0-9][0-9]::19[0-9][0-9]::`                                                     NA
## SubsectionName.fctrPolitics                                                                           NA
## `SubsectionName.fctrSports::Sports`                                                                   NA
## `SubsectionName.fctrmyMisc::Opinion`                                                                  NA
## `SubsectionName.fctrNational::National`                                                               NA
## `SubsectionName.fctrVerbatim::Verbatim::`                                                             NA
## `SubsectionName.fctrFirst Draft::First Draft::`                                                       NA
## `SubsectionName.fctrToday in (Politics|Small Business)::Today in (Politics|Small Business)::`         NA
## `SubsectionName.fctrScience::Science`                                                                 NA
## `SubsectionName.fctrCulture::Culture`                                                                 NA
## `SubsectionName.fctrStyles::Health`                                                                   NA
## `SubsectionName.fctrBusiness::Business`                                                               NA
## `SubsectionName.fctrmyMisc::U.S.`                                                                     NA
## `SubsectionName.fctrmyMisc::Health`                                                                   NA
## `SubsectionName.fctrThe Daily Gift::The Daily Gift::`                                                 NA
## `SubsectionName.fctrmyMisc::Travel`                                                                   NA
## `SubsectionName.fctrOpEd::OpEd`                                                                       NA
## `SubsectionName.fctrmyMisc::Crosswords/Games`                                                  -19326208
## `SubsectionName.fctrmyMisc::Arts`                                                                     NA
## `SubsectionName.fctrmyMisc::Technology`                                                               NA
## `SubsectionName.fctrmyMisc::N.Y. / Region`                                                            NA
## `SubsectionName.fctrNew York Today::N.Y. / Region`                                                    NA
## H.num.chars.log                                                                                 56789472
## H.num.words.log                                                                                  1816195
## H.num.words.unq.log                                                                            -20769875
## A.num.words.log                                                                                -82968097
## A.num.chars.log                                                                                -90686546
## S.num.chars.log                                                                                 92193330
## A.num.words.unq.log                                                                             89109935
## S.num.words.log                                                                                 88344256
## S.num.words.unq.log                                                                            -99709813
##                                                                                               Pr(>|z|)
## (Intercept)                                                                                     <2e-16
## WordCount.log                                                                                   <2e-16
## PubDate.hour                                                                                    <2e-16
## PubDate.apm.fctrpm                                                                              <2e-16
## A.can                                                                                           <2e-16
## S.can                                                                                           <2e-16
## H.has.ebola                                                                                     <2e-16
## S.make                                                                                          <2e-16
## S.one                                                                                           <2e-16
## S.state                                                                                         <2e-16
## A.one                                                                                           <2e-16
## S.said                                                                                          <2e-16
## .rnorm                                                                                          <2e-16
## `PubDate.date.fctr(7,13]`                                                                       <2e-16
## `PubDate.date.fctr(13,19]`                                                                      <2e-16
## `PubDate.date.fctr(19,25]`                                                                      <2e-16
## `PubDate.date.fctr(25,31]`                                                                      <2e-16
## PubDate.second                                                                                  <2e-16
## S.presid                                                                                        <2e-16
## A.presid                                                                                            NA
## S.take                                                                                          <2e-16
## A.take                                                                                          <2e-16
## PubDate.minute                                                                                  <2e-16
## S.new                                                                                           <2e-16
## A.new                                                                                           <2e-16
## PubDate.wkday.fctr1                                                                             <2e-16
## PubDate.wkday.fctr2                                                                             <2e-16
## PubDate.wkday.fctr3                                                                             <2e-16
## PubDate.wkday.fctr4                                                                             <2e-16
## PubDate.wkday.fctr5                                                                             <2e-16
## PubDate.wkday.fctr6                                                                             <2e-16
## S.day                                                                                           <2e-16
## A.day                                                                                           <2e-16
## H.X2014                                                                                         <2e-16
## S.show                                                                                          <2e-16
## S.report                                                                                        <2e-16
## S.share                                                                                         <2e-16
## S.year                                                                                          <2e-16
## S.compani                                                                                       <2e-16
## A.compani                                                                                           NA
## H.new                                                                                           <2e-16
## S.first                                                                                         <2e-16
## S.time                                                                                          <2e-16
## A.time                                                                                          <2e-16
## H.newyork                                                                                       <2e-16
## S.articl                                                                                        <2e-16
## A.articl                                                                                            NA
## S.will                                                                                          <2e-16
## A.will                                                                                          <2e-16
## H.day                                                                                           <2e-16
## S.newyork                                                                                       <2e-16
## H.today                                                                                         <2e-16
## H.report                                                                                        <2e-16
## S.intern                                                                                        <2e-16
## A.intern                                                                                            NA
## H.week                                                                                          <2e-16
## H.fashion                                                                                       <2e-16
## S.week                                                                                          <2e-16
## S.fashion                                                                                       <2e-16
## `Headline.pfx.fctr19[0-9][0-9]::`                                                               <2e-16
## `Headline.pfx.fctrDaily (Clip )*Report::`                                                       <2e-16
## `Headline.pfx.fctr.*Fashion Week::`                                                             <2e-16
## `Headline.pfx.fctrWhat We're::`                                                                 <2e-16
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                                               <2e-16
## `Headline.pfx.fctrToday in (Politics|Small Business)::`                                         <2e-16
## `Headline.pfx.fctrMorning Agenda::`                                                             <2e-16
## `Headline.pfx.fctrNew York Today::`                                                             <2e-16
## `Headline.pfx.fctr6 Q's About the News::`                                                       <2e-16
## `Headline.pfx.fctrTest Yourself::`                                                              <2e-16
## `Headline.pfx.fctrWord of the Day::`                                                            <2e-16
## `Headline.pfx.fctrVerbatim::`                                                                   <2e-16
## `Headline.pfx.fctrFirst Draft::`                                                                <2e-16
## `Headline.pfx.fctrThe Daily Gift::`                                                             <2e-16
## NewsDesk.fctrCulture                                                                            <2e-16
## NewsDesk.fctrScience                                                                            <2e-16
## NewsDesk.fctrOpEd                                                                               <2e-16
## `NewsDesk.fctrmyMisc::`                                                                         <2e-16
## NewsDesk.fctrForeign                                                                            <2e-16
## NewsDesk.fctrStyles                                                                             <2e-16
## NewsDesk.fctrTStyle                                                                             <2e-16
## NewsDesk.fctrMagazine                                                                           <2e-16
## `NewsDesk.fctrWhat We're::`                                                                     <2e-16
## `NewsDesk.fctrPictures of the (Day|Year|.)::`                                                       NA
## NewsDesk.fctrTravel                                                                             <2e-16
## NewsDesk.fctrMetro                                                                              <2e-16
## `NewsDesk.fctrDaily (Clip )*Report::`                                                           <2e-16
## `NewsDesk.fctr6 Q's About the News::`                                                               NA
## `NewsDesk.fctrTest Yourself::`                                                                      NA
## `NewsDesk.fctrWord of the Day::`                                                                    NA
## `NewsDesk.fctr.*Fashion Week::`                                                                 <2e-16
## `NewsDesk.fctr19[0-9][0-9]::`                                                                   <2e-16
## NewsDesk.fctrNational                                                                           <2e-16
## NewsDesk.fctrSports                                                                             <2e-16
## `NewsDesk.fctrVerbatim::`                                                                           NA
## `NewsDesk.fctrFirst Draft::`                                                                        NA
## `NewsDesk.fctrToday in (Politics|Small Business)::`                                             <2e-16
## `NewsDesk.fctrThe Daily Gift::`                                                                     NA
## `NewsDesk.fctrNew York Today::`                                                                     NA
## SectionName.fctrArts                                                                                NA
## `SectionName.fctrBusiness Day`                                                                  <2e-16
## SectionName.fctrHealth                                                                          <2e-16
## SectionName.fctrOpinion                                                                         <2e-16
## SectionName.fctrForeign                                                                         <2e-16
## SectionName.fctrStyle                                                                           <2e-16
## SectionName.fctrTStyle                                                                              NA
## SectionName.fctrWorld                                                                               NA
## SectionName.fctrTechnology                                                                      <2e-16
## SectionName.fctrMagazine                                                                            NA
## SectionName.fctrMultimedia                                                                      <2e-16
## `SectionName.fctrmyMisc::`                                                                      <2e-16
## `SectionName.fctrWhat We're::`                                                                      NA
## SectionName.fctrTravel                                                                              NA
## SectionName.fctrU.S.                                                                            <2e-16
## `SectionName.fctrN.Y. / Region`                                                                     NA
## `SectionName.fctrDaily (Clip )*Report::`                                                            NA
## SectionName.fctrStyles                                                                          <2e-16
## SectionName.fctrOpen                                                                            <2e-16
## `SectionName.fctr.*Fashion Week::`                                                                  NA
## `SectionName.fctr19[0-9][0-9]::`                                                                    NA
## SectionName.fctrSports                                                                              NA
## SectionName.fctrNational                                                                        <2e-16
## `SectionName.fctrVerbatim::`                                                                        NA
## `SectionName.fctrFirst Draft::`                                                                     NA
## `SectionName.fctrToday in (Politics|Small Business)::`                                              NA
## SectionName.fctrScience                                                                         <2e-16
## SectionName.fctrCulture                                                                             NA
## SectionName.fctrBusiness                                                                        <2e-16
## `SectionName.fctrThe Daily Gift::`                                                                  NA
## SectionName.fctrOpEd                                                                                NA
## `SubsectionName.fctrCulture::Arts`                                                                  NA
## SubsectionName.fctrDealbook                                                                     <2e-16
## `SubsectionName.fctrScience::Health`                                                                NA
## `SubsectionName.fctrOpEd::Opinion`                                                                  NA
## `SubsectionName.fctrRoom For Debate`                                                            <2e-16
## `SubsectionName.fctrForeign::Foreign`                                                               NA
## `SubsectionName.fctrFashion & Style`                                                                NA
## `SubsectionName.fctrTStyle::TStyle`                                                                 NA
## `SubsectionName.fctrAsia Pacific`                                                               <2e-16
## `SubsectionName.fctrBusiness::Technology`                                                           NA
## `SubsectionName.fctrMagazine::Magazine`                                                             NA
## `SubsectionName.fctrmyMisc::Multimedia`                                                             NA
## `SubsectionName.fctrmyMisc::myMisc::`                                                               NA
## `SubsectionName.fctrWhat We're::What We're::`                                                       NA
## `SubsectionName.fctrPictures of the (Day|Year|.)::Multimedia`                                       NA
## `SubsectionName.fctrTravel::Travel`                                                                 NA
## SubsectionName.fctrEducation                                                                    <2e-16
## `SubsectionName.fctrThe Public Editor`                                                          <2e-16
## `SubsectionName.fctrStyles::U.S.`                                                                   NA
## `SubsectionName.fctrSmall Business`                                                                 NA
## `SubsectionName.fctrMetro::N.Y. / Region`                                                           NA
## `SubsectionName.fctrDaily (Clip )*Report::Daily (Clip )*Report::`                                   NA
## `SubsectionName.fctrStyles::Styles`                                                                 NA
## `SubsectionName.fctrmyMisc::Open`                                                                   NA
## `SubsectionName.fctr.*Fashion Week::.*Fashion Week::`                                               NA
## `SubsectionName.fctrForeign::World`                                                                 NA
## `SubsectionName.fctr19[0-9][0-9]::19[0-9][0-9]::`                                                   NA
## SubsectionName.fctrPolitics                                                                         NA
## `SubsectionName.fctrSports::Sports`                                                                 NA
## `SubsectionName.fctrmyMisc::Opinion`                                                                NA
## `SubsectionName.fctrNational::National`                                                             NA
## `SubsectionName.fctrVerbatim::Verbatim::`                                                           NA
## `SubsectionName.fctrFirst Draft::First Draft::`                                                     NA
## `SubsectionName.fctrToday in (Politics|Small Business)::Today in (Politics|Small Business)::`       NA
## `SubsectionName.fctrScience::Science`                                                               NA
## `SubsectionName.fctrCulture::Culture`                                                               NA
## `SubsectionName.fctrStyles::Health`                                                                 NA
## `SubsectionName.fctrBusiness::Business`                                                             NA
## `SubsectionName.fctrmyMisc::U.S.`                                                                   NA
## `SubsectionName.fctrmyMisc::Health`                                                                 NA
## `SubsectionName.fctrThe Daily Gift::The Daily Gift::`                                               NA
## `SubsectionName.fctrmyMisc::Travel`                                                                 NA
## `SubsectionName.fctrOpEd::OpEd`                                                                     NA
## `SubsectionName.fctrmyMisc::Crosswords/Games`                                                   <2e-16
## `SubsectionName.fctrmyMisc::Arts`                                                                   NA
## `SubsectionName.fctrmyMisc::Technology`                                                             NA
## `SubsectionName.fctrmyMisc::N.Y. / Region`                                                          NA
## `SubsectionName.fctrNew York Today::N.Y. / Region`                                                  NA
## H.num.chars.log                                                                                 <2e-16
## H.num.words.log                                                                                 <2e-16
## H.num.words.unq.log                                                                             <2e-16
## A.num.words.log                                                                                 <2e-16
## A.num.chars.log                                                                                 <2e-16
## S.num.chars.log                                                                                 <2e-16
## A.num.words.unq.log                                                                             <2e-16
## S.num.words.log                                                                                 <2e-16
## S.num.words.unq.log                                                                             <2e-16
##                                                                                                  
## (Intercept)                                                                                   ***
## WordCount.log                                                                                 ***
## PubDate.hour                                                                                  ***
## PubDate.apm.fctrpm                                                                            ***
## A.can                                                                                         ***
## S.can                                                                                         ***
## H.has.ebola                                                                                   ***
## S.make                                                                                        ***
## S.one                                                                                         ***
## S.state                                                                                       ***
## A.one                                                                                         ***
## S.said                                                                                        ***
## .rnorm                                                                                        ***
## `PubDate.date.fctr(7,13]`                                                                     ***
## `PubDate.date.fctr(13,19]`                                                                    ***
## `PubDate.date.fctr(19,25]`                                                                    ***
## `PubDate.date.fctr(25,31]`                                                                    ***
## PubDate.second                                                                                ***
## S.presid                                                                                      ***
## A.presid                                                                                         
## S.take                                                                                        ***
## A.take                                                                                        ***
## PubDate.minute                                                                                ***
## S.new                                                                                         ***
## A.new                                                                                         ***
## PubDate.wkday.fctr1                                                                           ***
## PubDate.wkday.fctr2                                                                           ***
## PubDate.wkday.fctr3                                                                           ***
## PubDate.wkday.fctr4                                                                           ***
## PubDate.wkday.fctr5                                                                           ***
## PubDate.wkday.fctr6                                                                           ***
## S.day                                                                                         ***
## A.day                                                                                         ***
## H.X2014                                                                                       ***
## S.show                                                                                        ***
## S.report                                                                                      ***
## S.share                                                                                       ***
## S.year                                                                                        ***
## S.compani                                                                                     ***
## A.compani                                                                                        
## H.new                                                                                         ***
## S.first                                                                                       ***
## S.time                                                                                        ***
## A.time                                                                                        ***
## H.newyork                                                                                     ***
## S.articl                                                                                      ***
## A.articl                                                                                         
## S.will                                                                                        ***
## A.will                                                                                        ***
## H.day                                                                                         ***
## S.newyork                                                                                     ***
## H.today                                                                                       ***
## H.report                                                                                      ***
## S.intern                                                                                      ***
## A.intern                                                                                         
## H.week                                                                                        ***
## H.fashion                                                                                     ***
## S.week                                                                                        ***
## S.fashion                                                                                     ***
## `Headline.pfx.fctr19[0-9][0-9]::`                                                             ***
## `Headline.pfx.fctrDaily (Clip )*Report::`                                                     ***
## `Headline.pfx.fctr.*Fashion Week::`                                                           ***
## `Headline.pfx.fctrWhat We're::`                                                               ***
## `Headline.pfx.fctrPictures of the (Day|Year|.)::`                                             ***
## `Headline.pfx.fctrToday in (Politics|Small Business)::`                                       ***
## `Headline.pfx.fctrMorning Agenda::`                                                           ***
## `Headline.pfx.fctrNew York Today::`                                                           ***
## `Headline.pfx.fctr6 Q's About the News::`                                                     ***
## `Headline.pfx.fctrTest Yourself::`                                                            ***
## `Headline.pfx.fctrWord of the Day::`                                                          ***
## `Headline.pfx.fctrVerbatim::`                                                                 ***
## `Headline.pfx.fctrFirst Draft::`                                                              ***
## `Headline.pfx.fctrThe Daily Gift::`                                                           ***
## NewsDesk.fctrCulture                                                                          ***
## NewsDesk.fctrScience                                                                          ***
## NewsDesk.fctrOpEd                                                                             ***
## `NewsDesk.fctrmyMisc::`                                                                       ***
## NewsDesk.fctrForeign                                                                          ***
## NewsDesk.fctrStyles                                                                           ***
## NewsDesk.fctrTStyle                                                                           ***
## NewsDesk.fctrMagazine                                                                         ***
## `NewsDesk.fctrWhat We're::`                                                                   ***
## `NewsDesk.fctrPictures of the (Day|Year|.)::`                                                    
## NewsDesk.fctrTravel                                                                           ***
## NewsDesk.fctrMetro                                                                            ***
## `NewsDesk.fctrDaily (Clip )*Report::`                                                         ***
## `NewsDesk.fctr6 Q's About the News::`                                                            
## `NewsDesk.fctrTest Yourself::`                                                                   
## `NewsDesk.fctrWord of the Day::`                                                                 
## `NewsDesk.fctr.*Fashion Week::`                                                               ***
## `NewsDesk.fctr19[0-9][0-9]::`                                                                 ***
## NewsDesk.fctrNational                                                                         ***
## NewsDesk.fctrSports                                                                           ***
## `NewsDesk.fctrVerbatim::`                                                                        
## `NewsDesk.fctrFirst Draft::`                                                                     
## `NewsDesk.fctrToday in (Politics|Small Business)::`                                           ***
## `NewsDesk.fctrThe Daily Gift::`                                                                  
## `NewsDesk.fctrNew York Today::`                                                                  
## SectionName.fctrArts                                                                             
## `SectionName.fctrBusiness Day`                                                                ***
## SectionName.fctrHealth                                                                        ***
## SectionName.fctrOpinion                                                                       ***
## SectionName.fctrForeign                                                                       ***
## SectionName.fctrStyle                                                                         ***
## SectionName.fctrTStyle                                                                           
## SectionName.fctrWorld                                                                            
## SectionName.fctrTechnology                                                                    ***
## SectionName.fctrMagazine                                                                         
## SectionName.fctrMultimedia                                                                    ***
## `SectionName.fctrmyMisc::`                                                                    ***
## `SectionName.fctrWhat We're::`                                                                   
## SectionName.fctrTravel                                                                           
## SectionName.fctrU.S.                                                                          ***
## `SectionName.fctrN.Y. / Region`                                                                  
## `SectionName.fctrDaily (Clip )*Report::`                                                         
## SectionName.fctrStyles                                                                        ***
## SectionName.fctrOpen                                                                          ***
## `SectionName.fctr.*Fashion Week::`                                                               
## `SectionName.fctr19[0-9][0-9]::`                                                                 
## SectionName.fctrSports                                                                           
## SectionName.fctrNational                                                                      ***
## `SectionName.fctrVerbatim::`                                                                     
## `SectionName.fctrFirst Draft::`                                                                  
## `SectionName.fctrToday in (Politics|Small Business)::`                                           
## SectionName.fctrScience                                                                       ***
## SectionName.fctrCulture                                                                          
## SectionName.fctrBusiness                                                                      ***
## `SectionName.fctrThe Daily Gift::`                                                               
## SectionName.fctrOpEd                                                                             
## `SubsectionName.fctrCulture::Arts`                                                               
## SubsectionName.fctrDealbook                                                                   ***
## `SubsectionName.fctrScience::Health`                                                             
## `SubsectionName.fctrOpEd::Opinion`                                                               
## `SubsectionName.fctrRoom For Debate`                                                          ***
## `SubsectionName.fctrForeign::Foreign`                                                            
## `SubsectionName.fctrFashion & Style`                                                             
## `SubsectionName.fctrTStyle::TStyle`                                                              
## `SubsectionName.fctrAsia Pacific`                                                             ***
## `SubsectionName.fctrBusiness::Technology`                                                        
## `SubsectionName.fctrMagazine::Magazine`                                                          
## `SubsectionName.fctrmyMisc::Multimedia`                                                          
## `SubsectionName.fctrmyMisc::myMisc::`                                                            
## `SubsectionName.fctrWhat We're::What We're::`                                                    
## `SubsectionName.fctrPictures of the (Day|Year|.)::Multimedia`                                    
## `SubsectionName.fctrTravel::Travel`                                                              
## SubsectionName.fctrEducation                                                                  ***
## `SubsectionName.fctrThe Public Editor`                                                        ***
## `SubsectionName.fctrStyles::U.S.`                                                                
## `SubsectionName.fctrSmall Business`                                                              
## `SubsectionName.fctrMetro::N.Y. / Region`                                                        
## `SubsectionName.fctrDaily (Clip )*Report::Daily (Clip )*Report::`                                
## `SubsectionName.fctrStyles::Styles`                                                              
## `SubsectionName.fctrmyMisc::Open`                                                                
## `SubsectionName.fctr.*Fashion Week::.*Fashion Week::`                                            
## `SubsectionName.fctrForeign::World`                                                              
## `SubsectionName.fctr19[0-9][0-9]::19[0-9][0-9]::`                                                
## SubsectionName.fctrPolitics                                                                      
## `SubsectionName.fctrSports::Sports`                                                              
## `SubsectionName.fctrmyMisc::Opinion`                                                             
## `SubsectionName.fctrNational::National`                                                          
## `SubsectionName.fctrVerbatim::Verbatim::`                                                        
## `SubsectionName.fctrFirst Draft::First Draft::`                                                  
## `SubsectionName.fctrToday in (Politics|Small Business)::Today in (Politics|Small Business)::`    
## `SubsectionName.fctrScience::Science`                                                            
## `SubsectionName.fctrCulture::Culture`                                                            
## `SubsectionName.fctrStyles::Health`                                                              
## `SubsectionName.fctrBusiness::Business`                                                          
## `SubsectionName.fctrmyMisc::U.S.`                                                                
## `SubsectionName.fctrmyMisc::Health`                                                              
## `SubsectionName.fctrThe Daily Gift::The Daily Gift::`                                            
## `SubsectionName.fctrmyMisc::Travel`                                                              
## `SubsectionName.fctrOpEd::OpEd`                                                                  
## `SubsectionName.fctrmyMisc::Crosswords/Games`                                                 ***
## `SubsectionName.fctrmyMisc::Arts`                                                                
## `SubsectionName.fctrmyMisc::Technology`                                                          
## `SubsectionName.fctrmyMisc::N.Y. / Region`                                                       
## `SubsectionName.fctrNew York Today::N.Y. / Region`                                               
## H.num.chars.log                                                                               ***
## H.num.words.log                                                                               ***
## H.num.words.unq.log                                                                           ***
## A.num.words.log                                                                               ***
## A.num.chars.log                                                                               ***
## S.num.chars.log                                                                               ***
## A.num.words.unq.log                                                                           ***
## S.num.words.log                                                                               ***
## S.num.words.unq.log                                                                           ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance:  4042.7  on 4474  degrees of freedom
## Residual deviance: 31213.8  on 4360  degrees of freedom
## AIC: 31444
## 
## Number of Fisher Scoring iterations: 25
## 
## [1] "    calling mypredict_mdl for fit:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-4.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-5.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.6982578
## 3        0.2 0.6982578
## 4        0.3 0.6982578
## 5        0.4 0.6982578
## 6        0.5 0.6982578
## 7        0.6 0.6982578
## 8        0.7 0.6982578
## 9        0.8 0.6982578
## 10       0.9 0.6982578
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     3541
## 2            Y                                      248
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                      185
## 2                                      501
##          Prediction
## Reference    N    Y
##         N 3541  185
##         Y  248  501
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.032402e-01   6.407720e-01   8.942018e-01   9.117484e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   6.115344e-42   2.886911e-03 
## [1] "    calling mypredict_mdl for OOB:"
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-6.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-7.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.7048458
## 3        0.2 0.7048458
## 4        0.3 0.7048458
## 5        0.4 0.7048458
## 6        0.5 0.7048458
## 7        0.6 0.7048458
## 8        0.7 0.7048458
## 9        0.8 0.7048458
## 10       0.9 0.7048458
## 11       1.0 0.0000000
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-8.png) 

```
## [1] "Classifier Probability Threshold: 0.9000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     1616
## 2            Y                                      104
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                       97
## 2                                      240
##          Prediction
## Reference    N    Y
##         N 1616   97
##         Y  104  240
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.022849e-01   6.463039e-01   8.886309e-01   9.147795e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   1.211631e-19   6.721440e-01 
##            model_id model_method
## 1 Conditional.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          feats
## 1 WordCount.log, PubDate.hour, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, .rnorm, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, A.compani, H.new, S.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, SubsectionName.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.words.log, A.num.chars.log, S.num.chars.log, A.num.words.unq.log, S.num.words.log, S.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                     25.684                 8.114
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8096204                    0.9       0.6982578        0.8229914
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8942018             0.9117484     0.4214281   0.8205243
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.9       0.7048458        0.9022849
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.8886309             0.9147795     0.6463039     31443.8
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.09862369       0.2716384
## [1] "fitting model: Conditional.X.rpart"
## [1] "    indep_vars: WordCount.log, PubDate.hour, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, A.compani, H.new, S.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, SubsectionName.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.words.log, A.num.chars.log, S.num.chars.log, A.num.words.unq.log, S.num.words.log, S.num.words.unq.log"
## + Fold1: cp=0.02804 
## - Fold1: cp=0.02804 
## + Fold2: cp=0.02804 
## - Fold2: cp=0.02804 
## + Fold3: cp=0.02804 
## - Fold3: cp=0.02804 
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.028 on full training set
```

```
## Warning in myfit_mdl(model_id = paste0(model_id_pfx, ""), model_method =
## method, : model's bestTune found at an extreme of tuneGrid for parameter:
## cp
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-9.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-10.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 4475 
## 
##           CP nsplit rel error
## 1 0.25901202      0 1.0000000
## 2 0.03938585      1 0.7409880
## 3 0.02803738      3 0.6622163
## 
## Variable importance
##                  NewsDesk.fctrOpEd   SubsectionName.fctrOpEd::Opinion 
##                                 24                                 24 
##            SectionName.fctrOpinion                      WordCount.log 
##                                 20                                  9 
##             SectionName.fctrHealth               NewsDesk.fctrScience 
##                                  5                                  5 
## SubsectionName.fctrScience::Health                    A.num.chars.log 
##                                  5                                  3 
##                    S.num.chars.log                S.num.words.unq.log 
##                                  3                                  3 
## 
## Node number 1: 4475 observations,    complexity param=0.259012
##   predicted class=N  expected loss=0.1673743  P(node) =1
##     class counts:  3726   749
##    probabilities: 0.833 0.167 
##   left son=2 (4131 obs) right son=3 (344 obs)
##   Primary splits:
##       NewsDesk.fctrOpEd                < 0.5      to the left,  improve=281.52360, (0 missing)
##       SubsectionName.fctrOpEd::Opinion < 0.5      to the left,  improve=281.52360, (0 missing)
##       SectionName.fctrOpinion          < 0.5      to the left,  improve=244.96950, (0 missing)
##       WordCount.log                    < 6.528688 to the left,  improve=109.59970, (0 missing)
##       A.num.chars.log                  < 3.795426 to the right, improve= 95.79591, (0 missing)
##   Surrogate splits:
##       SubsectionName.fctrOpEd::Opinion < 0.5      to the left,  agree=1.000, adj=1.000, (0 split)
##       SectionName.fctrOpinion          < 0.5      to the left,  agree=0.986, adj=0.817, (0 split)
##       A.num.chars.log                  < 3.725621 to the right, agree=0.933, adj=0.134, (0 split)
##       S.num.chars.log                  < 3.725621 to the right, agree=0.933, adj=0.134, (0 split)
##       S.num.words.unq.log              < 1.497866 to the right, agree=0.933, adj=0.122, (0 split)
## 
## Node number 2: 4131 observations,    complexity param=0.03938585
##   predicted class=N  expected loss=0.1161946  P(node) =0.9231285
##     class counts:  3651   480
##    probabilities: 0.884 0.116 
##   left son=4 (3051 obs) right son=5 (1080 obs)
##   Primary splits:
##       WordCount.log                      < 6.528688 to the left,  improve=102.82800, (0 missing)
##       NewsDesk.fctrScience               < 0.5      to the left,  improve= 70.50963, (0 missing)
##       SectionName.fctrHealth             < 0.5      to the left,  improve= 69.73481, (0 missing)
##       SubsectionName.fctrScience::Health < 0.5      to the left,  improve= 69.16064, (0 missing)
##       SubsectionName.fctrStyles::U.S.    < 0.5      to the left,  improve= 58.96641, (0 missing)
##   Surrogate splits:
##       SectionName.fctrOpinion                              < 0.5      to the left,  agree=0.751, adj=0.047, (0 split)
##       SubsectionName.fctrRoom For Debate                   < 0.5      to the left,  agree=0.749, adj=0.041, (0 split)
##       Headline.pfx.fctrNew York Today::                    < 0.5      to the left,  agree=0.748, adj=0.037, (0 split)
##       NewsDesk.fctrToday in (Politics|Small Business)::    < 0.5      to the left,  agree=0.745, adj=0.026, (0 split)
##       SectionName.fctrToday in (Politics|Small Business):: < 0.5      to the left,  agree=0.745, adj=0.026, (0 split)
## 
## Node number 3: 344 observations
##   predicted class=Y  expected loss=0.2180233  P(node) =0.07687151
##     class counts:    75   269
##    probabilities: 0.218 0.782 
## 
## Node number 4: 3051 observations
##   predicted class=N  expected loss=0.04981973  P(node) =0.6817877
##     class counts:  2899   152
##    probabilities: 0.950 0.050 
## 
## Node number 5: 1080 observations,    complexity param=0.03938585
##   predicted class=N  expected loss=0.3037037  P(node) =0.2413408
##     class counts:   752   328
##    probabilities: 0.696 0.304 
##   left son=10 (1009 obs) right son=11 (71 obs)
##   Primary splits:
##       SectionName.fctrHealth             < 0.5      to the left,  improve=56.88852, (0 missing)
##       NewsDesk.fctrScience               < 0.5      to the left,  improve=55.81082, (0 missing)
##       SubsectionName.fctrScience::Health < 0.5      to the left,  improve=55.81082, (0 missing)
##       PubDate.hour                       < 7.5      to the left,  improve=18.31450, (0 missing)
##       SubsectionName.fctrStyles::U.S.    < 0.5      to the left,  improve=17.71194, (0 missing)
##   Surrogate splits:
##       NewsDesk.fctrScience               < 0.5      to the left,  agree=0.999, adj=0.986, (0 split)
##       SubsectionName.fctrScience::Health < 0.5      to the left,  agree=0.999, adj=0.986, (0 split)
##       PubDate.hour                       < 0.5      to the right, agree=0.940, adj=0.085, (0 split)
## 
## Node number 10: 1009 observations
##   predicted class=N  expected loss=0.2606541  P(node) =0.2254749
##     class counts:   746   263
##    probabilities: 0.739 0.261 
## 
## Node number 11: 71 observations
##   predicted class=Y  expected loss=0.08450704  P(node) =0.01586592
##     class counts:     6    65
##    probabilities: 0.085 0.915 
## 
## n= 4475 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##  1) root 4475 749 N (0.83262570 0.16737430)  
##    2) NewsDesk.fctrOpEd< 0.5 4131 480 N (0.88380537 0.11619463)  
##      4) WordCount.log< 6.528688 3051 152 N (0.95018027 0.04981973) *
##      5) WordCount.log>=6.528688 1080 328 N (0.69629630 0.30370370)  
##       10) SectionName.fctrHealth< 0.5 1009 263 N (0.73934589 0.26065411) *
##       11) SectionName.fctrHealth>=0.5 71   6 Y (0.08450704 0.91549296) *
##    3) NewsDesk.fctrOpEd>=0.5 344  75 Y (0.21802326 0.78197674) *
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-11.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.5494708
## 3        0.2 0.5494708
## 4        0.3 0.5738832
## 5        0.4 0.5738832
## 6        0.5 0.5738832
## 7        0.6 0.5738832
## 8        0.7 0.5738832
## 9        0.8 0.1585366
## 10       0.9 0.1585366
## 11       1.0 0.0000000
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-12.png) 

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       3645
## 2            Y                                        415
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                         81
## 2                                        334
##          Prediction
## Reference    N    Y
##         N 3645   81
##         Y  415  334
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.891620e-01   5.161350e-01   8.795973e-01   8.982154e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   9.017296e-27   1.508203e-50 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-13.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.5299313
## 3        0.2 0.5299313
## 4        0.3 0.6014493
## 5        0.4 0.6014493
## 6        0.5 0.6014493
## 7        0.6 0.6014493
## 8        0.7 0.6014493
## 9        0.8 0.1440000
## 10       0.9 0.1440000
## 11       1.0 0.0000000
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-14.png) 

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       1671
## 2            Y                                        178
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                         42
## 2                                        166
##          Prediction
## Reference    N    Y
##         N 1671   42
##         Y  178  166
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.930481e-01   5.439760e-01   8.788850e-01   9.060795e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   6.262798e-15   8.893031e-20 
##              model_id model_method
## 1 Conditional.X.rpart        rpart
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  feats
## 1 WordCount.log, PubDate.hour, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, A.compani, H.new, S.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, SubsectionName.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.words.log, A.num.chars.log, S.num.chars.log, A.num.words.unq.log, S.num.words.log, S.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                     11.254                 2.053
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8289627                    0.7       0.5738832        0.8909486
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8795973             0.8982154     0.5397107   0.8220499
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.7       0.6014493        0.8930481
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1              0.878885             0.9060795      0.543976
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.004747551      0.03340874
## [1] "fitting model: Conditional.X.cp.0.rpart"
## [1] "    indep_vars: WordCount.log, PubDate.hour, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, A.compani, H.new, S.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, SubsectionName.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.words.log, A.num.chars.log, S.num.chars.log, A.num.words.unq.log, S.num.words.log, S.num.words.unq.log"
## Fitting cp = 0 on full training set
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-15.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 4475 
## 
##              CP nsplit rel error
## 1  0.2590120160      0 1.0000000
## 2  0.0393858478      1 0.7409880
## 3  0.0280373832      3 0.6622163
## 4  0.0267022697      4 0.6341789
## 5  0.0133511348      5 0.6074766
## 6  0.0115709835      6 0.5941255
## 7  0.0093457944      9 0.5594126
## 8  0.0080106809     10 0.5500668
## 9  0.0066755674     12 0.5340454
## 10 0.0062305296     16 0.5073431
## 11 0.0031152648     19 0.4886515
## 12 0.0030040053     27 0.4566088
## 13 0.0020026702     31 0.4445928
## 14 0.0015020027     42 0.4165554
## 15 0.0013351135     50 0.4045394
## 16 0.0004450378     63 0.3858478
## 17 0.0003337784     66 0.3845127
## 18 0.0000000000     70 0.3831776
## 
## Variable importance
##                    NewsDesk.fctrOpEd     SubsectionName.fctrOpEd::Opinion 
##                                   14                                   14 
##              SectionName.fctrOpinion                        WordCount.log 
##                                   12                                    7 
##                      A.num.chars.log                      S.num.chars.log 
##                                    5                                    5 
##                         PubDate.hour                  S.num.words.unq.log 
##                                    4                                    4 
##               SectionName.fctrHealth                 NewsDesk.fctrScience 
##                                    3                                    3 
##   SubsectionName.fctrScience::Health      SubsectionName.fctrStyles::U.S. 
##                                    3                                    3 
##                       PubDate.minute                      S.num.words.log 
##                                    2                                    2 
##                      A.num.words.log                  A.num.words.unq.log 
##                                    2                                    2 
##                      H.num.chars.log                      H.num.words.log 
##                                    2                                    1 
##                  H.num.words.unq.log                       PubDate.second 
##                                    1                                    1 
##                  NewsDesk.fctrStyles             SectionName.fctrmyMisc:: 
##                                    1                                    1 
##  SubsectionName.fctrmyMisc::myMisc::                 SectionName.fctrU.S. 
##                                    1                                    1 
##                  PubDate.wkday.fctr6 SubsectionName.fctrThe Public Editor 
##                                    1                                    1 
## 
## Node number 1: 4475 observations,    complexity param=0.259012
##   predicted class=N  expected loss=0.1673743  P(node) =1
##     class counts:  3726   749
##    probabilities: 0.833 0.167 
##   left son=2 (4131 obs) right son=3 (344 obs)
##   Primary splits:
##       NewsDesk.fctrOpEd                < 0.5      to the left,  improve=281.52360, (0 missing)
##       SubsectionName.fctrOpEd::Opinion < 0.5      to the left,  improve=281.52360, (0 missing)
##       SectionName.fctrOpinion          < 0.5      to the left,  improve=244.96950, (0 missing)
##       WordCount.log                    < 6.528688 to the left,  improve=109.59970, (0 missing)
##       A.num.chars.log                  < 3.795426 to the right, improve= 95.79591, (0 missing)
##   Surrogate splits:
##       SubsectionName.fctrOpEd::Opinion < 0.5      to the left,  agree=1.000, adj=1.000, (0 split)
##       SectionName.fctrOpinion          < 0.5      to the left,  agree=0.986, adj=0.817, (0 split)
##       A.num.chars.log                  < 3.725621 to the right, agree=0.933, adj=0.134, (0 split)
##       S.num.chars.log                  < 3.725621 to the right, agree=0.933, adj=0.134, (0 split)
##       S.num.words.unq.log              < 1.497866 to the right, agree=0.933, adj=0.122, (0 split)
## 
## Node number 2: 4131 observations,    complexity param=0.03938585
##   predicted class=N  expected loss=0.1161946  P(node) =0.9231285
##     class counts:  3651   480
##    probabilities: 0.884 0.116 
##   left son=4 (3051 obs) right son=5 (1080 obs)
##   Primary splits:
##       WordCount.log                      < 6.528688 to the left,  improve=102.82800, (0 missing)
##       NewsDesk.fctrScience               < 0.5      to the left,  improve= 70.50963, (0 missing)
##       SectionName.fctrHealth             < 0.5      to the left,  improve= 69.73481, (0 missing)
##       SubsectionName.fctrScience::Health < 0.5      to the left,  improve= 69.16064, (0 missing)
##       SubsectionName.fctrStyles::U.S.    < 0.5      to the left,  improve= 58.96641, (0 missing)
##   Surrogate splits:
##       SectionName.fctrOpinion                              < 0.5      to the left,  agree=0.751, adj=0.047, (0 split)
##       SubsectionName.fctrRoom For Debate                   < 0.5      to the left,  agree=0.749, adj=0.041, (0 split)
##       Headline.pfx.fctrNew York Today::                    < 0.5      to the left,  agree=0.748, adj=0.037, (0 split)
##       NewsDesk.fctrToday in (Politics|Small Business)::    < 0.5      to the left,  agree=0.745, adj=0.026, (0 split)
##       SectionName.fctrToday in (Politics|Small Business):: < 0.5      to the left,  agree=0.745, adj=0.026, (0 split)
## 
## Node number 3: 344 observations,    complexity param=0.008010681
##   predicted class=Y  expected loss=0.2180233  P(node) =0.07687151
##     class counts:    75   269
##    probabilities: 0.218 0.782 
##   left son=6 (8 obs) right son=7 (336 obs)
##   Primary splits:
##       A.num.chars.log     < 5.138628 to the right, improve=7.070321, (0 missing)
##       S.num.chars.log     < 5.138628 to the right, improve=7.070321, (0 missing)
##       A.num.words.log     < 2.802901 to the right, improve=5.005347, (0 missing)
##       A.num.words.unq.log < 2.802901 to the right, improve=5.005347, (0 missing)
##       S.num.words.log     < 2.802901 to the right, improve=5.005347, (0 missing)
##   Surrogate splits:
##       S.num.chars.log     < 5.138628 to the right, agree=1.000, adj=1.000, (0 split)
##       A.num.words.log     < 2.861793 to the right, agree=0.991, adj=0.625, (0 split)
##       A.num.words.unq.log < 2.861793 to the right, agree=0.991, adj=0.625, (0 split)
##       S.num.words.log     < 2.888826 to the right, agree=0.991, adj=0.625, (0 split)
##       S.num.words.unq.log < 2.888826 to the right, agree=0.991, adj=0.625, (0 split)
## 
## Node number 4: 3051 observations,    complexity param=0.01157098
##   predicted class=N  expected loss=0.04981973  P(node) =0.6817877
##     class counts:  2899   152
##    probabilities: 0.950 0.050 
##   left son=8 (2989 obs) right son=9 (62 obs)
##   Primary splits:
##       SubsectionName.fctrStyles::U.S. < 0.5      to the left,  improve=27.52239, (0 missing)
##       PubDate.hour                    < 21.5     to the left,  improve=23.86280, (0 missing)
##       NewsDesk.fctrStyles             < 0.5      to the left,  improve=11.21036, (0 missing)
##       A.num.chars.log                 < 3.817652 to the right, improve=10.73037, (0 missing)
##       S.num.chars.log                 < 3.817652 to the right, improve=10.73037, (0 missing)
##   Surrogate splits:
##       S.week < 1.5      to the left,  agree=0.982, adj=0.113, (0 split)
## 
## Node number 5: 1080 observations,    complexity param=0.03938585
##   predicted class=N  expected loss=0.3037037  P(node) =0.2413408
##     class counts:   752   328
##    probabilities: 0.696 0.304 
##   left son=10 (1009 obs) right son=11 (71 obs)
##   Primary splits:
##       SectionName.fctrHealth             < 0.5      to the left,  improve=56.88852, (0 missing)
##       NewsDesk.fctrScience               < 0.5      to the left,  improve=55.81082, (0 missing)
##       SubsectionName.fctrScience::Health < 0.5      to the left,  improve=55.81082, (0 missing)
##       PubDate.hour                       < 7.5      to the left,  improve=18.31450, (0 missing)
##       SubsectionName.fctrStyles::U.S.    < 0.5      to the left,  improve=17.71194, (0 missing)
##   Surrogate splits:
##       NewsDesk.fctrScience               < 0.5      to the left,  agree=0.999, adj=0.986, (0 split)
##       SubsectionName.fctrScience::Health < 0.5      to the left,  agree=0.999, adj=0.986, (0 split)
##       PubDate.hour                       < 0.5      to the right, agree=0.940, adj=0.085, (0 split)
## 
## Node number 6: 8 observations
##   predicted class=N  expected loss=0.125  P(node) =0.001787709
##     class counts:     7     1
##    probabilities: 0.875 0.125 
## 
## Node number 7: 336 observations,    complexity param=0.006675567
##   predicted class=Y  expected loss=0.202381  P(node) =0.0750838
##     class counts:    68   268
##    probabilities: 0.202 0.798 
##   left son=14 (67 obs) right son=15 (269 obs)
##   Primary splits:
##       PubDate.wkday.fctr5 < 0.5      to the right, improve=4.064272, (0 missing)
##       WordCount.log       < 4.68196  to the left,  improve=3.316861, (0 missing)
##       PubDate.hour        < 10.5     to the right, improve=2.041174, (0 missing)
##       S.one               < 0.5      to the right, improve=1.947315, (0 missing)
##       A.one               < 0.5      to the right, improve=1.947315, (0 missing)
##   Surrogate splits:
##       A.can < 0.5      to the right, agree=0.804, adj=0.015, (0 split)
##       S.can < 0.5      to the right, agree=0.804, adj=0.015, (0 split)
##       S.one < 0.5      to the right, agree=0.804, adj=0.015, (0 split)
##       A.one < 0.5      to the right, agree=0.804, adj=0.015, (0 split)
## 
## Node number 8: 2989 observations,    complexity param=0.01157098
##   predicted class=N  expected loss=0.04014721  P(node) =0.667933
##     class counts:  2869   120
##    probabilities: 0.960 0.040 
##   left son=16 (2911 obs) right son=17 (78 obs)
##   Primary splits:
##       PubDate.hour    < 21.5     to the left,  improve=25.087110, (0 missing)
##       A.num.chars.log < 3.817652 to the right, improve=11.326440, (0 missing)
##       S.num.chars.log < 3.817652 to the right, improve=11.326440, (0 missing)
##       H.num.chars.log < 2.861793 to the right, improve= 9.144133, (0 missing)
##       H.num.words.log < 1.242453 to the right, improve= 7.827486, (0 missing)
## 
## Node number 9: 62 observations,    complexity param=0.009345794
##   predicted class=Y  expected loss=0.483871  P(node) =0.01385475
##     class counts:    30    32
##    probabilities: 0.484 0.516 
##   left son=18 (47 obs) right son=19 (15 obs)
##   Primary splits:
##       PubDate.wkday.fctr1 < 0.5      to the left,  improve=3.189019, (0 missing)
##       PubDate.second      < 8.5      to the right, improve=2.925813, (0 missing)
##       H.num.chars.log     < 4.034201 to the left,  improve=2.669088, (0 missing)
##       PubDate.minute      < 51.5     to the left,  improve=2.365890, (0 missing)
##       H.num.words.log     < 1.868835 to the left,  improve=2.249904, (0 missing)
##   Surrogate splits:
##       S.show < 0.5      to the left,  agree=0.790, adj=0.133, (0 split)
##       S.make < 0.5      to the left,  agree=0.774, adj=0.067, (0 split)
##       S.new  < 0.5      to the left,  agree=0.774, adj=0.067, (0 split)
##       A.new  < 0.5      to the left,  agree=0.774, adj=0.067, (0 split)
## 
## Node number 10: 1009 observations,    complexity param=0.02803738
##   predicted class=N  expected loss=0.2606541  P(node) =0.2254749
##     class counts:   746   263
##    probabilities: 0.739 0.261 
##   left son=20 (944 obs) right son=21 (65 obs)
##   Primary splits:
##       SubsectionName.fctrStyles::U.S. < 0.5      to the left,  improve=22.33062, (0 missing)
##       PubDate.hour                    < 21.5     to the left,  improve=21.57735, (0 missing)
##       NewsDesk.fctrStyles             < 0.5      to the left,  improve=14.28041, (0 missing)
##       A.num.chars.log                 < 4.297262 to the right, improve=13.31900, (0 missing)
##       S.num.chars.log                 < 4.297262 to the right, improve=13.31900, (0 missing)
##   Surrogate splits:
##       NewsDesk.fctrStyles  < 0.5      to the left,  agree=0.987, adj=0.800, (0 split)
##       SectionName.fctrU.S. < 0.5      to the left,  agree=0.979, adj=0.677, (0 split)
## 
## Node number 11: 71 observations
##   predicted class=Y  expected loss=0.08450704  P(node) =0.01586592
##     class counts:     6    65
##    probabilities: 0.085 0.915 
## 
## Node number 14: 67 observations,    complexity param=0.006675567
##   predicted class=Y  expected loss=0.358209  P(node) =0.01497207
##     class counts:    24    43
##    probabilities: 0.358 0.642 
##   left son=28 (14 obs) right son=29 (53 obs)
##   Primary splits:
##       PubDate.hour       < 17.5     to the right, improve=8.811361, (0 missing)
##       WordCount.log      < 4.794936 to the left,  improve=4.487911, (0 missing)
##       PubDate.apm.fctrpm < 0.5      to the right, improve=2.743955, (0 missing)
##       A.num.chars.log    < 3.417592 to the left,  improve=1.978384, (0 missing)
##       S.num.chars.log    < 3.417592 to the left,  improve=1.978384, (0 missing)
##   Surrogate splits:
##       WordCount.log       < 4.794936 to the left,  agree=0.881, adj=0.429, (0 split)
##       A.num.words.log     < 1.242453 to the left,  agree=0.836, adj=0.214, (0 split)
##       A.num.words.unq.log < 1.242453 to the left,  agree=0.836, adj=0.214, (0 split)
##       S.num.words.log     < 1.242453 to the left,  agree=0.836, adj=0.214, (0 split)
##       S.num.words.unq.log < 1.242453 to the left,  agree=0.836, adj=0.214, (0 split)
## 
## Node number 15: 269 observations,    complexity param=0.001502003
##   predicted class=Y  expected loss=0.1635688  P(node) =0.06011173
##     class counts:    44   225
##    probabilities: 0.164 0.836 
##   left son=30 (216 obs) right son=31 (53 obs)
##   Primary splits:
##       A.num.chars.log < 3.676221 to the right, improve=2.090225, (0 missing)
##       S.num.chars.log < 3.676221 to the right, improve=2.090225, (0 missing)
##       H.num.chars.log < 4.197174 to the right, improve=1.715843, (0 missing)
##       PubDate.hour    < 10.5     to the right, improve=1.510385, (0 missing)
##       A.num.words.log < 1.242453 to the right, improve=1.410030, (0 missing)
##   Surrogate splits:
##       S.num.chars.log     < 3.676221 to the right, agree=1.000, adj=1.000, (0 split)
##       A.num.words.unq.log < 1.497866 to the right, agree=0.948, adj=0.736, (0 split)
##       S.num.words.unq.log < 1.497866 to the right, agree=0.948, adj=0.736, (0 split)
##       A.num.words.log     < 1.497866 to the right, agree=0.944, adj=0.717, (0 split)
##       S.num.words.log     < 1.497866 to the right, agree=0.944, adj=0.717, (0 split)
## 
## Node number 16: 2911 observations,    complexity param=0.003004005
##   predicted class=N  expected loss=0.02954311  P(node) =0.6505028
##     class counts:  2825    86
##    probabilities: 0.970 0.030 
##   left son=32 (2853 obs) right son=33 (58 obs)
##   Primary splits:
##       NewsDesk.fctrScience               < 0.5      to the left,  improve=8.221641, (0 missing)
##       SubsectionName.fctrScience::Health < 0.5      to the left,  improve=7.334818, (0 missing)
##       SectionName.fctrHealth             < 0.5      to the left,  improve=7.181152, (0 missing)
##       A.num.chars.log                    < 3.817652 to the right, improve=2.413903, (0 missing)
##       S.num.chars.log                    < 3.817652 to the right, improve=2.413903, (0 missing)
##   Surrogate splits:
##       SubsectionName.fctrScience::Health < 0.5      to the left,  agree=1.000, adj=0.983, (0 split)
##       SectionName.fctrHealth             < 0.5      to the left,  agree=0.999, adj=0.966, (0 split)
## 
## Node number 17: 78 observations,    complexity param=0.01157098
##   predicted class=N  expected loss=0.4358974  P(node) =0.01743017
##     class counts:    44    34
##    probabilities: 0.564 0.436 
##   left son=34 (44 obs) right son=35 (34 obs)
##   Primary splits:
##       PubDate.minute      < 0.5      to the right, improve=20.96593, (0 missing)
##       A.num.chars.log     < 4.151009 to the right, improve=16.76855, (0 missing)
##       S.num.chars.log     < 4.151009 to the right, improve=16.76855, (0 missing)
##       H.num.chars.log     < 3.065613 to the right, improve=16.02564, (0 missing)
##       A.num.words.unq.log < 2.249905 to the right, improve=15.57093, (0 missing)
##   Surrogate splits:
##       H.num.words.log     < 1.700599 to the right, agree=0.872, adj=0.706, (0 split)
##       H.num.words.unq.log < 1.700599 to the right, agree=0.872, adj=0.706, (0 split)
##       H.num.chars.log     < 3.569433 to the right, agree=0.859, adj=0.676, (0 split)
##       A.num.chars.log     < 4.151009 to the right, agree=0.782, adj=0.500, (0 split)
##       S.num.chars.log     < 4.151009 to the right, agree=0.782, adj=0.500, (0 split)
## 
## Node number 18: 47 observations,    complexity param=0.006675567
##   predicted class=N  expected loss=0.4255319  P(node) =0.01050279
##     class counts:    27    20
##    probabilities: 0.574 0.426 
##   left son=36 (40 obs) right son=37 (7 obs)
##   Primary splits:
##       PubDate.second  < 8.5      to the right, improve=3.064438, (0 missing)
##       A.num.words.log < 3.020127 to the right, improve=2.978723, (0 missing)
##       S.num.words.log < 3.020127 to the right, improve=2.978723, (0 missing)
##       WordCount.log   < 6.299797 to the left,  improve=1.873961, (0 missing)
##       H.num.chars.log < 4.042897 to the left,  improve=1.873961, (0 missing)
##   Surrogate splits:
##       PubDate.minute < 57.5     to the left,  agree=0.894, adj=0.286, (0 split)
##       S.year         < 0.5      to the left,  agree=0.872, adj=0.143, (0 split)
## 
## Node number 19: 15 observations
##   predicted class=Y  expected loss=0.2  P(node) =0.003351955
##     class counts:     3    12
##    probabilities: 0.200 0.800 
## 
## Node number 20: 944 observations,    complexity param=0.02670227
##   predicted class=N  expected loss=0.2330508  P(node) =0.2109497
##     class counts:   724   220
##    probabilities: 0.767 0.233 
##   left son=40 (878 obs) right son=41 (66 obs)
##   Primary splits:
##       PubDate.hour        < 21.5     to the left,  improve=24.85239, (0 missing)
##       A.num.chars.log     < 4.375737 to the right, improve=15.44054, (0 missing)
##       S.num.chars.log     < 4.375737 to the right, improve=15.44054, (0 missing)
##       A.num.words.unq.log < 2.138333 to the right, improve=13.60119, (0 missing)
##       S.num.words.unq.log < 2.138333 to the right, improve=13.26882, (0 missing)
##   Surrogate splits:
##       A.num.words.log     < 1.700599 to the right, agree=0.939, adj=0.121, (0 split)
##       A.num.chars.log     < 3.806416 to the right, agree=0.939, adj=0.121, (0 split)
##       S.num.chars.log     < 3.806416 to the right, agree=0.939, adj=0.121, (0 split)
##       A.num.words.unq.log < 1.700599 to the right, agree=0.939, adj=0.121, (0 split)
##       S.num.words.log     < 1.700599 to the right, agree=0.939, adj=0.121, (0 split)
## 
## Node number 21: 65 observations,    complexity param=0.008010681
##   predicted class=Y  expected loss=0.3384615  P(node) =0.01452514
##     class counts:    22    43
##    probabilities: 0.338 0.662 
##   left son=42 (14 obs) right son=43 (51 obs)
##   Primary splits:
##       PubDate.wkday.fctr2 < 0.5      to the right, improve=5.040465, (0 missing)
##       A.num.chars.log     < 5.167635 to the left,  improve=2.587692, (0 missing)
##       S.num.chars.log     < 5.167635 to the left,  improve=2.587692, (0 missing)
##       PubDate.wkday.fctr3 < 0.5      to the left,  improve=2.223077, (0 missing)
##       A.num.words.log     < 2.861793 to the left,  improve=1.632005, (0 missing)
##   Surrogate splits:
##       A.num.chars.log < 5.480422 to the right, agree=0.8, adj=0.071, (0 split)
##       S.num.chars.log < 5.468025 to the right, agree=0.8, adj=0.071, (0 split)
## 
## Node number 28: 14 observations
##   predicted class=N  expected loss=0.1428571  P(node) =0.003128492
##     class counts:    12     2
##    probabilities: 0.857 0.143 
## 
## Node number 29: 53 observations,    complexity param=0.001335113
##   predicted class=Y  expected loss=0.2264151  P(node) =0.01184358
##     class counts:    12    41
##    probabilities: 0.226 0.774 
##   left son=58 (41 obs) right son=59 (12 obs)
##   Primary splits:
##       PubDate.second      < 47       to the left,  improve=1.5904280, (0 missing)
##       PubDate.minute      < 32       to the left,  improve=1.1979220, (0 missing)
##       H.num.chars.log     < 3.860674 to the right, improve=0.8288949, (0 missing)
##       A.num.words.unq.log < 2.012676 to the right, improve=0.7486464, (0 missing)
##       S.num.words.unq.log < 2.012676 to the right, improve=0.7486464, (0 missing)
##   Surrogate splits:
##       H.num.chars.log < 3.020127 to the right, agree=0.792, adj=0.083, (0 split)
## 
## Node number 30: 216 observations,    complexity param=0.001502003
##   predicted class=Y  expected loss=0.1944444  P(node) =0.04826816
##     class counts:    42   174
##    probabilities: 0.194 0.806 
##   left son=60 (7 obs) right son=61 (209 obs)
##   Primary splits:
##       WordCount.log           < 4.331079 to the left,  improve=2.056277, (0 missing)
##       A.num.words.log         < 1.497866 to the left,  improve=2.056277, (0 missing)
##       A.num.words.unq.log     < 1.497866 to the left,  improve=2.056277, (0 missing)
##       PubDate.wkday.fctr4     < 0.5      to the right, improve=1.580879, (0 missing)
##       PubDate.date.fctr(7,13] < 0.5      to the right, improve=1.411935, (0 missing)
## 
## Node number 31: 53 observations
##   predicted class=Y  expected loss=0.03773585  P(node) =0.01184358
##     class counts:     2    51
##    probabilities: 0.038 0.962 
## 
## Node number 32: 2853 observations,    complexity param=0.003004005
##   predicted class=N  expected loss=0.02418507  P(node) =0.6375419
##     class counts:  2784    69
##    probabilities: 0.976 0.024 
##   left son=64 (2095 obs) right son=65 (758 obs)
##   Primary splits:
##       WordCount.log                           < 5.985194 to the left,  improve=2.555349, (0 missing)
##       A.num.chars.log                         < 3.817652 to the right, improve=2.548060, (0 missing)
##       S.num.chars.log                         < 3.817652 to the right, improve=2.548060, (0 missing)
##       SectionName.fctrTechnology              < 0.5      to the left,  improve=1.691907, (0 missing)
##       SubsectionName.fctrBusiness::Technology < 0.5      to the left,  improve=1.691907, (0 missing)
##   Surrogate splits:
##       SectionName.fctrBusiness Day      < 0.5      to the left,  agree=0.753, adj=0.071, (0 split)
##       SectionName.fctrWorld             < 0.5      to the left,  agree=0.747, adj=0.049, (0 split)
##       SubsectionName.fctrAsia Pacific   < 0.5      to the left,  agree=0.747, adj=0.049, (0 split)
##       SubsectionName.fctrSmall Business < 0.5      to the left,  agree=0.747, adj=0.046, (0 split)
##       H.today                           < 0.5      to the left,  agree=0.746, adj=0.044, (0 split)
## 
## Node number 33: 58 observations,    complexity param=0.003004005
##   predicted class=N  expected loss=0.2931034  P(node) =0.01296089
##     class counts:    41    17
##    probabilities: 0.707 0.293 
##   left son=66 (48 obs) right son=67 (10 obs)
##   Primary splits:
##       PubDate.hour        < 9.5      to the right, improve=4.0011490, (0 missing)
##       H.num.chars.log     < 3.860674 to the right, improve=1.8137040, (0 missing)
##       A.num.chars.log     < 4.867416 to the left,  improve=0.9536747, (0 missing)
##       S.num.chars.log     < 4.867416 to the left,  improve=0.9536747, (0 missing)
##       PubDate.wkday.fctr5 < 0.5      to the left,  improve=0.9507221, (0 missing)
##   Surrogate splits:
##       A.can          < 0.5      to the left,  agree=0.845, adj=0.1, (0 split)
##       S.can          < 0.5      to the left,  agree=0.845, adj=0.1, (0 split)
##       PubDate.second < 2.5      to the right, agree=0.845, adj=0.1, (0 split)
## 
## Node number 34: 44 observations
##   predicted class=N  expected loss=0.1136364  P(node) =0.009832402
##     class counts:    39     5
##    probabilities: 0.886 0.114 
## 
## Node number 35: 34 observations,    complexity param=0.001335113
##   predicted class=Y  expected loss=0.1470588  P(node) =0.007597765
##     class counts:     5    29
##    probabilities: 0.147 0.853 
##   left son=70 (7 obs) right son=71 (27 obs)
##   Primary splits:
##       WordCount.log   < 5.794443 to the left,  improve=3.174914, (0 missing)
##       H.num.chars.log < 3.417592 to the right, improve=3.174914, (0 missing)
##       A.num.chars.log < 4.424829 to the right, improve=3.174914, (0 missing)
##       S.num.chars.log < 4.424829 to the right, improve=3.174914, (0 missing)
##       S.num.words.log < 2.249905 to the right, improve=3.174914, (0 missing)
##   Surrogate splits:
##       NewsDesk.fctrMetro                      < 0.5      to the right, agree=0.882, adj=0.429, (0 split)
##       SectionName.fctrN.Y. / Region           < 0.5      to the right, agree=0.882, adj=0.429, (0 split)
##       SubsectionName.fctrMetro::N.Y. / Region < 0.5      to the right, agree=0.882, adj=0.429, (0 split)
##       A.num.words.log                         < 2.602003 to the right, agree=0.853, adj=0.286, (0 split)
##       A.num.chars.log                         < 4.846846 to the right, agree=0.853, adj=0.286, (0 split)
## 
## Node number 36: 40 observations,    complexity param=0.006675567
##   predicted class=N  expected loss=0.35  P(node) =0.008938547
##     class counts:    26    14
##    probabilities: 0.650 0.350 
##   left son=72 (29 obs) right son=73 (11 obs)
##   Primary splits:
##       WordCount.log       < 6.299797 to the left,  improve=4.319122, (0 missing)
##       PubDate.wkday.fctr4 < 0.5      to the left,  improve=2.329032, (0 missing)
##       S.week              < 0.5      to the right, improve=2.253333, (0 missing)
##       A.num.words.log     < 3.020127 to the right, improve=2.078788, (0 missing)
##       S.num.words.log     < 3.020127 to the right, improve=2.078788, (0 missing)
##   Surrogate splits:
##       A.can           < 0.5      to the left,  agree=0.75, adj=0.091, (0 split)
##       S.can           < 0.5      to the left,  agree=0.75, adj=0.091, (0 split)
##       PubDate.minute  < 43       to the left,  agree=0.75, adj=0.091, (0 split)
##       H.num.chars.log < 4.102609 to the left,  agree=0.75, adj=0.091, (0 split)
##       H.num.words.log < 2.012676 to the left,  agree=0.75, adj=0.091, (0 split)
## 
## Node number 37: 7 observations
##   predicted class=Y  expected loss=0.1428571  P(node) =0.001564246
##     class counts:     1     6
##    probabilities: 0.143 0.857 
## 
## Node number 40: 878 observations,    complexity param=0.00623053
##   predicted class=N  expected loss=0.2015945  P(node) =0.1962011
##     class counts:   701   177
##    probabilities: 0.798 0.202 
##   left son=80 (705 obs) right son=81 (173 obs)
##   Primary splits:
##       SectionName.fctrmyMisc::             < 0.5      to the left,  improve=13.065280, (0 missing)
##       SubsectionName.fctrmyMisc::myMisc::  < 0.5      to the left,  improve=13.065280, (0 missing)
##       PubDate.wkday.fctr6                  < 0.5      to the left,  improve= 9.962150, (0 missing)
##       SubsectionName.fctrThe Public Editor < 0.5      to the left,  improve= 8.470074, (0 missing)
##       PubDate.hour                         < 7.5      to the left,  improve= 7.304023, (0 missing)
##   Surrogate splits:
##       SubsectionName.fctrmyMisc::myMisc:: < 0.5      to the left,  agree=1.000, adj=1.000, (0 split)
##       NewsDesk.fctrmyMisc::               < 0.5      to the left,  agree=0.874, adj=0.358, (0 split)
##       H.report                            < 0.5      to the left,  agree=0.806, adj=0.017, (0 split)
##       S.report                            < 0.5      to the left,  agree=0.805, adj=0.012, (0 split)
##       WordCount.log                       < 8.49686  to the left,  agree=0.804, adj=0.006, (0 split)
## 
## Node number 41: 66 observations,    complexity param=0.01335113
##   predicted class=Y  expected loss=0.3484848  P(node) =0.0147486
##     class counts:    23    43
##    probabilities: 0.348 0.652 
##   left son=82 (26 obs) right son=83 (40 obs)
##   Primary splits:
##       A.num.chars.log     < 4.532368 to the right, improve=10.142770, (0 missing)
##       S.num.chars.log     < 4.532368 to the right, improve=10.142770, (0 missing)
##       S.num.words.log     < 2.35024  to the right, improve= 8.000466, (0 missing)
##       S.num.words.unq.log < 2.35024  to the right, improve= 8.000466, (0 missing)
##       A.num.words.unq.log < 2.35024  to the right, improve= 7.636364, (0 missing)
##   Surrogate splits:
##       S.num.chars.log     < 4.532368 to the right, agree=1.000, adj=1.000, (0 split)
##       S.num.words.log     < 2.35024  to the right, agree=0.970, adj=0.923, (0 split)
##       S.num.words.unq.log < 2.35024  to the right, agree=0.970, adj=0.923, (0 split)
##       A.num.words.log     < 2.35024  to the right, agree=0.955, adj=0.885, (0 split)
##       A.num.words.unq.log < 2.35024  to the right, agree=0.939, adj=0.846, (0 split)
## 
## Node number 42: 14 observations
##   predicted class=N  expected loss=0.2857143  P(node) =0.003128492
##     class counts:    10     4
##    probabilities: 0.714 0.286 
## 
## Node number 43: 51 observations,    complexity param=0.00200267
##   predicted class=Y  expected loss=0.2352941  P(node) =0.01139665
##     class counts:    12    39
##    probabilities: 0.235 0.765 
##   left son=86 (30 obs) right son=87 (21 obs)
##   Primary splits:
##       A.num.chars.log         < 5.167635 to the left,  improve=1.4005600, (0 missing)
##       S.num.chars.log         < 5.167635 to the left,  improve=1.4005600, (0 missing)
##       WordCount.log           < 6.613352 to the right, improve=0.8983957, (0 missing)
##       PubDate.date.fctr(7,13] < 0.5      to the left,  improve=0.8752084, (0 missing)
##       PubDate.wkday.fctr3     < 0.5      to the left,  improve=0.8752084, (0 missing)
##   Surrogate splits:
##       S.num.chars.log     < 5.167635 to the left,  agree=1.000, adj=1.000, (0 split)
##       A.num.words.log     < 2.740319 to the left,  agree=0.824, adj=0.571, (0 split)
##       A.num.words.unq.log < 2.740319 to the left,  agree=0.824, adj=0.571, (0 split)
##       S.num.words.log     < 2.802901 to the left,  agree=0.824, adj=0.571, (0 split)
##       S.num.words.unq.log < 2.740319 to the left,  agree=0.824, adj=0.571, (0 split)
## 
## Node number 58: 41 observations,    complexity param=0.001335113
##   predicted class=Y  expected loss=0.2926829  P(node) =0.009162011
##     class counts:    12    29
##    probabilities: 0.293 0.707 
##   left son=116 (8 obs) right son=117 (33 obs)
##   Primary splits:
##       PubDate.date.fctr(13,19] < 0.5      to the right, improve=2.1953070, (0 missing)
##       H.num.chars.log          < 3.860674 to the right, improve=1.4780250, (0 missing)
##       H.num.words.log          < 1.868835 to the right, improve=1.1369000, (0 missing)
##       H.num.words.unq.log      < 1.868835 to the right, improve=1.1369000, (0 missing)
##       PubDate.minute           < 12.5     to the right, improve=0.9820614, (0 missing)
##   Surrogate splits:
##       PubDate.hour < 8.5      to the left,  agree=0.854, adj=0.25, (0 split)
## 
## Node number 59: 12 observations
##   predicted class=Y  expected loss=0  P(node) =0.002681564
##     class counts:     0    12
##    probabilities: 0.000 1.000 
## 
## Node number 60: 7 observations
##   predicted class=N  expected loss=0.4285714  P(node) =0.001564246
##     class counts:     4     3
##    probabilities: 0.571 0.429 
## 
## Node number 61: 209 observations,    complexity param=0.001502003
##   predicted class=Y  expected loss=0.1818182  P(node) =0.04670391
##     class counts:    38   171
##    probabilities: 0.182 0.818 
##   left son=122 (48 obs) right son=123 (161 obs)
##   Primary splits:
##       PubDate.wkday.fctr4 < 0.5      to the right, improve=1.503764, (0 missing)
##       H.num.chars.log     < 3.156774 to the right, improve=1.461895, (0 missing)
##       PubDate.minute      < 2.5      to the right, improve=1.302237, (0 missing)
##       H.num.words.log     < 2.138333 to the right, improve=1.140217, (0 missing)
##       H.num.words.unq.log < 2.138333 to the right, improve=1.140217, (0 missing)
##   Surrogate splits:
##       S.time < 0.5      to the right, agree=0.785, adj=0.062, (0 split)
##       A.time < 0.5      to the right, agree=0.785, adj=0.062, (0 split)
## 
## Node number 64: 2095 observations
##   predicted class=N  expected loss=0.01145585  P(node) =0.4681564
##     class counts:  2071    24
##    probabilities: 0.989 0.011 
## 
## Node number 65: 758 observations,    complexity param=0.003004005
##   predicted class=N  expected loss=0.05936675  P(node) =0.1693855
##     class counts:   713    45
##    probabilities: 0.941 0.059 
##   left son=130 (747 obs) right son=131 (11 obs)
##   Primary splits:
##       A.num.chars.log     < 4.051747 to the right, improve=9.958684, (0 missing)
##       S.num.chars.log     < 4.051747 to the right, improve=9.958684, (0 missing)
##       A.num.words.log     < 2.012676 to the right, improve=4.096908, (0 missing)
##       A.num.words.unq.log < 2.012676 to the right, improve=3.923323, (0 missing)
##       S.num.words.log     < 2.012676 to the right, improve=3.617432, (0 missing)
##   Surrogate splits:
##       S.num.chars.log     < 4.051747 to the right, agree=1.000, adj=1.000, (0 split)
##       S.num.words.log     < 1.700599 to the right, agree=0.991, adj=0.364, (0 split)
##       S.num.words.unq.log < 1.700599 to the right, agree=0.991, adj=0.364, (0 split)
##       A.num.words.log     < 1.700599 to the right, agree=0.989, adj=0.273, (0 split)
##       A.num.words.unq.log < 1.700599 to the right, agree=0.989, adj=0.273, (0 split)
## 
## Node number 66: 48 observations
##   predicted class=N  expected loss=0.2083333  P(node) =0.01072626
##     class counts:    38    10
##    probabilities: 0.792 0.208 
## 
## Node number 67: 10 observations
##   predicted class=Y  expected loss=0.3  P(node) =0.002234637
##     class counts:     3     7
##    probabilities: 0.300 0.700 
## 
## Node number 70: 7 observations
##   predicted class=N  expected loss=0.4285714  P(node) =0.001564246
##     class counts:     4     3
##    probabilities: 0.571 0.429 
## 
## Node number 71: 27 observations
##   predicted class=Y  expected loss=0.03703704  P(node) =0.00603352
##     class counts:     1    26
##    probabilities: 0.037 0.963 
## 
## Node number 72: 29 observations
##   predicted class=N  expected loss=0.2068966  P(node) =0.006480447
##     class counts:    23     6
##    probabilities: 0.793 0.207 
## 
## Node number 73: 11 observations
##   predicted class=Y  expected loss=0.2727273  P(node) =0.002458101
##     class counts:     3     8
##    probabilities: 0.273 0.727 
## 
## Node number 80: 705 observations,    complexity param=0.00623053
##   predicted class=N  expected loss=0.1588652  P(node) =0.1575419
##     class counts:   593   112
##    probabilities: 0.841 0.159 
##   left son=160 (688 obs) right son=161 (17 obs)
##   Primary splits:
##       PubDate.wkday.fctr6                  < 0.5      to the left,  improve=10.425130, (0 missing)
##       SubsectionName.fctrThe Public Editor < 0.5      to the left,  improve= 9.714944, (0 missing)
##       PubDate.hour                         < 7.5      to the left,  improve= 5.862049, (0 missing)
##       H.num.chars.log                      < 2.6365   to the right, improve= 4.362199, (0 missing)
##       A.num.words.log                      < 2.917405 to the right, improve= 3.734140, (0 missing)
##   Surrogate splits:
##       H.num.chars.log < 2.249905 to the right, agree=0.98, adj=0.176, (0 split)
## 
## Node number 81: 173 observations,    complexity param=0.003115265
##   predicted class=N  expected loss=0.3757225  P(node) =0.03865922
##     class counts:   108    65
##    probabilities: 0.624 0.376 
##   left son=162 (16 obs) right son=163 (157 obs)
##   Primary splits:
##       S.report        < 0.5      to the right, improve=3.459413, (0 missing)
##       H.num.chars.log < 3.156774 to the right, improve=3.427566, (0 missing)
##       PubDate.minute  < 27.5     to the right, improve=2.875971, (0 missing)
##       PubDate.hour    < 15.5     to the right, improve=2.506819, (0 missing)
##       S.time          < 0.5      to the right, improve=2.435711, (0 missing)
## 
## Node number 82: 26 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3076923  P(node) =0.005810056
##     class counts:    18     8
##    probabilities: 0.692 0.308 
##   left son=164 (19 obs) right son=165 (7 obs)
##   Primary splits:
##       A.num.words.log     < 2.861793 to the left,  improve=1.332562, (0 missing)
##       A.num.words.unq.log < 2.861793 to the left,  improve=1.332562, (0 missing)
##       S.num.words.log     < 2.861793 to the left,  improve=1.332562, (0 missing)
##       S.num.words.unq.log < 2.861793 to the left,  improve=1.332562, (0 missing)
##       PubDate.hour        < 22.5     to the right, improve=1.063851, (0 missing)
##   Surrogate splits:
##       A.num.words.unq.log < 2.861793 to the left,  agree=1.000, adj=1.000, (0 split)
##       S.num.words.log     < 2.861793 to the left,  agree=1.000, adj=1.000, (0 split)
##       S.num.words.unq.log < 2.861793 to the left,  agree=1.000, adj=1.000, (0 split)
##       A.num.chars.log     < 5.117922 to the left,  agree=0.923, adj=0.714, (0 split)
##       S.num.chars.log     < 5.117922 to the left,  agree=0.923, adj=0.714, (0 split)
## 
## Node number 83: 40 observations
##   predicted class=Y  expected loss=0.125  P(node) =0.008938547
##     class counts:     5    35
##    probabilities: 0.125 0.875 
## 
## Node number 86: 30 observations,    complexity param=0.00200267
##   predicted class=Y  expected loss=0.3333333  P(node) =0.006703911
##     class counts:    10    20
##    probabilities: 0.333 0.667 
##   left son=172 (13 obs) right son=173 (17 obs)
##   Primary splits:
##       A.num.words.log     < 2.673554 to the right, improve=3.650075, (0 missing)
##       S.num.words.log     < 2.673554 to the right, improve=2.400000, (0 missing)
##       S.num.words.unq.log < 2.673554 to the right, improve=2.133333, (0 missing)
##       A.num.words.unq.log < 2.673554 to the right, improve=1.856061, (0 missing)
##       A.num.chars.log     < 4.986973 to the right, improve=1.458333, (0 missing)
##   Surrogate splits:
##       S.num.words.log     < 2.673554 to the right, agree=0.933, adj=0.846, (0 split)
##       A.num.words.unq.log < 2.602003 to the right, agree=0.867, adj=0.692, (0 split)
##       S.num.words.unq.log < 2.673554 to the right, agree=0.833, adj=0.615, (0 split)
##       PubDate.minute      < 29       to the right, agree=0.733, adj=0.385, (0 split)
##       A.num.chars.log     < 4.973274 to the right, agree=0.733, adj=0.385, (0 split)
## 
## Node number 87: 21 observations
##   predicted class=Y  expected loss=0.0952381  P(node) =0.004692737
##     class counts:     2    19
##    probabilities: 0.095 0.905 
## 
## Node number 116: 8 observations
##   predicted class=N  expected loss=0.375  P(node) =0.001787709
##     class counts:     5     3
##    probabilities: 0.625 0.375 
## 
## Node number 117: 33 observations,    complexity param=0.001335113
##   predicted class=Y  expected loss=0.2121212  P(node) =0.007374302
##     class counts:     7    26
##    probabilities: 0.212 0.788 
##   left son=234 (22 obs) right son=235 (11 obs)
##   Primary splits:
##       A.num.words.log     < 1.868835 to the right, improve=1.484848, (0 missing)
##       A.num.words.unq.log < 1.868835 to the right, improve=1.484848, (0 missing)
##       S.num.words.log     < 1.868835 to the right, improve=1.484848, (0 missing)
##       S.num.words.unq.log < 1.868835 to the right, improve=1.484848, (0 missing)
##       H.num.words.log     < 1.868835 to the right, improve=1.335859, (0 missing)
##   Surrogate splits:
##       A.num.words.unq.log < 1.868835 to the right, agree=1.000, adj=1.000, (0 split)
##       S.num.words.log     < 1.868835 to the right, agree=1.000, adj=1.000, (0 split)
##       S.num.words.unq.log < 1.868835 to the right, agree=1.000, adj=1.000, (0 split)
##       A.num.chars.log     < 4.085941 to the right, agree=0.939, adj=0.818, (0 split)
##       S.num.chars.log     < 4.085941 to the right, agree=0.939, adj=0.818, (0 split)
## 
## Node number 122: 48 observations,    complexity param=0.001502003
##   predicted class=Y  expected loss=0.2916667  P(node) =0.01072626
##     class counts:    14    34
##    probabilities: 0.292 0.708 
##   left son=244 (22 obs) right son=245 (26 obs)
##   Primary splits:
##       PubDate.hour    < 14.5     to the left,  improve=2.155012, (0 missing)
##       H.num.chars.log < 3.540854 to the right, improve=1.633333, (0 missing)
##       A.num.chars.log < 4.844063 to the left,  improve=1.394309, (0 missing)
##       S.num.chars.log < 4.844063 to the left,  improve=1.394309, (0 missing)
##       PubDate.minute  < 19.5     to the right, improve=1.388889, (0 missing)
##   Surrogate splits:
##       PubDate.apm.fctrpm < 0.5      to the left,  agree=0.708, adj=0.364, (0 split)
##       H.num.chars.log    < 4.025192 to the right, agree=0.667, adj=0.273, (0 split)
##       A.num.chars.log    < 4.629851 to the left,  agree=0.667, adj=0.273, (0 split)
##       S.num.chars.log    < 4.629851 to the left,  agree=0.667, adj=0.273, (0 split)
##       WordCount.log      < 5.393462 to the left,  agree=0.625, adj=0.182, (0 split)
## 
## Node number 123: 161 observations,    complexity param=0.001502003
##   predicted class=Y  expected loss=0.1490683  P(node) =0.03597765
##     class counts:    24   137
##    probabilities: 0.149 0.851 
##   left son=246 (47 obs) right son=247 (114 obs)
##   Primary splits:
##       PubDate.wkday.fctr2      < 0.5      to the right, improve=1.4986960, (0 missing)
##       PubDate.minute           < 28.5     to the left,  improve=1.3562980, (0 missing)
##       PubDate.date.fctr(13,19] < 0.5      to the left,  improve=1.1087710, (0 missing)
##       PubDate.second           < 11.5     to the left,  improve=0.8137127, (0 missing)
##       PubDate.hour             < 10.5     to the right, improve=0.7041323, (0 missing)
##   Surrogate splits:
##       PubDate.hour        < 20.5     to the right, agree=0.739, adj=0.106, (0 split)
##       WordCount.log       < 5.708703 to the left,  agree=0.733, adj=0.085, (0 split)
##       S.said              < 0.5      to the right, agree=0.727, adj=0.064, (0 split)
##       A.num.words.log     < 2.802901 to the right, agree=0.720, adj=0.043, (0 split)
##       A.num.words.unq.log < 2.802901 to the right, agree=0.720, adj=0.043, (0 split)
## 
## Node number 130: 747 observations,    complexity param=0.0004450378
##   predicted class=N  expected loss=0.04953146  P(node) =0.1669274
##     class counts:   710    37
##    probabilities: 0.950 0.050 
##   left son=260 (665 obs) right son=261 (82 obs)
##   Primary splits:
##       SectionName.fctrmyMisc::            < 0.5      to the left,  improve=2.188954, (0 missing)
##       SubsectionName.fctrmyMisc::myMisc:: < 0.5      to the left,  improve=2.188954, (0 missing)
##       SectionName.fctrBusiness Day        < 0.5      to the right, improve=1.621507, (0 missing)
##       NewsDesk.fctrmyMisc::               < 0.5      to the left,  improve=1.317747, (0 missing)
##       SectionName.fctrTechnology          < 0.5      to the left,  improve=1.254840, (0 missing)
##   Surrogate splits:
##       SubsectionName.fctrmyMisc::myMisc:: < 0.5      to the left,  agree=1.000, adj=1.000, (0 split)
##       NewsDesk.fctrmyMisc::               < 0.5      to the left,  agree=0.933, adj=0.390, (0 split)
##       H.num.chars.log                     < 2.65906  to the right, agree=0.894, adj=0.037, (0 split)
##       WordCount.log                       < 6.527226 to the left,  agree=0.893, adj=0.024, (0 split)
##       S.num.chars.log                     < 5.523457 to the left,  agree=0.893, adj=0.024, (0 split)
## 
## Node number 131: 11 observations
##   predicted class=Y  expected loss=0.2727273  P(node) =0.002458101
##     class counts:     3     8
##    probabilities: 0.273 0.727 
## 
## Node number 160: 688 observations,    complexity param=0.00623053
##   predicted class=N  expected loss=0.1453488  P(node) =0.153743
##     class counts:   588   100
##    probabilities: 0.855 0.145 
##   left son=320 (677 obs) right son=321 (11 obs)
##   Primary splits:
##       SubsectionName.fctrThe Public Editor < 0.5      to the left,  improve=10.121320, (0 missing)
##       PubDate.hour                         < 7.5      to the left,  improve= 4.625184, (0 missing)
##       SubsectionName.fctrDealbook          < 0.5      to the left,  improve= 4.594187, (0 missing)
##       A.num.words.unq.log                  < 2.917405 to the right, improve= 2.762093, (0 missing)
##       SectionName.fctrBusiness Day         < 0.5      to the left,  improve= 2.721899, (0 missing)
## 
## Node number 161: 17 observations
##   predicted class=Y  expected loss=0.2941176  P(node) =0.003798883
##     class counts:     5    12
##    probabilities: 0.294 0.706 
## 
## Node number 162: 16 observations
##   predicted class=N  expected loss=0.0625  P(node) =0.003575419
##     class counts:    15     1
##    probabilities: 0.938 0.062 
## 
## Node number 163: 157 observations,    complexity param=0.003115265
##   predicted class=N  expected loss=0.4076433  P(node) =0.0350838
##     class counts:    93    64
##    probabilities: 0.592 0.408 
##   left son=326 (30 obs) right son=327 (127 obs)
##   Primary splits:
##       PubDate.minute  < 40.5     to the right, improve=2.253677, (0 missing)
##       H.num.chars.log < 3.156774 to the right, improve=2.225412, (0 missing)
##       PubDate.second  < 53.5     to the left,  improve=1.700777, (0 missing)
##       PubDate.hour    < 15.5     to the right, improve=1.671353, (0 missing)
##       WordCount.log   < 7.107424 to the left,  improve=1.530309, (0 missing)
##   Surrogate splits:
##       WordCount.log   < 8.54954  to the right, agree=0.822, adj=0.067, (0 split)
##       A.num.words.log < 2.012676 to the left,  agree=0.815, adj=0.033, (0 split)
##       A.num.chars.log < 4.262581 to the left,  agree=0.815, adj=0.033, (0 split)
##       S.num.chars.log < 4.262581 to the left,  agree=0.815, adj=0.033, (0 split)
## 
## Node number 164: 19 observations
##   predicted class=N  expected loss=0.2105263  P(node) =0.00424581
##     class counts:    15     4
##    probabilities: 0.789 0.211 
## 
## Node number 165: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.001564246
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 172: 13 observations
##   predicted class=N  expected loss=0.3846154  P(node) =0.002905028
##     class counts:     8     5
##    probabilities: 0.615 0.385 
## 
## Node number 173: 17 observations
##   predicted class=Y  expected loss=0.1176471  P(node) =0.003798883
##     class counts:     2    15
##    probabilities: 0.118 0.882 
## 
## Node number 234: 22 observations,    complexity param=0.001335113
##   predicted class=Y  expected loss=0.3181818  P(node) =0.004916201
##     class counts:     7    15
##    probabilities: 0.318 0.682 
##   left son=468 (7 obs) right son=469 (15 obs)
##   Primary splits:
##       A.num.chars.log     < 4.329954 to the left,  improve=3.221645, (0 missing)
##       S.num.chars.log     < 4.329954 to the left,  improve=3.221645, (0 missing)
##       A.num.words.log     < 2.249905 to the left,  improve=2.912121, (0 missing)
##       A.num.words.unq.log < 2.249905 to the left,  improve=2.912121, (0 missing)
##       S.num.words.log     < 2.249905 to the left,  improve=2.912121, (0 missing)
##   Surrogate splits:
##       S.num.chars.log     < 4.329954 to the left,  agree=1.000, adj=1.000, (0 split)
##       A.num.words.log     < 2.249905 to the left,  agree=0.864, adj=0.571, (0 split)
##       A.num.words.unq.log < 2.249905 to the left,  agree=0.864, adj=0.571, (0 split)
##       S.num.words.log     < 2.249905 to the left,  agree=0.864, adj=0.571, (0 split)
##       S.num.words.unq.log < 2.249905 to the left,  agree=0.864, adj=0.571, (0 split)
## 
## Node number 235: 11 observations
##   predicted class=Y  expected loss=0  P(node) =0.002458101
##     class counts:     0    11
##    probabilities: 0.000 1.000 
## 
## Node number 244: 22 observations,    complexity param=0.001502003
##   predicted class=Y  expected loss=0.4545455  P(node) =0.004916201
##     class counts:    10    12
##    probabilities: 0.455 0.545 
##   left son=488 (14 obs) right son=489 (8 obs)
##   Primary splits:
##       PubDate.second  < 15.5     to the right, improve=2.7305190, (0 missing)
##       PubDate.minute  < 37       to the left,  improve=1.9948050, (0 missing)
##       A.num.chars.log < 4.610145 to the left,  improve=1.9948050, (0 missing)
##       S.num.chars.log < 4.610145 to the left,  improve=1.9948050, (0 missing)
##       WordCount.log   < 5.95454  to the left,  improve=0.7305195, (0 missing)
##   Surrogate splits:
##       PubDate.hour       < 10.5     to the right, agree=0.818, adj=0.500, (0 split)
##       PubDate.minute     < 37       to the left,  agree=0.773, adj=0.375, (0 split)
##       PubDate.apm.fctrpm < 0.5      to the right, agree=0.727, adj=0.250, (0 split)
##       A.num.chars.log    < 4.620047 to the left,  agree=0.727, adj=0.250, (0 split)
##       S.num.chars.log    < 4.620047 to the left,  agree=0.727, adj=0.250, (0 split)
## 
## Node number 245: 26 observations,    complexity param=0.001335113
##   predicted class=Y  expected loss=0.1538462  P(node) =0.005810056
##     class counts:     4    22
##    probabilities: 0.154 0.846 
##   left son=490 (7 obs) right son=491 (19 obs)
##   Primary splits:
##       PubDate.minute  < 37.5     to the right, improve=3.3406590, (0 missing)
##       H.num.chars.log < 3.828405 to the left,  improve=0.9025641, (0 missing)
##       PubDate.second  < 39.5     to the left,  improve=0.5470085, (0 missing)
##       A.num.chars.log < 4.695914 to the right, improve=0.5389277, (0 missing)
##       S.num.chars.log < 4.695914 to the right, improve=0.5389277, (0 missing)
##   Surrogate splits:
##       WordCount.log < 5.618309 to the left,  agree=0.808, adj=0.286, (0 split)
##       S.state       < 0.5      to the right, agree=0.808, adj=0.286, (0 split)
##       PubDate.hour  < 15.5     to the left,  agree=0.769, adj=0.143, (0 split)
## 
## Node number 246: 47 observations,    complexity param=0.001502003
##   predicted class=Y  expected loss=0.2553191  P(node) =0.01050279
##     class counts:    12    35
##    probabilities: 0.255 0.745 
##   left son=492 (23 obs) right son=493 (24 obs)
##   Primary splits:
##       PubDate.minute  < 21.5     to the left,  improve=1.6658190, (0 missing)
##       PubDate.hour    < 12.5     to the right, improve=0.8239533, (0 missing)
##       PubDate.second  < 46.5     to the right, improve=0.7963170, (0 missing)
##       A.num.chars.log < 4.460128 to the right, improve=0.7708912, (0 missing)
##       S.num.chars.log < 4.460128 to the right, improve=0.7708912, (0 missing)
##   Surrogate splits:
##       WordCount.log   < 5.930847 to the left,  agree=0.660, adj=0.304, (0 split)
##       A.num.chars.log < 4.835147 to the right, agree=0.617, adj=0.217, (0 split)
##       S.num.chars.log < 4.835147 to the right, agree=0.617, adj=0.217, (0 split)
##       H.num.chars.log < 3.725621 to the right, agree=0.596, adj=0.174, (0 split)
##       H.num.words.log < 1.868835 to the right, agree=0.596, adj=0.174, (0 split)
## 
## Node number 247: 114 observations,    complexity param=0.001335113
##   predicted class=Y  expected loss=0.1052632  P(node) =0.02547486
##     class counts:    12   102
##    probabilities: 0.105 0.895 
##   left son=494 (45 obs) right son=495 (69 obs)
##   Primary splits:
##       H.num.chars.log     < 3.449862 to the left,  improve=1.3345540, (0 missing)
##       A.num.chars.log     < 3.941535 to the left,  improve=1.1368420, (0 missing)
##       S.num.chars.log     < 3.941535 to the left,  improve=1.1368420, (0 missing)
##       H.num.words.log     < 1.242453 to the left,  improve=0.7245203, (0 missing)
##       H.num.words.unq.log < 1.242453 to the left,  improve=0.7245203, (0 missing)
##   Surrogate splits:
##       H.num.words.log     < 1.497866 to the left,  agree=0.842, adj=0.600, (0 split)
##       H.num.words.unq.log < 1.497866 to the left,  agree=0.833, adj=0.578, (0 split)
##       S.num.words.unq.log < 1.868835 to the left,  agree=0.702, adj=0.244, (0 split)
##       A.num.chars.log     < 3.960768 to the left,  agree=0.693, adj=0.222, (0 split)
##       S.num.chars.log     < 3.960768 to the left,  agree=0.693, adj=0.222, (0 split)
## 
## Node number 260: 665 observations
##   predicted class=N  expected loss=0.03609023  P(node) =0.1486034
##     class counts:   641    24
##    probabilities: 0.964 0.036 
## 
## Node number 261: 82 observations,    complexity param=0.0004450378
##   predicted class=N  expected loss=0.1585366  P(node) =0.01832402
##     class counts:    69    13
##    probabilities: 0.841 0.159 
##   left son=522 (50 obs) right son=523 (32 obs)
##   Primary splits:
##       PubDate.second      < 17       to the right, improve=2.4880490, (0 missing)
##       A.num.words.log     < 2.441401 to the left,  improve=1.0978290, (0 missing)
##       A.num.words.unq.log < 2.441401 to the left,  improve=1.0978290, (0 missing)
##       PubDate.wkday.fctr1 < 0.5      to the left,  improve=1.0688300, (0 missing)
##       PubDate.minute      < 0.5      to the left,  improve=0.9228249, (0 missing)
##   Surrogate splits:
##       PubDate.hour        < 12.5     to the right, agree=0.659, adj=0.125, (0 split)
##       PubDate.apm.fctrpm  < 0.5      to the right, agree=0.646, adj=0.094, (0 split)
##       PubDate.wkday.fctr1 < 0.5      to the left,  agree=0.646, adj=0.094, (0 split)
##       A.num.chars.log     < 4.753553 to the right, agree=0.646, adj=0.094, (0 split)
##       S.num.chars.log     < 4.753553 to the right, agree=0.646, adj=0.094, (0 split)
## 
## Node number 320: 677 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.1344165  P(node) =0.1512849
##     class counts:   586    91
##    probabilities: 0.866 0.134 
##   left son=640 (466 obs) right son=641 (211 obs)
##   Primary splits:
##       SubsectionName.fctrDealbook  < 0.5      to the left,  improve=5.865298, (0 missing)
##       PubDate.hour                 < 19.5     to the left,  improve=4.281164, (0 missing)
##       SectionName.fctrBusiness Day < 0.5      to the left,  improve=3.886842, (0 missing)
##       NewsDesk.fctrmyMisc::        < 0.5      to the right, improve=3.025302, (0 missing)
##       SectionName.fctrTechnology   < 0.5      to the left,  improve=2.850321, (0 missing)
##   Surrogate splits:
##       SectionName.fctrBusiness Day      < 0.5      to the left,  agree=0.934, adj=0.787, (0 split)
##       PubDate.hour                      < 19.5     to the left,  agree=0.756, adj=0.218, (0 split)
##       Headline.pfx.fctrMorning Agenda:: < 0.5      to the left,  agree=0.716, adj=0.090, (0 split)
##       S.num.words.unq.log               < 3.198465 to the left,  agree=0.703, adj=0.047, (0 split)
##       S.said                            < 0.5      to the left,  agree=0.699, adj=0.033, (0 split)
## 
## Node number 321: 11 observations
##   predicted class=Y  expected loss=0.1818182  P(node) =0.002458101
##     class counts:     2     9
##    probabilities: 0.182 0.818 
## 
## Node number 326: 30 observations
##   predicted class=N  expected loss=0.2333333  P(node) =0.006703911
##     class counts:    23     7
##    probabilities: 0.767 0.233 
## 
## Node number 327: 127 observations,    complexity param=0.003115265
##   predicted class=N  expected loss=0.4488189  P(node) =0.02837989
##     class counts:    70    57
##    probabilities: 0.551 0.449 
##   left son=654 (114 obs) right son=655 (13 obs)
##   Primary splits:
##       H.num.chars.log     < 3.156774 to the right, improve=2.973647, (0 missing)
##       A.num.chars.log     < 4.68674  to the left,  improve=2.606467, (0 missing)
##       S.num.chars.log     < 4.68674  to the left,  improve=2.606467, (0 missing)
##       A.num.words.unq.log < 2.602003 to the left,  improve=2.308909, (0 missing)
##       PubDate.hour        < 15.5     to the right, improve=2.234466, (0 missing)
##   Surrogate splits:
##       H.num.words.log     < 1.242453 to the right, agree=0.913, adj=0.154, (0 split)
##       H.num.words.unq.log < 1.242453 to the right, agree=0.913, adj=0.154, (0 split)
## 
## Node number 468: 7 observations
##   predicted class=N  expected loss=0.2857143  P(node) =0.001564246
##     class counts:     5     2
##    probabilities: 0.714 0.286 
## 
## Node number 469: 15 observations
##   predicted class=Y  expected loss=0.1333333  P(node) =0.003351955
##     class counts:     2    13
##    probabilities: 0.133 0.867 
## 
## Node number 488: 14 observations
##   predicted class=N  expected loss=0.3571429  P(node) =0.003128492
##     class counts:     9     5
##    probabilities: 0.643 0.357 
## 
## Node number 489: 8 observations
##   predicted class=Y  expected loss=0.125  P(node) =0.001787709
##     class counts:     1     7
##    probabilities: 0.125 0.875 
## 
## Node number 490: 7 observations
##   predicted class=N  expected loss=0.4285714  P(node) =0.001564246
##     class counts:     4     3
##    probabilities: 0.571 0.429 
## 
## Node number 491: 19 observations
##   predicted class=Y  expected loss=0  P(node) =0.00424581
##     class counts:     0    19
##    probabilities: 0.000 1.000 
## 
## Node number 492: 23 observations,    complexity param=0.001502003
##   predicted class=Y  expected loss=0.3913043  P(node) =0.005139665
##     class counts:     9    14
##    probabilities: 0.391 0.609 
##   left son=984 (8 obs) right son=985 (15 obs)
##   Primary splits:
##       WordCount.log   < 6.174632 to the right, improve=3.1565220, (0 missing)
##       PubDate.hour    < 12.5     to the right, improve=3.0815220, (0 missing)
##       PubDate.minute  < 11       to the right, improve=1.3398550, (0 missing)
##       H.num.chars.log < 3.930093 to the right, improve=1.3398550, (0 missing)
##       A.num.chars.log < 4.430179 to the right, improve=0.5928854, (0 missing)
##   Surrogate splits:
##       H.num.chars.log          < 3.596467 to the left,  agree=0.826, adj=0.500, (0 split)
##       PubDate.date.fctr(25,31] < 0.5      to the right, agree=0.739, adj=0.250, (0 split)
##       H.num.words.log          < 1.242453 to the left,  agree=0.739, adj=0.250, (0 split)
##       H.num.words.unq.log      < 1.242453 to the left,  agree=0.739, adj=0.250, (0 split)
##       PubDate.date.fctr(7,13]  < 0.5      to the right, agree=0.696, adj=0.125, (0 split)
## 
## Node number 493: 24 observations
##   predicted class=Y  expected loss=0.125  P(node) =0.005363128
##     class counts:     3    21
##    probabilities: 0.125 0.875 
## 
## Node number 494: 45 observations,    complexity param=0.001335113
##   predicted class=Y  expected loss=0.2  P(node) =0.01005587
##     class counts:     9    36
##    probabilities: 0.200 0.800 
##   left son=988 (28 obs) right son=989 (17 obs)
##   Primary splits:
##       H.num.chars.log          < 3.156774 to the right, improve=2.185714, (0 missing)
##       PubDate.second           < 13.5     to the left,  improve=1.751351, (0 missing)
##       PubDate.date.fctr(13,19] < 0.5      to the left,  improve=1.309091, (0 missing)
##       A.num.chars.log          < 3.941535 to the left,  improve=1.003687, (0 missing)
##       S.num.chars.log          < 3.941535 to the left,  improve=1.003687, (0 missing)
##   Surrogate splits:
##       H.num.words.log          < 1.242453 to the right, agree=0.778, adj=0.412, (0 split)
##       H.num.words.unq.log      < 1.242453 to the right, agree=0.778, adj=0.412, (0 split)
##       PubDate.second           < 39.5     to the left,  agree=0.733, adj=0.294, (0 split)
##       WordCount.log            < 7.44272  to the left,  agree=0.667, adj=0.118, (0 split)
##       PubDate.date.fctr(13,19] < 0.5      to the left,  agree=0.667, adj=0.118, (0 split)
## 
## Node number 495: 69 observations
##   predicted class=Y  expected loss=0.04347826  P(node) =0.01541899
##     class counts:     3    66
##    probabilities: 0.043 0.957 
## 
## Node number 522: 50 observations
##   predicted class=N  expected loss=0.06  P(node) =0.01117318
##     class counts:    47     3
##    probabilities: 0.940 0.060 
## 
## Node number 523: 32 observations,    complexity param=0.0004450378
##   predicted class=N  expected loss=0.3125  P(node) =0.007150838
##     class counts:    22    10
##    probabilities: 0.688 0.313 
##   left son=1046 (19 obs) right son=1047 (13 obs)
##   Primary splits:
##       PubDate.hour        < 14.5     to the left,  improve=2.235830, (0 missing)
##       A.num.words.log     < 2.441401 to the left,  improve=2.016667, (0 missing)
##       A.num.words.unq.log < 2.441401 to the left,  improve=2.016667, (0 missing)
##       S.num.words.log     < 2.441401 to the left,  improve=2.016667, (0 missing)
##       S.num.words.unq.log < 2.441401 to the left,  improve=2.016667, (0 missing)
##   Surrogate splits:
##       PubDate.apm.fctrpm       < 0.5      to the left,  agree=0.875, adj=0.692, (0 split)
##       PubDate.date.fctr(25,31] < 0.5      to the left,  agree=0.719, adj=0.308, (0 split)
##       PubDate.minute           < 10.5     to the right, agree=0.719, adj=0.308, (0 split)
##       WordCount.log            < 6.169609 to the left,  agree=0.656, adj=0.154, (0 split)
##       H.has.ebola              < 0.5      to the left,  agree=0.656, adj=0.154, (0 split)
## 
## Node number 640: 466 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.09012876  P(node) =0.1041341
##     class counts:   424    42
##    probabilities: 0.910 0.090 
##   left son=1280 (420 obs) right son=1281 (46 obs)
##   Primary splits:
##       SectionName.fctrTechnology              < 0.5      to the left,  improve=4.684257, (0 missing)
##       SubsectionName.fctrBusiness::Technology < 0.5      to the left,  improve=4.684257, (0 missing)
##       WordCount.log                           < 8.065446 to the left,  improve=1.628064, (0 missing)
##       H.num.chars.log                         < 2.740319 to the right, improve=1.628064, (0 missing)
##       S.compani                               < 0.5      to the left,  improve=1.539750, (0 missing)
##   Surrogate splits:
##       SubsectionName.fctrBusiness::Technology < 0.5      to the left,  agree=1.00, adj=1.000, (0 split)
##       PubDate.hour                            < 0.5      to the right, agree=0.91, adj=0.087, (0 split)
## 
## Node number 641: 211 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.2322275  P(node) =0.04715084
##     class counts:   162    49
##    probabilities: 0.768 0.232 
##   left son=1282 (78 obs) right son=1283 (133 obs)
##   Primary splits:
##       S.num.words.log     < 2.917405 to the right, improve=5.969294, (0 missing)
##       A.num.words.unq.log < 2.861793 to the right, improve=5.626078, (0 missing)
##       A.num.words.log     < 2.917405 to the right, improve=5.392883, (0 missing)
##       S.num.words.unq.log < 2.861793 to the right, improve=4.141049, (0 missing)
##       A.num.chars.log     < 5.244386 to the right, improve=3.732510, (0 missing)
##   Surrogate splits:
##       A.num.words.log     < 2.917405 to the right, agree=0.986, adj=0.962, (0 split)
##       S.num.words.unq.log < 2.917405 to the right, agree=0.948, adj=0.859, (0 split)
##       A.num.words.unq.log < 2.917405 to the right, agree=0.938, adj=0.833, (0 split)
##       A.num.chars.log     < 5.178967 to the right, agree=0.886, adj=0.692, (0 split)
##       S.num.chars.log     < 5.178967 to the right, agree=0.886, adj=0.692, (0 split)
## 
## Node number 654: 114 observations,    complexity param=0.003115265
##   predicted class=N  expected loss=0.4122807  P(node) =0.02547486
##     class counts:    67    47
##    probabilities: 0.588 0.412 
##   left son=1308 (102 obs) right son=1309 (12 obs)
##   Primary splits:
##       PubDate.second      < 52.5     to the left,  improve=3.059340, (0 missing)
##       H.num.chars.log     < 3.481122 to the left,  improve=1.900895, (0 missing)
##       PubDate.wkday.fctr3 < 0.5      to the right, improve=1.601170, (0 missing)
##       S.make              < 0.5      to the left,  improve=1.360434, (0 missing)
##       A.num.chars.log     < 5.24701  to the right, improve=1.293275, (0 missing)
##   Surrogate splits:
##       A.num.words.unq.log < 3.067782 to the left,  agree=0.912, adj=0.167, (0 split)
##       S.num.words.unq.log < 3.067782 to the left,  agree=0.904, adj=0.083, (0 split)
## 
## Node number 655: 13 observations
##   predicted class=Y  expected loss=0.2307692  P(node) =0.002905028
##     class counts:     3    10
##    probabilities: 0.231 0.769 
## 
## Node number 984: 8 observations
##   predicted class=N  expected loss=0.25  P(node) =0.001787709
##     class counts:     6     2
##    probabilities: 0.750 0.250 
## 
## Node number 985: 15 observations
##   predicted class=Y  expected loss=0.2  P(node) =0.003351955
##     class counts:     3    12
##    probabilities: 0.200 0.800 
## 
## Node number 988: 28 observations,    complexity param=0.001335113
##   predicted class=Y  expected loss=0.3214286  P(node) =0.006256983
##     class counts:     9    19
##    probabilities: 0.321 0.679 
##   left son=1976 (7 obs) right son=1977 (21 obs)
##   Primary splits:
##       H.num.words.log     < 1.242453 to the left,  improve=2.8809520, (0 missing)
##       H.num.words.unq.log < 1.242453 to the left,  improve=2.8809520, (0 missing)
##       PubDate.second      < 19.5     to the left,  improve=1.4540520, (0 missing)
##       PubDate.hour        < 16       to the left,  improve=1.3630040, (0 missing)
##       A.num.chars.log     < 3.941535 to the left,  improve=0.9920635, (0 missing)
##   Surrogate splits:
##       H.num.words.unq.log < 1.242453 to the left,  agree=1.000, adj=1.000, (0 split)
##       S.make              < 0.5      to the right, agree=0.786, adj=0.143, (0 split)
##       PubDate.second      < 9.5      to the left,  agree=0.786, adj=0.143, (0 split)
##       PubDate.minute      < 41       to the right, agree=0.786, adj=0.143, (0 split)
## 
## Node number 989: 17 observations
##   predicted class=Y  expected loss=0  P(node) =0.003798883
##     class counts:     0    17
##    probabilities: 0.000 1.000 
## 
## Node number 1046: 19 observations
##   predicted class=N  expected loss=0.1578947  P(node) =0.00424581
##     class counts:    16     3
##    probabilities: 0.842 0.158 
## 
## Node number 1047: 13 observations
##   predicted class=Y  expected loss=0.4615385  P(node) =0.002905028
##     class counts:     6     7
##    probabilities: 0.462 0.538 
## 
## Node number 1280: 420 observations,    complexity param=0.0003337784
##   predicted class=N  expected loss=0.06666667  P(node) =0.09385475
##     class counts:   392    28
##    probabilities: 0.933 0.067 
##   left son=2560 (413 obs) right son=2561 (7 obs)
##   Primary splits:
##       H.num.chars.log     < 2.740319 to the right, improve=1.8647300, (0 missing)
##       PubDate.minute      < 6.5      to the right, improve=1.1568660, (0 missing)
##       PubDate.wkday.fctr1 < 0.5      to the left,  improve=0.9291901, (0 missing)
##       PubDate.hour        < 8.5      to the left,  improve=0.8809417, (0 missing)
##       WordCount.log       < 7.925871 to the left,  improve=0.8303922, (0 missing)
## 
## Node number 1281: 46 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.3043478  P(node) =0.01027933
##     class counts:    32    14
##    probabilities: 0.696 0.304 
##   left son=2562 (36 obs) right son=2563 (10 obs)
##   Primary splits:
##       WordCount.log       < 6.864843 to the left,  improve=4.000483, (0 missing)
##       PubDate.minute      < 41       to the left,  improve=2.937720, (0 missing)
##       PubDate.second      < 41       to the right, improve=1.874531, (0 missing)
##       H.num.chars.log     < 4.151009 to the left,  improve=1.490431, (0 missing)
##       PubDate.wkday.fctr2 < 0.5      to the left,  improve=1.412195, (0 missing)
##   Surrogate splits:
##       H.num.chars.log     < 4.337098 to the left,  agree=0.804, adj=0.1, (0 split)
##       A.num.chars.log     < 4.82391  to the right, agree=0.804, adj=0.1, (0 split)
##       S.num.chars.log     < 4.82391  to the right, agree=0.804, adj=0.1, (0 split)
##       S.num.words.unq.log < 2.970086 to the left,  agree=0.804, adj=0.1, (0 split)
## 
## Node number 1282: 78 observations
##   predicted class=N  expected loss=0.07692308  P(node) =0.01743017
##     class counts:    72     6
##    probabilities: 0.923 0.077 
## 
## Node number 1283: 133 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.3233083  P(node) =0.02972067
##     class counts:    90    43
##    probabilities: 0.677 0.323 
##   left son=2566 (44 obs) right son=2567 (89 obs)
##   Primary splits:
##       WordCount.log   < 6.836789 to the left,  improve=4.595897, (0 missing)
##       A.num.chars.log < 5.135781 to the left,  improve=2.382155, (0 missing)
##       S.num.chars.log < 5.135781 to the left,  improve=2.382155, (0 missing)
##       S.said          < 0.5      to the left,  improve=1.783643, (0 missing)
##       S.num.words.log < 2.441401 to the left,  improve=1.078416, (0 missing)
##   Surrogate splits:
##       S.report        < 0.5      to the right, agree=0.699, adj=0.091, (0 split)
##       A.num.chars.log < 4.722943 to the left,  agree=0.692, adj=0.068, (0 split)
##       S.num.chars.log < 4.722943 to the left,  agree=0.692, adj=0.068, (0 split)
##       PubDate.hour    < 1.5      to the left,  agree=0.684, adj=0.045, (0 split)
##       H.num.chars.log < 3.55494  to the left,  agree=0.684, adj=0.045, (0 split)
## 
## Node number 1308: 102 observations,    complexity param=0.003115265
##   predicted class=N  expected loss=0.372549  P(node) =0.0227933
##     class counts:    64    38
##    probabilities: 0.627 0.373 
##   left son=2616 (16 obs) right son=2617 (86 obs)
##   Primary splits:
##       PubDate.second  < 47.5     to the right, improve=2.325809, (0 missing)
##       S.time          < 0.5      to the right, improve=1.956005, (0 missing)
##       A.time          < 0.5      to the right, improve=1.956005, (0 missing)
##       A.num.chars.log < 5.220341 to the right, improve=1.956005, (0 missing)
##       S.num.chars.log < 5.220341 to the right, improve=1.956005, (0 missing)
##   Surrogate splits:
##       H.has.ebola < 0.5      to the right, agree=0.853, adj=0.062, (0 split)
## 
## Node number 1309: 12 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.002681564
##     class counts:     3     9
##    probabilities: 0.250 0.750 
## 
## Node number 1976: 7 observations
##   predicted class=N  expected loss=0.2857143  P(node) =0.001564246
##     class counts:     5     2
##    probabilities: 0.714 0.286 
## 
## Node number 1977: 21 observations
##   predicted class=Y  expected loss=0.1904762  P(node) =0.004692737
##     class counts:     4    17
##    probabilities: 0.190 0.810 
## 
## Node number 2560: 413 observations,    complexity param=0.0003337784
##   predicted class=N  expected loss=0.06053269  P(node) =0.0922905
##     class counts:   388    25
##    probabilities: 0.939 0.061 
##   left son=5120 (320 obs) right son=5121 (93 obs)
##   Primary splits:
##       PubDate.wkday.fctr1 < 0.5      to the left,  improve=1.1263900, (0 missing)
##       PubDate.minute      < 6.5      to the right, improve=0.7652375, (0 missing)
##       WordCount.log       < 8.01605  to the left,  improve=0.7221341, (0 missing)
##       S.articl            < 0.5      to the left,  improve=0.7221341, (0 missing)
##       A.articl            < 0.5      to the left,  improve=0.7221341, (0 missing)
##   Surrogate splits:
##       A.num.chars.log < 3.806416 to the right, agree=0.782, adj=0.032, (0 split)
##       S.num.chars.log < 3.806416 to the right, agree=0.782, adj=0.032, (0 split)
##       WordCount.log   < 8.100634 to the left,  agree=0.780, adj=0.022, (0 split)
##       PubDate.hour    < 20.5     to the left,  agree=0.780, adj=0.022, (0 split)
##       S.num.words.log < 1.868835 to the right, agree=0.780, adj=0.022, (0 split)
## 
## Node number 2561: 7 observations
##   predicted class=N  expected loss=0.4285714  P(node) =0.001564246
##     class counts:     4     3
##    probabilities: 0.571 0.429 
## 
## Node number 2562: 36 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.1944444  P(node) =0.008044693
##     class counts:    29     7
##    probabilities: 0.806 0.194 
##   left son=5124 (29 obs) right son=5125 (7 obs)
##   Primary splits:
##       PubDate.minute           < 41       to the left,  improve=2.4698960, (0 missing)
##       WordCount.log            < 6.714152 to the right, improve=1.3611110, (0 missing)
##       PubDate.second           < 15.5     to the left,  improve=1.0470090, (0 missing)
##       PubDate.date.fctr(19,25] < 0.5      to the left,  improve=0.9526546, (0 missing)
##       S.compani                < 0.5      to the left,  improve=0.9526546, (0 missing)
##   Surrogate splits:
##       S.state             < 0.5      to the left,  agree=0.861, adj=0.286, (0 split)
##       S.year              < 0.5      to the left,  agree=0.861, adj=0.286, (0 split)
##       S.said              < 0.5      to the left,  agree=0.833, adj=0.143, (0 split)
##       PubDate.wkday.fctr2 < 0.5      to the left,  agree=0.833, adj=0.143, (0 split)
## 
## Node number 2563: 10 observations
##   predicted class=Y  expected loss=0.3  P(node) =0.002234637
##     class counts:     3     7
##    probabilities: 0.300 0.700 
## 
## Node number 2566: 44 observations
##   predicted class=N  expected loss=0.1363636  P(node) =0.009832402
##     class counts:    38     6
##    probabilities: 0.864 0.136 
## 
## Node number 2567: 89 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.4157303  P(node) =0.01988827
##     class counts:    52    37
##    probabilities: 0.584 0.416 
##   left son=5134 (78 obs) right son=5135 (11 obs)
##   Primary splits:
##       A.num.chars.log < 4.766429 to the right, improve=2.436421, (0 missing)
##       S.num.chars.log < 4.766429 to the right, improve=2.436421, (0 missing)
##       WordCount.log   < 7.168194 to the left,  improve=2.309736, (0 missing)
##       PubDate.hour    < 12.5     to the right, improve=2.309736, (0 missing)
##       H.num.chars.log < 3.901922 to the right, improve=1.722653, (0 missing)
##   Surrogate splits:
##       S.num.chars.log     < 4.766429 to the right, agree=1.000, adj=1.000, (0 split)
##       A.num.words.log     < 2.35024  to the right, agree=0.910, adj=0.273, (0 split)
##       S.num.words.log     < 2.35024  to the right, agree=0.910, adj=0.273, (0 split)
##       S.num.words.unq.log < 2.35024  to the right, agree=0.910, adj=0.273, (0 split)
##       A.num.words.unq.log < 2.249905 to the right, agree=0.899, adj=0.182, (0 split)
## 
## Node number 2616: 16 observations
##   predicted class=N  expected loss=0.125  P(node) =0.003575419
##     class counts:    14     2
##    probabilities: 0.875 0.125 
## 
## Node number 2617: 86 observations,    complexity param=0.003115265
##   predicted class=N  expected loss=0.4186047  P(node) =0.01921788
##     class counts:    50    36
##    probabilities: 0.581 0.419 
##   left son=5234 (18 obs) right son=5235 (68 obs)
##   Primary splits:
##       PubDate.wkday.fctr3 < 0.5      to the right, improve=1.755890, (0 missing)
##       A.num.chars.log     < 5.220341 to the right, improve=1.520722, (0 missing)
##       S.num.chars.log     < 5.220341 to the right, improve=1.520722, (0 missing)
##       S.time              < 0.5      to the right, improve=1.158838, (0 missing)
##       A.time              < 0.5      to the right, improve=1.158838, (0 missing)
##   Surrogate splits:
##       WordCount.log  < 6.543171 to the left,  agree=0.814, adj=0.111, (0 split)
##       S.day          < 0.5      to the right, agree=0.814, adj=0.111, (0 split)
##       A.day          < 0.5      to the right, agree=0.814, adj=0.111, (0 split)
##       PubDate.second < 41       to the right, agree=0.802, adj=0.056, (0 split)
## 
## Node number 5120: 320 observations
##   predicted class=N  expected loss=0.040625  P(node) =0.07150838
##     class counts:   307    13
##    probabilities: 0.959 0.041 
## 
## Node number 5121: 93 observations,    complexity param=0.0003337784
##   predicted class=N  expected loss=0.1290323  P(node) =0.02078212
##     class counts:    81    12
##    probabilities: 0.871 0.129 
##   left son=10242 (51 obs) right son=10243 (42 obs)
##   Primary splits:
##       PubDate.minute  < 1.5      to the right, improve=2.704346, (0 missing)
##       A.num.chars.log < 4.714015 to the right, improve=1.973016, (0 missing)
##       S.num.chars.log < 4.714015 to the right, improve=1.973016, (0 missing)
##       PubDate.second  < 53.5     to the left,  improve=1.358375, (0 missing)
##       WordCount.log   < 7.136761 to the left,  improve=1.210305, (0 missing)
##   Surrogate splits:
##       SectionName.fctrMultimedia            < 0.5      to the left,  agree=0.645, adj=0.214, (0 split)
##       SubsectionName.fctrmyMisc::Multimedia < 0.5      to the left,  agree=0.645, adj=0.214, (0 split)
##       SectionName.fctrBusiness Day          < 0.5      to the left,  agree=0.624, adj=0.167, (0 split)
##       SubsectionName.fctrSmall Business     < 0.5      to the left,  agree=0.624, adj=0.167, (0 split)
##       PubDate.hour                          < 13.5     to the right, agree=0.602, adj=0.119, (0 split)
## 
## Node number 5124: 29 observations
##   predicted class=N  expected loss=0.1034483  P(node) =0.006480447
##     class counts:    26     3
##    probabilities: 0.897 0.103 
## 
## Node number 5125: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.001564246
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 5134: 78 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.3717949  P(node) =0.01743017
##     class counts:    49    29
##    probabilities: 0.628 0.372 
##   left son=10268 (58 obs) right son=10269 (20 obs)
##   Primary splits:
##       PubDate.hour       < 12.5     to the right, improve=2.801415, (0 missing)
##       WordCount.log      < 7.168194 to the left,  improve=2.290534, (0 missing)
##       A.num.chars.log    < 5.135781 to the left,  improve=2.036669, (0 missing)
##       S.num.chars.log    < 5.135781 to the left,  improve=2.036669, (0 missing)
##       PubDate.apm.fctrpm < 0.5      to the right, improve=1.851282, (0 missing)
##   Surrogate splits:
##       PubDate.apm.fctrpm < 0.5      to the right, agree=0.910, adj=0.65, (0 split)
##       PubDate.minute     < 1        to the right, agree=0.756, adj=0.05, (0 split)
##       H.num.chars.log    < 4.375579 to the left,  agree=0.756, adj=0.05, (0 split)
##       S.num.words.log    < 2.524928 to the right, agree=0.756, adj=0.05, (0 split)
## 
## Node number 5135: 11 observations
##   predicted class=Y  expected loss=0.2727273  P(node) =0.002458101
##     class counts:     3     8
##    probabilities: 0.273 0.727 
## 
## Node number 5234: 18 observations
##   predicted class=N  expected loss=0.2222222  P(node) =0.004022346
##     class counts:    14     4
##    probabilities: 0.778 0.222 
## 
## Node number 5235: 68 observations,    complexity param=0.003115265
##   predicted class=N  expected loss=0.4705882  P(node) =0.01519553
##     class counts:    36    32
##    probabilities: 0.529 0.471 
##   left son=10470 (25 obs) right son=10471 (43 obs)
##   Primary splits:
##       PubDate.minute  < 25       to the right, improve=2.872120, (0 missing)
##       A.num.chars.log < 5.233605 to the right, improve=1.676264, (0 missing)
##       S.num.chars.log < 5.233605 to the right, improve=1.676264, (0 missing)
##       S.num.words.log < 2.917405 to the right, improve=1.418067, (0 missing)
##       H.num.chars.log < 3.481122 to the left,  improve=1.411765, (0 missing)
##   Surrogate splits:
##       WordCount.log   < 7.231285 to the right, agree=0.676, adj=0.12, (0 split)
##       PubDate.second  < 5.5      to the left,  agree=0.676, adj=0.12, (0 split)
##       S.week          < 0.5      to the right, agree=0.676, adj=0.12, (0 split)
##       A.num.chars.log < 5.132849 to the right, agree=0.676, adj=0.12, (0 split)
##       S.num.chars.log < 5.132849 to the right, agree=0.676, adj=0.12, (0 split)
## 
## Node number 10242: 51 observations
##   predicted class=N  expected loss=0.01960784  P(node) =0.01139665
##     class counts:    50     1
##    probabilities: 0.980 0.020 
## 
## Node number 10243: 42 observations,    complexity param=0.0003337784
##   predicted class=N  expected loss=0.2619048  P(node) =0.009385475
##     class counts:    31    11
##    probabilities: 0.738 0.262 
##   left son=20486 (27 obs) right son=20487 (15 obs)
##   Primary splits:
##       WordCount.log      < 7.117409 to the left,  improve=3.438095, (0 missing)
##       PubDate.hour       < 11       to the left,  improve=2.930403, (0 missing)
##       PubDate.apm.fctrpm < 0.5      to the left,  improve=2.930403, (0 missing)
##       A.num.chars.log    < 4.682088 to the right, improve=2.880005, (0 missing)
##       S.num.chars.log    < 4.682088 to the right, improve=2.880005, (0 missing)
##   Surrogate splits:
##       PubDate.hour       < 8        to the left,  agree=0.786, adj=0.400, (0 split)
##       PubDate.apm.fctrpm < 0.5      to the left,  agree=0.786, adj=0.400, (0 split)
##       H.num.chars.log    < 3.331566 to the right, agree=0.762, adj=0.333, (0 split)
##       A.num.chars.log    < 4.394144 to the right, agree=0.738, adj=0.267, (0 split)
##       S.num.chars.log    < 4.394144 to the right, agree=0.738, adj=0.267, (0 split)
## 
## Node number 10268: 58 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.2931034  P(node) =0.01296089
##     class counts:    41    17
##    probabilities: 0.707 0.293 
##   left son=20536 (50 obs) right son=20537 (8 obs)
##   Primary splits:
##       WordCount.log   < 6.933411 to the right, improve=2.044483, (0 missing)
##       PubDate.hour    < 14.5     to the left,  improve=1.830401, (0 missing)
##       PubDate.minute  < 14.5     to the left,  improve=1.728927, (0 missing)
##       A.num.chars.log < 5.178871 to the left,  improve=1.467589, (0 missing)
##       S.num.chars.log < 5.178871 to the left,  improve=1.467589, (0 missing)
##   Surrogate splits:
##       H.num.chars.log < 3.700616 to the right, agree=0.879, adj=0.125, (0 split)
## 
## Node number 10269: 20 observations,    complexity param=0.00200267
##   predicted class=Y  expected loss=0.4  P(node) =0.004469274
##     class counts:     8    12
##    probabilities: 0.400 0.600 
##   left son=20538 (11 obs) right son=20539 (9 obs)
##   Primary splits:
##       H.num.words.log     < 2.012676 to the right, improve=2.731313, (0 missing)
##       H.num.words.unq.log < 2.012676 to the right, improve=2.731313, (0 missing)
##       A.num.words.unq.log < 2.673554 to the left,  improve=2.327273, (0 missing)
##       H.num.chars.log     < 4.06899  to the right, improve=2.127473, (0 missing)
##       A.num.chars.log     < 5.078289 to the left,  improve=2.016667, (0 missing)
##   Surrogate splits:
##       H.num.words.unq.log      < 2.012676 to the right, agree=1.00, adj=1.000, (0 split)
##       H.num.chars.log          < 4.06899  to the right, agree=0.80, adj=0.556, (0 split)
##       PubDate.date.fctr(19,25] < 0.5      to the left,  agree=0.75, adj=0.444, (0 split)
##       PubDate.date.fctr(7,13]  < 0.5      to the right, agree=0.70, adj=0.333, (0 split)
##       PubDate.second           < 45.5     to the left,  agree=0.70, adj=0.333, (0 split)
## 
## Node number 10470: 25 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.28  P(node) =0.005586592
##     class counts:    18     7
##    probabilities: 0.720 0.280 
##   left son=20940 (18 obs) right son=20941 (7 obs)
##   Primary splits:
##       PubDate.date.fctr(7,13] < 0.5      to the left,  improve=1.6514290, (0 missing)
##       PubDate.hour            < 10       to the left,  improve=1.5244440, (0 missing)
##       PubDate.apm.fctrpm      < 0.5      to the left,  improve=0.7605556, (0 missing)
##       PubDate.second          < 9        to the left,  improve=0.5652941, (0 missing)
##       H.num.chars.log         < 3.481122 to the left,  improve=0.5652941, (0 missing)
##   Surrogate splits:
##       PubDate.minute  < 39.5     to the left,  agree=0.80, adj=0.286, (0 split)
##       A.num.chars.log < 4.418187 to the right, agree=0.80, adj=0.286, (0 split)
##       S.num.chars.log < 4.418187 to the right, agree=0.80, adj=0.286, (0 split)
##       PubDate.second  < 3        to the right, agree=0.76, adj=0.143, (0 split)
## 
## Node number 10471: 43 observations,    complexity param=0.003115265
##   predicted class=Y  expected loss=0.4186047  P(node) =0.009608939
##     class counts:    18    25
##    probabilities: 0.419 0.581 
##   left son=20942 (14 obs) right son=20943 (29 obs)
##   Primary splits:
##       PubDate.hour             < 13.5     to the right, improve=2.0878680, (0 missing)
##       PubDate.apm.fctrpm       < 0.5      to the right, improve=1.1613440, (0 missing)
##       PubDate.second           < 20.5     to the right, improve=1.0635660, (0 missing)
##       PubDate.date.fctr(19,25] < 0.5      to the right, improve=0.8575053, (0 missing)
##       S.new                    < 0.5      to the right, improve=0.8373754, (0 missing)
##   Surrogate splits:
##       PubDate.apm.fctrpm       < 0.5      to the right, agree=0.907, adj=0.714, (0 split)
##       WordCount.log            < 6.707458 to the left,  agree=0.744, adj=0.214, (0 split)
##       PubDate.date.fctr(25,31] < 0.5      to the right, agree=0.744, adj=0.214, (0 split)
##       PubDate.second           < 45.5     to the right, agree=0.721, adj=0.143, (0 split)
##       S.time                   < 0.5      to the right, agree=0.698, adj=0.071, (0 split)
## 
## Node number 20486: 27 observations
##   predicted class=N  expected loss=0.1111111  P(node) =0.00603352
##     class counts:    24     3
##    probabilities: 0.889 0.111 
## 
## Node number 20487: 15 observations
##   predicted class=Y  expected loss=0.4666667  P(node) =0.003351955
##     class counts:     7     8
##    probabilities: 0.467 0.533 
## 
## Node number 20536: 50 observations
##   predicted class=N  expected loss=0.24  P(node) =0.01117318
##     class counts:    38    12
##    probabilities: 0.760 0.240 
## 
## Node number 20537: 8 observations
##   predicted class=Y  expected loss=0.375  P(node) =0.001787709
##     class counts:     3     5
##    probabilities: 0.375 0.625 
## 
## Node number 20538: 11 observations
##   predicted class=N  expected loss=0.3636364  P(node) =0.002458101
##     class counts:     7     4
##    probabilities: 0.636 0.364 
## 
## Node number 20539: 9 observations
##   predicted class=Y  expected loss=0.1111111  P(node) =0.002011173
##     class counts:     1     8
##    probabilities: 0.111 0.889 
## 
## Node number 20940: 18 observations
##   predicted class=N  expected loss=0.1666667  P(node) =0.004022346
##     class counts:    15     3
##    probabilities: 0.833 0.167 
## 
## Node number 20941: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.001564246
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 20942: 14 observations
##   predicted class=N  expected loss=0.3571429  P(node) =0.003128492
##     class counts:     9     5
##    probabilities: 0.643 0.357 
## 
## Node number 20943: 29 observations,    complexity param=0.001335113
##   predicted class=Y  expected loss=0.3103448  P(node) =0.006480447
##     class counts:     9    20
##    probabilities: 0.310 0.690 
##   left son=41886 (7 obs) right son=41887 (22 obs)
##   Primary splits:
##       S.new           < 0.5      to the right, improve=1.2579490, (0 missing)
##       A.new           < 0.5      to the right, improve=1.2579490, (0 missing)
##       PubDate.second  < 19.5     to the right, improve=1.0360150, (0 missing)
##       H.num.chars.log < 3.511434 to the left,  improve=0.7947455, (0 missing)
##       WordCount.log   < 6.791271 to the right, improve=0.7590312, (0 missing)
##   Surrogate splits:
##       A.new          < 0.5      to the right, agree=1.000, adj=1.000, (0 split)
##       WordCount.log  < 6.650941 to the left,  agree=0.793, adj=0.143, (0 split)
##       S.presid       < 0.5      to the right, agree=0.793, adj=0.143, (0 split)
##       A.presid       < 0.5      to the right, agree=0.793, adj=0.143, (0 split)
##       PubDate.minute < 22.5     to the right, agree=0.793, adj=0.143, (0 split)
## 
## Node number 41886: 7 observations
##   predicted class=N  expected loss=0.4285714  P(node) =0.001564246
##     class counts:     4     3
##    probabilities: 0.571 0.429 
## 
## Node number 41887: 22 observations
##   predicted class=Y  expected loss=0.2272727  P(node) =0.004916201
##     class counts:     5    17
##    probabilities: 0.227 0.773 
## 
## n= 4475 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##     1) root 4475 749 N (0.83262570 0.16737430)  
##       2) NewsDesk.fctrOpEd< 0.5 4131 480 N (0.88380537 0.11619463)  
##         4) WordCount.log< 6.528688 3051 152 N (0.95018027 0.04981973)  
##           8) SubsectionName.fctrStyles::U.S.< 0.5 2989 120 N (0.95985279 0.04014721)  
##            16) PubDate.hour< 21.5 2911  86 N (0.97045689 0.02954311)  
##              32) NewsDesk.fctrScience< 0.5 2853  69 N (0.97581493 0.02418507)  
##                64) WordCount.log< 5.985194 2095  24 N (0.98854415 0.01145585) *
##                65) WordCount.log>=5.985194 758  45 N (0.94063325 0.05936675)  
##                 130) A.num.chars.log>=4.051747 747  37 N (0.95046854 0.04953146)  
##                   260) SectionName.fctrmyMisc::< 0.5 665  24 N (0.96390977 0.03609023) *
##                   261) SectionName.fctrmyMisc::>=0.5 82  13 N (0.84146341 0.15853659)  
##                     522) PubDate.second>=17 50   3 N (0.94000000 0.06000000) *
##                     523) PubDate.second< 17 32  10 N (0.68750000 0.31250000)  
##                      1046) PubDate.hour< 14.5 19   3 N (0.84210526 0.15789474) *
##                      1047) PubDate.hour>=14.5 13   6 Y (0.46153846 0.53846154) *
##                 131) A.num.chars.log< 4.051747 11   3 Y (0.27272727 0.72727273) *
##              33) NewsDesk.fctrScience>=0.5 58  17 N (0.70689655 0.29310345)  
##                66) PubDate.hour>=9.5 48  10 N (0.79166667 0.20833333) *
##                67) PubDate.hour< 9.5 10   3 Y (0.30000000 0.70000000) *
##            17) PubDate.hour>=21.5 78  34 N (0.56410256 0.43589744)  
##              34) PubDate.minute>=0.5 44   5 N (0.88636364 0.11363636) *
##              35) PubDate.minute< 0.5 34   5 Y (0.14705882 0.85294118)  
##                70) WordCount.log< 5.794443 7   3 N (0.57142857 0.42857143) *
##                71) WordCount.log>=5.794443 27   1 Y (0.03703704 0.96296296) *
##           9) SubsectionName.fctrStyles::U.S.>=0.5 62  30 Y (0.48387097 0.51612903)  
##            18) PubDate.wkday.fctr1< 0.5 47  20 N (0.57446809 0.42553191)  
##              36) PubDate.second>=8.5 40  14 N (0.65000000 0.35000000)  
##                72) WordCount.log< 6.299797 29   6 N (0.79310345 0.20689655) *
##                73) WordCount.log>=6.299797 11   3 Y (0.27272727 0.72727273) *
##              37) PubDate.second< 8.5 7   1 Y (0.14285714 0.85714286) *
##            19) PubDate.wkday.fctr1>=0.5 15   3 Y (0.20000000 0.80000000) *
##         5) WordCount.log>=6.528688 1080 328 N (0.69629630 0.30370370)  
##          10) SectionName.fctrHealth< 0.5 1009 263 N (0.73934589 0.26065411)  
##            20) SubsectionName.fctrStyles::U.S.< 0.5 944 220 N (0.76694915 0.23305085)  
##              40) PubDate.hour< 21.5 878 177 N (0.79840547 0.20159453)  
##                80) SectionName.fctrmyMisc::< 0.5 705 112 N (0.84113475 0.15886525)  
##                 160) PubDate.wkday.fctr6< 0.5 688 100 N (0.85465116 0.14534884)  
##                   320) SubsectionName.fctrThe Public Editor< 0.5 677  91 N (0.86558346 0.13441654)  
##                     640) SubsectionName.fctrDealbook< 0.5 466  42 N (0.90987124 0.09012876)  
##                      1280) SectionName.fctrTechnology< 0.5 420  28 N (0.93333333 0.06666667)  
##                        2560) H.num.chars.log>=2.740319 413  25 N (0.93946731 0.06053269)  
##                          5120) PubDate.wkday.fctr1< 0.5 320  13 N (0.95937500 0.04062500) *
##                          5121) PubDate.wkday.fctr1>=0.5 93  12 N (0.87096774 0.12903226)  
##                           10242) PubDate.minute>=1.5 51   1 N (0.98039216 0.01960784) *
##                           10243) PubDate.minute< 1.5 42  11 N (0.73809524 0.26190476)  
##                             20486) WordCount.log< 7.117409 27   3 N (0.88888889 0.11111111) *
##                             20487) WordCount.log>=7.117409 15   7 Y (0.46666667 0.53333333) *
##                        2561) H.num.chars.log< 2.740319 7   3 N (0.57142857 0.42857143) *
##                      1281) SectionName.fctrTechnology>=0.5 46  14 N (0.69565217 0.30434783)  
##                        2562) WordCount.log< 6.864843 36   7 N (0.80555556 0.19444444)  
##                          5124) PubDate.minute< 41 29   3 N (0.89655172 0.10344828) *
##                          5125) PubDate.minute>=41 7   3 Y (0.42857143 0.57142857) *
##                        2563) WordCount.log>=6.864843 10   3 Y (0.30000000 0.70000000) *
##                     641) SubsectionName.fctrDealbook>=0.5 211  49 N (0.76777251 0.23222749)  
##                      1282) S.num.words.log>=2.917405 78   6 N (0.92307692 0.07692308) *
##                      1283) S.num.words.log< 2.917405 133  43 N (0.67669173 0.32330827)  
##                        2566) WordCount.log< 6.836789 44   6 N (0.86363636 0.13636364) *
##                        2567) WordCount.log>=6.836789 89  37 N (0.58426966 0.41573034)  
##                          5134) A.num.chars.log>=4.766429 78  29 N (0.62820513 0.37179487)  
##                           10268) PubDate.hour>=12.5 58  17 N (0.70689655 0.29310345)  
##                             20536) WordCount.log>=6.933411 50  12 N (0.76000000 0.24000000) *
##                             20537) WordCount.log< 6.933411 8   3 Y (0.37500000 0.62500000) *
##                           10269) PubDate.hour< 12.5 20   8 Y (0.40000000 0.60000000)  
##                             20538) H.num.words.log>=2.012676 11   4 N (0.63636364 0.36363636) *
##                             20539) H.num.words.log< 2.012676 9   1 Y (0.11111111 0.88888889) *
##                          5135) A.num.chars.log< 4.766429 11   3 Y (0.27272727 0.72727273) *
##                   321) SubsectionName.fctrThe Public Editor>=0.5 11   2 Y (0.18181818 0.81818182) *
##                 161) PubDate.wkday.fctr6>=0.5 17   5 Y (0.29411765 0.70588235) *
##                81) SectionName.fctrmyMisc::>=0.5 173  65 N (0.62427746 0.37572254)  
##                 162) S.report>=0.5 16   1 N (0.93750000 0.06250000) *
##                 163) S.report< 0.5 157  64 N (0.59235669 0.40764331)  
##                   326) PubDate.minute>=40.5 30   7 N (0.76666667 0.23333333) *
##                   327) PubDate.minute< 40.5 127  57 N (0.55118110 0.44881890)  
##                     654) H.num.chars.log>=3.156774 114  47 N (0.58771930 0.41228070)  
##                      1308) PubDate.second< 52.5 102  38 N (0.62745098 0.37254902)  
##                        2616) PubDate.second>=47.5 16   2 N (0.87500000 0.12500000) *
##                        2617) PubDate.second< 47.5 86  36 N (0.58139535 0.41860465)  
##                          5234) PubDate.wkday.fctr3>=0.5 18   4 N (0.77777778 0.22222222) *
##                          5235) PubDate.wkday.fctr3< 0.5 68  32 N (0.52941176 0.47058824)  
##                           10470) PubDate.minute>=25 25   7 N (0.72000000 0.28000000)  
##                             20940) PubDate.date.fctr(7,13]< 0.5 18   3 N (0.83333333 0.16666667) *
##                             20941) PubDate.date.fctr(7,13]>=0.5 7   3 Y (0.42857143 0.57142857) *
##                           10471) PubDate.minute< 25 43  18 Y (0.41860465 0.58139535)  
##                             20942) PubDate.hour>=13.5 14   5 N (0.64285714 0.35714286) *
##                             20943) PubDate.hour< 13.5 29   9 Y (0.31034483 0.68965517)  
##                               41886) S.new>=0.5 7   3 N (0.57142857 0.42857143) *
##                               41887) S.new< 0.5 22   5 Y (0.22727273 0.77272727) *
##                      1309) PubDate.second>=52.5 12   3 Y (0.25000000 0.75000000) *
##                     655) H.num.chars.log< 3.156774 13   3 Y (0.23076923 0.76923077) *
##              41) PubDate.hour>=21.5 66  23 Y (0.34848485 0.65151515)  
##                82) A.num.chars.log>=4.532368 26   8 N (0.69230769 0.30769231)  
##                 164) A.num.words.log< 2.861793 19   4 N (0.78947368 0.21052632) *
##                 165) A.num.words.log>=2.861793 7   3 Y (0.42857143 0.57142857) *
##                83) A.num.chars.log< 4.532368 40   5 Y (0.12500000 0.87500000) *
##            21) SubsectionName.fctrStyles::U.S.>=0.5 65  22 Y (0.33846154 0.66153846)  
##              42) PubDate.wkday.fctr2>=0.5 14   4 N (0.71428571 0.28571429) *
##              43) PubDate.wkday.fctr2< 0.5 51  12 Y (0.23529412 0.76470588)  
##                86) A.num.chars.log< 5.167635 30  10 Y (0.33333333 0.66666667)  
##                 172) A.num.words.log>=2.673554 13   5 N (0.61538462 0.38461538) *
##                 173) A.num.words.log< 2.673554 17   2 Y (0.11764706 0.88235294) *
##                87) A.num.chars.log>=5.167635 21   2 Y (0.09523810 0.90476190) *
##          11) SectionName.fctrHealth>=0.5 71   6 Y (0.08450704 0.91549296) *
##       3) NewsDesk.fctrOpEd>=0.5 344  75 Y (0.21802326 0.78197674)  
##         6) A.num.chars.log>=5.138628 8   1 N (0.87500000 0.12500000) *
##         7) A.num.chars.log< 5.138628 336  68 Y (0.20238095 0.79761905)  
##          14) PubDate.wkday.fctr5>=0.5 67  24 Y (0.35820896 0.64179104)  
##            28) PubDate.hour>=17.5 14   2 N (0.85714286 0.14285714) *
##            29) PubDate.hour< 17.5 53  12 Y (0.22641509 0.77358491)  
##              58) PubDate.second< 47 41  12 Y (0.29268293 0.70731707)  
##               116) PubDate.date.fctr(13,19]>=0.5 8   3 N (0.62500000 0.37500000) *
##               117) PubDate.date.fctr(13,19]< 0.5 33   7 Y (0.21212121 0.78787879)  
##                 234) A.num.words.log>=1.868835 22   7 Y (0.31818182 0.68181818)  
##                   468) A.num.chars.log< 4.329954 7   2 N (0.71428571 0.28571429) *
##                   469) A.num.chars.log>=4.329954 15   2 Y (0.13333333 0.86666667) *
##                 235) A.num.words.log< 1.868835 11   0 Y (0.00000000 1.00000000) *
##              59) PubDate.second>=47 12   0 Y (0.00000000 1.00000000) *
##          15) PubDate.wkday.fctr5< 0.5 269  44 Y (0.16356877 0.83643123)  
##            30) A.num.chars.log>=3.676221 216  42 Y (0.19444444 0.80555556)  
##              60) WordCount.log< 4.331079 7   3 N (0.57142857 0.42857143) *
##              61) WordCount.log>=4.331079 209  38 Y (0.18181818 0.81818182)  
##               122) PubDate.wkday.fctr4>=0.5 48  14 Y (0.29166667 0.70833333)  
##                 244) PubDate.hour< 14.5 22  10 Y (0.45454545 0.54545455)  
##                   488) PubDate.second>=15.5 14   5 N (0.64285714 0.35714286) *
##                   489) PubDate.second< 15.5 8   1 Y (0.12500000 0.87500000) *
##                 245) PubDate.hour>=14.5 26   4 Y (0.15384615 0.84615385)  
##                   490) PubDate.minute>=37.5 7   3 N (0.57142857 0.42857143) *
##                   491) PubDate.minute< 37.5 19   0 Y (0.00000000 1.00000000) *
##               123) PubDate.wkday.fctr4< 0.5 161  24 Y (0.14906832 0.85093168)  
##                 246) PubDate.wkday.fctr2>=0.5 47  12 Y (0.25531915 0.74468085)  
##                   492) PubDate.minute< 21.5 23   9 Y (0.39130435 0.60869565)  
##                     984) WordCount.log>=6.174632 8   2 N (0.75000000 0.25000000) *
##                     985) WordCount.log< 6.174632 15   3 Y (0.20000000 0.80000000) *
##                   493) PubDate.minute>=21.5 24   3 Y (0.12500000 0.87500000) *
##                 247) PubDate.wkday.fctr2< 0.5 114  12 Y (0.10526316 0.89473684)  
##                   494) H.num.chars.log< 3.449862 45   9 Y (0.20000000 0.80000000)  
##                     988) H.num.chars.log>=3.156774 28   9 Y (0.32142857 0.67857143)  
##                      1976) H.num.words.log< 1.242453 7   2 N (0.71428571 0.28571429) *
##                      1977) H.num.words.log>=1.242453 21   4 Y (0.19047619 0.80952381) *
##                     989) H.num.chars.log< 3.156774 17   0 Y (0.00000000 1.00000000) *
##                   495) H.num.chars.log>=3.449862 69   3 Y (0.04347826 0.95652174) *
##            31) A.num.chars.log< 3.676221 53   2 Y (0.03773585 0.96226415) *
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-16.png) 

```
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.6947152
## 3        0.2 0.7502890
## 4        0.3 0.7946667
## 5        0.4 0.7972222
## 6        0.5 0.7957295
## 7        0.6 0.7846608
## 8        0.7 0.7818991
## 9        0.8 0.7164907
## 10       0.9 0.5452812
## 11       1.0 0.1460396
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-17.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            3609
## 2            Y                                             175
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                             117
## 2                                             574
##          Prediction
## Reference    N    Y
##         N 3609  117
##         Y  175  574
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.347486e-01   7.584159e-01   9.271129e-01   9.418107e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   7.071467e-93   8.509328e-04 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-18.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.6606943
## 3        0.2 0.6919192
## 4        0.3 0.6695652
## 5        0.4 0.6596067
## 6        0.5 0.6513057
## 7        0.6 0.6593060
## 8        0.7 0.6581470
## 9        0.8 0.6376307
## 10       0.9 0.5020243
## 11       1.0 0.1688654
```

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            1539
## 2            Y                                              70
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                             174
## 2                                             274
##          Prediction
## Reference    N    Y
##         N 1539  174
##         Y   70  274
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.813807e-01   6.200315e-01   8.666185e-01   8.950458e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   4.284949e-10   4.284223e-11 
##                   model_id model_method
## 1 Conditional.X.cp.0.rpart        rpart
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  feats
## 1 WordCount.log, PubDate.hour, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, A.compani, H.new, S.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, SubsectionName.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.words.log, A.num.chars.log, S.num.chars.log, A.num.words.unq.log, S.num.words.log, S.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                      2.705                 2.046
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9444842                    0.4       0.7972222        0.9347486
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9271129             0.9418107     0.7584159   0.9117886
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.6919192        0.8813807
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8666185             0.8950458     0.6200315
## [1] "fitting model: Conditional.X.rf"
## [1] "    indep_vars: WordCount.log, PubDate.hour, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, .rnorm, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, A.compani, H.new, S.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, SubsectionName.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.words.log, A.num.chars.log, S.num.chars.log, A.num.words.unq.log, S.num.words.log, S.num.words.unq.log"
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-19.png) 

```
## + : mtry=  2 
## - : mtry=  2 
## + : mtry= 93 
## - : mtry= 93 
## + : mtry=185 
## - : mtry=185 
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 93 on full training set
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-20.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-21.png) 

```
##                 Length Class      Mode     
## call               4   -none-     call     
## type               1   -none-     character
## predicted       4475   factor     numeric  
## err.rate        1500   -none-     numeric  
## confusion          6   -none-     numeric  
## votes           8950   matrix     numeric  
## oob.times       4475   -none-     numeric  
## classes            2   -none-     character
## importance       185   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y               4475   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames           185   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-22.png) 

```
##    threshold     f.score
## 1        0.0 0.286753446
## 2        0.1 0.788421053
## 3        0.2 0.923551171
## 4        0.3 0.976531943
## 5        0.4 1.000000000
## 6        0.5 1.000000000
## 7        0.6 0.998663102
## 8        0.7 0.928469242
## 9        0.8 0.803514377
## 10       0.9 0.631963470
## 11       1.0 0.007978723
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-23.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    3726
## 2            Y                                      NA
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                      NA
## 2                                     749
##          Prediction
## Reference    N    Y
##         N 3726    0
##         Y    0  749
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      1.0000000      1.0000000      0.9991760      1.0000000      0.8326257 
## AccuracyPValue  McnemarPValue 
##      0.0000000            NaN 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-24.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.5977654
## 3        0.2 0.6696329
## 4        0.3 0.7068063
## 5        0.4 0.7201166
## 6        0.5 0.6915888
## 7        0.6 0.6734007
## 8        0.7 0.6292948
## 9        0.8 0.5040984
## 10       0.9 0.3165468
## 11       1.0 0.0000000
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-25.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    1618
## 2            Y                                      97
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                      95
## 2                                     247
##          Prediction
## Reference    N    Y
##         N 1618   95
##         Y   97  247
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.066602e-01   6.641079e-01   8.932593e-01   9.188884e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   3.600992e-22   9.424676e-01 
##           model_id model_method
## 1 Conditional.X.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          feats
## 1 WordCount.log, PubDate.hour, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, .rnorm, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, A.compani, H.new, S.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, SubsectionName.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.words.log, A.num.chars.log, S.num.chars.log, A.num.words.unq.log, S.num.words.log, S.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                    292.137                74.375
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1        0.9090503
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.999176                     1     0.6507779    0.928432
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7201166        0.9066602
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8932593             0.9188884     0.6641079
## [1] "fitting model: Conditional.X.no.rnorm.rf"
## [1] "    indep_vars: WordCount.log, PubDate.hour, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, A.compani, H.new, S.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, SubsectionName.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.words.log, A.num.chars.log, S.num.chars.log, A.num.words.unq.log, S.num.words.log, S.num.words.unq.log"
## + : mtry=  2 
## - : mtry=  2 
## + : mtry= 93 
## - : mtry= 93 
## + : mtry=184 
## - : mtry=184 
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 93 on full training set
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-26.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-27.png) 

```
##                 Length Class      Mode     
## call               4   -none-     call     
## type               1   -none-     character
## predicted       4475   factor     numeric  
## err.rate        1500   -none-     numeric  
## confusion          6   -none-     numeric  
## votes           8950   matrix     numeric  
## oob.times       4475   -none-     numeric  
## classes            2   -none-     character
## importance       184   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y               4475   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames           184   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-28.png) 

```
##    threshold     f.score
## 1        0.0 0.286753446
## 2        0.1 0.793012176
## 3        0.2 0.920712969
## 4        0.3 0.975260417
## 5        0.4 1.000000000
## 6        0.5 1.000000000
## 7        0.6 0.998663102
## 8        0.7 0.930763740
## 9        0.8 0.812995246
## 10       0.9 0.643115942
## 11       1.0 0.002666667
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-29.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             3726
## 2            Y                                               NA
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                               NA
## 2                                              749
##          Prediction
## Reference    N    Y
##         N 3726    0
##         Y    0  749
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      1.0000000      1.0000000      0.9991760      1.0000000      0.8326257 
## AccuracyPValue  McnemarPValue 
##      0.0000000            NaN 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-30.png) 

```
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.6054872
## 3        0.2 0.6741321
## 4        0.3 0.7105943
## 5        0.4 0.7165468
## 6        0.5 0.7042254
## 7        0.6 0.6677909
## 8        0.7 0.6319569
## 9        0.8 0.5261044
## 10       0.9 0.3245823
## 11       1.0 0.0000000
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_1-31.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             1611
## 2            Y                                               95
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                              102
## 2                                              249
##          Prediction
## Reference    N    Y
##         N 1611  102
##         Y   95  249
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.042295e-01   6.589346e-01   8.906870e-01   9.166067e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   9.648468e-21   6.690281e-01 
##                    model_id model_method
## 1 Conditional.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  feats
## 1 WordCount.log, PubDate.hour, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, A.compani, H.new, S.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, SubsectionName.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.words.log, A.num.chars.log, S.num.chars.log, A.num.words.unq.log, S.num.words.log, S.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                    285.204                74.041
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1        0.9090503
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.999176                     1       0.65276   0.9306203
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7165468        0.9042295
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1              0.890687             0.9166067     0.6589346
```

```r
# User specified
    # easier to exclude features
#model_id_pfx <- "";
# indep_vars_vctr <- setdiff(names(glb_fitent_df), 
#                         union(union(glb_rsp_var, glb_exclude_vars_as_features), 
#                                 c("<feat1_name>", "<feat2_name>")))
# method <- ""                                

    # easier to include features
#model_id_pfx <- ""; indep_vars_vctr <- c("<feat1_name>", "<feat1_name>"); method <- ""

    # User specified bivariate models
#     indep_vars_vctr_lst <- list()
#     for (feat in setdiff(names(glb_fitent_df), 
#                          union(glb_rsp_var, glb_exclude_vars_as_features)))
#         indep_vars_vctr_lst[["feat"]] <- feat

    # User specified combinatorial models
#     indep_vars_vctr_lst <- list()
#     combn_mtrx <- combn(c("<feat1_name>", "<feat2_name>", "<featn_name>"), 
#                           <num_feats_to_choose>)
#     for (combn_ix in 1:ncol(combn_mtrx))
#         #print(combn_mtrx[, combn_ix])
#         indep_vars_vctr_lst[[combn_ix]] <- combn_mtrx[, combn_ix]
    
    # template for myfit_mdl
    #   rf is hard-coded in caret to recognize only Accuracy / Kappa evaluation metrics
    #       only for OOB in trainControl ?
    
#     ret_lst <- myfit_mdl_fn(model_id=paste0(model_id_pfx, ""), model_method=method,
#                             indep_vars_vctr=indep_vars_vctr,
#                             rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                             fit_df=glb_fitent_df, OOB_df=glb_OOBent_df,
#                             n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df,
#                             model_loss_mtrx=glb_model_metric_terms,
#                             model_summaryFunction=glb_model_metric_smmry,
#                             model_metric=glb_model_metric,
#                             model_metric_maximize=glb_model_metric_maximize)

# Simplify a model
# fit_df <- glb_fitent_df; glb_mdl <- step(<complex>_mdl)

# Non-caret models
#     rpart_area_mdl <- rpart(reformulate("Area", response=glb_rsp_var), 
#                                data=glb_fitent_df, #method="class", 
#                                control=rpart.control(cp=0.12),
#                            parms=list(loss=glb_model_metric_terms))
#     print("rpart_sel_wlm_mdl"); prp(rpart_sel_wlm_mdl)
# 

print(glb_models_df)
```

```
##                     model_id     model_method
## 1          MFO.myMFO_classfr    myMFO_classfr
## 2    Random.myrandom_classfr myrandom_classfr
## 3       Max.cor.Y.cv.0.rpart            rpart
## 4  Max.cor.Y.cv.0.cp.0.rpart            rpart
## 5            Max.cor.Y.rpart            rpart
## 6              Max.cor.Y.glm              glm
## 7    Interact.High.cor.Y.glm              glm
## 8              Low.cor.X.glm              glm
## 9          Conditional.X.glm              glm
## 10       Conditional.X.rpart            rpart
## 11  Conditional.X.cp.0.rpart            rpart
## 12          Conditional.X.rf               rf
## 13 Conditional.X.no.rnorm.rf               rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           feats
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .rnorm
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .rnorm
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 WordCount.log
## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 WordCount.log
## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 WordCount.log
## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 WordCount.log
## 7                                                                                                                                                                                                                                                                                                WordCount.log, WordCount.log:PubDate.apm.fctr, WordCount.log:S.can, WordCount.log:S.presid, WordCount.log:S.take, WordCount.log:S.new, WordCount.log:S.day, WordCount.log:S.compani, WordCount.log:S.time, WordCount.log:S.articl, WordCount.log:S.will, WordCount.log:S.intern, WordCount.log:H.week, WordCount.log:NewsDesk.fctr, WordCount.log:H.num.chars.log, WordCount.log:A.num.chars.log, WordCount.log:A.num.words.log, WordCount.log:S.num.chars.log
## 8                                                                                                                                                                                                                      WordCount.log, PubDate.apm.fctr, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, .rnorm, PubDate.date.fctr, PubDate.second, S.presid, S.take, PubDate.minute, S.new, PubDate.wkday.fctr, S.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, H.new, S.first, S.time, H.newyork, S.articl, S.will, H.day, S.newyork, H.today, H.report, S.intern, H.week, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, H.num.chars.log, H.num.words.log, A.num.words.log, A.num.chars.log, S.num.words.log
## 9  WordCount.log, PubDate.hour, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, .rnorm, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, A.compani, H.new, S.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, SubsectionName.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.words.log, A.num.chars.log, S.num.chars.log, A.num.words.unq.log, S.num.words.log, S.num.words.unq.log
## 10         WordCount.log, PubDate.hour, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, A.compani, H.new, S.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, SubsectionName.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.words.log, A.num.chars.log, S.num.chars.log, A.num.words.unq.log, S.num.words.log, S.num.words.unq.log
## 11         WordCount.log, PubDate.hour, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, A.compani, H.new, S.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, SubsectionName.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.words.log, A.num.chars.log, S.num.chars.log, A.num.words.unq.log, S.num.words.log, S.num.words.unq.log
## 12 WordCount.log, PubDate.hour, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, .rnorm, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, A.compani, H.new, S.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, SubsectionName.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.words.log, A.num.chars.log, S.num.chars.log, A.num.words.unq.log, S.num.words.log, S.num.words.unq.log
## 13         WordCount.log, PubDate.hour, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, A.compani, H.new, S.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, SubsectionName.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.words.log, A.num.chars.log, S.num.chars.log, A.num.words.unq.log, S.num.words.log, S.num.words.unq.log
##    max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1                0                      0.985                 0.003
## 2                0                      0.349                 0.001
## 3                0                      0.769                 0.069
## 4                0                      0.633                 0.062
## 5                3                      1.353                 0.071
## 6                1                      1.211                 0.080
## 7                1                      3.703                 0.880
## 8                1                     10.977                 3.218
## 9                1                     25.684                 8.114
## 10               3                     11.254                 2.053
## 11               0                      2.705                 2.046
## 12               3                    292.137                74.375
## 13               3                    285.204                74.041
##    max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1    0.5000000                    0.5       0.0000000        0.8326257
## 2    0.4975446                    0.1       0.2867534        0.1673743
## 3    0.5000000                    0.5       0.0000000        0.8326257
## 4    0.7074274                    0.2       0.4572127        0.8015642
## 5    0.5000000                    0.5       0.0000000        0.8174308
## 6    0.7301738                    0.2       0.4222222        0.8306149
## 7    0.9020505                    0.3       0.6483957        0.8862589
## 8    0.9396716                    0.3       0.7170543        0.9016767
## 9    0.8096204                    0.9       0.6982578        0.8229914
## 10   0.8289627                    0.7       0.5738832        0.8909486
## 11   0.9444842                    0.4       0.7972222        0.9347486
## 12   1.0000000                    0.5       1.0000000        0.9090503
## 13   1.0000000                    0.5       1.0000000        0.9090503
##    max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.8213602             0.8434553    0.00000000   0.5000000
## 2              0.1565447             0.1786398    0.00000000   0.4821958
## 3              0.8213602             0.8434553    0.00000000   0.5000000
## 4              0.7895723             0.8131613    0.33685715   0.6504263
## 5              0.8213602             0.8434553    0.06210715   0.5000000
## 6              0.6959502             0.7227703    0.01169498   0.7342331
## 7              0.8726598             0.8917541    0.53380944   0.9123444
## 8              0.8930408             0.9106763    0.61829280   0.9301172
## 9              0.8942018             0.9117484    0.42142811   0.8205243
## 10             0.8795973             0.8982154    0.53971068   0.8220499
## 11             0.9271129             0.9418107    0.75841588   0.9117886
## 12             0.9991760             1.0000000    0.65077788   0.9284320
## 13             0.9991760             1.0000000    0.65275997   0.9306203
##    opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                     0.5       0.0000000        0.8327662
## 2                     0.1       0.2865473        0.1672338
## 3                     0.5       0.0000000        0.8327662
## 4                     0.2       0.3799472        0.7715119
## 5                     0.5       0.0000000        0.8327662
## 6                     0.2       0.3961722        0.6932426
## 7                     0.4       0.6829268        0.8988819
## 8                     0.4       0.7079646        0.9037433
## 9                     0.9       0.7048458        0.9022849
## 10                    0.7       0.6014493        0.8930481
## 11                    0.2       0.6919192        0.8813807
## 12                    0.4       0.7201166        0.9066602
## 13                    0.4       0.7165468        0.9042295
##    max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1              0.8159247             0.8486533     0.0000000
## 2              0.1513467             0.1840753     0.0000000
## 3              0.8159247             0.8486533     0.0000000
## 4              0.7527449             0.7895019     0.2413609
## 5              0.8159247             0.8486533     0.0000000
## 6              0.6728061             0.7131270     0.2215049
## 7              0.8850365             0.9115781     0.6229467
## 8              0.8901728             0.9161500     0.6503545
## 9              0.8886309             0.9147795     0.6463039
## 10             0.8788850             0.9060795     0.5439760
## 11             0.8666185             0.8950458     0.6200315
## 12             0.8932593             0.9188884     0.6641079
## 13             0.8906870             0.9166067     0.6589346
##    max.AccuracySD.fit max.KappaSD.fit min.aic.fit
## 1                  NA              NA          NA
## 2                  NA              NA          NA
## 3                  NA              NA          NA
## 4                  NA              NA          NA
## 5         0.002468564     0.018885476          NA
## 6         0.001627400     0.007870559    3674.923
## 7         0.007433434     0.033812633    2535.551
## 8         0.004828153     0.019639721    2226.837
## 9         0.098623693     0.271638367   31443.804
## 10        0.004747551     0.033408745          NA
## 11                 NA              NA          NA
## 12                 NA              NA          NA
## 13                 NA              NA          NA
```

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="fit.models", 
    chunk_step_major=glb_script_df[nrow(glb_script_df), "chunk_step_major"], 
    chunk_step_minor=glb_script_df[nrow(glb_script_df), "chunk_step_minor"]+1,                              
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##           chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed10  fit.models                6                1 199.203
## elapsed11  fit.models                6                2 849.561
```


```r
if (!is.null(glb_model_metric_smmry)) {
    stats_df <- glb_models_df[, "model_id", FALSE]

    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_fitent_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "fit",
        						glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_OOBent_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "OOB",
            					glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
#     tmp_models_df <- orderBy(~model_id, glb_models_df)
#     rownames(tmp_models_df) <- seq(1, nrow(tmp_models_df))
#     all.equal(subset(tmp_models_df[, names(stats_df)], model_id != "Random.myrandom_classfr"),
#               subset(stats_df, model_id != "Random.myrandom_classfr"))
#     print(subset(tmp_models_df[, names(stats_df)], model_id != "Random.myrandom_classfr")[, c("model_id", "max.Accuracy.fit")])
#     print(subset(stats_df, model_id != "Random.myrandom_classfr")[, c("model_id", "max.Accuracy.fit")])

    print("Merging following data into glb_models_df:")
    print(stats_mrg_df <- stats_df[, c(1, grep(glb_model_metric, names(stats_df)))])
    print(tmp_models_df <- orderBy(~model_id, glb_models_df[, c("model_id", grep(glb_model_metric, names(stats_df), value=TRUE))]))

    tmp2_models_df <- glb_models_df[, c("model_id", setdiff(names(glb_models_df), grep(glb_model_metric, names(stats_df), value=TRUE)))]
    tmp3_models_df <- merge(tmp2_models_df, stats_mrg_df, all.x=TRUE, sort=FALSE)
    print(tmp3_models_df)
    print(names(tmp3_models_df))
    print(glb_models_df <- subset(tmp3_models_df, select=-model_id.1))
}

plt_models_df <- glb_models_df[, -grep("SD|Upper|Lower", names(glb_models_df))]
for (var in grep("^min.", names(plt_models_df), value=TRUE)) {
    plt_models_df[, sub("min.", "inv.", var)] <- 
        #ifelse(all(is.na(tmp <- plt_models_df[, var])), NA, 1.0 / tmp)
        1.0 / plt_models_df[, var]
    plt_models_df <- plt_models_df[ , -grep(var, names(plt_models_df))]
}
print(plt_models_df)
```

```
##                     model_id     model_method
## 1          MFO.myMFO_classfr    myMFO_classfr
## 2    Random.myrandom_classfr myrandom_classfr
## 3       Max.cor.Y.cv.0.rpart            rpart
## 4  Max.cor.Y.cv.0.cp.0.rpart            rpart
## 5            Max.cor.Y.rpart            rpart
## 6              Max.cor.Y.glm              glm
## 7    Interact.High.cor.Y.glm              glm
## 8              Low.cor.X.glm              glm
## 9          Conditional.X.glm              glm
## 10       Conditional.X.rpart            rpart
## 11  Conditional.X.cp.0.rpart            rpart
## 12          Conditional.X.rf               rf
## 13 Conditional.X.no.rnorm.rf               rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           feats
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .rnorm
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        .rnorm
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 WordCount.log
## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 WordCount.log
## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 WordCount.log
## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 WordCount.log
## 7                                                                                                                                                                                                                                                                                                WordCount.log, WordCount.log:PubDate.apm.fctr, WordCount.log:S.can, WordCount.log:S.presid, WordCount.log:S.take, WordCount.log:S.new, WordCount.log:S.day, WordCount.log:S.compani, WordCount.log:S.time, WordCount.log:S.articl, WordCount.log:S.will, WordCount.log:S.intern, WordCount.log:H.week, WordCount.log:NewsDesk.fctr, WordCount.log:H.num.chars.log, WordCount.log:A.num.chars.log, WordCount.log:A.num.words.log, WordCount.log:S.num.chars.log
## 8                                                                                                                                                                                                                      WordCount.log, PubDate.apm.fctr, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, .rnorm, PubDate.date.fctr, PubDate.second, S.presid, S.take, PubDate.minute, S.new, PubDate.wkday.fctr, S.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, H.new, S.first, S.time, H.newyork, S.articl, S.will, H.day, S.newyork, H.today, H.report, S.intern, H.week, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, H.num.chars.log, H.num.words.log, A.num.words.log, A.num.chars.log, S.num.words.log
## 9  WordCount.log, PubDate.hour, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, .rnorm, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, A.compani, H.new, S.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, SubsectionName.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.words.log, A.num.chars.log, S.num.chars.log, A.num.words.unq.log, S.num.words.log, S.num.words.unq.log
## 10         WordCount.log, PubDate.hour, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, A.compani, H.new, S.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, SubsectionName.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.words.log, A.num.chars.log, S.num.chars.log, A.num.words.unq.log, S.num.words.log, S.num.words.unq.log
## 11         WordCount.log, PubDate.hour, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, A.compani, H.new, S.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, SubsectionName.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.words.log, A.num.chars.log, S.num.chars.log, A.num.words.unq.log, S.num.words.log, S.num.words.unq.log
## 12 WordCount.log, PubDate.hour, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, .rnorm, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, A.compani, H.new, S.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, SubsectionName.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.words.log, A.num.chars.log, S.num.chars.log, A.num.words.unq.log, S.num.words.log, S.num.words.unq.log
## 13         WordCount.log, PubDate.hour, PubDate.apm.fctr, A.can, S.can, H.has.ebola, S.make, S.one, S.state, A.one, S.said, PubDate.date.fctr, PubDate.second, S.presid, A.presid, S.take, A.take, PubDate.minute, S.new, A.new, PubDate.wkday.fctr, S.day, A.day, H.X2014, S.show, S.report, S.share, S.year, S.compani, A.compani, H.new, S.first, S.time, A.time, H.newyork, S.articl, A.articl, S.will, A.will, H.day, S.newyork, H.today, H.report, S.intern, A.intern, H.week, H.fashion, S.week, S.fashion, Headline.pfx.fctr, NewsDesk.fctr, SectionName.fctr, SubsectionName.fctr, H.num.chars.log, H.num.words.log, H.num.words.unq.log, A.num.words.log, A.num.chars.log, S.num.chars.log, A.num.words.unq.log, S.num.words.log, S.num.words.unq.log
##    max.nTuningRuns max.auc.fit opt.prob.threshold.fit max.f.score.fit
## 1                0   0.5000000                    0.5       0.0000000
## 2                0   0.4975446                    0.1       0.2867534
## 3                0   0.5000000                    0.5       0.0000000
## 4                0   0.7074274                    0.2       0.4572127
## 5                3   0.5000000                    0.5       0.0000000
## 6                1   0.7301738                    0.2       0.4222222
## 7                1   0.9020505                    0.3       0.6483957
## 8                1   0.9396716                    0.3       0.7170543
## 9                1   0.8096204                    0.9       0.6982578
## 10               3   0.8289627                    0.7       0.5738832
## 11               0   0.9444842                    0.4       0.7972222
## 12               3   1.0000000                    0.5       1.0000000
## 13               3   1.0000000                    0.5       1.0000000
##    max.Accuracy.fit max.Kappa.fit max.auc.OOB opt.prob.threshold.OOB
## 1         0.8326257    0.00000000   0.5000000                    0.5
## 2         0.1673743    0.00000000   0.4821958                    0.1
## 3         0.8326257    0.00000000   0.5000000                    0.5
## 4         0.8015642    0.33685715   0.6504263                    0.2
## 5         0.8174308    0.06210715   0.5000000                    0.5
## 6         0.8306149    0.01169498   0.7342331                    0.2
## 7         0.8862589    0.53380944   0.9123444                    0.4
## 8         0.9016767    0.61829280   0.9301172                    0.4
## 9         0.8229914    0.42142811   0.8205243                    0.9
## 10        0.8909486    0.53971068   0.8220499                    0.7
## 11        0.9347486    0.75841588   0.9117886                    0.2
## 12        0.9090503    0.65077788   0.9284320                    0.4
## 13        0.9090503    0.65275997   0.9306203                    0.4
##    max.f.score.OOB max.Accuracy.OOB max.Kappa.OOB
## 1        0.0000000        0.8327662     0.0000000
## 2        0.2865473        0.1672338     0.0000000
## 3        0.0000000        0.8327662     0.0000000
## 4        0.3799472        0.7715119     0.2413609
## 5        0.0000000        0.8327662     0.0000000
## 6        0.3961722        0.6932426     0.2215049
## 7        0.6829268        0.8988819     0.6229467
## 8        0.7079646        0.9037433     0.6503545
## 9        0.7048458        0.9022849     0.6463039
## 10       0.6014493        0.8930481     0.5439760
## 11       0.6919192        0.8813807     0.6200315
## 12       0.7201166        0.9066602     0.6641079
## 13       0.7165468        0.9042295     0.6589346
##    inv.elapsedtime.everything inv.elapsedtime.final  inv.aic.fit
## 1                 1.015228426          3.333333e+02           NA
## 2                 2.865329513          1.000000e+03           NA
## 3                 1.300390117          1.449275e+01           NA
## 4                 1.579778831          1.612903e+01           NA
## 5                 0.739098300          1.408451e+01           NA
## 6                 0.825763832          1.250000e+01 2.721146e-04
## 7                 0.270051310          1.136364e+00 3.943916e-04
## 8                 0.091099572          3.107520e-01 4.490674e-04
## 9                 0.038934745          1.232438e-01 3.180277e-05
## 10                0.088857295          4.870921e-01           NA
## 11                0.369685767          4.887586e-01           NA
## 12                0.003423052          1.344538e-02           NA
## 13                0.003506262          1.350603e-02           NA
```

```r
print(myplot_radar(radar_inp_df=plt_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 13. Consider specifying shapes manually. if you must have them.
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

```
## Warning: Removed 104 rows containing missing values (geom_point).
```

```
## Warning: Removed 9 rows containing missing values (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 13. Consider specifying shapes manually. if you must have them.
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_2-1.png) 

```r
# print(myplot_radar(radar_inp_df=subset(plt_models_df, 
#         !(model_id %in% grep("random|MFO", plt_models_df$model_id, value=TRUE)))))

# Compute CI for <metric>SD
glb_models_df <- mutate(glb_models_df, 
                max.df = ifelse(max.nTuningRuns > 1, max.nTuningRuns - 1, NA),
                min.sd2ci.scaler = ifelse(is.na(max.df), NA, qt(0.975, max.df)))
for (var in grep("SD", names(glb_models_df), value=TRUE)) {
    # Does CI alredy exist ?
    var_components <- unlist(strsplit(var, "SD"))
    varActul <- paste0(var_components[1],          var_components[2])
    varUpper <- paste0(var_components[1], "Upper", var_components[2])
    varLower <- paste0(var_components[1], "Lower", var_components[2])
    if (varUpper %in% names(glb_models_df)) {
        warning(varUpper, " already exists in glb_models_df")
        # Assuming Lower also exists
        next
    }    
    print(sprintf("var:%s", var))
    # CI is dependent on sample size in t distribution; df=n-1
    glb_models_df[, varUpper] <- glb_models_df[, varActul] + 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
    glb_models_df[, varLower] <- glb_models_df[, varActul] - 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
}
```

```
## Warning: max.AccuracyUpper.fit already exists in glb_models_df
```

```
## [1] "var:max.KappaSD.fit"
```

```r
# Plot metrics with CI
plt_models_df <- glb_models_df[, "model_id", FALSE]
pltCI_models_df <- glb_models_df[, "model_id", FALSE]
for (var in grep("Upper", names(glb_models_df), value=TRUE)) {
    var_components <- unlist(strsplit(var, "Upper"))
    col_name <- unlist(paste(var_components, collapse=""))
    plt_models_df[, col_name] <- glb_models_df[, col_name]
    for (name in paste0(var_components[1], c("Upper", "Lower"), var_components[2]))
        pltCI_models_df[, name] <- glb_models_df[, name]
}

build_statsCI_data <- function(plt_models_df) {
    mltd_models_df <- melt(plt_models_df, id.vars="model_id")
    mltd_models_df$data <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) tail(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), "[.]")), 1))
    mltd_models_df$label <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) head(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), 
            paste0(".", mltd_models_df[row_ix, "data"]))), 1))
    #print(mltd_models_df)
    
    return(mltd_models_df)
}
mltd_models_df <- build_statsCI_data(plt_models_df)

mltdCI_models_df <- melt(pltCI_models_df, id.vars="model_id")
for (row_ix in 1:nrow(mltdCI_models_df)) {
    for (type in c("Upper", "Lower")) {
        if (length(var_components <- unlist(strsplit(
                as.character(mltdCI_models_df[row_ix, "variable"]), type))) > 1) {
            #print(sprintf("row_ix:%d; type:%s; ", row_ix, type))
            mltdCI_models_df[row_ix, "label"] <- var_components[1]
            mltdCI_models_df[row_ix, "data"] <- 
                unlist(strsplit(var_components[2], "[.]"))[2]
            mltdCI_models_df[row_ix, "type"] <- type
            break
        }
    }    
}
#print(mltdCI_models_df)
# castCI_models_df <- dcast(mltdCI_models_df, value ~ type, fun.aggregate=sum)
# print(castCI_models_df)
wideCI_models_df <- reshape(subset(mltdCI_models_df, select=-variable), 
                            timevar="type", 
        idvar=setdiff(names(mltdCI_models_df), c("type", "value", "variable")), 
                            direction="wide")
#print(wideCI_models_df)
mrgdCI_models_df <- merge(wideCI_models_df, mltd_models_df, all.x=TRUE)
#print(mrgdCI_models_df)

# Merge stats back in if CIs don't exist
goback_vars <- c()
for (var in unique(mltd_models_df$label)) {
    for (type in unique(mltd_models_df$data)) {
        var_type <- paste0(var, ".", type)
        # if this data is already present, next
        if (var_type %in% unique(paste(mltd_models_df$label, mltd_models_df$data,
                                       sep=".")))
            next
        #print(sprintf("var_type:%s", var_type))
        goback_vars <- c(goback_vars, var_type)
    }
}

if (length(goback_vars) > 0) {
    mltd_goback_df <- build_statsCI_data(glb_models_df[, c("model_id", goback_vars)])
    mltd_models_df <- rbind(mltd_models_df, mltd_goback_df)
}

mltd_models_df <- merge(mltd_models_df, glb_models_df[, c("model_id", "model_method")], 
                        all.x=TRUE)

png(paste0(glb_out_pfx, "models_bar.png"), width=480*3, height=480*2)
print(gp <- myplot_bar(mltd_models_df, "model_id", "value", colorcol_name="model_method") + 
        geom_errorbar(data=mrgdCI_models_df, 
            mapping=aes(x=model_id, ymax=value.Upper, ymin=value.Lower), width=0.5) + 
          facet_grid(label ~ data, scales="free") + 
          theme(axis.text.x = element_text(angle = 90,vjust = 0.5)))
dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
print(gp)
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_2-2.png) 

```r
# used for console inspection
model_evl_terms <- c(NULL)
for (metric in glb_model_evl_criteria)
    model_evl_terms <- c(model_evl_terms, 
                         ifelse(length(grep("max", metric)) > 0, "-", "+"), metric)
if (glb_is_classification && glb_is_binomial)
    model_evl_terms <- c(model_evl_terms, "-", "opt.prob.threshold.OOB")
model_sel_frmla <- as.formula(paste(c("~ ", model_evl_terms), collapse=" "))
print(dsp_models_df <- orderBy(model_sel_frmla, glb_models_df)
      [, c("model_id", glb_model_evl_criteria, 
           ifelse(glb_is_classification && glb_is_binomial, 
                  "opt.prob.threshold.OOB", NULL))])
```

```
##                     model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 12          Conditional.X.rf        0.9066602   0.9284320     0.6641079
## 13 Conditional.X.no.rnorm.rf        0.9042295   0.9306203     0.6589346
## 8              Low.cor.X.glm        0.9037433   0.9301172     0.6503545
## 9          Conditional.X.glm        0.9022849   0.8205243     0.6463039
## 7    Interact.High.cor.Y.glm        0.8988819   0.9123444     0.6229467
## 10       Conditional.X.rpart        0.8930481   0.8220499     0.5439760
## 11  Conditional.X.cp.0.rpart        0.8813807   0.9117886     0.6200315
## 1          MFO.myMFO_classfr        0.8327662   0.5000000     0.0000000
## 3       Max.cor.Y.cv.0.rpart        0.8327662   0.5000000     0.0000000
## 5            Max.cor.Y.rpart        0.8327662   0.5000000     0.0000000
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.7715119   0.6504263     0.2413609
## 6              Max.cor.Y.glm        0.6932426   0.7342331     0.2215049
## 2    Random.myrandom_classfr        0.1672338   0.4821958     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 12          NA                    0.4
## 13          NA                    0.4
## 8     2226.837                    0.4
## 9    31443.804                    0.9
## 7     2535.551                    0.4
## 10          NA                    0.7
## 11          NA                    0.2
## 1           NA                    0.5
## 3           NA                    0.5
## 5           NA                    0.5
## 4           NA                    0.2
## 6     3674.923                    0.2
## 2           NA                    0.1
```

```r
print(myplot_radar(radar_inp_df=dsp_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 13. Consider specifying shapes manually. if you must have them.
```

```
## Warning: Removed 46 rows containing missing values (geom_point).
```

```
## Warning: Removed 9 rows containing missing values (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 13. Consider specifying shapes manually. if you must have them.
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_2-3.png) 

```r
print("Metrics used for model selection:"); print(model_sel_frmla)
```

```
## [1] "Metrics used for model selection:"
```

```
## ~-max.Accuracy.OOB - max.auc.OOB - max.Kappa.OOB + min.aic.fit - 
##     opt.prob.threshold.OOB
```

```r
print(sprintf("Best model id: %s", dsp_models_df[1, "model_id"]))
```

```
## [1] "Best model id: Conditional.X.rf"
```

```r
if (is.null(glb_sel_mdl_id)) 
    { glb_sel_mdl_id <- dsp_models_df[1, "model_id"] } else 
        print(sprintf("User specified selection: %s", glb_sel_mdl_id))   
    
myprint_mdl(glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]])
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_2-4.png) 

```
##                 Length Class      Mode     
## call               4   -none-     call     
## type               1   -none-     character
## predicted       4475   factor     numeric  
## err.rate        1500   -none-     numeric  
## confusion          6   -none-     numeric  
## votes           8950   matrix     numeric  
## oob.times       4475   -none-     numeric  
## classes            2   -none-     character
## importance       185   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y               4475   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames           185   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
```

```
## [1] TRUE
```

```r
# From here to save(), this should all be in one function
#   these are executed in the same seq twice more:
#       fit.data.training & predict.data.new chunks
glb_get_predictions <- function(df, mdl_id, rsp_var_out, prob_threshold_def=NULL) {
    mdl <- glb_models_lst[[mdl_id]]
    rsp_var_out <- paste0(rsp_var_out, mdl_id)

    if (glb_is_regression) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        print(myplot_scatter(df, glb_rsp_var, rsp_var_out, smooth=TRUE))
        df[, paste0(rsp_var_out, ".err")] <- 
            abs(df[, rsp_var_out] - df[, glb_rsp_var])
        print(head(orderBy(reformulate(c("-", paste0(glb_rsp_var_out, ".err"))), 
                           df)))                             
    }

    if (glb_is_classification && glb_is_binomial) {
        prob_threshold <- glb_models_df[glb_models_df$model_id == mdl_id, 
                                        "opt.prob.threshold.OOB"]
        if (is.null(prob_threshold) || is.na(prob_threshold)) {
            warning("Using default probability threshold: ", prob_threshold_def)
            if (is.null(prob_threshold <- prob_threshold_def))
                stop("Default probability threshold is NULL")
        }
        
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")[, 2]
        df[, rsp_var_out] <- 
        		factor(levels(df[, glb_rsp_var])[
    				(df[, paste0(rsp_var_out, ".prob")] >=
    					prob_threshold) * 1 + 1], levels(df[, glb_rsp_var]))
    
        # prediction stats already reported by myfit_mdl ???
    }    
    
    if (glb_is_classification && !glb_is_binomial) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
    }

    return(df)
}    
glb_OOBent_df <- glb_get_predictions(df=glb_OOBent_df, glb_sel_mdl_id, glb_rsp_var_out)
glb_feats_df <- 
    mymerge_feats_importance(feats_df=glb_feats_df, sel_mdl=glb_sel_mdl, glb_fitent_df)
glb_feats_df[, paste0(glb_sel_mdl_id, ".importance")] <- glb_feats_df$importance
print(glb_feats_df)
```

```
##                                      id        cor.y exclude.as.feat
## WordCount.log             WordCount.log  0.265952699           FALSE
## SubsectionName.fctr SubsectionName.fctr -0.168723053           FALSE
## NewsDesk.fctr             NewsDesk.fctr -0.161611606           FALSE
## PubDate.hour               PubDate.hour  0.159167673           FALSE
## PubDate.minute           PubDate.minute -0.031469083           FALSE
## H.num.chars.log         H.num.chars.log -0.171062360           FALSE
## .rnorm                           .rnorm -0.008703337           FALSE
## S.num.chars.log         S.num.chars.log -0.224692967           FALSE
## A.num.chars.log         A.num.chars.log -0.224548821           FALSE
## PubDate.second           PubDate.second -0.012253600           FALSE
## SectionName.fctr       SectionName.fctr -0.165854115           FALSE
## H.num.words.unq.log H.num.words.unq.log -0.204496360           FALSE
## H.num.words.log         H.num.words.log -0.200686356           FALSE
## S.num.words.unq.log S.num.words.unq.log -0.250796919           FALSE
## A.num.words.unq.log A.num.words.unq.log -0.226995906           FALSE
## A.num.words.log         A.num.words.log -0.221953007           FALSE
## S.num.words.log         S.num.words.log -0.245354135           FALSE
## PubDate.wkday.fctr   PubDate.wkday.fctr -0.039801288           FALSE
## PubDate.date.fctr     PubDate.date.fctr -0.011647558           FALSE
## PubDate.apm.fctr       PubDate.apm.fctr  0.101472715           FALSE
## S.year                           S.year -0.051146178           FALSE
## A.one                             A.one  0.005696039           FALSE
## S.one                             S.one  0.006342094           FALSE
## S.week                           S.week -0.084814939           FALSE
## S.new                             S.new -0.034948520           FALSE
## A.time                           A.time -0.057790617           FALSE
## S.time                           S.time -0.057595102           FALSE
## A.new                             A.new -0.035359447           FALSE
## S.state                         S.state  0.006069626           FALSE
## S.report                       S.report -0.050211524           FALSE
## Headline.pfx.fctr     Headline.pfx.fctr -0.156829778           FALSE
## S.said                           S.said  0.001363226           FALSE
## S.will                           S.will -0.060575493           FALSE
## S.can                             S.can  0.029999780           FALSE
## H.day                             H.day -0.061669687           FALSE
## A.will                           A.will -0.061025004           FALSE
## A.can                             A.can  0.031498867           FALSE
## S.make                           S.make  0.023138853           FALSE
## S.share                         S.share -0.050329686           FALSE
## A.compani                     A.compani -0.053099633           FALSE
## S.compani                     S.compani -0.053012962           FALSE
## A.take                           A.take -0.026086108           FALSE
## S.newyork                     S.newyork -0.062117105           FALSE
## S.show                           S.show -0.048801740           FALSE
## S.presid                       S.presid -0.019828826           FALSE
## S.take                           S.take -0.025762398           FALSE
## S.first                         S.first -0.053388178           FALSE
## A.presid                       A.presid -0.019828826           FALSE
## S.day                             S.day -0.045649185           FALSE
## A.day                             A.day -0.045909684           FALSE
## H.new                             H.new -0.053121542           FALSE
## H.has.ebola                 H.has.ebola  0.025881397           FALSE
## H.report                       H.report -0.064948102           FALSE
## S.articl                       S.articl -0.059520554           FALSE
## S.intern                       S.intern -0.068485701           FALSE
## A.articl                       A.articl -0.059520554           FALSE
## A.intern                       A.intern -0.068485701           FALSE
## H.newyork                     H.newyork -0.057970095           FALSE
## H.X2014                         H.X2014 -0.046206380           FALSE
## H.today                         H.today -0.063723058           FALSE
## H.week                           H.week -0.075105216           FALSE
## H.fashion                     H.fashion -0.081708612           FALSE
## S.fashion                     S.fashion -0.086446251           FALSE
## A.has.http                   A.has.http -0.013592603           FALSE
## A.num.chars                 A.num.chars -0.177037425            TRUE
## A.num.words                 A.num.words -0.187423227            TRUE
## A.num.words.unq         A.num.words.unq -0.192819225            TRUE
## H.daili                         H.daili -0.069192975           FALSE
## H.has.http                   H.has.http           NA           FALSE
## H.num.chars                 H.num.chars -0.147211183            TRUE
## H.num.words                 H.num.words -0.186036895            TRUE
## H.num.words.unq         H.num.words.unq -0.189702157            TRUE
## H.X2015                         H.X2015 -0.066584892           FALSE
## Popular                         Popular  1.000000000            TRUE
## Popular.fctr               Popular.fctr           NA            TRUE
## PubDate.month.fctr   PubDate.month.fctr  0.019148739            TRUE
## PubDate.year               PubDate.year           NA            TRUE
## S.has.http                   S.has.http           NA           FALSE
## S.num.chars                 S.num.chars -0.179331806            TRUE
## S.num.words                 S.num.words -0.206385049            TRUE
## S.num.words.unq         S.num.words.unq -0.212102717            TRUE
## UniqueID                       UniqueID  0.011824920            TRUE
## WordCount                     WordCount  0.257526549            TRUE
##                       cor.y.abs       cor.high.X is.ConditionalX.y
## WordCount.log       0.265952699             <NA>              TRUE
## SubsectionName.fctr 0.168723053    NewsDesk.fctr              TRUE
## NewsDesk.fctr       0.161611606             <NA>              TRUE
## PubDate.hour        0.159167673 PubDate.apm.fctr              TRUE
## PubDate.minute      0.031469083             <NA>              TRUE
## H.num.chars.log     0.171062360             <NA>              TRUE
## .rnorm              0.008703337             <NA>              TRUE
## S.num.chars.log     0.224692967  A.num.chars.log              TRUE
## A.num.chars.log     0.224548821             <NA>              TRUE
## PubDate.second      0.012253600             <NA>              TRUE
## SectionName.fctr    0.165854115             <NA>              TRUE
## H.num.words.unq.log 0.204496360  H.num.chars.log              TRUE
## H.num.words.log     0.200686356             <NA>              TRUE
## S.num.words.unq.log 0.250796919  S.num.chars.log              TRUE
## A.num.words.unq.log 0.226995906  A.num.words.log              TRUE
## A.num.words.log     0.221953007             <NA>              TRUE
## S.num.words.log     0.245354135             <NA>              TRUE
## PubDate.wkday.fctr  0.039801288             <NA>              TRUE
## PubDate.date.fctr   0.011647558             <NA>              TRUE
## PubDate.apm.fctr    0.101472715             <NA>              TRUE
## S.year              0.051146178             <NA>              TRUE
## A.one               0.005696039             <NA>              TRUE
## S.one               0.006342094             <NA>              TRUE
## S.week              0.084814939             <NA>              TRUE
## S.new               0.034948520             <NA>              TRUE
## A.time              0.057790617           S.time              TRUE
## S.time              0.057595102             <NA>              TRUE
## A.new               0.035359447            S.new              TRUE
## S.state             0.006069626             <NA>              TRUE
## S.report            0.050211524             <NA>              TRUE
## Headline.pfx.fctr   0.156829778             <NA>              TRUE
## S.said              0.001363226             <NA>              TRUE
## S.will              0.060575493             <NA>              TRUE
## S.can               0.029999780             <NA>              TRUE
## H.day               0.061669687             <NA>              TRUE
## A.will              0.061025004           S.will              TRUE
## A.can               0.031498867            S.can              TRUE
## S.make              0.023138853             <NA>              TRUE
## S.share             0.050329686             <NA>              TRUE
## A.compani           0.053099633        S.compani              TRUE
## S.compani           0.053012962             <NA>              TRUE
## A.take              0.026086108           S.take              TRUE
## S.newyork           0.062117105             <NA>              TRUE
## S.show              0.048801740             <NA>              TRUE
## S.presid            0.019828826             <NA>              TRUE
## S.take              0.025762398             <NA>              TRUE
## S.first             0.053388178             <NA>              TRUE
## A.presid            0.019828826         S.presid              TRUE
## S.day               0.045649185             <NA>              TRUE
## A.day               0.045909684            S.day              TRUE
## H.new               0.053121542             <NA>              TRUE
## H.has.ebola         0.025881397             <NA>              TRUE
## H.report            0.064948102             <NA>              TRUE
## S.articl            0.059520554             <NA>              TRUE
## S.intern            0.068485701             <NA>              TRUE
## A.articl            0.059520554         S.articl              TRUE
## A.intern            0.068485701         S.intern              TRUE
## H.newyork           0.057970095             <NA>              TRUE
## H.X2014             0.046206380             <NA>              TRUE
## H.today             0.063723058             <NA>              TRUE
## H.week              0.075105216             <NA>              TRUE
## H.fashion           0.081708612           H.week              TRUE
## S.fashion           0.086446251             <NA>              TRUE
## A.has.http          0.013592603             <NA>             FALSE
## A.num.chars         0.177037425             <NA>                NA
## A.num.words         0.187423227             <NA>                NA
## A.num.words.unq     0.192819225             <NA>                NA
## H.daili             0.069192975             <NA>             FALSE
## H.has.http                   NA             <NA>             FALSE
## H.num.chars         0.147211183             <NA>                NA
## H.num.words         0.186036895             <NA>                NA
## H.num.words.unq     0.189702157             <NA>                NA
## H.X2015             0.066584892             <NA>             FALSE
## Popular             1.000000000             <NA>                NA
## Popular.fctr                 NA             <NA>                NA
## PubDate.month.fctr  0.019148739             <NA>                NA
## PubDate.year                 NA             <NA>                NA
## S.has.http                   NA             <NA>             FALSE
## S.num.chars         0.179331806             <NA>                NA
## S.num.words         0.206385049             <NA>                NA
## S.num.words.unq     0.212102717             <NA>                NA
## UniqueID            0.011824920             <NA>                NA
## WordCount           0.257526549             <NA>                NA
##                     is.cor.y.abs.low rsp_var_raw id_var rsp_var
## WordCount.log                  FALSE       FALSE     NA      NA
## SubsectionName.fctr            FALSE       FALSE     NA      NA
## NewsDesk.fctr                  FALSE       FALSE     NA      NA
## PubDate.hour                   FALSE       FALSE     NA      NA
## PubDate.minute                 FALSE       FALSE     NA      NA
## H.num.chars.log                FALSE       FALSE     NA      NA
## .rnorm                         FALSE       FALSE     NA      NA
## S.num.chars.log                FALSE       FALSE     NA      NA
## A.num.chars.log                FALSE       FALSE     NA      NA
## PubDate.second                 FALSE       FALSE     NA      NA
## SectionName.fctr               FALSE       FALSE     NA      NA
## H.num.words.unq.log            FALSE       FALSE     NA      NA
## H.num.words.log                FALSE       FALSE     NA      NA
## S.num.words.unq.log            FALSE       FALSE     NA      NA
## A.num.words.unq.log            FALSE       FALSE     NA      NA
## A.num.words.log                FALSE       FALSE     NA      NA
## S.num.words.log                FALSE       FALSE     NA      NA
## PubDate.wkday.fctr             FALSE       FALSE     NA      NA
## PubDate.date.fctr              FALSE       FALSE     NA      NA
## PubDate.apm.fctr               FALSE       FALSE     NA      NA
## S.year                         FALSE       FALSE     NA      NA
## A.one                           TRUE       FALSE     NA      NA
## S.one                           TRUE       FALSE     NA      NA
## S.week                         FALSE       FALSE     NA      NA
## S.new                          FALSE       FALSE     NA      NA
## A.time                         FALSE       FALSE     NA      NA
## S.time                         FALSE       FALSE     NA      NA
## A.new                          FALSE       FALSE     NA      NA
## S.state                         TRUE       FALSE     NA      NA
## S.report                       FALSE       FALSE     NA      NA
## Headline.pfx.fctr              FALSE       FALSE     NA      NA
## S.said                          TRUE       FALSE     NA      NA
## S.will                         FALSE       FALSE     NA      NA
## S.can                          FALSE       FALSE     NA      NA
## H.day                          FALSE       FALSE     NA      NA
## A.will                         FALSE       FALSE     NA      NA
## A.can                          FALSE       FALSE     NA      NA
## S.make                         FALSE       FALSE     NA      NA
## S.share                        FALSE       FALSE     NA      NA
## A.compani                      FALSE       FALSE     NA      NA
## S.compani                      FALSE       FALSE     NA      NA
## A.take                         FALSE       FALSE     NA      NA
## S.newyork                      FALSE       FALSE     NA      NA
## S.show                         FALSE       FALSE     NA      NA
## S.presid                       FALSE       FALSE     NA      NA
## S.take                         FALSE       FALSE     NA      NA
## S.first                        FALSE       FALSE     NA      NA
## A.presid                       FALSE       FALSE     NA      NA
## S.day                          FALSE       FALSE     NA      NA
## A.day                          FALSE       FALSE     NA      NA
## H.new                          FALSE       FALSE     NA      NA
## H.has.ebola                    FALSE       FALSE     NA      NA
## H.report                       FALSE       FALSE     NA      NA
## S.articl                       FALSE       FALSE     NA      NA
## S.intern                       FALSE       FALSE     NA      NA
## A.articl                       FALSE       FALSE     NA      NA
## A.intern                       FALSE       FALSE     NA      NA
## H.newyork                      FALSE       FALSE     NA      NA
## H.X2014                        FALSE       FALSE     NA      NA
## H.today                        FALSE       FALSE     NA      NA
## H.week                         FALSE       FALSE     NA      NA
## H.fashion                      FALSE       FALSE     NA      NA
## S.fashion                      FALSE       FALSE     NA      NA
## A.has.http                     FALSE       FALSE     NA      NA
## A.num.chars                    FALSE       FALSE     NA      NA
## A.num.words                    FALSE       FALSE     NA      NA
## A.num.words.unq                FALSE       FALSE     NA      NA
## H.daili                        FALSE       FALSE     NA      NA
## H.has.http                        NA       FALSE     NA      NA
## H.num.chars                    FALSE       FALSE     NA      NA
## H.num.words                    FALSE       FALSE     NA      NA
## H.num.words.unq                FALSE       FALSE     NA      NA
## H.X2015                        FALSE       FALSE     NA      NA
## Popular                        FALSE        TRUE     NA      NA
## Popular.fctr                      NA          NA     NA    TRUE
## PubDate.month.fctr             FALSE       FALSE     NA      NA
## PubDate.year                      NA       FALSE     NA      NA
## S.has.http                        NA       FALSE     NA      NA
## S.num.chars                    FALSE       FALSE     NA      NA
## S.num.words                    FALSE       FALSE     NA      NA
## S.num.words.unq                FALSE       FALSE     NA      NA
## UniqueID                       FALSE       FALSE   TRUE      NA
## WordCount                      FALSE       FALSE     NA      NA
##                       importance Conditional.X.rf.importance
## WordCount.log       100.00000000                100.00000000
## SubsectionName.fctr  70.97214742                 70.97214742
## NewsDesk.fctr        70.70203028                 70.70203028
## PubDate.hour         50.57780939                 50.57780939
## PubDate.minute       31.47633849                 31.47633849
## H.num.chars.log      31.21715292                 31.21715292
## .rnorm               28.87805050                 28.87805050
## S.num.chars.log      27.96642552                 27.96642552
## A.num.chars.log      27.78062382                 27.78062382
## PubDate.second       25.37252912                 25.37252912
## SectionName.fctr     23.23417011                 23.23417011
## H.num.words.unq.log  11.54737849                 11.54737849
## H.num.words.log      11.38651891                 11.38651891
## S.num.words.unq.log  11.23537408                 11.23537408
## A.num.words.unq.log  11.11909402                 11.11909402
## A.num.words.log      10.43622871                 10.43622871
## S.num.words.log      10.23317558                 10.23317558
## PubDate.wkday.fctr    5.07569087                  5.07569087
## PubDate.date.fctr     3.41328270                  3.41328270
## PubDate.apm.fctr      3.33816477                  3.33816477
## S.year                2.44850130                  2.44850130
## A.one                 2.28481932                  2.28481932
## S.one                 2.05652251                  2.05652251
## S.week                2.02506391                  2.02506391
## S.new                 2.00587235                  2.00587235
## A.time                1.98473239                  1.98473239
## S.time                1.96971733                  1.96971733
## A.new                 1.94965236                  1.94965236
## S.state               1.92409986                  1.92409986
## S.report              1.81584808                  1.81584808
## Headline.pfx.fctr     1.68916167                  1.68916167
## S.said                1.67152559                  1.67152559
## S.will                1.57706331                  1.57706331
## S.can                 1.57504901                  1.57504901
## H.day                 1.45649237                  1.45649237
## A.will                1.45150094                  1.45150094
## A.can                 1.39035571                  1.39035571
## S.make                1.35787270                  1.35787270
## S.share               1.21163721                  1.21163721
## A.compani             1.19132133                  1.19132133
## S.compani             1.12248041                  1.12248041
## A.take                1.02430134                  1.02430134
## S.newyork             1.01138846                  1.01138846
## S.show                0.98001272                  0.98001272
## S.presid              0.95023344                  0.95023344
## S.take                0.93917819                  0.93917819
## S.first               0.87223540                  0.87223540
## A.presid              0.84346221                  0.84346221
## S.day                 0.78168195                  0.78168195
## A.day                 0.73118231                  0.73118231
## H.new                 0.67963347                  0.67963347
## H.has.ebola           0.67310512                  0.67310512
## H.report              0.62938276                  0.62938276
## S.articl              0.42849098                  0.42849098
## S.intern              0.42760187                  0.42760187
## A.articl              0.42722693                  0.42722693
## A.intern              0.41988579                  0.41988579
## H.newyork             0.37023925                  0.37023925
## H.X2014               0.27382624                  0.27382624
## H.today               0.26092552                  0.26092552
## H.week                0.25618220                  0.25618220
## H.fashion             0.13609276                  0.13609276
## S.fashion             0.08449006                  0.08449006
## A.has.http                    NA                          NA
## A.num.chars                   NA                          NA
## A.num.words                   NA                          NA
## A.num.words.unq               NA                          NA
## H.daili                       NA                          NA
## H.has.http                    NA                          NA
## H.num.chars                   NA                          NA
## H.num.words                   NA                          NA
## H.num.words.unq               NA                          NA
## H.X2015                       NA                          NA
## Popular                       NA                          NA
## Popular.fctr                  NA                          NA
## PubDate.month.fctr            NA                          NA
## PubDate.year                  NA                          NA
## S.has.http                    NA                          NA
## S.num.chars                   NA                          NA
## S.num.words                   NA                          NA
## S.num.words.unq               NA                          NA
## UniqueID                      NA                          NA
## WordCount                     NA                          NA
```

```r
# Used again in fit.data.training & predict.data.new chunks
glb_analytics_diag_plots <- function(obs_df, mdl_id, prob_threshold=NULL) {
    if (length(vars <- subset(glb_feats_df, importance > 0)$id) > 5) {
        warning("Limiting important feature scatter plots to 5 out of ", length(vars))
        vars <- vars[1:5]
    }
    require(reshape2)
    rsp_var_out <- paste0(glb_rsp_var_out, mdl_id)
    for (var in vars) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_rsp_var, rsp_var_out))
#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", colorcol_name="variable",
                                 facet_colcol_name="variable", jitter=TRUE) + 
                      guides(color=FALSE))
    }
    
    if (glb_is_regression) {
#         plot_vars_df <- subset(glb_feats_df, importance > 
#                         glb_feats_df[glb_feats_df$id == ".rnorm", "importance"])
        plot_vars_df <- orderBy(~ -importance, glb_feats_df)
        if (nrow(plot_vars_df) == 0)
            warning("No important features in glb_fin_mdl") else
            print(myplot_prediction_regression(df=obs_df, 
                        feat_x=ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2],
                                      ".rownames"), 
                                               feat_y=plot_vars_df$id[1],
                        rsp_var=glb_rsp_var, rsp_var_out=rsp_var_out,
                        id_vars=glb_id_vars)
    #               + facet_wrap(reformulate(plot_vars_df$id[2])) # if [1 or 2] is a factor                                                         
    #               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
                  )
    }    
    
    if (glb_is_classification) {
        if (nrow(plot_vars_df <- subset(glb_feats_df, importance > 0)) == 0)
            warning("No features in selected model are statistically important")
        else print(myplot_prediction_classification(df=obs_df, 
                feat_x=ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], 
                              ".rownames"),
                                               feat_y=plot_vars_df$id[1],
                     rsp_var=glb_rsp_var, 
                     rsp_var_out=rsp_var_out, 
                     id_vars=glb_id_vars,
                    prob_threshold=prob_threshold)
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
glb_analytics_diag_plots(obs_df=glb_OOBent_df, mdl_id=glb_sel_mdl_id, 
            prob_threshold=ifelse(glb_is_classification && glb_is_binomial, 
                              glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_OOBent_df, mdl_id =
## glb_sel_mdl_id, : Limiting important feature scatter plots to 5 out of 63
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_2-5.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_2-6.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_2-7.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_2-8.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_2-9.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID Popular.fctr Popular.fctr.predict.Conditional.X.rf.prob
## 6370     6370            Y                                       0.38
## 1654     1654            N                                       0.00
##      Popular.fctr.predict.Conditional.X.rf
## 6370                                     N
## 1654                                     N
##      Popular.fctr.predict.Conditional.X.rf.accurate
## 6370                                          FALSE
## 1654                                           TRUE
##      Popular.fctr.predict.Conditional.X.rf.error .label
## 6370                                       -0.02   6370
## 1654                                        0.00   1654
## [1] "Inaccurate: "
##      UniqueID Popular.fctr Popular.fctr.predict.Conditional.X.rf.prob
## 2527     2527            Y                                      0.000
## 3554     3554            Y                                      0.000
## 5486     5486            Y                                      0.000
## 1156     1156            Y                                      0.002
## 4721     4721            Y                                      0.002
## 92         92            Y                                      0.004
##      Popular.fctr.predict.Conditional.X.rf
## 2527                                     N
## 3554                                     N
## 5486                                     N
## 1156                                     N
## 4721                                     N
## 92                                       N
##      Popular.fctr.predict.Conditional.X.rf.accurate
## 2527                                          FALSE
## 3554                                          FALSE
## 5486                                          FALSE
## 1156                                          FALSE
## 4721                                          FALSE
## 92                                            FALSE
##      Popular.fctr.predict.Conditional.X.rf.error
## 2527                                      -0.400
## 3554                                      -0.400
## 5486                                      -0.400
## 1156                                      -0.398
## 4721                                      -0.398
## 92                                        -0.396
##      UniqueID Popular.fctr Popular.fctr.predict.Conditional.X.rf.prob
## 427       427            Y                                      0.202
## 2756     2756            Y                                      0.268
## 4725     4725            Y                                      0.310
## 6013     6013            Y                                      0.324
## 1369     1369            N                                      0.572
## 2557     2557            N                                      0.912
##      Popular.fctr.predict.Conditional.X.rf
## 427                                      N
## 2756                                     N
## 4725                                     N
## 6013                                     N
## 1369                                     Y
## 2557                                     Y
##      Popular.fctr.predict.Conditional.X.rf.accurate
## 427                                           FALSE
## 2756                                          FALSE
## 4725                                          FALSE
## 6013                                          FALSE
## 1369                                          FALSE
## 2557                                          FALSE
##      Popular.fctr.predict.Conditional.X.rf.error
## 427                                       -0.198
## 2756                                      -0.132
## 4725                                      -0.090
## 6013                                      -0.076
## 1369                                       0.172
## 2557                                       0.512
##      UniqueID Popular.fctr Popular.fctr.predict.Conditional.X.rf.prob
## 2557     2557            N                                      0.912
## 4943     4943            N                                      0.930
## 4763     4763            N                                      0.932
## 4929     4929            N                                      0.950
## 4771     4771            N                                      0.960
## 2510     2510            N                                      0.964
##      Popular.fctr.predict.Conditional.X.rf
## 2557                                     Y
## 4943                                     Y
## 4763                                     Y
## 4929                                     Y
## 4771                                     Y
## 2510                                     Y
##      Popular.fctr.predict.Conditional.X.rf.accurate
## 2557                                          FALSE
## 4943                                          FALSE
## 4763                                          FALSE
## 4929                                          FALSE
## 4771                                          FALSE
## 2510                                          FALSE
##      Popular.fctr.predict.Conditional.X.rf.error
## 2557                                       0.512
## 4943                                       0.530
## 4763                                       0.532
## 4929                                       0.550
## 4771                                       0.560
## 2510                                       0.564
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_2-10.png) 

```r
# gather predictions from models better than MFO.*
#mdl_id <- "Conditional.X.rf"
#mdl_id <- "Conditional.X.cp.0.rpart"
#mdl_id <- "Conditional.X.rpart"
# glb_OOBent_df <- glb_get_predictions(df=glb_OOBent_df, mdl_id,
#                                      glb_rsp_var_out)
# print(t(confusionMatrix(glb_OOBent_df[, paste0(glb_rsp_var_out, mdl_id)], 
#                         glb_OOBent_df[, glb_rsp_var])$table))
FN_OOB_ids <- c(4721, 4020, 693, 92)
print(glb_OOBent_df[glb_OOBent_df$UniqueID %in% FN_OOB_ids, 
                    grep(glb_rsp_var, names(glb_OOBent_df), value=TRUE)])
```

```
##      Popular.fctr Popular.fctr.predict.Conditional.X.rf.prob
## 92              Y                                      0.004
## 693             Y                                      0.004
## 4020            Y                                      0.124
## 4721            Y                                      0.002
##      Popular.fctr.predict.Conditional.X.rf
## 92                                       N
## 693                                      N
## 4020                                     N
## 4721                                     N
```

```r
print(glb_OOBent_df[glb_OOBent_df$UniqueID %in% FN_OOB_ids, 
                    glb_feats_df$id[1:5]])
```

```
##      WordCount.log  SubsectionName.fctr NewsDesk.fctr PubDate.hour
## 92        5.723585             Dealbook      Business            8
## 693       5.641907       Small Business      Business           14
## 4020      3.610918 Metro::N.Y. / Region         Metro           21
## 4721      6.030685         Asia Pacific       Foreign            4
##      PubDate.minute
## 92                2
## 693              10
## 4020             13
## 4721             23
```

```r
print(glb_OOBent_df[glb_OOBent_df$UniqueID %in% FN_OOB_ids, 
                    glb_txt_vars])
```

```
##                                                                   Headline
## 92   Moelis & Co. Hires Cantor, Ex-House Majority Leader, as Vice Chairman
## 693                                Do You Hire Employees on a Trial Basis?
## 4020       Video: News Conference About Ebola Patient at Bellevue Hospital
## 4721     Hong Kong Politician Likens Protesters to African-American Slaves
##                                                                                                                                                                                                                         Snippet
## 92                                                                            Eric Cantor, who suffered a surprising electoral defeat this year, will be joining Moelis & Company as vice chairman and a director on its board.
## 693                                                                                                                Do you think job candidates are willing to work for three months on a contract before being hired full-time?
## 4020                                                                                                                    A news conference about Dr. Craig Spencer at Bellevue Hospital who tested positive for the Ebola virus.
## 4721 A prominent businesswoman and politician has come under fire for saying, erroneously, that black Americans did not get voting rights for 107 years after the countrys slaves were freed, so Hong Kongers should also wait.
##                                                                                                                                                                                                                        Abstract
## 92                                                                            Eric Cantor, who suffered a surprising electoral defeat this year, will be joining Moelis & Company as vice chairman and a director on its board.
## 693                                                                                                                Do you think job candidates are willing to work for three months on a contract before being hired full-time?
## 4020                                                                                                                    A news conference about Dr. Craig Spencer at Bellevue Hospital who tested positive for the Ebola virus.
## 4721 A prominent businesswoman and politician has come under fire for saying, erroneously, that black Americans did not get voting rights for 107 years after the countrys slaves were freed, so Hong Kongers should also wait.
```

```r
# print(glb_entity_df[glb_entity_df$UniqueID %in% FN_OOB_ids, 
#                     glb_txt_vars])
# dsp_tbl(Headline.contains="[Ee]bola")
# sum(sel_obs(Headline.contains="[Ee]bola"))
# ftable(xtabs(Popular ~ NewsDesk.fctr, data=glb_entity_df[sel_obs(Headline.contains="[Ee]bola") ,]))
# xtabs(NewsDesk ~ Popular, #Popular ~ NewsDesk.fctr, 
#       data=glb_entity_df[sel_obs(Headline.contains="[Ee]bola") ,],
#       exclude=NULL)
# print(mycreate_xtab_df(df=glb_entity_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_entity_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_entity_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular")))
# print(mycreate_tbl_df(df=glb_entity_df[sel_obs(Headline.contains="[Ee]bola") ,], 
#                       tbl_col_names=c("Popular", "NewsDesk")))

save(glb_feats_df, 
     glb_entity_df, glb_trnent_df, glb_fitent_df, glb_OOBent_df, glb_newent_df,
     glb_models_df, dsp_models_df, glb_models_lst, glb_sel_mdl, glb_sel_mdl_id,
     glb_model_type,
    file=paste0(glb_out_pfx, "selmdl_dsk.RData"))
#load(paste0(glb_out_pfx, "selmdl_dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "model.selected")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.models_2-11.png) 

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="fit.data.training", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                 chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed11        fit.models                6                2 849.561
## elapsed12 fit.data.training                7                0 949.427
```

## Step `7`: fit.data.training

```r
#load(paste0(glb_inp_pfx, "dsk.RData"))

# To create specific models
# glb_fin_mdl_id <- NULL; glb_fin_mdl <- NULL; 
# glb_sel_mdl_id <- "Conditional.X.cp.0.rpart"; 
# glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]]; print(glb_sel_mdl)
    
if (!is.null(glb_fin_mdl_id) && (glb_fin_mdl_id %in% names(glb_models_lst))) {
    warning("Final model same as user selected model")
    glb_fin_mdl <- glb_sel_mdl
} else {    
    print(mdl_feats_df <- myextract_mdl_feats(sel_mdl=glb_sel_mdl, 
                                              entity_df=glb_fitent_df))
    
    if ((model_method <- glb_sel_mdl$method) == "custom")
        # get actual method from the model_id
        model_method <- tail(unlist(strsplit(glb_sel_mdl_id, "[.]")), 1)
        
    tune_finmdl_df <- NULL
    if (nrow(glb_sel_mdl$bestTune) > 0) {
        for (param in names(glb_sel_mdl$bestTune)) {
            #print(sprintf("param: %s", param))
            if (glb_sel_mdl$bestTune[1, param] != "none")
                tune_finmdl_df <- rbind(tune_finmdl_df, 
                    data.frame(parameter=param, 
                               min=glb_sel_mdl$bestTune[1, param], 
                               max=glb_sel_mdl$bestTune[1, param], 
                               by=1)) # by val does not matter
        }
    } 
    
    # Sync with parameters in mydsutils.R
    ret_lst <- myfit_mdl(model_id="Final", model_method=model_method,
                            indep_vars_vctr=mdl_feats_df$id, model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out, 
                            fit_df=glb_trnent_df, OOB_df=NULL,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=tune_finmdl_df,
                         # Automate from here
                         #  Issues if glb_sel_mdl$method == "rf" b/c trainControl is "oob"; not "cv"
                            model_loss_mtrx=glb_model_metric_terms,
                            model_summaryFunction=glb_sel_mdl$control$summaryFunction,
                            model_metric=glb_sel_mdl$metric,
                            model_metric_maximize=glb_sel_mdl$maximize)
    glb_fin_mdl <- glb_models_lst[[length(glb_models_lst)]] 
    glb_fin_mdl_id <- glb_models_df[length(glb_models_lst), "model_id"]
}
```

```
##                                      id   importance
## WordCount.log             WordCount.log 100.00000000
## SubsectionName.fctr SubsectionName.fctr  70.97214742
## NewsDesk.fctr             NewsDesk.fctr  70.70203028
## PubDate.hour               PubDate.hour  50.57780939
## PubDate.minute           PubDate.minute  31.47633849
## H.num.chars.log         H.num.chars.log  31.21715292
## .rnorm                           .rnorm  28.87805050
## S.num.chars.log         S.num.chars.log  27.96642552
## A.num.chars.log         A.num.chars.log  27.78062382
## PubDate.second           PubDate.second  25.37252912
## SectionName.fctr       SectionName.fctr  23.23417011
## H.num.words.unq.log H.num.words.unq.log  11.54737849
## H.num.words.log         H.num.words.log  11.38651891
## S.num.words.unq.log S.num.words.unq.log  11.23537408
## A.num.words.unq.log A.num.words.unq.log  11.11909402
## A.num.words.log         A.num.words.log  10.43622871
## S.num.words.log         S.num.words.log  10.23317558
## PubDate.wkday.fctr   PubDate.wkday.fctr   5.07569087
## PubDate.date.fctr     PubDate.date.fctr   3.41328270
## PubDate.apm.fctr       PubDate.apm.fctr   3.33816477
## S.year                           S.year   2.44850130
## A.one                             A.one   2.28481932
## S.one                             S.one   2.05652251
## S.week                           S.week   2.02506391
## S.new                             S.new   2.00587235
## A.time                           A.time   1.98473239
## S.time                           S.time   1.96971733
## A.new                             A.new   1.94965236
## S.state                         S.state   1.92409986
## S.report                       S.report   1.81584808
## Headline.pfx.fctr     Headline.pfx.fctr   1.68916167
## S.said                           S.said   1.67152559
## S.will                           S.will   1.57706331
## S.can                             S.can   1.57504901
## H.day                             H.day   1.45649237
## A.will                           A.will   1.45150094
## A.can                             A.can   1.39035571
## S.make                           S.make   1.35787270
## S.share                         S.share   1.21163721
## A.compani                     A.compani   1.19132133
## S.compani                     S.compani   1.12248041
## A.take                           A.take   1.02430134
## S.newyork                     S.newyork   1.01138846
## S.show                           S.show   0.98001272
## S.presid                       S.presid   0.95023344
## S.take                           S.take   0.93917819
## S.first                         S.first   0.87223540
## A.presid                       A.presid   0.84346221
## S.day                             S.day   0.78168195
## A.day                             A.day   0.73118231
## H.new                             H.new   0.67963347
## H.has.ebola                 H.has.ebola   0.67310512
## H.report                       H.report   0.62938276
## S.articl                       S.articl   0.42849098
## S.intern                       S.intern   0.42760187
## A.articl                       A.articl   0.42722693
## A.intern                       A.intern   0.41988579
## H.newyork                     H.newyork   0.37023925
## H.X2014                         H.X2014   0.27382624
## H.today                         H.today   0.26092552
## H.week                           H.week   0.25618220
## H.fashion                     H.fashion   0.13609276
## S.fashion                     S.fashion   0.08449006
## [1] "fitting model: Final.rf"
## [1] "    indep_vars: WordCount.log, SubsectionName.fctr, NewsDesk.fctr, PubDate.hour, PubDate.minute, H.num.chars.log, .rnorm, S.num.chars.log, A.num.chars.log, PubDate.second, SectionName.fctr, H.num.words.unq.log, H.num.words.log, S.num.words.unq.log, A.num.words.unq.log, A.num.words.log, S.num.words.log, PubDate.wkday.fctr, PubDate.date.fctr, PubDate.apm.fctr, S.year, A.one, S.one, S.week, S.new, A.time, S.time, A.new, S.state, S.report, Headline.pfx.fctr, S.said, S.will, S.can, H.day, A.will, A.can, S.make, S.share, A.compani, S.compani, A.take, S.newyork, S.show, S.presid, S.take, S.first, A.presid, S.day, A.day, H.new, H.has.ebola, H.report, S.articl, S.intern, A.articl, A.intern, H.newyork, H.X2014, H.today, H.week, H.fashion, S.fashion"
## + : mtry=93 
## - : mtry=93 
## Aggregating results
## Fitting final model on full training set
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.data.training_0-1.png) 

```
##                 Length Class      Mode     
## call                4  -none-     call     
## type                1  -none-     character
## predicted        6532  factor     numeric  
## err.rate         1500  -none-     numeric  
## confusion           6  -none-     numeric  
## votes           13064  matrix     numeric  
## oob.times        6532  -none-     numeric  
## classes             2  -none-     character
## importance        185  -none-     numeric  
## importanceSD        0  -none-     NULL     
## localImportance     0  -none-     NULL     
## proximity           0  -none-     NULL     
## ntree               1  -none-     numeric  
## mtry                1  -none-     numeric  
## forest             14  -none-     list     
## y                6532  factor     numeric  
## test                0  -none-     NULL     
## inbag               0  -none-     NULL     
## xNames            185  -none-     character
## problemType         1  -none-     character
## tuneValue           1  data.frame list     
## obsLevels           2  -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.data.training_0-2.png) 

```
##    threshold    f.score
## 1        0.0 0.28668852
## 2        0.1 0.79202899
## 3        0.2 0.92587886
## 4        0.3 0.97720161
## 5        0.4 1.00000000
## 6        0.5 1.00000000
## 7        0.6 0.99816682
## 8        0.7 0.92632613
## 9        0.8 0.80741953
## 10       0.9 0.62515723
## 11       1.0 0.01453224
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Final.rf.N
## 1            N                            5439
## 2            Y                              NA
##   Popular.fctr.predict.Final.rf.Y
## 1                              NA
## 2                            1093
##          Prediction
## Reference    N    Y
##         N 5439    0
##         Y    0 1093
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##      1.0000000      1.0000000      0.9994354      1.0000000      0.8326699 
## AccuracyPValue  McnemarPValue 
##      0.0000000            NaN
```

```
## Warning in mypredict_mdl(mdl, df = fit_df, rsp_var, rsp_var_out,
## model_id_method, : Expecting 1 metric: Accuracy; recd: Accuracy, Kappa;
## retaining Accuracy only
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.data.training_0-3.png) 

```
##   model_id model_method
## 1 Final.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          feats
## 1 WordCount.log, SubsectionName.fctr, NewsDesk.fctr, PubDate.hour, PubDate.minute, H.num.chars.log, .rnorm, S.num.chars.log, A.num.chars.log, PubDate.second, SectionName.fctr, H.num.words.unq.log, H.num.words.log, S.num.words.unq.log, A.num.words.unq.log, A.num.words.log, S.num.words.log, PubDate.wkday.fctr, PubDate.date.fctr, PubDate.apm.fctr, S.year, A.one, S.one, S.week, S.new, A.time, S.time, A.new, S.state, S.report, Headline.pfx.fctr, S.said, S.will, S.can, H.day, A.will, A.can, S.make, S.share, A.compani, S.compani, A.take, S.newyork, S.show, S.presid, S.take, S.first, A.presid, S.day, A.day, H.new, H.has.ebola, H.report, S.articl, S.intern, A.articl, A.intern, H.newyork, H.X2014, H.today, H.week, H.fashion, S.fashion
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                    270.856               133.926
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1        0.9096754
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit
## 1             0.9994354                     1     0.6543864
```

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="fit.data.training", 
    chunk_step_major=glb_script_df[nrow(glb_script_df), "chunk_step_major"], 
    chunk_step_minor=glb_script_df[nrow(glb_script_df), "chunk_step_minor"]+1,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                 chunk_label chunk_step_major chunk_step_minor  elapsed
## elapsed12 fit.data.training                7                0  949.427
## elapsed13 fit.data.training                7                1 1230.549
```


```r
glb_trnent_df <- glb_get_predictions(df=glb_trnent_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_get_predictions(df = glb_trnent_df, mdl_id =
## glb_fin_mdl_id, : Using default probability threshold: 0.4
```

```r
glb_feats_df <- mymerge_feats_importance(feats_df=glb_feats_df, sel_mdl=glb_fin_mdl, 
                                               entity_df=glb_trnent_df)
glb_feats_df[, paste0(glb_fin_mdl_id, ".importance")] <- glb_feats_df$importance
print(glb_feats_df)
```

```
##                                      id   importance        cor.y
## WordCount.log             WordCount.log 100.00000000  0.265952699
## SubsectionName.fctr SubsectionName.fctr  70.97214742 -0.168723053
## NewsDesk.fctr             NewsDesk.fctr  70.70203028 -0.161611606
## PubDate.hour               PubDate.hour  50.57780939  0.159167673
## PubDate.minute           PubDate.minute  31.47633849 -0.031469083
## H.num.chars.log         H.num.chars.log  31.21715292 -0.171062360
## .rnorm                           .rnorm  28.87805050 -0.008703337
## S.num.chars.log         S.num.chars.log  27.96642552 -0.224692967
## A.num.chars.log         A.num.chars.log  27.78062382 -0.224548821
## PubDate.second           PubDate.second  25.37252912 -0.012253600
## SectionName.fctr       SectionName.fctr  23.23417011 -0.165854115
## H.num.words.unq.log H.num.words.unq.log  11.54737849 -0.204496360
## H.num.words.log         H.num.words.log  11.38651891 -0.200686356
## S.num.words.unq.log S.num.words.unq.log  11.23537408 -0.250796919
## A.num.words.unq.log A.num.words.unq.log  11.11909402 -0.226995906
## A.num.words.log         A.num.words.log  10.43622871 -0.221953007
## S.num.words.log         S.num.words.log  10.23317558 -0.245354135
## PubDate.wkday.fctr   PubDate.wkday.fctr   5.07569087 -0.039801288
## PubDate.date.fctr     PubDate.date.fctr   3.41328270 -0.011647558
## PubDate.apm.fctr       PubDate.apm.fctr   3.33816477  0.101472715
## S.year                           S.year   2.44850130 -0.051146178
## A.one                             A.one   2.28481932  0.005696039
## S.one                             S.one   2.05652251  0.006342094
## S.week                           S.week   2.02506391 -0.084814939
## S.new                             S.new   2.00587235 -0.034948520
## A.time                           A.time   1.98473239 -0.057790617
## S.time                           S.time   1.96971733 -0.057595102
## A.new                             A.new   1.94965236 -0.035359447
## S.state                         S.state   1.92409986  0.006069626
## S.report                       S.report   1.81584808 -0.050211524
## Headline.pfx.fctr     Headline.pfx.fctr   1.68916167 -0.156829778
## S.said                           S.said   1.67152559  0.001363226
## S.will                           S.will   1.57706331 -0.060575493
## S.can                             S.can   1.57504901  0.029999780
## H.day                             H.day   1.45649237 -0.061669687
## A.will                           A.will   1.45150094 -0.061025004
## A.can                             A.can   1.39035571  0.031498867
## S.make                           S.make   1.35787270  0.023138853
## S.share                         S.share   1.21163721 -0.050329686
## A.compani                     A.compani   1.19132133 -0.053099633
## S.compani                     S.compani   1.12248041 -0.053012962
## A.take                           A.take   1.02430134 -0.026086108
## S.newyork                     S.newyork   1.01138846 -0.062117105
## S.show                           S.show   0.98001272 -0.048801740
## S.presid                       S.presid   0.95023344 -0.019828826
## S.take                           S.take   0.93917819 -0.025762398
## S.first                         S.first   0.87223540 -0.053388178
## A.presid                       A.presid   0.84346221 -0.019828826
## S.day                             S.day   0.78168195 -0.045649185
## A.day                             A.day   0.73118231 -0.045909684
## H.new                             H.new   0.67963347 -0.053121542
## H.has.ebola                 H.has.ebola   0.67310512  0.025881397
## H.report                       H.report   0.62938276 -0.064948102
## S.articl                       S.articl   0.42849098 -0.059520554
## S.intern                       S.intern   0.42760187 -0.068485701
## A.articl                       A.articl   0.42722693 -0.059520554
## A.intern                       A.intern   0.41988579 -0.068485701
## H.newyork                     H.newyork   0.37023925 -0.057970095
## H.X2014                         H.X2014   0.27382624 -0.046206380
## H.today                         H.today   0.26092552 -0.063723058
## H.week                           H.week   0.25618220 -0.075105216
## H.fashion                     H.fashion   0.13609276 -0.081708612
## S.fashion                     S.fashion   0.08449006 -0.086446251
## A.has.http                   A.has.http           NA -0.013592603
## A.num.chars                 A.num.chars           NA -0.177037425
## A.num.words                 A.num.words           NA -0.187423227
## A.num.words.unq         A.num.words.unq           NA -0.192819225
## H.daili                         H.daili           NA -0.069192975
## H.has.http                   H.has.http           NA           NA
## H.num.chars                 H.num.chars           NA -0.147211183
## H.num.words                 H.num.words           NA -0.186036895
## H.num.words.unq         H.num.words.unq           NA -0.189702157
## H.X2015                         H.X2015           NA -0.066584892
## Popular                         Popular           NA  1.000000000
## Popular.fctr               Popular.fctr           NA           NA
## PubDate.month.fctr   PubDate.month.fctr           NA  0.019148739
## PubDate.year               PubDate.year           NA           NA
## S.has.http                   S.has.http           NA           NA
## S.num.chars                 S.num.chars           NA -0.179331806
## S.num.words                 S.num.words           NA -0.206385049
## S.num.words.unq         S.num.words.unq           NA -0.212102717
## UniqueID                       UniqueID           NA  0.011824920
## WordCount                     WordCount           NA  0.257526549
##                     exclude.as.feat   cor.y.abs       cor.high.X
## WordCount.log                 FALSE 0.265952699             <NA>
## SubsectionName.fctr           FALSE 0.168723053    NewsDesk.fctr
## NewsDesk.fctr                 FALSE 0.161611606             <NA>
## PubDate.hour                  FALSE 0.159167673 PubDate.apm.fctr
## PubDate.minute                FALSE 0.031469083             <NA>
## H.num.chars.log               FALSE 0.171062360             <NA>
## .rnorm                        FALSE 0.008703337             <NA>
## S.num.chars.log               FALSE 0.224692967  A.num.chars.log
## A.num.chars.log               FALSE 0.224548821             <NA>
## PubDate.second                FALSE 0.012253600             <NA>
## SectionName.fctr              FALSE 0.165854115             <NA>
## H.num.words.unq.log           FALSE 0.204496360  H.num.chars.log
## H.num.words.log               FALSE 0.200686356             <NA>
## S.num.words.unq.log           FALSE 0.250796919  S.num.chars.log
## A.num.words.unq.log           FALSE 0.226995906  A.num.words.log
## A.num.words.log               FALSE 0.221953007             <NA>
## S.num.words.log               FALSE 0.245354135             <NA>
## PubDate.wkday.fctr            FALSE 0.039801288             <NA>
## PubDate.date.fctr             FALSE 0.011647558             <NA>
## PubDate.apm.fctr              FALSE 0.101472715             <NA>
## S.year                        FALSE 0.051146178             <NA>
## A.one                         FALSE 0.005696039             <NA>
## S.one                         FALSE 0.006342094             <NA>
## S.week                        FALSE 0.084814939             <NA>
## S.new                         FALSE 0.034948520             <NA>
## A.time                        FALSE 0.057790617           S.time
## S.time                        FALSE 0.057595102             <NA>
## A.new                         FALSE 0.035359447            S.new
## S.state                       FALSE 0.006069626             <NA>
## S.report                      FALSE 0.050211524             <NA>
## Headline.pfx.fctr             FALSE 0.156829778             <NA>
## S.said                        FALSE 0.001363226             <NA>
## S.will                        FALSE 0.060575493             <NA>
## S.can                         FALSE 0.029999780             <NA>
## H.day                         FALSE 0.061669687             <NA>
## A.will                        FALSE 0.061025004           S.will
## A.can                         FALSE 0.031498867            S.can
## S.make                        FALSE 0.023138853             <NA>
## S.share                       FALSE 0.050329686             <NA>
## A.compani                     FALSE 0.053099633        S.compani
## S.compani                     FALSE 0.053012962             <NA>
## A.take                        FALSE 0.026086108           S.take
## S.newyork                     FALSE 0.062117105             <NA>
## S.show                        FALSE 0.048801740             <NA>
## S.presid                      FALSE 0.019828826             <NA>
## S.take                        FALSE 0.025762398             <NA>
## S.first                       FALSE 0.053388178             <NA>
## A.presid                      FALSE 0.019828826         S.presid
## S.day                         FALSE 0.045649185             <NA>
## A.day                         FALSE 0.045909684            S.day
## H.new                         FALSE 0.053121542             <NA>
## H.has.ebola                   FALSE 0.025881397             <NA>
## H.report                      FALSE 0.064948102             <NA>
## S.articl                      FALSE 0.059520554             <NA>
## S.intern                      FALSE 0.068485701             <NA>
## A.articl                      FALSE 0.059520554         S.articl
## A.intern                      FALSE 0.068485701         S.intern
## H.newyork                     FALSE 0.057970095             <NA>
## H.X2014                       FALSE 0.046206380             <NA>
## H.today                       FALSE 0.063723058             <NA>
## H.week                        FALSE 0.075105216             <NA>
## H.fashion                     FALSE 0.081708612           H.week
## S.fashion                     FALSE 0.086446251             <NA>
## A.has.http                    FALSE 0.013592603             <NA>
## A.num.chars                    TRUE 0.177037425             <NA>
## A.num.words                    TRUE 0.187423227             <NA>
## A.num.words.unq                TRUE 0.192819225             <NA>
## H.daili                       FALSE 0.069192975             <NA>
## H.has.http                    FALSE          NA             <NA>
## H.num.chars                    TRUE 0.147211183             <NA>
## H.num.words                    TRUE 0.186036895             <NA>
## H.num.words.unq                TRUE 0.189702157             <NA>
## H.X2015                       FALSE 0.066584892             <NA>
## Popular                        TRUE 1.000000000             <NA>
## Popular.fctr                   TRUE          NA             <NA>
## PubDate.month.fctr             TRUE 0.019148739             <NA>
## PubDate.year                   TRUE          NA             <NA>
## S.has.http                    FALSE          NA             <NA>
## S.num.chars                    TRUE 0.179331806             <NA>
## S.num.words                    TRUE 0.206385049             <NA>
## S.num.words.unq                TRUE 0.212102717             <NA>
## UniqueID                       TRUE 0.011824920             <NA>
## WordCount                      TRUE 0.257526549             <NA>
##                     is.ConditionalX.y is.cor.y.abs.low rsp_var_raw id_var
## WordCount.log                    TRUE            FALSE       FALSE     NA
## SubsectionName.fctr              TRUE            FALSE       FALSE     NA
## NewsDesk.fctr                    TRUE            FALSE       FALSE     NA
## PubDate.hour                     TRUE            FALSE       FALSE     NA
## PubDate.minute                   TRUE            FALSE       FALSE     NA
## H.num.chars.log                  TRUE            FALSE       FALSE     NA
## .rnorm                           TRUE            FALSE       FALSE     NA
## S.num.chars.log                  TRUE            FALSE       FALSE     NA
## A.num.chars.log                  TRUE            FALSE       FALSE     NA
## PubDate.second                   TRUE            FALSE       FALSE     NA
## SectionName.fctr                 TRUE            FALSE       FALSE     NA
## H.num.words.unq.log              TRUE            FALSE       FALSE     NA
## H.num.words.log                  TRUE            FALSE       FALSE     NA
## S.num.words.unq.log              TRUE            FALSE       FALSE     NA
## A.num.words.unq.log              TRUE            FALSE       FALSE     NA
## A.num.words.log                  TRUE            FALSE       FALSE     NA
## S.num.words.log                  TRUE            FALSE       FALSE     NA
## PubDate.wkday.fctr               TRUE            FALSE       FALSE     NA
## PubDate.date.fctr                TRUE            FALSE       FALSE     NA
## PubDate.apm.fctr                 TRUE            FALSE       FALSE     NA
## S.year                           TRUE            FALSE       FALSE     NA
## A.one                            TRUE             TRUE       FALSE     NA
## S.one                            TRUE             TRUE       FALSE     NA
## S.week                           TRUE            FALSE       FALSE     NA
## S.new                            TRUE            FALSE       FALSE     NA
## A.time                           TRUE            FALSE       FALSE     NA
## S.time                           TRUE            FALSE       FALSE     NA
## A.new                            TRUE            FALSE       FALSE     NA
## S.state                          TRUE             TRUE       FALSE     NA
## S.report                         TRUE            FALSE       FALSE     NA
## Headline.pfx.fctr                TRUE            FALSE       FALSE     NA
## S.said                           TRUE             TRUE       FALSE     NA
## S.will                           TRUE            FALSE       FALSE     NA
## S.can                            TRUE            FALSE       FALSE     NA
## H.day                            TRUE            FALSE       FALSE     NA
## A.will                           TRUE            FALSE       FALSE     NA
## A.can                            TRUE            FALSE       FALSE     NA
## S.make                           TRUE            FALSE       FALSE     NA
## S.share                          TRUE            FALSE       FALSE     NA
## A.compani                        TRUE            FALSE       FALSE     NA
## S.compani                        TRUE            FALSE       FALSE     NA
## A.take                           TRUE            FALSE       FALSE     NA
## S.newyork                        TRUE            FALSE       FALSE     NA
## S.show                           TRUE            FALSE       FALSE     NA
## S.presid                         TRUE            FALSE       FALSE     NA
## S.take                           TRUE            FALSE       FALSE     NA
## S.first                          TRUE            FALSE       FALSE     NA
## A.presid                         TRUE            FALSE       FALSE     NA
## S.day                            TRUE            FALSE       FALSE     NA
## A.day                            TRUE            FALSE       FALSE     NA
## H.new                            TRUE            FALSE       FALSE     NA
## H.has.ebola                      TRUE            FALSE       FALSE     NA
## H.report                         TRUE            FALSE       FALSE     NA
## S.articl                         TRUE            FALSE       FALSE     NA
## S.intern                         TRUE            FALSE       FALSE     NA
## A.articl                         TRUE            FALSE       FALSE     NA
## A.intern                         TRUE            FALSE       FALSE     NA
## H.newyork                        TRUE            FALSE       FALSE     NA
## H.X2014                          TRUE            FALSE       FALSE     NA
## H.today                          TRUE            FALSE       FALSE     NA
## H.week                           TRUE            FALSE       FALSE     NA
## H.fashion                        TRUE            FALSE       FALSE     NA
## S.fashion                        TRUE            FALSE       FALSE     NA
## A.has.http                      FALSE            FALSE       FALSE     NA
## A.num.chars                        NA            FALSE       FALSE     NA
## A.num.words                        NA            FALSE       FALSE     NA
## A.num.words.unq                    NA            FALSE       FALSE     NA
## H.daili                         FALSE            FALSE       FALSE     NA
## H.has.http                      FALSE               NA       FALSE     NA
## H.num.chars                        NA            FALSE       FALSE     NA
## H.num.words                        NA            FALSE       FALSE     NA
## H.num.words.unq                    NA            FALSE       FALSE     NA
## H.X2015                         FALSE            FALSE       FALSE     NA
## Popular                            NA            FALSE        TRUE     NA
## Popular.fctr                       NA               NA          NA     NA
## PubDate.month.fctr                 NA            FALSE       FALSE     NA
## PubDate.year                       NA               NA       FALSE     NA
## S.has.http                      FALSE               NA       FALSE     NA
## S.num.chars                        NA            FALSE       FALSE     NA
## S.num.words                        NA            FALSE       FALSE     NA
## S.num.words.unq                    NA            FALSE       FALSE     NA
## UniqueID                           NA            FALSE       FALSE   TRUE
## WordCount                          NA            FALSE       FALSE     NA
##                     rsp_var Conditional.X.rf.importance
## WordCount.log            NA                100.00000000
## SubsectionName.fctr      NA                 70.97214742
## NewsDesk.fctr            NA                 70.70203028
## PubDate.hour             NA                 50.57780939
## PubDate.minute           NA                 31.47633849
## H.num.chars.log          NA                 31.21715292
## .rnorm                   NA                 28.87805050
## S.num.chars.log          NA                 27.96642552
## A.num.chars.log          NA                 27.78062382
## PubDate.second           NA                 25.37252912
## SectionName.fctr         NA                 23.23417011
## H.num.words.unq.log      NA                 11.54737849
## H.num.words.log          NA                 11.38651891
## S.num.words.unq.log      NA                 11.23537408
## A.num.words.unq.log      NA                 11.11909402
## A.num.words.log          NA                 10.43622871
## S.num.words.log          NA                 10.23317558
## PubDate.wkday.fctr       NA                  5.07569087
## PubDate.date.fctr        NA                  3.41328270
## PubDate.apm.fctr         NA                  3.33816477
## S.year                   NA                  2.44850130
## A.one                    NA                  2.28481932
## S.one                    NA                  2.05652251
## S.week                   NA                  2.02506391
## S.new                    NA                  2.00587235
## A.time                   NA                  1.98473239
## S.time                   NA                  1.96971733
## A.new                    NA                  1.94965236
## S.state                  NA                  1.92409986
## S.report                 NA                  1.81584808
## Headline.pfx.fctr        NA                  1.68916167
## S.said                   NA                  1.67152559
## S.will                   NA                  1.57706331
## S.can                    NA                  1.57504901
## H.day                    NA                  1.45649237
## A.will                   NA                  1.45150094
## A.can                    NA                  1.39035571
## S.make                   NA                  1.35787270
## S.share                  NA                  1.21163721
## A.compani                NA                  1.19132133
## S.compani                NA                  1.12248041
## A.take                   NA                  1.02430134
## S.newyork                NA                  1.01138846
## S.show                   NA                  0.98001272
## S.presid                 NA                  0.95023344
## S.take                   NA                  0.93917819
## S.first                  NA                  0.87223540
## A.presid                 NA                  0.84346221
## S.day                    NA                  0.78168195
## A.day                    NA                  0.73118231
## H.new                    NA                  0.67963347
## H.has.ebola              NA                  0.67310512
## H.report                 NA                  0.62938276
## S.articl                 NA                  0.42849098
## S.intern                 NA                  0.42760187
## A.articl                 NA                  0.42722693
## A.intern                 NA                  0.41988579
## H.newyork                NA                  0.37023925
## H.X2014                  NA                  0.27382624
## H.today                  NA                  0.26092552
## H.week                   NA                  0.25618220
## H.fashion                NA                  0.13609276
## S.fashion                NA                  0.08449006
## A.has.http               NA                          NA
## A.num.chars              NA                          NA
## A.num.words              NA                          NA
## A.num.words.unq          NA                          NA
## H.daili                  NA                          NA
## H.has.http               NA                          NA
## H.num.chars              NA                          NA
## H.num.words              NA                          NA
## H.num.words.unq          NA                          NA
## H.X2015                  NA                          NA
## Popular                  NA                          NA
## Popular.fctr           TRUE                          NA
## PubDate.month.fctr       NA                          NA
## PubDate.year             NA                          NA
## S.has.http               NA                          NA
## S.num.chars              NA                          NA
## S.num.words              NA                          NA
## S.num.words.unq          NA                          NA
## UniqueID                 NA                          NA
## WordCount                NA                          NA
##                     Final.rf.importance
## WordCount.log              100.00000000
## SubsectionName.fctr         70.97214742
## NewsDesk.fctr               70.70203028
## PubDate.hour                50.57780939
## PubDate.minute              31.47633849
## H.num.chars.log             31.21715292
## .rnorm                      28.87805050
## S.num.chars.log             27.96642552
## A.num.chars.log             27.78062382
## PubDate.second              25.37252912
## SectionName.fctr            23.23417011
## H.num.words.unq.log         11.54737849
## H.num.words.log             11.38651891
## S.num.words.unq.log         11.23537408
## A.num.words.unq.log         11.11909402
## A.num.words.log             10.43622871
## S.num.words.log             10.23317558
## PubDate.wkday.fctr           5.07569087
## PubDate.date.fctr            3.41328270
## PubDate.apm.fctr             3.33816477
## S.year                       2.44850130
## A.one                        2.28481932
## S.one                        2.05652251
## S.week                       2.02506391
## S.new                        2.00587235
## A.time                       1.98473239
## S.time                       1.96971733
## A.new                        1.94965236
## S.state                      1.92409986
## S.report                     1.81584808
## Headline.pfx.fctr            1.68916167
## S.said                       1.67152559
## S.will                       1.57706331
## S.can                        1.57504901
## H.day                        1.45649237
## A.will                       1.45150094
## A.can                        1.39035571
## S.make                       1.35787270
## S.share                      1.21163721
## A.compani                    1.19132133
## S.compani                    1.12248041
## A.take                       1.02430134
## S.newyork                    1.01138846
## S.show                       0.98001272
## S.presid                     0.95023344
## S.take                       0.93917819
## S.first                      0.87223540
## A.presid                     0.84346221
## S.day                        0.78168195
## A.day                        0.73118231
## H.new                        0.67963347
## H.has.ebola                  0.67310512
## H.report                     0.62938276
## S.articl                     0.42849098
## S.intern                     0.42760187
## A.articl                     0.42722693
## A.intern                     0.41988579
## H.newyork                    0.37023925
## H.X2014                      0.27382624
## H.today                      0.26092552
## H.week                       0.25618220
## H.fashion                    0.13609276
## S.fashion                    0.08449006
## A.has.http                           NA
## A.num.chars                          NA
## A.num.words                          NA
## A.num.words.unq                      NA
## H.daili                              NA
## H.has.http                           NA
## H.num.chars                          NA
## H.num.words                          NA
## H.num.words.unq                      NA
## H.X2015                              NA
## Popular                              NA
## Popular.fctr                         NA
## PubDate.month.fctr                   NA
## PubDate.year                         NA
## S.has.http                           NA
## S.num.chars                          NA
## S.num.words                          NA
## S.num.words.unq                      NA
## UniqueID                             NA
## WordCount                            NA
```

```r
glb_analytics_diag_plots(obs_df=glb_trnent_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=ifelse(glb_is_classification && glb_is_binomial, 
                              glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_trnent_df, mdl_id =
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 63
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.data.training_1-1.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.data.training_1-2.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.data.training_1-3.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.data.training_1-4.png) ![](NYTBlogs_hdlpfx7_files/figure-html/fit.data.training_1-5.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID Popular.fctr Popular.fctr.predict.Final.rf.prob
## 1507     1507            N                              0.002
## 6370     6370            Y                              0.742
##      Popular.fctr.predict.Final.rf Popular.fctr.predict.Final.rf.accurate
## 1507                             N                                   TRUE
## 6370                             Y                                   TRUE
##      Popular.fctr.predict.Final.rf.error .label
## 1507                                   0   1507
## 6370                                   0   6370
## [1] "Inaccurate: "
## [1] UniqueID                              
## [2] Popular.fctr                          
## [3] Popular.fctr.predict.Final.rf.prob    
## [4] Popular.fctr.predict.Final.rf         
## [5] Popular.fctr.predict.Final.rf.accurate
## [6] Popular.fctr.predict.Final.rf.error   
## <0 rows> (or 0-length row.names)
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.data.training_1-6.png) 

```r
dsp_feats_vctr <- c(NULL)
for(var in grep(".importance", names(glb_feats_df), fixed=TRUE, value=TRUE))
    dsp_feats_vctr <- union(dsp_feats_vctr, 
                            glb_feats_df[!is.na(glb_feats_df[, var]), "id"])

print(glb_trnent_df[glb_trnent_df$UniqueID %in% FN_OOB_ids, 
                    grep(glb_rsp_var, names(glb_trnent_df), value=TRUE)])
```

```
##      Popular.fctr Popular.fctr.predict.Final.rf.prob
## 92              Y                              0.674
## 693             Y                              0.612
## 4020            Y                              0.664
## 4721            Y                              0.636
##      Popular.fctr.predict.Final.rf
## 92                               Y
## 693                              Y
## 4020                             Y
## 4721                             Y
```

```r
save(glb_feats_df, glb_entity_df, 
     glb_trnent_df, glb_fitent_df, glb_OOBent_df, glb_newent_df,
     glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
     glb_sel_mdl, glb_sel_mdl_id,
     glb_fin_mdl, glb_fin_mdl_id,
    file=paste0(glb_out_pfx, "dsk.RData"))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all.prediction","model.final")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0 
## 3.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  data.training.all.prediction 
## 4.0000 	 5 	 0 1 1 1 
## 4.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  model.final 
## 5.0000 	 4 	 0 0 2 1
```

![](NYTBlogs_hdlpfx7_files/figure-html/fit.data.training_1-7.png) 

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="predict.data.new", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                 chunk_label chunk_step_major chunk_step_minor  elapsed
## elapsed13 fit.data.training                7                1 1230.549
## elapsed14  predict.data.new                8                0 1334.661
```

## Step `8`: predict data.new

```r
# Compute final model predictions
glb_newent_df <- glb_get_predictions(glb_newent_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_get_predictions(glb_newent_df, mdl_id = glb_fin_mdl_id,
## rsp_var_out = glb_rsp_var_out, : Using default probability threshold: 0.4
```

```r
glb_analytics_diag_plots(obs_df=glb_newent_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=ifelse(glb_is_classification && glb_is_binomial, 
                              glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                      "opt.prob.threshold.OOB"], NULL))
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_newent_df, mdl_id =
## glb_fin_mdl_id, : Limiting important feature scatter plots to 5 out of 63
```

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

![](NYTBlogs_hdlpfx7_files/figure-html/predict.data.new-1.png) 

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

![](NYTBlogs_hdlpfx7_files/figure-html/predict.data.new-2.png) 

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

![](NYTBlogs_hdlpfx7_files/figure-html/predict.data.new-3.png) 

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

![](NYTBlogs_hdlpfx7_files/figure-html/predict.data.new-4.png) 

```
## Warning in min(x): no non-missing arguments to min; returning Inf
```

```
## Warning in max(x): no non-missing arguments to max; returning -Inf
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

![](NYTBlogs_hdlpfx7_files/figure-html/predict.data.new-5.png) 

```
## [1] "Min/Max Boundaries: "
##      UniqueID Popular.fctr Popular.fctr.predict.Final.rf.prob
## 6753     6753         <NA>                              0.596
## 7309     7309         <NA>                              0.128
##      Popular.fctr.predict.Final.rf Popular.fctr.predict.Final.rf.accurate
## 6753                             Y                                     NA
## 7309                             N                                     NA
##      Popular.fctr.predict.Final.rf.error .label
## 6753                                   0   6753
## 7309                                   0   7309
## [1] "Inaccurate: "
##      UniqueID Popular.fctr Popular.fctr.predict.Final.rf.prob
## NA         NA         <NA>                                 NA
## NA.1       NA         <NA>                                 NA
## NA.2       NA         <NA>                                 NA
## NA.3       NA         <NA>                                 NA
## NA.4       NA         <NA>                                 NA
## NA.5       NA         <NA>                                 NA
##      Popular.fctr.predict.Final.rf Popular.fctr.predict.Final.rf.accurate
## NA                            <NA>                                     NA
## NA.1                          <NA>                                     NA
## NA.2                          <NA>                                     NA
## NA.3                          <NA>                                     NA
## NA.4                          <NA>                                     NA
## NA.5                          <NA>                                     NA
##      Popular.fctr.predict.Final.rf.error
## NA                                    NA
## NA.1                                  NA
## NA.2                                  NA
## NA.3                                  NA
## NA.4                                  NA
## NA.5                                  NA
##         UniqueID Popular.fctr Popular.fctr.predict.Final.rf.prob
## NA.311        NA         <NA>                                 NA
## NA.886        NA         <NA>                                 NA
## NA.1007       NA         <NA>                                 NA
## NA.1279       NA         <NA>                                 NA
## NA.1578       NA         <NA>                                 NA
## NA.1655       NA         <NA>                                 NA
##         Popular.fctr.predict.Final.rf
## NA.311                           <NA>
## NA.886                           <NA>
## NA.1007                          <NA>
## NA.1279                          <NA>
## NA.1578                          <NA>
## NA.1655                          <NA>
##         Popular.fctr.predict.Final.rf.accurate
## NA.311                                      NA
## NA.886                                      NA
## NA.1007                                     NA
## NA.1279                                     NA
## NA.1578                                     NA
## NA.1655                                     NA
##         Popular.fctr.predict.Final.rf.error
## NA.311                                   NA
## NA.886                                   NA
## NA.1007                                  NA
## NA.1279                                  NA
## NA.1578                                  NA
## NA.1655                                  NA
##         UniqueID Popular.fctr Popular.fctr.predict.Final.rf.prob
## NA.1864       NA         <NA>                                 NA
## NA.1865       NA         <NA>                                 NA
## NA.1866       NA         <NA>                                 NA
## NA.1867       NA         <NA>                                 NA
## NA.1868       NA         <NA>                                 NA
## NA.1869       NA         <NA>                                 NA
##         Popular.fctr.predict.Final.rf
## NA.1864                          <NA>
## NA.1865                          <NA>
## NA.1866                          <NA>
## NA.1867                          <NA>
## NA.1868                          <NA>
## NA.1869                          <NA>
##         Popular.fctr.predict.Final.rf.accurate
## NA.1864                                     NA
## NA.1865                                     NA
## NA.1866                                     NA
## NA.1867                                     NA
## NA.1868                                     NA
## NA.1869                                     NA
##         Popular.fctr.predict.Final.rf.error
## NA.1864                                  NA
## NA.1865                                  NA
## NA.1866                                  NA
## NA.1867                                  NA
## NA.1868                                  NA
## NA.1869                                  NA
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

![](NYTBlogs_hdlpfx7_files/figure-html/predict.data.new-6.png) 

```r
submit_df <- glb_newent_df[, c(glb_id_vars, 
                               paste0(glb_rsp_var_out, glb_fin_mdl_id, ".prob"))]
names(submit_df)[2] <- "Probability1"
write.csv(submit_df, 
    paste0(gsub(".", "_", paste0(glb_out_pfx, glb_fin_mdl_id), fixed=TRUE), 
           "_submit.csv"), row.names=FALSE)

# print(orderBy(~ -max.auc.OOB, glb_models_df[, c("model_id", 
#             "max.auc.OOB", "max.Accuracy.OOB")]))
print(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                    "opt.prob.threshold.OOB"])
```

```
## [1] 0.4
```

```r
print(sprintf("glb_sel_mdl_id: %s", glb_sel_mdl_id))
```

```
## [1] "glb_sel_mdl_id: Conditional.X.rf"
```

```r
print(sprintf("glb_fin_mdl_id: %s", glb_fin_mdl_id))
```

```
## [1] "glb_fin_mdl_id: Final.rf"
```

```r
print(dim(glb_fitent_df))
```

```
## [1] 4475   92
```

```r
print(dsp_models_df)
```

```
##                     model_id max.Accuracy.OOB max.auc.OOB max.Kappa.OOB
## 12          Conditional.X.rf        0.9066602   0.9284320     0.6641079
## 13 Conditional.X.no.rnorm.rf        0.9042295   0.9306203     0.6589346
## 8              Low.cor.X.glm        0.9037433   0.9301172     0.6503545
## 9          Conditional.X.glm        0.9022849   0.8205243     0.6463039
## 7    Interact.High.cor.Y.glm        0.8988819   0.9123444     0.6229467
## 10       Conditional.X.rpart        0.8930481   0.8220499     0.5439760
## 11  Conditional.X.cp.0.rpart        0.8813807   0.9117886     0.6200315
## 1          MFO.myMFO_classfr        0.8327662   0.5000000     0.0000000
## 3       Max.cor.Y.cv.0.rpart        0.8327662   0.5000000     0.0000000
## 5            Max.cor.Y.rpart        0.8327662   0.5000000     0.0000000
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.7715119   0.6504263     0.2413609
## 6              Max.cor.Y.glm        0.6932426   0.7342331     0.2215049
## 2    Random.myrandom_classfr        0.1672338   0.4821958     0.0000000
##    min.aic.fit opt.prob.threshold.OOB
## 12          NA                    0.4
## 13          NA                    0.4
## 8     2226.837                    0.4
## 9    31443.804                    0.9
## 7     2535.551                    0.4
## 10          NA                    0.7
## 11          NA                    0.2
## 1           NA                    0.5
## 3           NA                    0.5
## 5           NA                    0.5
## 4           NA                    0.2
## 6     3674.923                    0.2
## 2           NA                    0.1
```

```r
print(sprintf("%s OOB confusion matrix: ", glb_sel_mdl_id))
```

```
## [1] "Conditional.X.rf OOB confusion matrix: "
```

```r
print(t(confusionMatrix(glb_OOBent_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
                        glb_OOBent_df[, glb_rsp_var])$table))
```

```
##          Prediction
## Reference    N    Y
##         N 1618   95
##         Y   97  247
```

```r
print(glb_OOBent_df[glb_OOBent_df$UniqueID %in% FN_OOB_ids, 
                    grep(glb_rsp_var, names(glb_OOBent_df), value=TRUE)])
```

```
##      Popular.fctr Popular.fctr.predict.Conditional.X.rf.prob
## 92              Y                                      0.004
## 693             Y                                      0.004
## 4020            Y                                      0.124
## 4721            Y                                      0.002
##      Popular.fctr.predict.Conditional.X.rf
## 92                                       N
## 693                                      N
## 4020                                     N
## 4721                                     N
```

```r
print(glb_OOBent_df[glb_OOBent_df$UniqueID %in% FN_OOB_ids, 
                    glb_txt_vars])
```

```
##                                                                   Headline
## 92   Moelis & Co. Hires Cantor, Ex-House Majority Leader, as Vice Chairman
## 693                                Do You Hire Employees on a Trial Basis?
## 4020       Video: News Conference About Ebola Patient at Bellevue Hospital
## 4721     Hong Kong Politician Likens Protesters to African-American Slaves
##                                                                                                                                                                                                                         Snippet
## 92                                                                            Eric Cantor, who suffered a surprising electoral defeat this year, will be joining Moelis & Company as vice chairman and a director on its board.
## 693                                                                                                                Do you think job candidates are willing to work for three months on a contract before being hired full-time?
## 4020                                                                                                                    A news conference about Dr. Craig Spencer at Bellevue Hospital who tested positive for the Ebola virus.
## 4721 A prominent businesswoman and politician has come under fire for saying, erroneously, that black Americans did not get voting rights for 107 years after the countrys slaves were freed, so Hong Kongers should also wait.
##                                                                                                                                                                                                                        Abstract
## 92                                                                            Eric Cantor, who suffered a surprising electoral defeat this year, will be joining Moelis & Company as vice chairman and a director on its board.
## 693                                                                                                                Do you think job candidates are willing to work for three months on a contract before being hired full-time?
## 4020                                                                                                                    A news conference about Dr. Craig Spencer at Bellevue Hospital who tested positive for the Ebola virus.
## 4721 A prominent businesswoman and politician has come under fire for saying, erroneously, that black Americans did not get voting rights for 107 years after the countrys slaves were freed, so Hong Kongers should also wait.
```

```r
print(dsp_vctr <- colSums(glb_OOBent_df[glb_OOBent_df$UniqueID %in% FN_OOB_ids, 
                    setdiff(grep("[HSA].", names(glb_OOBent_df), value=TRUE),
                            union(myfind_chr_cols_df(glb_OOBent_df),
                grep(".fctr", names(glb_OOBent_df), fixed=TRUE, value=TRUE)))]))
```

```
##             H.X2014             H.X2015             H.daili 
##            0.000000            0.000000            0.000000 
##               H.day           H.fashion               H.new 
##            0.000000            0.000000            0.000000 
##           H.newyork            H.report             H.today 
##            0.000000            0.000000            0.000000 
##              H.week          H.has.http         H.has.ebola 
##            0.000000            0.000000            1.000000 
##         H.num.chars         H.num.words     H.num.words.unq 
##          236.000000           26.000000           26.000000 
##     H.num.chars.log     H.num.words.log H.num.words.unq.log 
##           16.285913            7.965546            7.965546 
##            S.articl               S.can           S.compani 
##            0.000000            0.000000            1.000000 
##               S.day           S.fashion             S.first 
##            0.000000            0.000000            0.000000 
##            S.intern              S.make               S.new 
##            0.000000            0.000000            0.000000 
##           S.newyork               S.one            S.presid 
##            0.000000            0.000000            0.000000 
##            S.report              S.said             S.share 
##            0.000000            0.000000            0.000000 
##              S.show             S.state              S.take 
##            0.000000            0.000000            0.000000 
##              S.time              S.week              S.will 
##            0.000000            0.000000            2.000000 
##              S.year          S.has.http         S.num.chars 
##            2.000000            0.000000          574.000000 
##         S.num.words     S.num.words.unq     S.num.chars.log 
##           56.000000           56.000000           19.708417 
##     S.num.words.log S.num.words.unq.log            A.articl 
##           10.659422           10.659422            0.000000 
##               A.can           A.compani               A.day 
##            0.000000            1.000000            0.000000 
##            A.intern               A.new               A.one 
##            0.000000            0.000000            0.000000 
##            A.presid              A.take              A.time 
##            0.000000            0.000000            0.000000 
##              A.will          A.has.http         A.num.chars 
##            2.000000            0.000000          574.000000 
##         A.num.words     A.num.words.unq     A.num.chars.log 
##           55.000000           55.000000           19.708417 
##     A.num.words.log A.num.words.unq.log 
##           10.594883           10.594883
```

```r
# print(glb_OOBent_df[glb_OOBent_df$UniqueID %in% FN_OOB_ids, 
#                     c("WordCount", "WordCount.log", "myMultimedia",
#                       "NewsDesk", "SectionName", "SubsectionName")])
# print(mycreate_sqlxtab_df(glb_entity_df[sel_obs(Headline.contains="[Vv]ideo"), ], 
#                           c(glb_rsp_var, "myMultimedia")))
# dsp_chisq.test(Headline.contains="[Vi]deo")
# print(glb_entity_df[sel_obs(Headline.contains="[Vv]ideo"), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline")])
# print(glb_entity_df[sel_obs(Headline.contains="[Ee]bola", Popular=1), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline",
#                             "NewsDesk", "SectionName", "SubsectionName")])
print(subset(glb_feats_df, !is.na(importance))[,
    c("is.ConditionalX.y", 
      grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
```

```
##                     is.ConditionalX.y   importance
## WordCount.log                    TRUE 100.00000000
## SubsectionName.fctr              TRUE  70.97214742
## NewsDesk.fctr                    TRUE  70.70203028
## PubDate.hour                     TRUE  50.57780939
## PubDate.minute                   TRUE  31.47633849
## H.num.chars.log                  TRUE  31.21715292
## .rnorm                           TRUE  28.87805050
## S.num.chars.log                  TRUE  27.96642552
## A.num.chars.log                  TRUE  27.78062382
## PubDate.second                   TRUE  25.37252912
## SectionName.fctr                 TRUE  23.23417011
## H.num.words.unq.log              TRUE  11.54737849
## H.num.words.log                  TRUE  11.38651891
## S.num.words.unq.log              TRUE  11.23537408
## A.num.words.unq.log              TRUE  11.11909402
## A.num.words.log                  TRUE  10.43622871
## S.num.words.log                  TRUE  10.23317558
## PubDate.wkday.fctr               TRUE   5.07569087
## PubDate.date.fctr                TRUE   3.41328270
## PubDate.apm.fctr                 TRUE   3.33816477
## S.year                           TRUE   2.44850130
## A.one                            TRUE   2.28481932
## S.one                            TRUE   2.05652251
## S.week                           TRUE   2.02506391
## S.new                            TRUE   2.00587235
## A.time                           TRUE   1.98473239
## S.time                           TRUE   1.96971733
## A.new                            TRUE   1.94965236
## S.state                          TRUE   1.92409986
## S.report                         TRUE   1.81584808
## Headline.pfx.fctr                TRUE   1.68916167
## S.said                           TRUE   1.67152559
## S.will                           TRUE   1.57706331
## S.can                            TRUE   1.57504901
## H.day                            TRUE   1.45649237
## A.will                           TRUE   1.45150094
## A.can                            TRUE   1.39035571
## S.make                           TRUE   1.35787270
## S.share                          TRUE   1.21163721
## A.compani                        TRUE   1.19132133
## S.compani                        TRUE   1.12248041
## A.take                           TRUE   1.02430134
## S.newyork                        TRUE   1.01138846
## S.show                           TRUE   0.98001272
## S.presid                         TRUE   0.95023344
## S.take                           TRUE   0.93917819
## S.first                          TRUE   0.87223540
## A.presid                         TRUE   0.84346221
## S.day                            TRUE   0.78168195
## A.day                            TRUE   0.73118231
## H.new                            TRUE   0.67963347
## H.has.ebola                      TRUE   0.67310512
## H.report                         TRUE   0.62938276
## S.articl                         TRUE   0.42849098
## S.intern                         TRUE   0.42760187
## A.articl                         TRUE   0.42722693
## A.intern                         TRUE   0.41988579
## H.newyork                        TRUE   0.37023925
## H.X2014                          TRUE   0.27382624
## H.today                          TRUE   0.26092552
## H.week                           TRUE   0.25618220
## H.fashion                        TRUE   0.13609276
## S.fashion                        TRUE   0.08449006
##                     Conditional.X.rf.importance Final.rf.importance
## WordCount.log                      100.00000000        100.00000000
## SubsectionName.fctr                 70.97214742         70.97214742
## NewsDesk.fctr                       70.70203028         70.70203028
## PubDate.hour                        50.57780939         50.57780939
## PubDate.minute                      31.47633849         31.47633849
## H.num.chars.log                     31.21715292         31.21715292
## .rnorm                              28.87805050         28.87805050
## S.num.chars.log                     27.96642552         27.96642552
## A.num.chars.log                     27.78062382         27.78062382
## PubDate.second                      25.37252912         25.37252912
## SectionName.fctr                    23.23417011         23.23417011
## H.num.words.unq.log                 11.54737849         11.54737849
## H.num.words.log                     11.38651891         11.38651891
## S.num.words.unq.log                 11.23537408         11.23537408
## A.num.words.unq.log                 11.11909402         11.11909402
## A.num.words.log                     10.43622871         10.43622871
## S.num.words.log                     10.23317558         10.23317558
## PubDate.wkday.fctr                   5.07569087          5.07569087
## PubDate.date.fctr                    3.41328270          3.41328270
## PubDate.apm.fctr                     3.33816477          3.33816477
## S.year                               2.44850130          2.44850130
## A.one                                2.28481932          2.28481932
## S.one                                2.05652251          2.05652251
## S.week                               2.02506391          2.02506391
## S.new                                2.00587235          2.00587235
## A.time                               1.98473239          1.98473239
## S.time                               1.96971733          1.96971733
## A.new                                1.94965236          1.94965236
## S.state                              1.92409986          1.92409986
## S.report                             1.81584808          1.81584808
## Headline.pfx.fctr                    1.68916167          1.68916167
## S.said                               1.67152559          1.67152559
## S.will                               1.57706331          1.57706331
## S.can                                1.57504901          1.57504901
## H.day                                1.45649237          1.45649237
## A.will                               1.45150094          1.45150094
## A.can                                1.39035571          1.39035571
## S.make                               1.35787270          1.35787270
## S.share                              1.21163721          1.21163721
## A.compani                            1.19132133          1.19132133
## S.compani                            1.12248041          1.12248041
## A.take                               1.02430134          1.02430134
## S.newyork                            1.01138846          1.01138846
## S.show                               0.98001272          0.98001272
## S.presid                             0.95023344          0.95023344
## S.take                               0.93917819          0.93917819
## S.first                              0.87223540          0.87223540
## A.presid                             0.84346221          0.84346221
## S.day                                0.78168195          0.78168195
## A.day                                0.73118231          0.73118231
## H.new                                0.67963347          0.67963347
## H.has.ebola                          0.67310512          0.67310512
## H.report                             0.62938276          0.62938276
## S.articl                             0.42849098          0.42849098
## S.intern                             0.42760187          0.42760187
## A.articl                             0.42722693          0.42722693
## A.intern                             0.41988579          0.41988579
## H.newyork                            0.37023925          0.37023925
## H.X2014                              0.27382624          0.27382624
## H.today                              0.26092552          0.26092552
## H.week                               0.25618220          0.25618220
## H.fashion                            0.13609276          0.13609276
## S.fashion                            0.08449006          0.08449006
```

```r
print(subset(glb_feats_df, is.ConditionalX.y & is.na(importance))[,
    c("is.ConditionalX.y", 
      grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
```

```
## [1] is.ConditionalX.y           importance                 
## [3] Conditional.X.rf.importance Final.rf.importance        
## <0 rows> (or 0-length row.names)
```

```r
save(glb_feats_df, glb_entity_df, 
     glb_trnent_df, glb_fitent_df, glb_OOBent_df, glb_newent_df,
     glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
     glb_sel_mdl, glb_sel_mdl_id,
     glb_fin_mdl, glb_fin_mdl_id,
    file=paste0(glb_out_pfx, "prdnew_dsk.RData"))

# tmp_replay_lst <- replay.petrisim(pn=glb_analytics_pn, 
#     replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
#         "data.new.prediction")), flip_coord=TRUE)
# print(ggplot.petrinet(tmp_replay_lst[["pn"]]) + coord_flip())
```

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 
#```{r q1, cache=FALSE}
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
#```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
##                   chunk_label chunk_step_major chunk_step_minor  elapsed
## 12                 fit.models                6                2  849.561
## 14          fit.data.training                7                1 1230.549
## 15           predict.data.new                8                0 1334.661
## 13          fit.data.training                7                0  949.427
## 7             select.features                4                0  139.703
## 11                 fit.models                6                1  199.203
## 3                cleanse.data                2                1   25.148
## 4         manage.missing.data                2                1   36.598
## 5         encodeORretype.data                2                2   40.549
## 9     partition.data.training                5                0  146.153
## 8  remove.correlated.features                4                1  142.664
## 10                 fit.models                6                0  148.494
## 2       inspectORexplore.data                2                0    1.209
## 6            extract.features                3                0   40.615
## 1                 import_data                1                0    0.002
##    elapsed_diff
## 12      650.358
## 14      281.122
## 15      104.112
## 13       99.866
## 7        99.088
## 11       50.709
## 3        23.939
## 4        11.450
## 5         3.951
## 9         3.489
## 8         2.961
## 10        2.341
## 2         1.207
## 6         0.066
## 1         0.000
```

```
## [1] "Total Elapsed Time: 1,334.661 secs"
```

![](NYTBlogs_hdlpfx7_files/figure-html/print_sessionInfo-1.png) 

```
## R version 3.1.3 (2015-03-09)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.3 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] tcltk     grid      stats     graphics  grDevices utils     datasets 
## [8] methods   base     
## 
## other attached packages:
##  [1] randomForest_4.6-10 rpart.plot_1.5.2    rpart_4.1-9        
##  [4] ROCR_1.0-7          gplots_2.16.0       caTools_1.17.1     
##  [7] caret_6.0-41        tm_0.6              NLP_0.1-6          
## [10] mice_2.22           lattice_0.20-31     Rcpp_0.11.5        
## [13] plyr_1.8.1          sqldf_0.4-10        RSQLite_1.0.0      
## [16] DBI_0.3.1           gsubfn_0.6-6        proto_0.3-10       
## [19] reshape2_1.4.1      doBy_4.5-13         survival_2.38-1    
## [22] ggplot2_1.0.1      
## 
## loaded via a namespace (and not attached):
##  [1] bitops_1.0-6        BradleyTerry2_1.0-6 brglm_0.5-9        
##  [4] car_2.0-25          chron_2.3-45        class_7.3-12       
##  [7] codetools_0.2-11    colorspace_1.2-6    compiler_3.1.3     
## [10] digest_0.6.8        e1071_1.6-4         evaluate_0.5.5     
## [13] foreach_1.4.2       formatR_1.1         gdata_2.13.3       
## [16] gtable_0.1.2        gtools_3.4.1        htmltools_0.2.6    
## [19] iterators_1.0.7     KernSmooth_2.23-14  knitr_1.9          
## [22] labeling_0.3        lme4_1.1-7          MASS_7.3-40        
## [25] Matrix_1.2-0        mgcv_1.8-6          minqa_1.2.4        
## [28] munsell_0.4.2       nlme_3.1-120        nloptr_1.0.4       
## [31] nnet_7.3-9          parallel_3.1.3      pbkrtest_0.4-2     
## [34] quantreg_5.11       RColorBrewer_1.1-2  rmarkdown_0.5.1    
## [37] scales_0.2.4        slam_0.1-32         SparseM_1.6        
## [40] splines_3.1.3       stringr_0.6.2       tools_3.1.3        
## [43] yaml_2.1.13
```
