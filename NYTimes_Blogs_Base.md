# NYTimes:Blogs:: Popular classification
bdanalytics  

**  **    
**Date: (Wed) Apr 29, 2015**    

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
glb_is_separate_newent_dataset <- TRUE    # or TRUE
glb_split_entity_newent_datasets <- TRUE   # or FALSE
glb_split_newdata_method <- "sample"          # "condition" or "sample" or "copy"
glb_split_newdata_condition <- "<col_name> <condition_operator> <value>"    # or NULL
glb_split_newdata_size_ratio <- 0.3               # > 0 & < 1
glb_split_sample.seed <- 123               # or any integer
glb_max_trnent_obs <- NULL # or any integer
glb_drop_vars <- c(NULL) # or c("<col_name>")

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
glb_id_vars <- c("UniqueID")
glb_date_vars <- c("PubDate")

glb_is_textual <- TRUE # vs. glb_is_numerical ???
#Sys.setlocale("LC_ALL", "C") # For english
glb_txt_vars <- c("Headline", "Snippet", "Abstract")   
glb_append_stop_words <- NULL # or c("<freq_word>") 
glb_sprs_threshold <- 0.950     # Ideally, numrows(glb_feats_df) << numrows(glb_trnent_df)

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

glb_impute_na_data <- FALSE            # or TRUE
glb_mice_complete.seed <- 144               # or any integer

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
        glb_model_evl_criteria <- c("max.Accuracy.OOB", "max.Kappa.OOB", "min.aic.fit") else
        glb_model_evl_criteria <- c("max.Accuracy.OOB", "max.Kappa.OOB")
}

glb_sel_mdl_id <- NULL # or "<model_id_prefix>.<model_method>"
glb_fin_mdl_id <- glb_sel_mdl_id # or "Final"

glb_out_pfx <- "NYTimes_Blogs_Base_"

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

![](NYTimes_Blogs_Base_files/figure-html/set_global_options-1.png) 

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
if (!is.null(glb_max_trnent_obs)) {
    if (nrow(glb_trnent_df) > glb_max_trnent_obs) {
        warning("glb_trnent_df restricted to glb_max_trnent_obs: ", 
                format(glb_max_trnent_obs, big.mark=","))
        org_entity_df <- glb_trnent_df
        glb_trnent_df <- org_entity_df[split <- 
            sample.split(org_entity_df[, glb_rsp_var_raw], 
                         SplitRatio=glb_max_trnent_obs), ]
        org_entity_df <- NULL
    }
#     if (nrow(glb_newent_df) > glb_max_obs) {
#         warning("glb_newent_df restricted to glb_max_obs: ", format(glb_max_obs, big.mark=","))        
#         org_newent_df <- glb_newent_df
#         glb_newent_df <- org_newent_df[split <- 
#             sample.split(org_newent_df[, glb_rsp_var_raw], SplitRatio=glb_max_obs), ]
#         org_newent_df <- NULL
#     }    
}

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

glb_script_df <- rbind(glb_script_df,
                   data.frame(chunk_label="cleanse_data", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##           chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed   import_data                1                0   0.002
## elapsed1 cleanse_data                2                0   0.864
```

## Step `2`: cleanse data

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="inspectORexplore.data", 
                              chunk_step_major=max(glb_script_df$chunk_step_major), 
                              chunk_step_minor=1,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                    chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed1          cleanse_data                2                0   0.864
## elapsed2 inspectORexplore.data                2                1   0.903
```

### Step `2`.`1`: inspect/explore data

```r
#print(str(glb_trnent_df))
#View(glb_trnent_df)

# List info gathered for various columns
# <col_name>:   <description>; <notes>

# Create new features that help diagnostics

#   Convert dates to numbers 
#       typically, dates come in as chars; 
#           so this must be done before converting chars to factors
myextract_dates_df <- function(df, vars) {
    for (var in vars) {
        dates_df <- data.frame(.date=strptime(df[, var], "%Y-%m-%d %H:%M:%S"))
        dates_df[, paste0(var, ".year")] <- as.numeric(format(dates_df$.date, "%Y"))
        dates_df[, paste0(var, ".month")] <- as.numeric(format(dates_df$.date, "%m")) 
        dates_df[, paste0(var, ".date")] <- as.numeric(format(dates_df$.date, "%d"))   
        dates_df[, paste0(var, ".wkday")] <- as.numeric(format(dates_df$.date, "%w")) 
        dates_df[, paste0(var, ".hour")] <- as.numeric(format(dates_df$.date, "%H"))                                
        dates_df[, paste0(var, ".minute")] <- as.numeric(format(dates_df$.date, "%M"))                                                                    
        dates_df[, paste0(var, ".second")] <- as.numeric(format(dates_df$.date, "%S")) 
    }
    #myprint_df(dates_df)
    return(subset(dates_df, select=-.date))
}

if (!is.null(glb_date_vars)) {
    glb_entity_df <- cbind(glb_entity_df, 
                           myextract_dates_df(glb_entity_df, glb_date_vars))
    glb_trnent_df <- cbind(glb_trnent_df, 
                           myextract_dates_df(glb_trnent_df, glb_date_vars))
    glb_newent_df <- cbind(glb_newent_df, 
                           myextract_dates_df(glb_newent_df, glb_date_vars))
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_date_vars)
}

#   Create factors of string variables
str_vars <- sapply(names(glb_trnent_df), function(var)  
                    ifelse(class(glb_trnent_df[, var]) == "character", var, ""))
print(str_vars <- str_vars[str_vars != ""])
```

```
##         NewsDesk      SectionName   SubsectionName         Headline 
##       "NewsDesk"    "SectionName" "SubsectionName"       "Headline" 
##          Snippet         Abstract          PubDate 
##        "Snippet"       "Abstract"        "PubDate"
```

```r
if (length(str_vars <- setdiff(str_vars, 
                               glb_exclude_vars_as_features)) > 0) {
    warning("Creating factors of string variables:", paste0(str_vars, collapse=", "))
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, str_vars)
    for (var in str_vars) {
        glb_entity_df[, paste0(var, ".fctr")] <- factor(glb_entity_df[, var], 
                        as.factor(unique(glb_entity_df[, var])))
        glb_trnent_df[, paste0(var, ".fctr")] <- factor(glb_trnent_df[, var], 
                        as.factor(unique(glb_entity_df[, var])))
        glb_newent_df[, paste0(var, ".fctr")] <- factor(glb_newent_df[, var], 
                        as.factor(unique(glb_entity_df[, var])))
    }
}
```

```
## Warning: Creating factors of string variables:NewsDesk, SectionName,
## SubsectionName
```

```r
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

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
#         <col_name>=<table>[as.character(<col2_name>)],
#         <col_name>=as.numeric(<col2_name>),

        .rnorm=rnorm(n=nrow(obs_df))
                        )

    # If levels of a factor are different across obs_df & glb_newent_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    print(summary(obs_df))
    print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}

glb_entity_df <- add_new_diag_feats(glb_entity_df)
```

```
## Loading required package: plyr
```

```
##    NewsDesk         SectionName        SubsectionName    
##  Length:8402        Length:8402        Length:8402       
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##    Headline           Snippet            Abstract        
##  Length:8402        Length:8402        Length:8402       
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##    WordCount         PubDate             Popular          UniqueID   
##  Min.   :    0.0   Length:8402        Min.   :0.0000   Min.   :   1  
##  1st Qu.:  188.0   Class :character   1st Qu.:0.0000   1st Qu.:2101  
##  Median :  377.0   Mode  :character   Median :0.0000   Median :4202  
##  Mean   :  528.8                      Mean   :0.1673   Mean   :4202  
##  3rd Qu.:  735.0                      3rd Qu.:0.0000   3rd Qu.:6302  
##  Max.   :10912.0                      Max.   :1.0000   Max.   :8402  
##                                       NA's   :1870                   
##   PubDate.year  PubDate.month    PubDate.date   PubDate.wkday  
##  Min.   :2014   Min.   : 9.00   Min.   : 1.00   Min.   :0.000  
##  1st Qu.:2014   1st Qu.: 9.00   1st Qu.: 8.00   1st Qu.:2.000  
##  Median :2014   Median :10.00   Median :15.00   Median :3.000  
##  Mean   :2014   Mean   :10.38   Mean   :15.17   Mean   :2.915  
##  3rd Qu.:2014   3rd Qu.:11.00   3rd Qu.:22.00   3rd Qu.:4.000  
##  Max.   :2014   Max.   :12.00   Max.   :31.00   Max.   :6.000  
##                                                                
##   PubDate.hour   PubDate.minute  PubDate.second   NewsDesk.fctr 
##  Min.   : 0.00   Min.   : 0.00   Min.   : 0.00           :2408  
##  1st Qu.: 9.00   1st Qu.: 5.00   1st Qu.:14.00   Business:2026  
##  Median :12.00   Median :24.00   Median :30.00   Culture : 909  
##  Mean   :12.22   Mean   :24.11   Mean   :29.49   TStyle  : 829  
##  3rd Qu.:16.00   3rd Qu.:40.00   3rd Qu.:44.00   OpEd    : 680  
##  Max.   :23.00   Max.   :59.00   Max.   :59.00   Foreign : 477  
##                                                  (Other) :1073  
##      SectionName.fctr      SubsectionName.fctr     .rnorm         
##              :2899                   :6176     Min.   :-3.881663  
##  Business Day:1437    Dealbook       :1256     1st Qu.:-0.665043  
##  Arts        : 849    Education      : 414     Median :-0.004510  
##  Opinion     : 783    Asia Pacific   : 259     Mean   :-0.006807  
##  U.S.        : 657    Small Business : 181     3rd Qu.: 0.664125  
##  Technology  : 442    Room For Debate:  82     Max.   : 3.356092  
##  (Other)     :1335    (Other)        :  34                        
##            NewsDesk         SectionName      SubsectionName 
##                   0                   0                   0 
##            Headline             Snippet            Abstract 
##                   0                   0                   0 
##           WordCount             PubDate             Popular 
##                   0                   0                1870 
##            UniqueID        PubDate.year       PubDate.month 
##                   0                   0                   0 
##        PubDate.date       PubDate.wkday        PubDate.hour 
##                   0                   0                   0 
##      PubDate.minute      PubDate.second       NewsDesk.fctr 
##                   0                   0                   0 
##    SectionName.fctr SubsectionName.fctr              .rnorm 
##                   0                   0                   0
```

```r
glb_trnent_df <- add_new_diag_feats(glb_trnent_df)
```

```
##    NewsDesk         SectionName        SubsectionName    
##  Length:6532        Length:6532        Length:6532       
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##    Headline           Snippet            Abstract        
##  Length:6532        Length:6532        Length:6532       
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##    WordCount         PubDate             Popular          UniqueID   
##  Min.   :    0.0   Length:6532        Min.   :0.0000   Min.   :   1  
##  1st Qu.:  187.0   Class :character   1st Qu.:0.0000   1st Qu.:1634  
##  Median :  374.0   Mode  :character   Median :0.0000   Median :3266  
##  Mean   :  524.4                      Mean   :0.1673   Mean   :3266  
##  3rd Qu.:  723.2                      3rd Qu.:0.0000   3rd Qu.:4899  
##  Max.   :10912.0                      Max.   :1.0000   Max.   :6532  
##                                                                      
##   PubDate.year  PubDate.month     PubDate.date   PubDate.wkday  
##  Min.   :2014   Min.   : 9.000   Min.   : 1.00   Min.   :0.000  
##  1st Qu.:2014   1st Qu.: 9.000   1st Qu.: 8.00   1st Qu.:2.000  
##  Median :2014   Median :10.000   Median :16.00   Median :3.000  
##  Mean   :2014   Mean   : 9.919   Mean   :15.68   Mean   :2.931  
##  3rd Qu.:2014   3rd Qu.:11.000   3rd Qu.:23.00   3rd Qu.:4.000  
##  Max.   :2014   Max.   :11.000   Max.   :31.00   Max.   :6.000  
##                                                                 
##   PubDate.hour   PubDate.minute  PubDate.second   NewsDesk.fctr 
##  Min.   : 0.00   Min.   : 0.00   Min.   : 0.00           :1846  
##  1st Qu.: 9.00   1st Qu.: 6.00   1st Qu.:14.00   Business:1548  
##  Median :12.00   Median :25.00   Median :29.00   TStyle  : 724  
##  Mean   :12.25   Mean   :24.42   Mean   :29.31   Culture : 676  
##  3rd Qu.:16.00   3rd Qu.:40.00   3rd Qu.:44.00   OpEd    : 521  
##  Max.   :23.00   Max.   :59.00   Max.   :59.00   Foreign : 375  
##                                                  (Other) : 842  
##      SectionName.fctr      SubsectionName.fctr     .rnorm         
##              :2300                   :4826     Min.   :-3.666157  
##  Business Day:1092    Dealbook       : 952     1st Qu.:-0.655927  
##  Arts        : 675    Education      : 325     Median : 0.002704  
##  Opinion     : 607    Asia Pacific   : 203     Mean   : 0.008495  
##  U.S.        : 505    Small Business : 140     3rd Qu.: 0.664102  
##  Technology  : 330    Room For Debate:  62     Max.   : 3.866465  
##  (Other)     :1023    (Other)        :  24                        
##            NewsDesk         SectionName      SubsectionName 
##                   0                   0                   0 
##            Headline             Snippet            Abstract 
##                   0                   0                   0 
##           WordCount             PubDate             Popular 
##                   0                   0                   0 
##            UniqueID        PubDate.year       PubDate.month 
##                   0                   0                   0 
##        PubDate.date       PubDate.wkday        PubDate.hour 
##                   0                   0                   0 
##      PubDate.minute      PubDate.second       NewsDesk.fctr 
##                   0                   0                   0 
##    SectionName.fctr SubsectionName.fctr              .rnorm 
##                   0                   0                   0
```

```r
glb_newent_df <- add_new_diag_feats(glb_newent_df)
```

```
##    NewsDesk         SectionName        SubsectionName    
##  Length:1870        Length:1870        Length:1870       
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##    Headline           Snippet            Abstract           WordCount     
##  Length:1870        Length:1870        Length:1870        Min.   :   0.0  
##  Class :character   Class :character   Class :character   1st Qu.: 189.0  
##  Mode  :character   Mode  :character   Mode  :character   Median : 384.5  
##                                                           Mean   : 544.2  
##                                                           3rd Qu.: 760.8  
##                                                           Max.   :5956.0  
##                                                                           
##    PubDate             UniqueID     PubDate.year  PubDate.month
##  Length:1870        Min.   :6533   Min.   :2014   Min.   :12   
##  Class :character   1st Qu.:7000   1st Qu.:2014   1st Qu.:12   
##  Mode  :character   Median :7468   Median :2014   Median :12   
##                     Mean   :7468   Mean   :2014   Mean   :12   
##                     3rd Qu.:7935   3rd Qu.:2014   3rd Qu.:12   
##                     Max.   :8402   Max.   :2014   Max.   :12   
##                                                                
##   PubDate.date   PubDate.wkday    PubDate.hour  PubDate.minute 
##  Min.   : 1.00   Min.   :0.000   Min.   : 0.0   Min.   : 0.00  
##  1st Qu.: 6.00   1st Qu.:2.000   1st Qu.: 9.0   1st Qu.: 4.00  
##  Median :12.00   Median :3.000   Median :12.0   Median :21.50  
##  Mean   :13.37   Mean   :2.858   Mean   :12.1   Mean   :23.04  
##  3rd Qu.:19.00   3rd Qu.:4.000   3rd Qu.:15.0   3rd Qu.:39.00  
##  Max.   :31.00   Max.   :6.000   Max.   :23.0   Max.   :59.00  
##                                                                
##  PubDate.second   NewsDesk.fctr     SectionName.fctr
##  Min.   : 0.00           :562               :599    
##  1st Qu.:15.00   Business:478   Business Day:345    
##  Median :30.00   Culture :233   Opinion     :176    
##  Mean   :30.14   OpEd    :159   Arts        :174    
##  3rd Qu.:46.00   TStyle  :105   U.S.        :152    
##  Max.   :59.00   Foreign :102   Technology  :112    
##                  (Other) :231   (Other)     :312    
##       SubsectionName.fctr     .rnorm         
##                 :1350     Min.   :-3.514057  
##  Dealbook       : 304     1st Qu.:-0.705270  
##  Education      :  89     Median :-0.000622  
##  Asia Pacific   :  56     Mean   :-0.006081  
##  Small Business :  41     3rd Qu.: 0.674599  
##  Room For Debate:  20     Max.   : 3.186664  
##  (Other)        :  10                        
##            NewsDesk         SectionName      SubsectionName 
##                   0                   0                   0 
##            Headline             Snippet            Abstract 
##                   0                   0                   0 
##           WordCount             PubDate            UniqueID 
##                   0                   0                   0 
##        PubDate.year       PubDate.month        PubDate.date 
##                   0                   0                   0 
##       PubDate.wkday        PubDate.hour      PubDate.minute 
##                   0                   0                   0 
##      PubDate.second       NewsDesk.fctr    SectionName.fctr 
##                   0                   0                   0 
## SubsectionName.fctr              .rnorm 
##                   0                   0
```

```r
# Histogram of predictor in glb_trnent_df & glb_newent_df
plot_df <- rbind(cbind(glb_trnent_df[, glb_rsp_var_raw, FALSE], data.frame(.data="Training")),
                 cbind(glb_trnent_df[, glb_rsp_var_raw, FALSE], data.frame(.data="New")))
print(myplot_histogram(plot_df, glb_rsp_var_raw) + facet_wrap(~ .data))
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](NYTimes_Blogs_Base_files/figure-html/inspectORexplore_data-1.png) 

```r
# used later in encode.retype.data chunk
glb_display_class_dstrb <- function(var) {
    plot_df <- rbind(cbind(glb_trnent_df[, var, FALSE], 
                           data.frame(.data="Training")),
                     cbind(glb_trnent_df[, var, FALSE], 
                           data.frame(.data="New")))
    xtab_df <- mycreate_xtab(plot_df, c(".data", var))
    rownames(xtab_df) <- xtab_df$.data
    xtab_df <- subset(xtab_df, select=-.data)
    print(xtab_df / rowSums(xtab_df))    
}    
if (glb_is_classification) glb_display_class_dstrb(glb_rsp_var_raw)
```

```
## Loading required package: reshape2
```

```
##          Popular.0 Popular.1
## New      0.8326699 0.1673301
## Training 0.8326699 0.1673301
```

```r
# Check for duplicates in glb_id_vars
if (length(glb_id_vars) > 0) {
    id_vars_dups_df <- subset(id_vars_df <- 
            mycreate_tbl_df(glb_entity_df[, glb_id_vars, FALSE], glb_id_vars),
                                .freq > 1)
} else {
    tmp_entity_df <- glb_entity_df
    tmp_entity_df$.rownames <- rownames(tmp_entity_df)
    id_vars_dups_df <- subset(id_vars_df <- 
            mycreate_tbl_df(tmp_entity_df[, ".rownames", FALSE], ".rownames"),
                                .freq > 1)
}

if (nrow(id_vars_dups_df) > 0) {
    warning("Duplicates found in glb_id_vars data:", nrow(id_vars_dups_df))
    myprint_df(id_vars_dups_df)
    } else {
        # glb_id_vars are unique across obs in both glb_<>_df
        glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_id_vars)
}

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
# print(mycreate_xtab(glb_trnent_df, <col1_name>))
# print(mycreate_xtab(glb_trnent_df, c(<col1_name>, <col2_name>)))
# print(<col1_name>_<col2_name>_xtab_glb_trnent_df <- 
#   mycreate_xtab(glb_trnent_df, c("<col1_name>", "<col2_name>")))
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
    data.frame(chunk_label="manage_missing_data", 
        chunk_step_major=max(glb_script_df$chunk_step_major), 
        chunk_step_minor=glb_script_df[nrow(glb_script_df), "chunk_step_minor"]+1,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                    chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed2 inspectORexplore.data                2                1   0.903
## elapsed3   manage_missing_data                2                2   2.831
```

### Step `2`.`2`: manage missing data

```r
# print(sapply(names(glb_trnent_df), function(col) sum(is.na(glb_trnent_df[, col]))))
# print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))
# glb_trnent_df <- na.omit(glb_trnent_df)
# glb_newent_df <- na.omit(glb_newent_df)
# df[is.na(df)] <- 0

# Not refactored into mydsutils.R since glb_*_df might be reassigned
glb_impute_missing_data <- function(entity_df, newent_df) {
    if (!glb_is_separate_newent_dataset) {
        # Combine entity & newent
        union_df <- rbind(mutate(entity_df, .src = "entity"),
                          mutate(newent_df, .src = "newent"))
        union_imputed_df <- union_df[, setdiff(setdiff(names(entity_df), 
                                                       glb_rsp_var), 
                                               glb_exclude_vars_as_features)]
        print(summary(union_imputed_df))
    
        require(mice)
        set.seed(glb_mice_complete.seed)
        union_imputed_df <- complete(mice(union_imputed_df))
        print(summary(union_imputed_df))
    
        union_df[, names(union_imputed_df)] <- union_imputed_df[, names(union_imputed_df)]
        print(summary(union_df))
#         union_df$.rownames <- rownames(union_df)
#         union_df <- orderBy(~.rownames, union_df)
#         
#         imp_entity_df <- myimport_data(
#             url="<imputed_trnng_url>", 
#             comment="imp_entity_df", force_header=TRUE, print_diagn=TRUE)
#         print(all.equal(subset(union_df, select=-c(.src, .rownames, .rnorm)), 
#                         imp_entity_df))
        
        # Partition again
        glb_trnent_df <<- subset(union_df, .src == "entity", select=-c(.src, .rownames))
        comment(glb_trnent_df) <- "entity_df"
        glb_newent_df <<- subset(union_df, .src == "newent", select=-c(.src, .rownames))
        comment(glb_newent_df) <- "newent_df"
        
        # Generate summaries
        print(summary(entity_df))
        print(sapply(names(entity_df), function(col) sum(is.na(entity_df[, col]))))
        print(summary(newent_df))
        print(sapply(names(newent_df), function(col) sum(is.na(newent_df[, col]))))
    
    } else stop("Not implemented yet")
}

if (glb_impute_na_data) {
    if ((sum(sapply(names(glb_trnent_df), 
                    function(col) sum(is.na(glb_trnent_df[, col])))) > 0) | 
        (sum(sapply(names(glb_newent_df), 
                    function(col) sum(is.na(glb_newent_df[, col])))) > 0))
        glb_impute_missing_data(glb_trnent_df, glb_newent_df)
}    

glb_script_df <- rbind(glb_script_df, 
    data.frame(chunk_label="encodeORretype.data", 
        chunk_step_major=max(glb_script_df$chunk_step_major), 
        chunk_step_minor=glb_script_df[nrow(glb_script_df), "chunk_step_minor"]+1,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                  chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed3 manage_missing_data                2                2   2.831
## elapsed4 encodeORretype.data                2                3   3.123
```

### Step `2`.`3`: encode/retype data

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

if (!is.null(glb_map_rsp_raw_to_var)) {
    glb_entity_df[, glb_rsp_var] <- 
        glb_map_rsp_raw_to_var(glb_entity_df[, glb_rsp_var_raw])
    mycheck_map_results(mapd_df=glb_entity_df, 
                        from_col_name=glb_rsp_var_raw, to_col_name=glb_rsp_var)
        
    glb_trnent_df[, glb_rsp_var] <- 
        glb_map_rsp_raw_to_var(glb_trnent_df[, glb_rsp_var_raw])
    mycheck_map_results(mapd_df=glb_trnent_df, 
                        from_col_name=glb_rsp_var_raw, to_col_name=glb_rsp_var)
        
    if (glb_rsp_var %in% names(glb_newent_df)) {
        glb_newent_df[, glb_rsp_var] <- 
            glb_map_rsp_raw_to_var(glb_newent_df[, glb_rsp_var_raw])
        mycheck_map_results(mapd_df=glb_newent_df, 
                            from_col_name=glb_rsp_var_raw, to_col_name=glb_rsp_var)
    }
    
    if (glb_is_classification) glb_display_class_dstrb(glb_rsp_var)
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

![](NYTimes_Blogs_Base_files/figure-html/encodeORretype.data-1.png) 

```
##   Popular Popular.fctr   .n
## 1       0            N 5439
## 2       1            Y 1093
```

![](NYTimes_Blogs_Base_files/figure-html/encodeORretype.data-2.png) 

```
##          Popular.fctr.N Popular.fctr.Y
## New           0.8326699      0.1673301
## Training      0.8326699      0.1673301
```

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="extract.features", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                  chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed4 encodeORretype.data                2                3   3.123
## elapsed5    extract.features                3                0   5.551
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

if (glb_is_textual) {
    require(tm)
    
    glb_corpus_lst <- list(); glb_full_DTM_lst <- list(); glb_sprs_DTM_lst <- list();
    for (txt_var in glb_txt_vars) {
        print(sprintf("Building corpus for %s...", txt_var))
        
        txt_corpus <- Corpus(VectorSource(glb_entity_df[, txt_var]))
        txt_corpus <- tm_map(txt_corpus, tolower)
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation)
        txt_corpus <- tm_map(txt_corpus, removeWords, 
                             c(glb_append_stop_words, stopwords("english")))
        txt_corpus <- tm_map(txt_corpus, stemDocument)
        
        full_freqs_DTM <- DocumentTermMatrix(txt_corpus)
        print("   Full freqs:"); print(full_freqs_DTM)
        full_freqs_vctr <- colSums(as.matrix(full_freqs_DTM))
        names(full_freqs_vctr) <- dimnames(full_freqs_DTM)[[2]]
        full_freqs_df <- as.data.frame(full_freqs_vctr)
        names(full_freqs_df) <- "freq.full"
        full_freqs_df$term <- rownames(full_freqs_df)
        full_freqs_df <- orderBy(~ -freq.full, full_freqs_df)
        
        sprs_freqs_DTM <- removeSparseTerms(full_freqs_DTM, glb_sprs_threshold)
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
                  geom_hline(yintercept=glb_sprs_threshold, linetype = "dotted"))
        print(myplot_hbar(head(melt_freqs_df, 20), "term", "value", 
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
        }            
    }        

    # a working copy of this is reqd in manage.missingdata chunk
    union_df <- myrbind_df(mutate(glb_trnent_df, .src = "trnent"),
                           mutate(glb_newent_df, .src = "newent"))
    tmp_entity_df <- glb_entity_df
    mrg_id_vars <- ifelse(length(glb_id_vars) > 0, glb_id_vars, ".rownames")
    if (mrg_id_vars == ".rownames") {
        union_df$.rownames <- rownames(union_df)
        tmp_entity_df$.rownames <- rownames(tmp_entity_df)
    }
    mrg_entity_df <- merge(tmp_entity_df, union_df[, c(".src", mrg_id_vars)])
    
    # Partition again
    glb_trnent_df <- subset(mrg_entity_df, .src == "trnent", select=-c(.src))
    glb_newent_df <- subset(mrg_entity_df, .src == "newent", select=-c(.src))
    if (mrg_id_vars == ".rownames") {
        glb_trnent_df <- subset(glb_trnent_df, select=-c(.rownames))
        glb_newent_df <- subset(glb_newent_df, select=-c(.rownames))
    }
    comment(glb_trnent_df) <- "trnent_df"
    comment(glb_newent_df) <- "newent_df"

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
## [1] "   Full freqs:"
## <<DocumentTermMatrix (documents: 8402, terms: 9205)>>
## Non-/sparse entries: 44655/77295755
## Sparsity           : 100%
## Maximal term length: 31
## Weighting          : term frequency (tf)
## [1] "   Sparse freqs:"
## <<DocumentTermMatrix (documents: 8402, terms: 1)>>
## Non-/sparse entries: 604/7798
## Sparsity           : 93%
## Maximal term length: 3
## Weighting          : term frequency (tf)
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

![](NYTimes_Blogs_Base_files/figure-html/extract.features-1.png) ![](NYTimes_Blogs_Base_files/figure-html/extract.features-2.png) ![](NYTimes_Blogs_Base_files/figure-html/extract.features-3.png) 

```
## [1] "Building corpus for Snippet..."
## [1] "   Full freqs:"
## <<DocumentTermMatrix (documents: 8402, terms: 13820)>>
## Non-/sparse entries: 105959/116009681
## Sparsity           : 100%
## Maximal term length: 25
## Weighting          : term frequency (tf)
## [1] "   Sparse freqs:"
## <<DocumentTermMatrix (documents: 8402, terms: 7)>>
## Non-/sparse entries: 4630/54184
## Sparsity           : 92%
## Maximal term length: 7
## Weighting          : term frequency (tf)
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

![](NYTimes_Blogs_Base_files/figure-html/extract.features-4.png) ![](NYTimes_Blogs_Base_files/figure-html/extract.features-5.png) ![](NYTimes_Blogs_Base_files/figure-html/extract.features-6.png) 

```
## [1] "Building corpus for Abstract..."
## [1] "   Full freqs:"
## <<DocumentTermMatrix (documents: 8402, terms: 13864)>>
## Non-/sparse entries: 106317/116379011
## Sparsity           : 100%
## Maximal term length: 112
## Weighting          : term frequency (tf)
## [1] "   Sparse freqs:"
## <<DocumentTermMatrix (documents: 8402, terms: 7)>>
## Non-/sparse entries: 4639/54175
## Sparsity           : 92%
## Maximal term length: 7
## Weighting          : term frequency (tf)
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

![](NYTimes_Blogs_Base_files/figure-html/extract.features-7.png) ![](NYTimes_Blogs_Base_files/figure-html/extract.features-8.png) ![](NYTimes_Blogs_Base_files/figure-html/extract.features-9.png) 

```
## [1] "Binding DTM for Headline..."
```

![](NYTimes_Blogs_Base_files/figure-html/extract.features-10.png) ![](NYTimes_Blogs_Base_files/figure-html/extract.features-11.png) ![](NYTimes_Blogs_Base_files/figure-html/extract.features-12.png) 

```
## [1] "Binding DTM for Snippet..."
```

![](NYTimes_Blogs_Base_files/figure-html/extract.features-13.png) ![](NYTimes_Blogs_Base_files/figure-html/extract.features-14.png) ![](NYTimes_Blogs_Base_files/figure-html/extract.features-15.png) 

```
## [1] "Binding DTM for Abstract..."
```

![](NYTimes_Blogs_Base_files/figure-html/extract.features-16.png) ![](NYTimes_Blogs_Base_files/figure-html/extract.features-17.png) ![](NYTimes_Blogs_Base_files/figure-html/extract.features-18.png) 

```r
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

![](NYTimes_Blogs_Base_files/figure-html/extract.features-19.png) 

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
## elapsed5 extract.features                3                0   5.551
## elapsed6  select.features                4                0  85.848
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
## WordCount                     WordCount  0.257526549               0
## S.num.words.unq.log S.num.words.unq.log -0.244677426               0
## A.num.words.unq.log A.num.words.unq.log -0.244569301               0
## S.num.words.log         S.num.words.log -0.239719045               0
## A.num.words.log         A.num.words.log -0.239527732               0
## S.num.words.unq         S.num.words.unq -0.213378613               0
## A.num.words.unq         A.num.words.unq -0.211522561               0
## H.num.words.unq.log H.num.words.unq.log -0.208052027               0
## S.num.words                 S.num.words -0.207846930               0
## A.num.words                 A.num.words -0.205674056               0
## A.num.chars.log         A.num.chars.log -0.205487403               0
## S.num.chars.log         S.num.chars.log -0.205402971               0
## H.num.words.log         H.num.words.log -0.204378687               0
## H.num.words.unq         H.num.words.unq -0.193599112               0
## H.num.words                 H.num.words -0.190083728               0
## S.num.chars                 S.num.chars -0.178433540               0
## A.num.chars                 A.num.chars -0.176187472               0
## H.num.chars.log         H.num.chars.log -0.171062360               0
## PubDate.hour               PubDate.hour  0.159167673               0
## H.num.chars                 H.num.chars -0.147211183               0
## SubsectionName.fctr SubsectionName.fctr -0.137898635               0
## NewsDesk.fctr             NewsDesk.fctr -0.115969991               0
## SectionName.fctr       SectionName.fctr -0.089486409               0
## S.week                           S.week -0.084814939               0
## A.week                           A.week -0.084814939               0
## H.new                             H.new -0.079839251               0
## A.new                             A.new -0.069393513               0
## S.new                             S.new -0.069088180               0
## S.york                           S.york -0.064751157               0
## A.york                           A.york -0.064751157               0
## A.will                           A.will -0.060842964               0
## S.will                           S.will -0.060392694               0
## A.time                           A.time -0.057790617               0
## S.time                           S.time -0.057595102               0
## A.compani                     A.compani -0.053099633               0
## S.compani                     S.compani -0.053012962               0
## S.year                           S.year -0.051235765               0
## A.year                           A.year -0.051235765               0
## PubDate.wkday             PubDate.wkday -0.039801288               0
## PubDate.minute           PubDate.minute -0.031469083               0
## PubDate.month             PubDate.month  0.019148739               0
## A.has.http                   A.has.http -0.013592603               0
## PubDate.second           PubDate.second -0.012253600               0
## PubDate.date               PubDate.date -0.012014100               0
## UniqueID                       UniqueID  0.011824920               1
## .rnorm                           .rnorm -0.008703337               0
## PubDate.year               PubDate.year           NA               0
## H.has.http                   H.has.http           NA               0
## S.has.http                   S.has.http           NA               0
##                       cor.y.abs
## Popular             1.000000000
## WordCount           0.257526549
## S.num.words.unq.log 0.244677426
## A.num.words.unq.log 0.244569301
## S.num.words.log     0.239719045
## A.num.words.log     0.239527732
## S.num.words.unq     0.213378613
## A.num.words.unq     0.211522561
## H.num.words.unq.log 0.208052027
## S.num.words         0.207846930
## A.num.words         0.205674056
## A.num.chars.log     0.205487403
## S.num.chars.log     0.205402971
## H.num.words.log     0.204378687
## H.num.words.unq     0.193599112
## H.num.words         0.190083728
## S.num.chars         0.178433540
## A.num.chars         0.176187472
## H.num.chars.log     0.171062360
## PubDate.hour        0.159167673
## H.num.chars         0.147211183
## SubsectionName.fctr 0.137898635
## NewsDesk.fctr       0.115969991
## SectionName.fctr    0.089486409
## S.week              0.084814939
## A.week              0.084814939
## H.new               0.079839251
## A.new               0.069393513
## S.new               0.069088180
## S.york              0.064751157
## A.york              0.064751157
## A.will              0.060842964
## S.will              0.060392694
## A.time              0.057790617
## S.time              0.057595102
## A.compani           0.053099633
## S.compani           0.053012962
## S.year              0.051235765
## A.year              0.051235765
## PubDate.wkday       0.039801288
## PubDate.minute      0.031469083
## PubDate.month       0.019148739
## A.has.http          0.013592603
## PubDate.second      0.012253600
## PubDate.date        0.012014100
## UniqueID            0.011824920
## .rnorm              0.008703337
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
## elapsed6  85.848
## elapsed7  88.177
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
## Loading required package: lattice
## 
## Attaching package: 'caret'
## 
## The following object is masked from 'package:survival':
## 
##     cluster
```

```
## [1] "cor(A.week, S.week)=1.0000"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-1.png) 

```
## [1] "cor(Popular.fctr, A.week)=-0.0848"
## [1] "cor(Popular.fctr, S.week)=-0.0848"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.week as highly correlated with A.week
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-2.png) 

```
## [1] "cor(A.year, S.year)=1.0000"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-3.png) 

```
## [1] "cor(Popular.fctr, A.year)=-0.0512"
## [1] "cor(Popular.fctr, S.year)=-0.0512"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.year as highly correlated with A.year
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-4.png) 

```
## [1] "cor(A.york, S.york)=1.0000"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-5.png) 

```
## [1] "cor(Popular.fctr, A.york)=-0.0648"
## [1] "cor(Popular.fctr, S.york)=-0.0648"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.york as highly correlated with A.york
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-6.png) 

```
## [1] "cor(A.time, S.time)=0.9991"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-7.png) 

```
## [1] "cor(Popular.fctr, A.time)=-0.0578"
## [1] "cor(Popular.fctr, S.time)=-0.0576"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.time as highly correlated with A.time
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-8.png) 

```
## [1] "cor(A.new, S.new)=0.9990"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-9.png) 

```
## [1] "cor(Popular.fctr, A.new)=-0.0694"
## [1] "cor(Popular.fctr, S.new)=-0.0691"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.new as highly correlated with A.new
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-10.png) 

```
## [1] "cor(A.num.words.unq.log, S.num.words.unq.log)=0.9990"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-11.png) 

```
## [1] "cor(Popular.fctr, A.num.words.unq.log)=-0.2446"
## [1] "cor(Popular.fctr, S.num.words.unq.log)=-0.2447"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.num.words.unq.log as highly correlated with
## S.num.words.unq.log
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-12.png) 

```
## [1] "cor(A.num.words.log, S.num.words.log)=0.9989"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-13.png) 

```
## [1] "cor(Popular.fctr, A.num.words.log)=-0.2395"
## [1] "cor(Popular.fctr, S.num.words.log)=-0.2397"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.num.words.log as highly correlated with
## S.num.words.log
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-14.png) 

```
## [1] "cor(A.num.chars.log, S.num.chars.log)=0.9988"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-15.png) 

```
## [1] "cor(Popular.fctr, A.num.chars.log)=-0.2055"
## [1] "cor(Popular.fctr, S.num.chars.log)=-0.2054"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.num.chars.log as highly correlated with
## A.num.chars.log
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-16.png) 

```
## [1] "cor(A.compani, S.compani)=0.9988"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-17.png) 

```
## [1] "cor(Popular.fctr, A.compani)=-0.0531"
## [1] "cor(Popular.fctr, S.compani)=-0.0530"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.compani as highly correlated with A.compani
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-18.png) 

```
## [1] "cor(A.will, S.will)=0.9976"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-19.png) 

```
## [1] "cor(Popular.fctr, A.will)=-0.0608"
## [1] "cor(Popular.fctr, S.will)=-0.0604"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.will as highly correlated with A.will
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-20.png) 

```
## [1] "cor(H.num.words.log, H.num.words.unq.log)=0.9966"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-21.png) 

```
## [1] "cor(Popular.fctr, H.num.words.log)=-0.2044"
## [1] "cor(Popular.fctr, H.num.words.unq.log)=-0.2081"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.num.words.log as highly correlated with
## H.num.words.unq.log
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-22.png) 

```
## [1] "cor(H.num.words, H.num.words.unq)=0.9963"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-23.png) 

```
## [1] "cor(Popular.fctr, H.num.words)=-0.1901"
## [1] "cor(Popular.fctr, H.num.words.unq)=-0.1936"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.num.words as highly correlated with
## H.num.words.unq
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-24.png) 

```
## [1] "cor(S.num.words.log, S.num.words.unq.log)=0.9955"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-25.png) 

```
## [1] "cor(Popular.fctr, S.num.words.log)=-0.2397"
## [1] "cor(Popular.fctr, S.num.words.unq.log)=-0.2447"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.num.words.log as highly correlated with
## S.num.words.unq.log
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-26.png) 

```
## [1] "cor(A.num.words.unq, S.num.words.unq)=0.9944"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-27.png) 

```
## [1] "cor(Popular.fctr, A.num.words.unq)=-0.2115"
## [1] "cor(Popular.fctr, S.num.words.unq)=-0.2134"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.num.words.unq as highly correlated with
## S.num.words.unq
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-28.png) 

```
## [1] "cor(A.num.words, S.num.words)=0.9936"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-29.png) 

```
## [1] "cor(Popular.fctr, A.num.words)=-0.2057"
## [1] "cor(Popular.fctr, S.num.words)=-0.2078"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.num.words as highly correlated with
## S.num.words
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-30.png) 

```
## [1] "cor(S.num.words, S.num.words.unq)=0.9920"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-31.png) 

```
## [1] "cor(Popular.fctr, S.num.words)=-0.2078"
## [1] "cor(Popular.fctr, S.num.words.unq)=-0.2134"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.num.words as highly correlated with
## S.num.words.unq
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-32.png) 

```
## [1] "cor(A.num.chars, S.num.chars)=0.9887"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-33.png) 

```
## [1] "cor(Popular.fctr, A.num.chars)=-0.1762"
## [1] "cor(Popular.fctr, S.num.chars)=-0.1784"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.num.chars as highly correlated with
## S.num.chars
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-34.png) 

```
## [1] "cor(H.num.words.unq, H.num.words.unq.log)=0.9745"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-35.png) 

```
## [1] "cor(Popular.fctr, H.num.words.unq)=-0.1936"
## [1] "cor(Popular.fctr, H.num.words.unq.log)=-0.2081"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.num.words.unq as highly correlated with
## H.num.words.unq.log
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-36.png) 

```
## [1] "cor(H.num.chars, H.num.chars.log)=0.9643"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-37.png) 

```
## [1] "cor(Popular.fctr, H.num.chars)=-0.1472"
## [1] "cor(Popular.fctr, H.num.chars.log)=-0.1711"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.num.chars as highly correlated with
## H.num.chars.log
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-38.png) 

```
## [1] "cor(S.num.words.unq, S.num.words.unq.log)=0.9552"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-39.png) 

```
## [1] "cor(Popular.fctr, S.num.words.unq)=-0.2134"
## [1] "cor(Popular.fctr, S.num.words.unq.log)=-0.2447"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.num.words.unq as highly correlated with
## S.num.words.unq.log
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-40.png) 

```
## [1] "cor(A.num.chars.log, S.num.words.unq.log)=0.9457"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-41.png) 

```
## [1] "cor(Popular.fctr, A.num.chars.log)=-0.2055"
## [1] "cor(Popular.fctr, S.num.words.unq.log)=-0.2447"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified A.num.chars.log as highly correlated with
## S.num.words.unq.log
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-42.png) 

```
## [1] "cor(S.num.chars, S.num.words.unq.log)=0.9043"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-43.png) 

```
## [1] "cor(Popular.fctr, S.num.chars)=-0.1784"
## [1] "cor(Popular.fctr, S.num.words.unq.log)=-0.2447"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified S.num.chars as highly correlated with
## S.num.words.unq.log
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-44.png) 

```
## [1] "cor(H.num.chars.log, H.num.words.unq.log)=0.8844"
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-45.png) 

```
## [1] "cor(Popular.fctr, H.num.chars.log)=-0.1711"
## [1] "cor(Popular.fctr, H.num.words.unq.log)=-0.2081"
```

```
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
## geom_smooth: Only one unique x value each group.Maybe you want aes(group = 1)?
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified H.num.chars.log as highly correlated with
## H.num.words.unq.log
```

![](NYTimes_Blogs_Base_files/figure-html/remove_correlated_features-46.png) 

```
##                                      id        cor.y exclude.as.feat
## Popular                         Popular  1.000000000               1
## WordCount                     WordCount  0.257526549               0
## PubDate.hour               PubDate.hour  0.159167673               0
## PubDate.month             PubDate.month  0.019148739               0
## UniqueID                       UniqueID  0.011824920               1
## .rnorm                           .rnorm -0.008703337               0
## PubDate.date               PubDate.date -0.012014100               0
## PubDate.second           PubDate.second -0.012253600               0
## A.has.http                   A.has.http -0.013592603               0
## PubDate.minute           PubDate.minute -0.031469083               0
## PubDate.wkday             PubDate.wkday -0.039801288               0
## S.year                           S.year -0.051235765               0
## A.year                           A.year -0.051235765               0
## S.compani                     S.compani -0.053012962               0
## A.compani                     A.compani -0.053099633               0
## S.time                           S.time -0.057595102               0
## A.time                           A.time -0.057790617               0
## S.will                           S.will -0.060392694               0
## A.will                           A.will -0.060842964               0
## S.york                           S.york -0.064751157               0
## A.york                           A.york -0.064751157               0
## S.new                             S.new -0.069088180               0
## A.new                             A.new -0.069393513               0
## H.new                             H.new -0.079839251               0
## S.week                           S.week -0.084814939               0
## A.week                           A.week -0.084814939               0
## SectionName.fctr       SectionName.fctr -0.089486409               0
## NewsDesk.fctr             NewsDesk.fctr -0.115969991               0
## SubsectionName.fctr SubsectionName.fctr -0.137898635               0
## H.num.chars                 H.num.chars -0.147211183               0
## H.num.chars.log         H.num.chars.log -0.171062360               0
## A.num.chars                 A.num.chars -0.176187472               0
## S.num.chars                 S.num.chars -0.178433540               0
## H.num.words                 H.num.words -0.190083728               0
## H.num.words.unq         H.num.words.unq -0.193599112               0
## H.num.words.log         H.num.words.log -0.204378687               0
## S.num.chars.log         S.num.chars.log -0.205402971               0
## A.num.chars.log         A.num.chars.log -0.205487403               0
## A.num.words                 A.num.words -0.205674056               0
## S.num.words                 S.num.words -0.207846930               0
## H.num.words.unq.log H.num.words.unq.log -0.208052027               0
## A.num.words.unq         A.num.words.unq -0.211522561               0
## S.num.words.unq         S.num.words.unq -0.213378613               0
## A.num.words.log         A.num.words.log -0.239527732               0
## S.num.words.log         S.num.words.log -0.239719045               0
## A.num.words.unq.log A.num.words.unq.log -0.244569301               0
## S.num.words.unq.log S.num.words.unq.log -0.244677426               0
## PubDate.year               PubDate.year           NA               0
## H.has.http                   H.has.http           NA               0
## S.has.http                   S.has.http           NA               0
##                       cor.y.abs      cor.high.X is.ConditionalX.y
## Popular             1.000000000            <NA>                NA
## WordCount           0.257526549            <NA>              TRUE
## PubDate.hour        0.159167673            <NA>              TRUE
## PubDate.month       0.019148739            <NA>              TRUE
## UniqueID            0.011824920            <NA>                NA
## .rnorm              0.008703337            <NA>              TRUE
## PubDate.date        0.012014100            <NA>              TRUE
## PubDate.second      0.012253600            <NA>              TRUE
## A.has.http          0.013592603            <NA>             FALSE
## PubDate.minute      0.031469083            <NA>              TRUE
## PubDate.wkday       0.039801288            <NA>              TRUE
## S.year              0.051235765            <NA>              TRUE
## A.year              0.051235765          S.year              TRUE
## S.compani           0.053012962            <NA>              TRUE
## A.compani           0.053099633       S.compani              TRUE
## S.time              0.057595102            <NA>              TRUE
## A.time              0.057790617          S.time              TRUE
## S.will              0.060392694            <NA>              TRUE
## A.will              0.060842964          S.will              TRUE
## S.york              0.064751157            <NA>              TRUE
## A.york              0.064751157          S.york              TRUE
## S.new               0.069088180            <NA>              TRUE
## A.new               0.069393513           S.new              TRUE
## H.new               0.079839251            <NA>              TRUE
## S.week              0.084814939            <NA>              TRUE
## A.week              0.084814939          S.week              TRUE
## SectionName.fctr    0.089486409            <NA>              TRUE
## NewsDesk.fctr       0.115969991            <NA>              TRUE
## SubsectionName.fctr 0.137898635            <NA>              TRUE
## H.num.chars         0.147211183            <NA>              TRUE
## H.num.chars.log     0.171062360     H.num.chars              TRUE
## A.num.chars         0.176187472            <NA>              TRUE
## S.num.chars         0.178433540     A.num.chars              TRUE
## H.num.words         0.190083728            <NA>              TRUE
## H.num.words.unq     0.193599112     H.num.words              TRUE
## H.num.words.log     0.204378687            <NA>              TRUE
## S.num.chars.log     0.205402971            <NA>              TRUE
## A.num.chars.log     0.205487403 S.num.chars.log              TRUE
## A.num.words         0.205674056            <NA>              TRUE
## S.num.words         0.207846930     A.num.words              TRUE
## H.num.words.unq.log 0.208052027 H.num.chars.log              TRUE
## A.num.words.unq     0.211522561            <NA>              TRUE
## S.num.words.unq     0.213378613     S.num.words              TRUE
## A.num.words.log     0.239527732            <NA>              TRUE
## S.num.words.log     0.239719045 A.num.words.log              TRUE
## A.num.words.unq.log 0.244569301            <NA>              TRUE
## S.num.words.unq.log 0.244677426     S.num.chars              TRUE
## PubDate.year                 NA            <NA>             FALSE
## H.has.http                   NA            <NA>             FALSE
## S.has.http                   NA            <NA>             FALSE
##                     is.cor.y.abs.low
## Popular                        FALSE
## WordCount                      FALSE
## PubDate.hour                   FALSE
## PubDate.month                  FALSE
## UniqueID                       FALSE
## .rnorm                         FALSE
## PubDate.date                   FALSE
## PubDate.second                 FALSE
## A.has.http                     FALSE
## PubDate.minute                 FALSE
## PubDate.wkday                  FALSE
## S.year                         FALSE
## A.year                         FALSE
## S.compani                      FALSE
## A.compani                      FALSE
## S.time                         FALSE
## A.time                         FALSE
## S.will                         FALSE
## A.will                         FALSE
## S.york                         FALSE
## A.york                         FALSE
## S.new                          FALSE
## A.new                          FALSE
## H.new                          FALSE
## S.week                         FALSE
## A.week                         FALSE
## SectionName.fctr               FALSE
## NewsDesk.fctr                  FALSE
## SubsectionName.fctr            FALSE
## H.num.chars                    FALSE
## H.num.chars.log                FALSE
## A.num.chars                    FALSE
## S.num.chars                    FALSE
## H.num.words                    FALSE
## H.num.words.unq                FALSE
## H.num.words.log                FALSE
## S.num.chars.log                FALSE
## A.num.chars.log                FALSE
## A.num.words                    FALSE
## S.num.words                    FALSE
## H.num.words.unq.log            FALSE
## A.num.words.unq                FALSE
## S.num.words.unq                FALSE
## A.num.words.log                FALSE
## S.num.words.log                FALSE
## A.num.words.unq.log            FALSE
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
## elapsed7  88.177
## elapsed8 124.117
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
glb_script_df <- rbind(glb_script_df, 
    data.frame(chunk_label="fit.models", 
        chunk_step_major=max(glb_script_df$chunk_step_major)+1,
        chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))        
print(tail(glb_script_df, 2))
```

```
##                      chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed8 partition.data.training                5                0 124.117
## elapsed9              fit.models                6                0 140.736
```

## Step `6`: fit models

```r
glb_models_lst <- list(); glb_models_df <- data.frame()

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
##           Reference
## Prediction    N    Y
##          N 3726  749
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.MFO.myMFO_classfr.N
## 1            N                                     3726
## 2            Y                                      749
##   Popular.fctr.predict.MFO.myMFO_classfr.Y
## 1                                        0
## 2                                        0
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] N
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] Y N
## Levels: N Y
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
##           Reference
## Prediction    N    Y
##          N 1713  344
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.MFO.myMFO_classfr.N
## 1            N                                     1713
## 2            Y                                      344
##   Popular.fctr.predict.MFO.myMFO_classfr.Y
## 1                                        0
## 2                                        0
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] N
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] N Y
## Levels: N Y
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
## 1                       0.46                 0.003         0.5
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

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-1.png) 

```
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 3726  749
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                              0
## 2            Y                                              0
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                           3726
## 2                                            749
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 3726  749
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                              0
## 2            Y                                              0
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                           3726
## 2                                            749
##           Reference
## Prediction    N    Y
##          N 3061  619
##          Y  665  130
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                           3061
## 2            Y                                            619
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                            665
## 2                                            130
##           Reference
## Prediction    N    Y
##          N 3061  619
##          Y  665  130
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                           3061
## 2            Y                                            619
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                            665
## 2                                            130
##           Reference
## Prediction    N    Y
##          N 3061  619
##          Y  665  130
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                           3061
## 2            Y                                            619
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                            665
## 2                                            130
##           Reference
## Prediction    N    Y
##          N 3061  619
##          Y  665  130
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                           3061
## 2            Y                                            619
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                            665
## 2                                            130
##           Reference
## Prediction    N    Y
##          N 3061  619
##          Y  665  130
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                           3061
## 2            Y                                            619
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                            665
## 2                                            130
##           Reference
## Prediction    N    Y
##          N 3061  619
##          Y  665  130
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                           3061
## 2            Y                                            619
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                            665
## 2                                            130
##           Reference
## Prediction    N    Y
##          N 3061  619
##          Y  665  130
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                           3061
## 2            Y                                            619
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                            665
## 2                                            130
##           Reference
## Prediction    N    Y
##          N 3726  749
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                           3726
## 2            Y                                            749
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                              0
## 2                                              0
##           Reference
## Prediction    N    Y
##          N 3726  749
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                           3726
## 2            Y                                            749
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                              0
## 2                                              0
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

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-2.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.Y
## 1            N                                           3726
## 2            Y                                            749
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 3726  749
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                              0
## 2            Y                                              0
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                           3726
## 2                                            749
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] Y
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] Y N
## Levels: N Y
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

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-3.png) 

```
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 1713  344
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                              0
## 2            Y                                              0
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                           1713
## 2                                            344
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 1713  344
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                              0
## 2            Y                                              0
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                           1713
## 2                                            344
##           Reference
## Prediction    N    Y
##          N 1408  295
##          Y  305   49
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                           1408
## 2            Y                                            295
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                            305
## 2                                             49
##           Reference
## Prediction    N    Y
##          N 1408  295
##          Y  305   49
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                           1408
## 2            Y                                            295
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                            305
## 2                                             49
##           Reference
## Prediction    N    Y
##          N 1408  295
##          Y  305   49
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                           1408
## 2            Y                                            295
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                            305
## 2                                             49
##           Reference
## Prediction    N    Y
##          N 1408  295
##          Y  305   49
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                           1408
## 2            Y                                            295
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                            305
## 2                                             49
##           Reference
## Prediction    N    Y
##          N 1408  295
##          Y  305   49
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                           1408
## 2            Y                                            295
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                            305
## 2                                             49
##           Reference
## Prediction    N    Y
##          N 1408  295
##          Y  305   49
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                           1408
## 2            Y                                            295
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                            305
## 2                                             49
##           Reference
## Prediction    N    Y
##          N 1408  295
##          Y  305   49
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                           1408
## 2            Y                                            295
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                            305
## 2                                             49
##           Reference
## Prediction    N    Y
##          N 1713  344
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                           1713
## 2            Y                                            344
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                              0
## 2                                              0
##           Reference
## Prediction    N    Y
##          N 1713  344
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                           1713
## 2            Y                                            344
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                              0
## 2                                              0
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

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-4.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.Y
## 1            N                                           1713
## 2            Y                                            344
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 1713  344
##   Popular.fctr Popular.fctr.predict.Random.myrandom_classfr.N
## 1            N                                              0
## 2            Y                                              0
##   Popular.fctr.predict.Random.myrandom_classfr.Y
## 1                                           1713
## 2                                            344
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] Y
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] N Y
## Levels: N Y
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
## 1                      0.336                 0.002   0.4975446
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
## [1] "    indep_vars: WordCount"
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

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-5.png) 

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
##           Reference
## Prediction    N    Y
##          N 3726  749
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.rpart.N
## 1            N                                        3726
## 2            Y                                         749
##   Popular.fctr.predict.Max.cor.Y.cv.0.rpart.Y
## 1                                           0
## 2                                           0
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] N
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] Y N
## Levels: N Y
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
##           Reference
## Prediction    N    Y
##          N 1713  344
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.rpart.N
## 1            N                                        1713
## 2            Y                                         344
##   Popular.fctr.predict.Max.cor.Y.cv.0.rpart.Y
## 1                                           0
## 2                                           0
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] N
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] N Y
## Levels: N Y
##          Prediction
## Reference    N    Y
##         N 1713    0
##         Y  344    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.327662e-01   0.000000e+00   8.159247e-01   8.486533e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   5.143944e-01   2.337097e-76 
##               model_id model_method     feats max.nTuningRuns
## 1 Max.cor.Y.cv.0.rpart        rpart WordCount               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.753                 0.063         0.5
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
## [1] "    indep_vars: WordCount"
## Fitting cp = 0 on full training set
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-6.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 4475 
## 
##             CP nsplit rel error
## 1 0.0022251891      0 1.0000000
## 2 0.0020026702     17 0.9559413
## 3 0.0019073050     19 0.9519359
## 4 0.0017801513     33 0.9238985
## 5 0.0013351135     39 0.9132176
## 6 0.0008900757     49 0.8998665
## 7 0.0002670227     54 0.8945260
## 8 0.0000000000     59 0.8931909
## 
## Variable importance
## WordCount 
##       100 
## 
## Node number 1: 4475 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.1673743  P(node) =1
##     class counts:  3726   749
##    probabilities: 0.833 0.167 
##   left son=2 (3291 obs) right son=3 (1184 obs)
##   Primary splits:
##       WordCount < 683.5  to the left,  improve=113.0263, (0 missing)
## 
## Node number 2: 3291 observations
##   predicted class=N  expected loss=0.09996961  P(node) =0.735419
##     class counts:  2962   329
##    probabilities: 0.900 0.100 
## 
## Node number 3: 1184 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.3547297  P(node) =0.264581
##     class counts:   764   420
##    probabilities: 0.645 0.355 
##   left son=6 (190 obs) right son=7 (994 obs)
##   Primary splits:
##       WordCount < 782.5  to the left,  improve=2.973082, (0 missing)
## 
## Node number 6: 190 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.2736842  P(node) =0.0424581
##     class counts:   138    52
##    probabilities: 0.726 0.274 
##   left son=12 (73 obs) right son=13 (117 obs)
##   Primary splits:
##       WordCount < 749.5  to the right, improve=2.166979, (0 missing)
## 
## Node number 7: 994 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.3702213  P(node) =0.2221229
##     class counts:   626   368
##    probabilities: 0.630 0.370 
##   left son=14 (85 obs) right son=15 (909 obs)
##   Primary splits:
##       WordCount < 1944.5 to the right, improve=3.384313, (0 missing)
## 
## Node number 12: 73 observations
##   predicted class=N  expected loss=0.1780822  P(node) =0.01631285
##     class counts:    60    13
##    probabilities: 0.822 0.178 
## 
## Node number 13: 117 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.3333333  P(node) =0.02614525
##     class counts:    78    39
##    probabilities: 0.667 0.333 
##   left son=26 (84 obs) right son=27 (33 obs)
##   Primary splits:
##       WordCount < 732.5  to the left,  improve=0.7597403, (0 missing)
## 
## Node number 14: 85 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.2352941  P(node) =0.01899441
##     class counts:    65    20
##    probabilities: 0.765 0.235 
##   left son=28 (77 obs) right son=29 (8 obs)
##   Primary splits:
##       WordCount < 3749   to the left,  improve=4.679144, (0 missing)
## 
## Node number 15: 909 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.3828383  P(node) =0.2031285
##     class counts:   561   348
##    probabilities: 0.617 0.383 
##   left son=30 (724 obs) right son=31 (185 obs)
##   Primary splits:
##       WordCount < 875.5  to the right, improve=1.405223, (0 missing)
## 
## Node number 26: 84 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.297619  P(node) =0.01877095
##     class counts:    59    25
##    probabilities: 0.702 0.298 
##   left son=52 (23 obs) right son=53 (61 obs)
##   Primary splits:
##       WordCount < 719.5  to the right, improve=1.770509, (0 missing)
## 
## Node number 27: 33 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.4242424  P(node) =0.007374302
##     class counts:    19    14
##    probabilities: 0.576 0.424 
##   left son=54 (20 obs) right son=55 (13 obs)
##   Primary splits:
##       WordCount < 738.5  to the right, improve=1.567366, (0 missing)
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
## Node number 30: 724 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.3687845  P(node) =0.1617877
##     class counts:   457   267
##    probabilities: 0.631 0.369 
##   left son=60 (11 obs) right son=61 (713 obs)
##   Primary splits:
##       WordCount < 881.5  to the left,  improve=3.038205, (0 missing)
## 
## Node number 31: 185 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.4378378  P(node) =0.04134078
##     class counts:   104    81
##    probabilities: 0.562 0.438 
##   left son=62 (175 obs) right son=63 (10 obs)
##   Primary splits:
##       WordCount < 871.5  to the left,  improve=2.773127, (0 missing)
## 
## Node number 52: 23 observations
##   predicted class=N  expected loss=0.1304348  P(node) =0.005139665
##     class counts:    20     3
##    probabilities: 0.870 0.130 
## 
## Node number 53: 61 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.3606557  P(node) =0.01363128
##     class counts:    39    22
##    probabilities: 0.639 0.361 
##   left son=106 (32 obs) right son=107 (29 obs)
##   Primary splits:
##       WordCount < 701    to the left,  improve=0.84882, (0 missing)
## 
## Node number 54: 20 observations
##   predicted class=N  expected loss=0.3  P(node) =0.004469274
##     class counts:    14     6
##    probabilities: 0.700 0.300 
## 
## Node number 55: 13 observations
##   predicted class=Y  expected loss=0.3846154  P(node) =0.002905028
##     class counts:     5     8
##    probabilities: 0.385 0.615 
## 
## Node number 60: 11 observations
##   predicted class=N  expected loss=0  P(node) =0.002458101
##     class counts:    11     0
##    probabilities: 1.000 0.000 
## 
## Node number 61: 713 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.3744741  P(node) =0.1593296
##     class counts:   446   267
##    probabilities: 0.626 0.374 
##   left son=122 (508 obs) right son=123 (205 obs)
##   Primary splits:
##       WordCount < 1289.5 to the left,  improve=1.727744, (0 missing)
## 
## Node number 62: 175 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.4171429  P(node) =0.03910615
##     class counts:   102    73
##    probabilities: 0.583 0.417 
##   left son=124 (124 obs) right son=125 (51 obs)
##   Primary splits:
##       WordCount < 841.5  to the left,  improve=0.7682371, (0 missing)
## 
## Node number 63: 10 observations
##   predicted class=Y  expected loss=0.2  P(node) =0.002234637
##     class counts:     2     8
##    probabilities: 0.200 0.800 
## 
## Node number 106: 32 observations
##   predicted class=N  expected loss=0.28125  P(node) =0.007150838
##     class counts:    23     9
##    probabilities: 0.719 0.281 
## 
## Node number 107: 29 observations,    complexity param=0.001780151
##   predicted class=N  expected loss=0.4482759  P(node) =0.006480447
##     class counts:    16    13
##    probabilities: 0.552 0.448 
##   left son=214 (18 obs) right son=215 (11 obs)
##   Primary splits:
##       WordCount < 710    to the right, improve=2.758969, (0 missing)
## 
## Node number 122: 508 observations,    complexity param=0.001907305
##   predicted class=N  expected loss=0.3523622  P(node) =0.1135196
##     class counts:   329   179
##    probabilities: 0.648 0.352 
##   left son=244 (187 obs) right son=245 (321 obs)
##   Primary splits:
##       WordCount < 1076.5 to the right, improve=2.812999, (0 missing)
## 
## Node number 123: 205 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.4292683  P(node) =0.04581006
##     class counts:   117    88
##    probabilities: 0.571 0.429 
##   left son=246 (196 obs) right son=247 (9 obs)
##   Primary splits:
##       WordCount < 1304.5 to the right, improve=2.286649, (0 missing)
## 
## Node number 124: 124 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.3870968  P(node) =0.0277095
##     class counts:    76    48
##    probabilities: 0.613 0.387 
##   left son=248 (40 obs) right son=249 (84 obs)
##   Primary splits:
##       WordCount < 822.5  to the right, improve=1.483948, (0 missing)
## 
## Node number 125: 51 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.4901961  P(node) =0.01139665
##     class counts:    26    25
##    probabilities: 0.510 0.490 
##   left son=250 (39 obs) right son=251 (12 obs)
##   Primary splits:
##       WordCount < 849.5  to the right, improve=2.118401, (0 missing)
## 
## Node number 214: 18 observations
##   predicted class=N  expected loss=0.2777778  P(node) =0.004022346
##     class counts:    13     5
##    probabilities: 0.722 0.278 
## 
## Node number 215: 11 observations
##   predicted class=Y  expected loss=0.2727273  P(node) =0.002458101
##     class counts:     3     8
##    probabilities: 0.273 0.727 
## 
## Node number 244: 187 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.2834225  P(node) =0.04178771
##     class counts:   134    53
##    probabilities: 0.717 0.283 
##   left son=488 (8 obs) right son=489 (179 obs)
##   Primary splits:
##       WordCount < 1283.5 to the right, improve=1.342694, (0 missing)
## 
## Node number 245: 321 observations,    complexity param=0.001907305
##   predicted class=N  expected loss=0.3925234  P(node) =0.07173184
##     class counts:   195   126
##    probabilities: 0.607 0.393 
##   left son=490 (229 obs) right son=491 (92 obs)
##   Primary splits:
##       WordCount < 1004   to the left,  improve=1.056392, (0 missing)
## 
## Node number 246: 196 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.4132653  P(node) =0.04379888
##     class counts:   115    81
##    probabilities: 0.587 0.413 
##   left son=492 (111 obs) right son=493 (85 obs)
##   Primary splits:
##       WordCount < 1443   to the right, improve=0.6230395, (0 missing)
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
## Node number 249: 84 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.4404762  P(node) =0.01877095
##     class counts:    47    37
##    probabilities: 0.560 0.440 
##   left son=498 (35 obs) right son=499 (49 obs)
##   Primary splits:
##       WordCount < 799.5  to the left,  improve=1.143537, (0 missing)
## 
## Node number 250: 39 observations
##   predicted class=N  expected loss=0.4102564  P(node) =0.008715084
##     class counts:    23    16
##    probabilities: 0.590 0.410 
## 
## Node number 251: 12 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.002681564
##     class counts:     3     9
##    probabilities: 0.250 0.750 
## 
## Node number 488: 8 observations
##   predicted class=N  expected loss=0  P(node) =0.001787709
##     class counts:     8     0
##    probabilities: 1.000 0.000 
## 
## Node number 489: 179 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.2960894  P(node) =0.04
##     class counts:   126    53
##    probabilities: 0.704 0.296 
##   left son=978 (125 obs) right son=979 (54 obs)
##   Primary splits:
##       WordCount < 1197   to the left,  improve=1.916451, (0 missing)
## 
## Node number 490: 229 observations,    complexity param=0.001907305
##   predicted class=N  expected loss=0.3668122  P(node) =0.05117318
##     class counts:   145    84
##    probabilities: 0.633 0.367 
##   left son=980 (36 obs) right son=981 (193 obs)
##   Primary splits:
##       WordCount < 983.5  to the right, improve=1.786024, (0 missing)
## 
## Node number 491: 92 observations,    complexity param=0.001907305
##   predicted class=N  expected loss=0.4565217  P(node) =0.02055866
##     class counts:    50    42
##    probabilities: 0.543 0.457 
##   left son=982 (61 obs) right son=983 (31 obs)
##   Primary splits:
##       WordCount < 1027.5 to the right, improve=0.7891385, (0 missing)
## 
## Node number 492: 111 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3783784  P(node) =0.02480447
##     class counts:    69    42
##    probabilities: 0.622 0.378 
##   left son=984 (34 obs) right son=985 (77 obs)
##   Primary splits:
##       WordCount < 1548.5 to the left,  improve=1.266636, (0 missing)
## 
## Node number 493: 85 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.4588235  P(node) =0.01899441
##     class counts:    46    39
##    probabilities: 0.541 0.459 
##   left son=986 (69 obs) right son=987 (16 obs)
##   Primary splits:
##       WordCount < 1417.5 to the left,  improve=1.088576, (0 missing)
## 
## Node number 498: 35 observations
##   predicted class=N  expected loss=0.3428571  P(node) =0.007821229
##     class counts:    23    12
##    probabilities: 0.657 0.343 
## 
## Node number 499: 49 observations,    complexity param=0.002225189
##   predicted class=Y  expected loss=0.4897959  P(node) =0.01094972
##     class counts:    24    25
##    probabilities: 0.490 0.510 
##   left son=998 (40 obs) right son=999 (9 obs)
##   Primary splits:
##       WordCount < 804.5  to the right, improve=1.578685, (0 missing)
## 
## Node number 978: 125 observations,    complexity param=0.0002670227
##   predicted class=N  expected loss=0.248  P(node) =0.02793296
##     class counts:    94    31
##    probabilities: 0.752 0.248 
##   left son=1956 (9 obs) right son=1957 (116 obs)
##   Primary splits:
##       WordCount < 1187   to the right, improve=1.192966, (0 missing)
## 
## Node number 979: 54 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.4074074  P(node) =0.01206704
##     class counts:    32    22
##    probabilities: 0.593 0.407 
##   left son=1958 (42 obs) right son=1959 (12 obs)
##   Primary splits:
##       WordCount < 1211   to the right, improve=0.9550265, (0 missing)
## 
## Node number 980: 36 observations
##   predicted class=N  expected loss=0.2222222  P(node) =0.008044693
##     class counts:    28     8
##    probabilities: 0.778 0.222 
## 
## Node number 981: 193 observations,    complexity param=0.001907305
##   predicted class=N  expected loss=0.3937824  P(node) =0.04312849
##     class counts:   117    76
##    probabilities: 0.606 0.394 
##   left son=1962 (183 obs) right son=1963 (10 obs)
##   Primary splits:
##       WordCount < 976    to the left,  improve=0.8969903, (0 missing)
## 
## Node number 982: 61 observations,    complexity param=0.001907305
##   predicted class=N  expected loss=0.4098361  P(node) =0.01363128
##     class counts:    36    25
##    probabilities: 0.590 0.410 
##   left son=1964 (10 obs) right son=1965 (51 obs)
##   Primary splits:
##       WordCount < 1034   to the left,  improve=2.296432, (0 missing)
## 
## Node number 983: 31 observations,    complexity param=0.001907305
##   predicted class=Y  expected loss=0.4516129  P(node) =0.006927374
##     class counts:    14    17
##    probabilities: 0.452 0.548 
##   left son=1966 (24 obs) right son=1967 (7 obs)
##   Primary splits:
##       WordCount < 1019.5 to the left,  improve=0.4976959, (0 missing)
## 
## Node number 984: 34 observations
##   predicted class=N  expected loss=0.2647059  P(node) =0.007597765
##     class counts:    25     9
##    probabilities: 0.735 0.265 
## 
## Node number 985: 77 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.4285714  P(node) =0.0172067
##     class counts:    44    33
##    probabilities: 0.571 0.429 
##   left son=1970 (36 obs) right son=1971 (41 obs)
##   Primary splits:
##       WordCount < 1709.5 to the right, improve=0.2129307, (0 missing)
## 
## Node number 986: 69 observations,    complexity param=0.00200267
##   predicted class=N  expected loss=0.4202899  P(node) =0.01541899
##     class counts:    40    29
##    probabilities: 0.580 0.420 
##   left son=1972 (14 obs) right son=1973 (55 obs)
##   Primary splits:
##       WordCount < 1327.5 to the left,  improve=0.6361754, (0 missing)
## 
## Node number 987: 16 observations
##   predicted class=Y  expected loss=0.375  P(node) =0.003575419
##     class counts:     6    10
##    probabilities: 0.375 0.625 
## 
## Node number 998: 40 observations,    complexity param=0.002225189
##   predicted class=N  expected loss=0.45  P(node) =0.008938547
##     class counts:    22    18
##    probabilities: 0.550 0.450 
##   left son=1996 (11 obs) right son=1997 (29 obs)
##   Primary splits:
##       WordCount < 810.5  to the left,  improve=2.182445, (0 missing)
## 
## Node number 999: 9 observations
##   predicted class=Y  expected loss=0.2222222  P(node) =0.002011173
##     class counts:     2     7
##    probabilities: 0.222 0.778 
## 
## Node number 1956: 9 observations
##   predicted class=N  expected loss=0  P(node) =0.002011173
##     class counts:     9     0
##    probabilities: 1.000 0.000 
## 
## Node number 1957: 116 observations,    complexity param=0.0002670227
##   predicted class=N  expected loss=0.2672414  P(node) =0.02592179
##     class counts:    85    31
##    probabilities: 0.733 0.267 
##   left son=3914 (96 obs) right son=3915 (20 obs)
##   Primary splits:
##       WordCount < 1164   to the left,  improve=0.8518678, (0 missing)
## 
## Node number 1958: 42 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.3571429  P(node) =0.009385475
##     class counts:    27    15
##    probabilities: 0.643 0.357 
##   left son=3916 (10 obs) right son=3917 (32 obs)
##   Primary splits:
##       WordCount < 1225.5 to the left,  improve=0.6482143, (0 missing)
## 
## Node number 1959: 12 observations
##   predicted class=Y  expected loss=0.4166667  P(node) =0.002681564
##     class counts:     5     7
##    probabilities: 0.417 0.583 
## 
## Node number 1962: 183 observations,    complexity param=0.001907305
##   predicted class=N  expected loss=0.3825137  P(node) =0.04089385
##     class counts:   113    70
##    probabilities: 0.617 0.383 
##   left son=3924 (18 obs) right son=3925 (165 obs)
##   Primary splits:
##       WordCount < 965    to the right, improve=1.025865, (0 missing)
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
## Node number 1965: 51 observations,    complexity param=0.001907305
##   predicted class=N  expected loss=0.4705882  P(node) =0.01139665
##     class counts:    27    24
##    probabilities: 0.529 0.471 
##   left son=3930 (23 obs) right son=3931 (28 obs)
##   Primary splits:
##       WordCount < 1056   to the right, improve=0.5266715, (0 missing)
## 
## Node number 1966: 24 observations,    complexity param=0.001907305
##   predicted class=N  expected loss=0.5  P(node) =0.005363128
##     class counts:    12    12
##    probabilities: 0.500 0.500 
##   left son=3932 (9 obs) right son=3933 (15 obs)
##   Primary splits:
##       WordCount < 1011.5 to the right, improve=0.8, (0 missing)
## 
## Node number 1967: 7 observations
##   predicted class=Y  expected loss=0.2857143  P(node) =0.001564246
##     class counts:     2     5
##    probabilities: 0.286 0.714 
## 
## Node number 1970: 36 observations
##   predicted class=N  expected loss=0.3888889  P(node) =0.008044693
##     class counts:    22    14
##    probabilities: 0.611 0.389 
## 
## Node number 1971: 41 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.4634146  P(node) =0.009162011
##     class counts:    22    19
##    probabilities: 0.537 0.463 
##   left son=3942 (26 obs) right son=3943 (15 obs)
##   Primary splits:
##       WordCount < 1644   to the left,  improve=0.8825516, (0 missing)
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
##       WordCount < 1350.5 to the right, improve=0.8727273, (0 missing)
## 
## Node number 1996: 11 observations
##   predicted class=N  expected loss=0.1818182  P(node) =0.002458101
##     class counts:     9     2
##    probabilities: 0.818 0.182 
## 
## Node number 1997: 29 observations
##   predicted class=Y  expected loss=0.4482759  P(node) =0.006480447
##     class counts:    13    16
##    probabilities: 0.448 0.552 
## 
## Node number 3914: 96 observations,    complexity param=0.0002670227
##   predicted class=N  expected loss=0.2395833  P(node) =0.02145251
##     class counts:    73    23
##    probabilities: 0.760 0.240 
##   left son=7828 (26 obs) right son=7829 (70 obs)
##   Primary splits:
##       WordCount < 1145.5 to the right, improve=0.5242216, (0 missing)
## 
## Node number 3915: 20 observations
##   predicted class=N  expected loss=0.4  P(node) =0.004469274
##     class counts:    12     8
##    probabilities: 0.600 0.400 
## 
## Node number 3916: 10 observations
##   predicted class=N  expected loss=0.2  P(node) =0.002234637
##     class counts:     8     2
##    probabilities: 0.800 0.200 
## 
## Node number 3917: 32 observations,    complexity param=0.0008900757
##   predicted class=N  expected loss=0.40625  P(node) =0.007150838
##     class counts:    19    13
##    probabilities: 0.594 0.406 
##   left son=7834 (22 obs) right son=7835 (10 obs)
##   Primary splits:
##       WordCount < 1245   to the right, improve=1.092045, (0 missing)
## 
## Node number 3924: 18 observations
##   predicted class=N  expected loss=0.2222222  P(node) =0.004022346
##     class counts:    14     4
##    probabilities: 0.778 0.222 
## 
## Node number 3925: 165 observations,    complexity param=0.001907305
##   predicted class=N  expected loss=0.4  P(node) =0.03687151
##     class counts:    99    66
##    probabilities: 0.600 0.400 
##   left son=7850 (150 obs) right son=7851 (15 obs)
##   Primary splits:
##       WordCount < 955    to the left,  improve=2.346667, (0 missing)
## 
## Node number 3930: 23 observations
##   predicted class=N  expected loss=0.3913043  P(node) =0.005139665
##     class counts:    14     9
##    probabilities: 0.609 0.391 
## 
## Node number 3931: 28 observations,    complexity param=0.001907305
##   predicted class=Y  expected loss=0.4642857  P(node) =0.006256983
##     class counts:    13    15
##    probabilities: 0.464 0.536 
##   left son=7862 (21 obs) right son=7863 (7 obs)
##   Primary splits:
##       WordCount < 1050   to the left,  improve=1.928571, (0 missing)
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
## Node number 3942: 26 observations
##   predicted class=N  expected loss=0.3846154  P(node) =0.005810056
##     class counts:    16    10
##    probabilities: 0.615 0.385 
## 
## Node number 3943: 15 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.003351955
##     class counts:     6     9
##    probabilities: 0.400 0.600 
## 
## Node number 3946: 40 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.4  P(node) =0.008938547
##     class counts:    24    16
##    probabilities: 0.600 0.400 
##   left son=7892 (7 obs) right son=7893 (33 obs)
##   Primary splits:
##       WordCount < 1366   to the left,  improve=1.122078, (0 missing)
## 
## Node number 3947: 15 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.003351955
##     class counts:     6     9
##    probabilities: 0.400 0.600 
## 
## Node number 7828: 26 observations
##   predicted class=N  expected loss=0.1538462  P(node) =0.005810056
##     class counts:    22     4
##    probabilities: 0.846 0.154 
## 
## Node number 7829: 70 observations,    complexity param=0.0002670227
##   predicted class=N  expected loss=0.2714286  P(node) =0.01564246
##     class counts:    51    19
##    probabilities: 0.729 0.271 
##   left son=15658 (30 obs) right son=15659 (40 obs)
##   Primary splits:
##       WordCount < 1101   to the left,  improve=0.5357143, (0 missing)
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
## Node number 7850: 150 observations,    complexity param=0.001907305
##   predicted class=N  expected loss=0.3733333  P(node) =0.03351955
##     class counts:    94    56
##    probabilities: 0.627 0.373 
##   left son=15700 (32 obs) right son=15701 (118 obs)
##   Primary splits:
##       WordCount < 936.5  to the right, improve=1.237514, (0 missing)
## 
## Node number 7851: 15 observations
##   predicted class=Y  expected loss=0.3333333  P(node) =0.003351955
##     class counts:     5    10
##    probabilities: 0.333 0.667 
## 
## Node number 7862: 21 observations
##   predicted class=N  expected loss=0.4285714  P(node) =0.004692737
##     class counts:    12     9
##    probabilities: 0.571 0.429 
## 
## Node number 7863: 7 observations
##   predicted class=Y  expected loss=0.1428571  P(node) =0.001564246
##     class counts:     1     6
##    probabilities: 0.143 0.857 
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
##       WordCount < 1391   to the right, improve=0.8080808, (0 missing)
## 
## Node number 15658: 30 observations
##   predicted class=N  expected loss=0.2  P(node) =0.006703911
##     class counts:    24     6
##    probabilities: 0.800 0.200 
## 
## Node number 15659: 40 observations,    complexity param=0.0002670227
##   predicted class=N  expected loss=0.325  P(node) =0.008938547
##     class counts:    27    13
##    probabilities: 0.675 0.325 
##   left son=31318 (29 obs) right son=31319 (11 obs)
##   Primary splits:
##       WordCount < 1110   to the right, improve=1.474765, (0 missing)
## 
## Node number 15700: 32 observations
##   predicted class=N  expected loss=0.25  P(node) =0.007150838
##     class counts:    24     8
##    probabilities: 0.750 0.250 
## 
## Node number 15701: 118 observations,    complexity param=0.001907305
##   predicted class=N  expected loss=0.4067797  P(node) =0.02636872
##     class counts:    70    48
##    probabilities: 0.593 0.407 
##   left son=31402 (103 obs) right son=31403 (15 obs)
##   Primary splits:
##       WordCount < 930.5  to the left,  improve=1.283133, (0 missing)
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
## Node number 31318: 29 observations
##   predicted class=N  expected loss=0.2413793  P(node) =0.006480447
##     class counts:    22     7
##    probabilities: 0.759 0.241 
## 
## Node number 31319: 11 observations
##   predicted class=Y  expected loss=0.4545455  P(node) =0.002458101
##     class counts:     5     6
##    probabilities: 0.455 0.545 
## 
## Node number 31402: 103 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3786408  P(node) =0.02301676
##     class counts:    64    39
##    probabilities: 0.621 0.379 
##   left son=62804 (75 obs) right son=62805 (28 obs)
##   Primary splits:
##       WordCount < 890.5  to the right, improve=1.132686, (0 missing)
## 
## Node number 31403: 15 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.003351955
##     class counts:     6     9
##    probabilities: 0.400 0.600 
## 
## Node number 62804: 75 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.3333333  P(node) =0.01675978
##     class counts:    50    25
##    probabilities: 0.667 0.333 
##   left son=125608 (11 obs) right son=125609 (64 obs)
##   Primary splits:
##       WordCount < 923.5  to the right, improve=0.5918561, (0 missing)
## 
## Node number 62805: 28 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.5  P(node) =0.006256983
##     class counts:    14    14
##    probabilities: 0.500 0.500 
##   left son=125610 (7 obs) right son=125611 (21 obs)
##   Primary splits:
##       WordCount < 888.5  to the right, improve=0.0952381, (0 missing)
## 
## Node number 125608: 11 observations
##   predicted class=N  expected loss=0.1818182  P(node) =0.002458101
##     class counts:     9     2
##    probabilities: 0.818 0.182 
## 
## Node number 125609: 64 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.359375  P(node) =0.01430168
##     class counts:    41    23
##    probabilities: 0.641 0.359 
##   left son=251218 (21 obs) right son=251219 (43 obs)
##   Primary splits:
##       WordCount < 903.5  to the left,  improve=0.9194698, (0 missing)
## 
## Node number 125610: 7 observations
##   predicted class=N  expected loss=0.4285714  P(node) =0.001564246
##     class counts:     4     3
##    probabilities: 0.571 0.429 
## 
## Node number 125611: 21 observations
##   predicted class=Y  expected loss=0.4761905  P(node) =0.004692737
##     class counts:    10    11
##    probabilities: 0.476 0.524 
## 
## Node number 251218: 21 observations
##   predicted class=N  expected loss=0.2380952  P(node) =0.004692737
##     class counts:    16     5
##    probabilities: 0.762 0.238 
## 
## Node number 251219: 43 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.4186047  P(node) =0.009608939
##     class counts:    25    18
##    probabilities: 0.581 0.419 
##   left son=502438 (35 obs) right son=502439 (8 obs)
##   Primary splits:
##       WordCount < 906.5  to the right, improve=2.158804, (0 missing)
## 
## Node number 502438: 35 observations
##   predicted class=N  expected loss=0.3428571  P(node) =0.007821229
##     class counts:    23    12
##    probabilities: 0.657 0.343 
## 
## Node number 502439: 8 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.001787709
##     class counts:     2     6
##    probabilities: 0.250 0.750 
## 
## n= 4475 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##      1) root 4475 749 N (0.83262570 0.16737430)  
##        2) WordCount< 683.5 3291 329 N (0.90003039 0.09996961) *
##        3) WordCount>=683.5 1184 420 N (0.64527027 0.35472973)  
##          6) WordCount< 782.5 190  52 N (0.72631579 0.27368421)  
##           12) WordCount>=749.5 73  13 N (0.82191781 0.17808219) *
##           13) WordCount< 749.5 117  39 N (0.66666667 0.33333333)  
##             26) WordCount< 732.5 84  25 N (0.70238095 0.29761905)  
##               52) WordCount>=719.5 23   3 N (0.86956522 0.13043478) *
##               53) WordCount< 719.5 61  22 N (0.63934426 0.36065574)  
##                106) WordCount< 701 32   9 N (0.71875000 0.28125000) *
##                107) WordCount>=701 29  13 N (0.55172414 0.44827586)  
##                  214) WordCount>=710 18   5 N (0.72222222 0.27777778) *
##                  215) WordCount< 710 11   3 Y (0.27272727 0.72727273) *
##             27) WordCount>=732.5 33  14 N (0.57575758 0.42424242)  
##               54) WordCount>=738.5 20   6 N (0.70000000 0.30000000) *
##               55) WordCount< 738.5 13   5 Y (0.38461538 0.61538462) *
##          7) WordCount>=782.5 994 368 N (0.62977867 0.37022133)  
##           14) WordCount>=1944.5 85  20 N (0.76470588 0.23529412)  
##             28) WordCount< 3749 77  14 N (0.81818182 0.18181818) *
##             29) WordCount>=3749 8   2 Y (0.25000000 0.75000000) *
##           15) WordCount< 1944.5 909 348 N (0.61716172 0.38283828)  
##             30) WordCount>=875.5 724 267 N (0.63121547 0.36878453)  
##               60) WordCount< 881.5 11   0 N (1.00000000 0.00000000) *
##               61) WordCount>=881.5 713 267 N (0.62552595 0.37447405)  
##                122) WordCount< 1289.5 508 179 N (0.64763780 0.35236220)  
##                  244) WordCount>=1076.5 187  53 N (0.71657754 0.28342246)  
##                    488) WordCount>=1283.5 8   0 N (1.00000000 0.00000000) *
##                    489) WordCount< 1283.5 179  53 N (0.70391061 0.29608939)  
##                      978) WordCount< 1197 125  31 N (0.75200000 0.24800000)  
##                       1956) WordCount>=1187 9   0 N (1.00000000 0.00000000) *
##                       1957) WordCount< 1187 116  31 N (0.73275862 0.26724138)  
##                         3914) WordCount< 1164 96  23 N (0.76041667 0.23958333)  
##                           7828) WordCount>=1145.5 26   4 N (0.84615385 0.15384615) *
##                           7829) WordCount< 1145.5 70  19 N (0.72857143 0.27142857)  
##                            15658) WordCount< 1101 30   6 N (0.80000000 0.20000000) *
##                            15659) WordCount>=1101 40  13 N (0.67500000 0.32500000)  
##                              31318) WordCount>=1110 29   7 N (0.75862069 0.24137931) *
##                              31319) WordCount< 1110 11   5 Y (0.45454545 0.54545455) *
##                         3915) WordCount>=1164 20   8 N (0.60000000 0.40000000) *
##                      979) WordCount>=1197 54  22 N (0.59259259 0.40740741)  
##                       1958) WordCount>=1211 42  15 N (0.64285714 0.35714286)  
##                         3916) WordCount< 1225.5 10   2 N (0.80000000 0.20000000) *
##                         3917) WordCount>=1225.5 32  13 N (0.59375000 0.40625000)  
##                           7834) WordCount>=1245 22   7 N (0.68181818 0.31818182) *
##                           7835) WordCount< 1245 10   4 Y (0.40000000 0.60000000) *
##                       1959) WordCount< 1211 12   5 Y (0.41666667 0.58333333) *
##                  245) WordCount< 1076.5 321 126 N (0.60747664 0.39252336)  
##                    490) WordCount< 1004 229  84 N (0.63318777 0.36681223)  
##                      980) WordCount>=983.5 36   8 N (0.77777778 0.22222222) *
##                      981) WordCount< 983.5 193  76 N (0.60621762 0.39378238)  
##                       1962) WordCount< 976 183  70 N (0.61748634 0.38251366)  
##                         3924) WordCount>=965 18   4 N (0.77777778 0.22222222) *
##                         3925) WordCount< 965 165  66 N (0.60000000 0.40000000)  
##                           7850) WordCount< 955 150  56 N (0.62666667 0.37333333)  
##                            15700) WordCount>=936.5 32   8 N (0.75000000 0.25000000) *
##                            15701) WordCount< 936.5 118  48 N (0.59322034 0.40677966)  
##                              31402) WordCount< 930.5 103  39 N (0.62135922 0.37864078)  
##                                62804) WordCount>=890.5 75  25 N (0.66666667 0.33333333)  
##                                 125608) WordCount>=923.5 11   2 N (0.81818182 0.18181818) *
##                                 125609) WordCount< 923.5 64  23 N (0.64062500 0.35937500)  
##                                   251218) WordCount< 903.5 21   5 N (0.76190476 0.23809524) *
##                                   251219) WordCount>=903.5 43  18 N (0.58139535 0.41860465)  
##                                     502438) WordCount>=906.5 35  12 N (0.65714286 0.34285714) *
##                                     502439) WordCount< 906.5 8   2 Y (0.25000000 0.75000000) *
##                                62805) WordCount< 890.5 28  14 N (0.50000000 0.50000000)  
##                                 125610) WordCount>=888.5 7   3 N (0.57142857 0.42857143) *
##                                 125611) WordCount< 888.5 21  10 Y (0.47619048 0.52380952) *
##                              31403) WordCount>=930.5 15   6 Y (0.40000000 0.60000000) *
##                           7851) WordCount>=955 15   5 Y (0.33333333 0.66666667) *
##                       1963) WordCount>=976 10   4 Y (0.40000000 0.60000000) *
##                    491) WordCount>=1004 92  42 N (0.54347826 0.45652174)  
##                      982) WordCount>=1027.5 61  25 N (0.59016393 0.40983607)  
##                       1964) WordCount< 1034 10   1 N (0.90000000 0.10000000) *
##                       1965) WordCount>=1034 51  24 N (0.52941176 0.47058824)  
##                         3930) WordCount>=1056 23   9 N (0.60869565 0.39130435) *
##                         3931) WordCount< 1056 28  13 Y (0.46428571 0.53571429)  
##                           7862) WordCount< 1050 21   9 N (0.57142857 0.42857143) *
##                           7863) WordCount>=1050 7   1 Y (0.14285714 0.85714286) *
##                      983) WordCount< 1027.5 31  14 Y (0.45161290 0.54838710)  
##                       1966) WordCount< 1019.5 24  12 N (0.50000000 0.50000000)  
##                         3932) WordCount>=1011.5 9   3 N (0.66666667 0.33333333) *
##                         3933) WordCount< 1011.5 15   6 Y (0.40000000 0.60000000) *
##                       1967) WordCount>=1019.5 7   2 Y (0.28571429 0.71428571) *
##                123) WordCount>=1289.5 205  88 N (0.57073171 0.42926829)  
##                  246) WordCount>=1304.5 196  81 N (0.58673469 0.41326531)  
##                    492) WordCount>=1443 111  42 N (0.62162162 0.37837838)  
##                      984) WordCount< 1548.5 34   9 N (0.73529412 0.26470588) *
##                      985) WordCount>=1548.5 77  33 N (0.57142857 0.42857143)  
##                       1970) WordCount>=1709.5 36  14 N (0.61111111 0.38888889) *
##                       1971) WordCount< 1709.5 41  19 N (0.53658537 0.46341463)  
##                         3942) WordCount< 1644 26  10 N (0.61538462 0.38461538) *
##                         3943) WordCount>=1644 15   6 Y (0.40000000 0.60000000) *
##                    493) WordCount< 1443 85  39 N (0.54117647 0.45882353)  
##                      986) WordCount< 1417.5 69  29 N (0.57971014 0.42028986)  
##                       1972) WordCount< 1327.5 14   4 N (0.71428571 0.28571429) *
##                       1973) WordCount>=1327.5 55  25 N (0.54545455 0.45454545)  
##                         3946) WordCount>=1350.5 40  16 N (0.60000000 0.40000000)  
##                           7892) WordCount< 1366 7   1 N (0.85714286 0.14285714) *
##                           7893) WordCount>=1366 33  15 N (0.54545455 0.45454545)  
##                            15786) WordCount>=1391 15   5 N (0.66666667 0.33333333) *
##                            15787) WordCount< 1391 18   8 Y (0.44444444 0.55555556) *
##                         3947) WordCount< 1350.5 15   6 Y (0.40000000 0.60000000) *
##                      987) WordCount>=1417.5 16   6 Y (0.37500000 0.62500000) *
##                  247) WordCount< 1304.5 9   2 Y (0.22222222 0.77777778) *
##             31) WordCount< 875.5 185  81 N (0.56216216 0.43783784)  
##               62) WordCount< 871.5 175  73 N (0.58285714 0.41714286)  
##                124) WordCount< 841.5 124  48 N (0.61290323 0.38709677)  
##                  248) WordCount>=822.5 40  11 N (0.72500000 0.27500000) *
##                  249) WordCount< 822.5 84  37 N (0.55952381 0.44047619)  
##                    498) WordCount< 799.5 35  12 N (0.65714286 0.34285714) *
##                    499) WordCount>=799.5 49  24 Y (0.48979592 0.51020408)  
##                      998) WordCount>=804.5 40  18 N (0.55000000 0.45000000)  
##                       1996) WordCount< 810.5 11   2 N (0.81818182 0.18181818) *
##                       1997) WordCount>=810.5 29  13 Y (0.44827586 0.55172414) *
##                      999) WordCount< 804.5 9   2 Y (0.22222222 0.77777778) *
##                125) WordCount>=841.5 51  25 N (0.50980392 0.49019608)  
##                  250) WordCount>=849.5 39  16 N (0.58974359 0.41025641) *
##                  251) WordCount< 849.5 12   3 Y (0.25000000 0.75000000) *
##               63) WordCount>=871.5 10   2 Y (0.20000000 0.80000000) *
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-7.png) 

```
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 3726  749
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                                0
## 2            Y                                                0
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                             3726
## 2                                              749
##           Reference
## Prediction    N    Y
##          N 2999  330
##          Y  727  419
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             2999
## 2            Y                                              330
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                              727
## 2                                              419
##           Reference
## Prediction    N    Y
##          N 3196  371
##          Y  530  378
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             3196
## 2            Y                                              371
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                              530
## 2                                              378
##           Reference
## Prediction    N    Y
##          N 3424  447
##          Y  302  302
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             3424
## 2            Y                                              447
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                              302
## 2                                              302
##           Reference
## Prediction    N    Y
##          N 3579  533
##          Y  147  216
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             3579
## 2            Y                                              533
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                              147
## 2                                              216
##           Reference
## Prediction    N    Y
##          N 3618  561
##          Y  108  188
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             3618
## 2            Y                                              561
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                              108
## 2                                              188
##           Reference
## Prediction    N    Y
##          N 3667  623
##          Y   59  126
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             3667
## 2            Y                                              623
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                               59
## 2                                              126
##           Reference
## Prediction    N    Y
##          N 3707  687
##          Y   19   62
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             3707
## 2            Y                                              687
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                               19
## 2                                               62
##           Reference
## Prediction    N    Y
##          N 3725  743
##          Y    1    6
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             3725
## 2            Y                                              743
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                                1
## 2                                                6
##           Reference
## Prediction    N    Y
##          N 3726  749
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             3726
## 2            Y                                              749
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                                0
## 2                                                0
##           Reference
## Prediction    N    Y
##          N 3726  749
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             3726
## 2            Y                                              749
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                                0
## 2                                                0
##    threshold    f.score
## 1        0.0 0.28675345
## 2        0.1 0.44221636
## 3        0.2 0.45624623
## 4        0.3 0.44641537
## 5        0.4 0.38848921
## 6        0.5 0.35980861
## 7        0.6 0.26980728
## 8        0.7 0.14939759
## 9        0.8 0.01587302
## 10       0.9 0.00000000
## 11       1.0 0.00000000
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-8.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             3196
## 2            Y                                              371
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                              530
## 2                                              378
##           Reference
## Prediction    N    Y
##          N 3196  371
##          Y  530  378
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             3196
## 2            Y                                              371
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                              530
## 2                                              378
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] N Y
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] Y N
## Levels: N Y
##          Prediction
## Reference    N    Y
##         N 3196  530
##         Y  371  378
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.986592e-01   3.340962e-01   7.866053e-01   8.103220e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   1.411516e-07 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-9.png) 

```
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 1713  344
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                                0
## 2            Y                                                0
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                             1713
## 2                                              344
##           Reference
## Prediction    N    Y
##          N 1350  164
##          Y  363  180
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             1350
## 2            Y                                              164
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                              363
## 2                                              180
##           Reference
## Prediction    N    Y
##          N 1442  195
##          Y  271  149
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             1442
## 2            Y                                              195
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                              271
## 2                                              149
##           Reference
## Prediction    N    Y
##          N 1532  245
##          Y  181   99
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             1532
## 2            Y                                              245
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                              181
## 2                                               99
##           Reference
## Prediction    N    Y
##          N 1610  292
##          Y  103   52
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             1610
## 2            Y                                              292
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                              103
## 2                                               52
##           Reference
## Prediction    N    Y
##          N 1629  300
##          Y   84   44
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             1629
## 2            Y                                              300
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                               84
## 2                                               44
##           Reference
## Prediction    N    Y
##          N 1652  310
##          Y   61   34
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             1652
## 2            Y                                              310
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                               61
## 2                                               34
##           Reference
## Prediction    N    Y
##          N 1681  326
##          Y   32   18
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             1681
## 2            Y                                              326
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                               32
## 2                                               18
##           Reference
## Prediction    N    Y
##          N 1710  343
##          Y    3    1
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             1710
## 2            Y                                              343
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                                3
## 2                                                1
##           Reference
## Prediction    N    Y
##          N 1713  344
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             1713
## 2            Y                                              344
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                                0
## 2                                                0
##           Reference
## Prediction    N    Y
##          N 1713  344
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             1713
## 2            Y                                              344
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                                0
## 2                                                0
##    threshold     f.score
## 1        0.0 0.286547272
## 2        0.1 0.405862458
## 3        0.2 0.390052356
## 4        0.3 0.317307692
## 5        0.4 0.208416834
## 6        0.5 0.186440678
## 7        0.6 0.154897494
## 8        0.7 0.091370558
## 9        0.8 0.005747126
## 10       0.9 0.000000000
## 11       1.0 0.000000000
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-10.png) 

```
## [1] "Classifier Probability Threshold: 0.1000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             1350
## 2            Y                                              164
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                              363
## 2                                              180
##           Reference
## Prediction    N    Y
##          N 1350  164
##          Y  363  180
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.N
## 1            N                                             1350
## 2            Y                                              164
##   Popular.fctr.predict.Max.cor.Y.cv.0.cp.0.rpart.Y
## 1                                              363
## 2                                              180
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] Y N
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] N Y
## Levels: N Y
##          Prediction
## Reference    N    Y
##         N 1350  363
##         Y  164  180
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.438017e-01   2.528893e-01   7.243549e-01   7.625511e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   6.408399e-18 
##                    model_id model_method     feats max.nTuningRuns
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart WordCount               0
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      0.599                 0.054   0.7072558
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.2       0.4562462        0.7986592
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7866053              0.810322     0.3340962   0.6552144
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.1       0.4058625        0.7438017
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.7243549             0.7625511     0.2528893
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
## [1] "    indep_vars: WordCount"
## + Fold1: cp=0.001907 
## - Fold1: cp=0.001907 
## + Fold2: cp=0.001907 
## - Fold2: cp=0.001907 
## + Fold3: cp=0.001907 
## - Fold3: cp=0.001907 
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.00223 on full training set
```

```
## Warning in myfit_mdl(model_id = "Max.cor.Y", model_method = "rpart",
## model_type = glb_model_type, : model's bestTune found at an extreme of
## tuneGrid for parameter: cp
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-11.png) ![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-12.png) 

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
##           Reference
## Prediction    N    Y
##          N 3726  749
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.rpart.N
## 1            N                                   3726
## 2            Y                                    749
##   Popular.fctr.predict.Max.cor.Y.rpart.Y
## 1                                      0
## 2                                      0
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] N
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] Y N
## Levels: N Y
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
##           Reference
## Prediction    N    Y
##          N 1713  344
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.rpart.N
## 1            N                                   1713
## 2            Y                                    344
##   Popular.fctr.predict.Max.cor.Y.rpart.Y
## 1                                      0
## 2                                      0
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] N
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] N Y
## Levels: N Y
##          Prediction
## Reference    N    Y
##         N 1713    0
##         Y  344    0
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.327662e-01   0.000000e+00   8.159247e-01   8.486533e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   5.143944e-01   2.337097e-76 
##          model_id model_method     feats max.nTuningRuns
## 1 Max.cor.Y.rpart        rpart WordCount               3
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.265                 0.063         0.5
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.5               0        0.8192183
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8213602             0.8434553     0.0736708         0.5
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.5               0        0.8327662
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8159247             0.8486533             0
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.003061652      0.02524502
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
## [1] "    indep_vars: WordCount"
## + Fold1: parameter=none 
## - Fold1: parameter=none 
## + Fold2: parameter=none 
## - Fold2: parameter=none 
## + Fold3: parameter=none 
## - Fold3: parameter=none 
## Aggregating results
## Fitting final model on full training set
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-13.png) ![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-14.png) ![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-15.png) ![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-16.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.8100  -0.5797  -0.4982  -0.4485   2.1935  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -2.318e+00  6.554e-02  -35.37   <2e-16 ***
## WordCount    1.202e-03  7.808e-05   15.39   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 3774.8  on 4473  degrees of freedom
## AIC: 3778.8
## 
## Number of Fisher Scoring iterations: 4
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-17.png) 

```
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 3726  749
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                    0
## 2            Y                                    0
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                 3726
## 2                                  749
##           Reference
## Prediction    N    Y
##          N  581   27
##          Y 3145  722
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                  581
## 2            Y                                   27
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                 3145
## 2                                  722
##           Reference
## Prediction    N    Y
##          N 3088  378
##          Y  638  371
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 3088
## 2            Y                                  378
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                  638
## 2                                  371
##           Reference
## Prediction    N    Y
##          N 3515  628
##          Y  211  121
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 3515
## 2            Y                                  628
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                  211
## 2                                  121
##           Reference
## Prediction    N    Y
##          N 3625  702
##          Y  101   47
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 3625
## 2            Y                                  702
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                  101
## 2                                   47
##           Reference
## Prediction    N    Y
##          N 3659  727
##          Y   67   22
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 3659
## 2            Y                                  727
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                   67
## 2                                   22
##           Reference
## Prediction    N    Y
##          N 3693  734
##          Y   33   15
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 3693
## 2            Y                                  734
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                   33
## 2                                   15
##           Reference
## Prediction    N    Y
##          N 3708  740
##          Y   18    9
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 3708
## 2            Y                                  740
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                   18
## 2                                    9
##           Reference
## Prediction    N    Y
##          N 3717  742
##          Y    9    7
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 3717
## 2            Y                                  742
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                    9
## 2                                    7
##           Reference
## Prediction    N    Y
##          N 3724  743
##          Y    2    6
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 3724
## 2            Y                                  743
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                    2
## 2                                    6
##           Reference
## Prediction    N    Y
##          N 3726  749
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 3726
## 2            Y                                  749
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                    0
## 2                                    0
##    threshold    f.score
## 1        0.0 0.28675345
## 2        0.1 0.31282496
## 3        0.2 0.42207053
## 4        0.3 0.22386679
## 5        0.4 0.10479376
## 6        0.5 0.05250597
## 7        0.6 0.03764115
## 8        0.7 0.02319588
## 9        0.8 0.01830065
## 10       0.9 0.01585205
## 11       1.0 0.00000000
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-18.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 3088
## 2            Y                                  378
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                  638
## 2                                  371
##           Reference
## Prediction    N    Y
##          N 3088  378
##          Y  638  371
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 3088
## 2            Y                                  378
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                  638
## 2                                  371
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] N Y
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] Y N
## Levels: N Y
##          Prediction
## Reference    N    Y
##         N 3088  638
##         Y  378  371
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.729609e-01   2.846273e-01   7.603993e-01   7.851652e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   4.453285e-16 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-19.png) 

```
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 1713  344
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                    0
## 2            Y                                    0
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                 1713
## 2                                  344
##           Reference
## Prediction    N    Y
##          N  250    5
##          Y 1463  339
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                  250
## 2            Y                                    5
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                 1463
## 2                                  339
##           Reference
## Prediction    N    Y
##          N 1418  182
##          Y  295  162
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 1418
## 2            Y                                  182
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                  295
## 2                                  162
##           Reference
## Prediction    N    Y
##          N 1602  272
##          Y  111   72
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 1602
## 2            Y                                  272
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                  111
## 2                                   72
##           Reference
## Prediction    N    Y
##          N 1653  314
##          Y   60   30
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 1653
## 2            Y                                  314
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                   60
## 2                                   30
##           Reference
## Prediction    N    Y
##          N 1670  329
##          Y   43   15
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 1670
## 2            Y                                  329
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                   43
## 2                                   15
##           Reference
## Prediction    N    Y
##          N 1686  336
##          Y   27    8
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 1686
## 2            Y                                  336
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                   27
## 2                                    8
##           Reference
## Prediction    N    Y
##          N 1700  341
##          Y   13    3
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 1700
## 2            Y                                  341
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                   13
## 2                                    3
##           Reference
## Prediction    N    Y
##          N 1708  341
##          Y    5    3
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 1708
## 2            Y                                  341
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                    5
## 2                                    3
##           Reference
## Prediction    N    Y
##          N 1710  342
##          Y    3    2
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 1710
## 2            Y                                  342
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                    3
## 2                                    2
##           Reference
## Prediction    N    Y
##          N 1713  344
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 1713
## 2            Y                                  344
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                    0
## 2                                    0
##    threshold    f.score
## 1        0.0 0.28654727
## 2        0.1 0.31593663
## 3        0.2 0.40449438
## 4        0.3 0.27324478
## 5        0.4 0.13824885
## 6        0.5 0.07462687
## 7        0.6 0.04221636
## 8        0.7 0.01666667
## 9        0.8 0.01704545
## 10       0.9 0.01146132
## 11       1.0 0.00000000
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-20.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 1418
## 2            Y                                  182
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                  295
## 2                                  162
##           Reference
## Prediction    N    Y
##          N 1418  182
##          Y  295  162
##   Popular.fctr Popular.fctr.predict.Max.cor.Y.glm.N
## 1            N                                 1418
## 2            Y                                  182
##   Popular.fctr.predict.Max.cor.Y.glm.Y
## 1                                  295
## 2                                  162
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] Y N
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] N Y
## Levels: N Y
##          Prediction
## Reference    N    Y
##         N 1418  295
##         Y  182  162
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   7.681089e-01   2.640573e-01   7.492527e-01   7.861979e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   1.000000e+00   2.926379e-07 
##        model_id model_method     feats max.nTuningRuns
## 1 Max.cor.Y.glm          glm WordCount               1
##   min.elapsedtime.everything min.elapsedtime.final max.auc.fit
## 1                      1.126                 0.062   0.7350785
##   opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1                    0.2       0.4220705        0.8225702
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.7603993             0.7851652    0.01764536   0.7378291
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2       0.4044944        0.7681089
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7492527             0.7861979     0.2640573     3778.81
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.003663409      0.01234363
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
## [1] "    indep_vars: WordCount, WordCount:S.year, WordCount:S.compani, WordCount:S.time, WordCount:S.will, WordCount:S.york, WordCount:S.new, WordCount:S.week, WordCount:H.num.chars, WordCount:A.num.chars, WordCount:H.num.words, WordCount:S.num.chars.log, WordCount:A.num.words, WordCount:H.num.chars.log, WordCount:S.num.words, WordCount:A.num.words.log, WordCount:S.num.chars"
## + Fold1: parameter=none 
## - Fold1: parameter=none 
## + Fold2: parameter=none 
## - Fold2: parameter=none 
## + Fold3: parameter=none 
## - Fold3: parameter=none 
## Aggregating results
## Fitting final model on full training set
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-21.png) ![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-22.png) ![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-23.png) ![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-24.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.9237  -0.5335  -0.4683  -0.4313   2.6081  
## 
## Coefficients:
##                               Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                 -2.352e+00  7.067e-02 -33.284  < 2e-16 ***
## WordCount                    2.622e-02  2.948e-03   8.893  < 2e-16 ***
## `WordCount:S.year`          -1.746e-04  2.874e-04  -0.607 0.543573    
## `WordCount:S.compani`       -3.102e-04  2.511e-04  -1.235 0.216652    
## `WordCount:S.time`          -4.821e-04  2.760e-04  -1.746 0.080739 .  
## `WordCount:S.will`          -2.414e-04  2.311e-04  -1.044 0.296267    
## `WordCount:S.york`           3.580e-04  3.998e-04   0.895 0.370597    
## `WordCount:S.new`           -6.275e-05  1.931e-04  -0.325 0.745180    
## `WordCount:S.week`          -1.193e-03  3.820e-04  -3.123 0.001788 ** 
## `WordCount:H.num.chars`      2.976e-05  1.358e-05   2.192 0.028407 *  
## `WordCount:A.num.chars`      4.853e-05  7.513e-05   0.646 0.518337    
## `WordCount:H.num.words`     -1.907e-04  6.670e-05  -2.859 0.004255 ** 
## `WordCount:S.num.chars.log` -7.817e-03  1.183e-03  -6.610 3.84e-11 ***
## `WordCount:A.num.words`     -1.200e-03  7.341e-04  -1.635 0.102153    
## `WordCount:H.num.chars.log` -5.536e-04  4.886e-04  -1.133 0.257175    
## `WordCount:S.num.words`      7.942e-04  7.297e-04   1.088 0.276414    
## `WordCount:A.num.words.log`  4.664e-03  1.248e-03   3.737 0.000186 ***
## `WordCount:S.num.chars`      1.000e-05  7.527e-05   0.133 0.894267    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 3546.1  on 4457  degrees of freedom
## AIC: 3582.1
## 
## Number of Fisher Scoring iterations: 6
## 
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-25.png) 

```
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 3726  749
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                              0
## 2            Y                                              0
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                           3726
## 2                                            749
##           Reference
## Prediction    N    Y
##          N 1257   46
##          Y 2469  703
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           1257
## 2            Y                                             46
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                           2469
## 2                                            703
##           Reference
## Prediction    N    Y
##          N 3282  338
##          Y  444  411
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           3282
## 2            Y                                            338
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                            444
## 2                                            411
##           Reference
## Prediction    N    Y
##          N 3533  517
##          Y  193  232
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           3533
## 2            Y                                            517
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                            193
## 2                                            232
##           Reference
## Prediction    N    Y
##          N 3608  591
##          Y  118  158
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           3608
## 2            Y                                            591
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                            118
## 2                                            158
##           Reference
## Prediction    N    Y
##          N 3639  648
##          Y   87  101
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           3639
## 2            Y                                            648
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                             87
## 2                                            101
##           Reference
## Prediction    N    Y
##          N 3659  680
##          Y   67   69
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           3659
## 2            Y                                            680
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                             67
## 2                                             69
##           Reference
## Prediction    N    Y
##          N 3678  703
##          Y   48   46
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           3678
## 2            Y                                            703
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                             48
## 2                                             46
##           Reference
## Prediction    N    Y
##          N 3696  720
##          Y   30   29
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           3696
## 2            Y                                            720
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                             30
## 2                                             29
##           Reference
## Prediction    N    Y
##          N 3709  735
##          Y   17   14
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           3709
## 2            Y                                            735
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                             17
## 2                                             14
##           Reference
## Prediction    N    Y
##          N 3726  749
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           3726
## 2            Y                                            749
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                              0
## 2                                              0
##    threshold    f.score
## 1        0.0 0.28675345
## 2        0.1 0.35858199
## 3        0.2 0.51246883
## 4        0.3 0.39522998
## 5        0.4 0.30829268
## 6        0.5 0.21558164
## 7        0.6 0.15593220
## 8        0.7 0.10913405
## 9        0.8 0.07178218
## 10       0.9 0.03589744
## 11       1.0 0.00000000
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-26.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           3282
## 2            Y                                            338
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                            444
## 2                                            411
##           Reference
## Prediction    N    Y
##          N 3282  338
##          Y  444  411
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           3282
## 2            Y                                            338
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                            444
## 2                                            411
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] Y N
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] Y N
## Levels: N Y
##          Prediction
## Reference    N    Y
##         N 3282  444
##         Y  338  411
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   0.8252513966   0.4065822072   0.8138021594   0.8362744678   0.8326256983 
## AccuracyPValue  McnemarPValue 
##   0.9095557524   0.0001734853 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-27.png) 

```
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 1713  344
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                              0
## 2            Y                                              0
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                           1713
## 2                                            344
##           Reference
## Prediction    N    Y
##          N  544   13
##          Y 1169  331
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                            544
## 2            Y                                             13
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                           1169
## 2                                            331
##           Reference
## Prediction    N    Y
##          N 1484  153
##          Y  229  191
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           1484
## 2            Y                                            153
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                            229
## 2                                            191
##           Reference
## Prediction    N    Y
##          N 1607  237
##          Y  106  107
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           1607
## 2            Y                                            237
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                            106
## 2                                            107
##           Reference
## Prediction    N    Y
##          N 1649  273
##          Y   64   71
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           1649
## 2            Y                                            273
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                             64
## 2                                             71
##           Reference
## Prediction    N    Y
##          N 1669  294
##          Y   44   50
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           1669
## 2            Y                                            294
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                             44
## 2                                             50
##           Reference
## Prediction    N    Y
##          N 1685  311
##          Y   28   33
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           1685
## 2            Y                                            311
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                             28
## 2                                             33
##           Reference
## Prediction    N    Y
##          N 1690  327
##          Y   23   17
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           1690
## 2            Y                                            327
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                             23
## 2                                             17
##           Reference
## Prediction    N    Y
##          N 1698  336
##          Y   15    8
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           1698
## 2            Y                                            336
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                             15
## 2                                              8
##           Reference
## Prediction    N    Y
##          N 1704  339
##          Y    9    5
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           1704
## 2            Y                                            339
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                              9
## 2                                              5
##           Reference
## Prediction    N    Y
##          N 1713  344
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           1713
## 2            Y                                            344
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                              0
## 2                                              0
##    threshold    f.score
## 1        0.0 0.28654727
## 2        0.1 0.35900217
## 3        0.2 0.50000000
## 4        0.3 0.38420108
## 5        0.4 0.29645094
## 6        0.5 0.22831050
## 7        0.6 0.16296296
## 8        0.7 0.08854167
## 9        0.8 0.04359673
## 10       0.9 0.02793296
## 11       1.0 0.00000000
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-28.png) 

```
## [1] "Classifier Probability Threshold: 0.2000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           1484
## 2            Y                                            153
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                            229
## 2                                            191
##           Reference
## Prediction    N    Y
##          N 1484  153
##          Y  229  191
##   Popular.fctr Popular.fctr.predict.Interact.High.cor.Y.glm.N
## 1            N                                           1484
## 2            Y                                            153
##   Popular.fctr.predict.Interact.High.cor.Y.glm.Y
## 1                                            229
## 2                                            191
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] N Y
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] N Y
## Levels: N Y
##          Prediction
## Reference    N    Y
##         N 1484  229
##         Y  153  191
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   0.8142926592   0.3873527586   0.7968011314   0.8308835268   0.8327661643 
## AccuracyPValue  McnemarPValue 
##   0.9877368727   0.0001243773 
##                  model_id model_method
## 1 Interact.High.cor.Y.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                  feats
## 1 WordCount, WordCount:S.year, WordCount:S.compani, WordCount:S.time, WordCount:S.will, WordCount:S.york, WordCount:S.new, WordCount:S.week, WordCount:H.num.chars, WordCount:A.num.chars, WordCount:H.num.words, WordCount:S.num.chars.log, WordCount:A.num.words, WordCount:H.num.chars.log, WordCount:S.num.words, WordCount:A.num.words.log, WordCount:S.num.chars
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.834                 0.278
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.8062116                    0.2       0.5124688        0.8359773
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8138022             0.8362745     0.1618606    0.815698
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.2             0.5        0.8142927
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.7968011             0.8308835     0.3873528    3582.118
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.003949821       0.0313674
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
## [1] "    indep_vars: WordCount, PubDate.hour, PubDate.month, .rnorm, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, S.compani, S.time, S.will, S.york, S.new, H.new, S.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, A.num.chars, H.num.words, H.num.words.log, S.num.chars.log, A.num.words, A.num.words.unq, A.num.words.log, A.num.words.unq.log"
## + Fold1: parameter=none
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
## Warning: not plotting observations with leverage one:
##   297
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-29.png) ![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-30.png) 

```
## Warning: not plotting observations with leverage one:
##   297
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-31.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -3.2760  -0.3642  -0.2129  -0.0002   3.4410  
## 
## Coefficients: (7 not defined because of singularities)
##                                          Estimate Std. Error z value
## (Intercept)                             3.176e+00  1.446e+00   2.196
## WordCount                               1.709e-03  1.368e-04  12.488
## PubDate.hour                            2.717e-02  1.250e-02   2.173
## PubDate.month                          -1.049e-01  7.316e-02  -1.434
## .rnorm                                 -3.150e-02  5.706e-02  -0.552
## PubDate.date                           -2.904e-03  6.729e-03  -0.432
## PubDate.second                         -7.826e-04  3.330e-03  -0.235
## PubDate.minute                         -3.938e-03  3.256e-03  -1.209
## PubDate.wkday                          -1.250e-01  3.677e-02  -3.401
## S.year                                 -3.537e-01  2.942e-01  -1.202
## S.compani                              -2.970e-01  2.710e-01  -1.096
## S.time                                 -4.330e-01  2.471e-01  -1.752
## S.will                                 -2.559e-01  2.121e-01  -1.206
## S.york                                  2.782e-01  3.905e-01   0.712
## S.new                                  -1.126e-01  1.978e-01  -0.569
## H.new                                  -6.285e-01  3.261e-01  -1.927
## S.week                                 -2.703e-01  2.499e-01  -1.082
## SectionName.fctrArts                   -3.497e+00  3.925e-01  -8.910
## `SectionName.fctrBusiness Day`         -5.018e+00  7.034e-01  -7.133
## SectionName.fctrHealth                 -7.987e-01  1.785e+00  -0.447
## SectionName.fctrOpinion                -1.333e+00  1.538e+00  -0.866
## SectionName.fctr                       -2.193e+00  1.042e+00  -2.104
## SectionName.fctrStyle                  -2.869e+00  4.647e+03  -0.001
## SectionName.fctrWorld                  -3.737e+00  3.262e+03  -0.001
## SectionName.fctrTechnology             -2.572e+00  4.199e-01  -6.126
## SectionName.fctrMagazine               -1.982e+01  1.337e+03  -0.015
## SectionName.fctrMultimedia             -4.481e+00  1.453e+00  -3.085
## SectionName.fctrTravel                 -4.856e+00  1.062e+00  -4.573
## SectionName.fctrU.S.                    1.653e+01  7.397e+02   0.022
## `SectionName.fctrN.Y. / Region`        -2.876e+00  4.786e-01  -6.008
## SectionName.fctrOpen                   -1.870e+01  4.590e+03  -0.004
## SectionName.fctrSports                 -2.049e+01  6.523e+03  -0.003
## NewsDesk.fctrCulture                           NA         NA      NA
## NewsDesk.fctrScience                    3.011e-01  1.758e+00   0.171
## NewsDesk.fctrOpEd                       1.227e+00  1.519e+00   0.808
## NewsDesk.fctr                          -1.191e+00  1.001e+00  -1.190
## NewsDesk.fctrForeign                   -1.678e+01  6.006e+02  -0.028
## NewsDesk.fctrStyles                    -1.727e+01  7.397e+02  -0.023
## NewsDesk.fctrTStyle                    -3.457e+00  1.079e+00  -3.203
## NewsDesk.fctrMagazine                          NA         NA      NA
## NewsDesk.fctrTravel                            NA         NA      NA
## NewsDesk.fctrMetro                             NA         NA      NA
## NewsDesk.fctrNational                  -1.676e+01  4.567e+03  -0.004
## NewsDesk.fctrSports                            NA         NA      NA
## SubsectionName.fctrDealbook             1.407e+00  6.289e-01   2.238
## `SubsectionName.fctrRoom For Debate`   -6.279e+00  1.556e+00  -4.036
## `SubsectionName.fctrFashion & Style`           NA         NA      NA
## `SubsectionName.fctrAsia Pacific`       1.459e+01  3.206e+03   0.005
## SubsectionName.fctrEducation           -3.487e+01  8.487e+02  -0.041
## `SubsectionName.fctrThe Public Editor`  1.622e+00  1.287e+00   1.260
## `SubsectionName.fctrSmall Business`            NA         NA      NA
## SubsectionName.fctrPolitics            -1.872e+01  6.528e+03  -0.003
## H.num.chars                             2.105e-02  7.977e-03   2.639
## A.num.chars                             8.331e-03  5.538e-03   1.504
## H.num.words                            -7.865e-02  1.466e-01  -0.536
## H.num.words.log                        -6.812e-01  7.494e-01  -0.909
## S.num.chars.log                         2.360e-01  5.663e-01   0.417
## A.num.words                            -4.139e-01  3.020e-01  -1.371
## A.num.words.unq                         3.317e-01  3.142e-01   1.056
## A.num.words.log                         5.155e+00  4.437e+00   1.162
## A.num.words.unq.log                    -6.047e+00  4.513e+00  -1.340
##                                        Pr(>|z|)    
## (Intercept)                            0.028083 *  
## WordCount                               < 2e-16 ***
## PubDate.hour                           0.029802 *  
## PubDate.month                          0.151599    
## .rnorm                                 0.580927    
## PubDate.date                           0.666049    
## PubDate.second                         0.814202    
## PubDate.minute                         0.226549    
## PubDate.wkday                          0.000672 ***
## S.year                                 0.229205    
## S.compani                              0.273123    
## S.time                                 0.079706 .  
## S.will                                 0.227638    
## S.york                                 0.476199    
## S.new                                  0.569182    
## H.new                                  0.053929 .  
## S.week                                 0.279370    
## SectionName.fctrArts                    < 2e-16 ***
## `SectionName.fctrBusiness Day`         9.81e-13 ***
## SectionName.fctrHealth                 0.654623    
## SectionName.fctrOpinion                0.386223    
## SectionName.fctr                       0.035340 *  
## SectionName.fctrStyle                  0.999507    
## SectionName.fctrWorld                  0.999086    
## SectionName.fctrTechnology             9.03e-10 ***
## SectionName.fctrMagazine               0.988171    
## SectionName.fctrMultimedia             0.002035 ** 
## SectionName.fctrTravel                 4.80e-06 ***
## SectionName.fctrU.S.                   0.982174    
## `SectionName.fctrN.Y. / Region`        1.87e-09 ***
## SectionName.fctrOpen                   0.996749    
## SectionName.fctrSports                 0.997493    
## NewsDesk.fctrCulture                         NA    
## NewsDesk.fctrScience                   0.864000    
## NewsDesk.fctrOpEd                      0.419183    
## NewsDesk.fctr                          0.233972    
## NewsDesk.fctrForeign                   0.977710    
## NewsDesk.fctrStyles                    0.981375    
## NewsDesk.fctrTStyle                    0.001360 ** 
## NewsDesk.fctrMagazine                        NA    
## NewsDesk.fctrTravel                          NA    
## NewsDesk.fctrMetro                           NA    
## NewsDesk.fctrNational                  0.997071    
## NewsDesk.fctrSports                          NA    
## SubsectionName.fctrDealbook            0.025252 *  
## `SubsectionName.fctrRoom For Debate`   5.43e-05 ***
## `SubsectionName.fctrFashion & Style`         NA    
## `SubsectionName.fctrAsia Pacific`      0.996370    
## SubsectionName.fctrEducation           0.967226    
## `SubsectionName.fctrThe Public Editor` 0.207530    
## `SubsectionName.fctrSmall Business`          NA    
## SubsectionName.fctrPolitics            0.997712    
## H.num.chars                            0.008316 ** 
## A.num.chars                            0.132493    
## H.num.words                            0.591692    
## H.num.words.log                        0.363367    
## S.num.chars.log                        0.676855    
## A.num.words                            0.170403    
## A.num.words.unq                        0.291142    
## A.num.words.log                        0.245223    
## A.num.words.unq.log                    0.180266    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 2098.3  on 4421  degrees of freedom
## AIC: 2206.3
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

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-32.png) ![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-33.png) 

```
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 3726  749
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                    0
## 2            Y                                    0
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                 3726
## 2                                  749
##           Reference
## Prediction    N    Y
##          N 3037   57
##          Y  689  692
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 3037
## 2            Y                                   57
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  689
## 2                                  692
##           Reference
## Prediction    N    Y
##          N 3362  145
##          Y  364  604
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 3362
## 2            Y                                  145
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  364
## 2                                  604
##           Reference
## Prediction    N    Y
##          N 3483  192
##          Y  243  557
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 3483
## 2            Y                                  192
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  243
## 2                                  557
##           Reference
## Prediction    N    Y
##          N 3528  222
##          Y  198  527
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 3528
## 2            Y                                  222
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  198
## 2                                  527
##           Reference
## Prediction    N    Y
##          N 3572  266
##          Y  154  483
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 3572
## 2            Y                                  266
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  154
## 2                                  483
##           Reference
## Prediction    N    Y
##          N 3607  309
##          Y  119  440
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 3607
## 2            Y                                  309
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  119
## 2                                  440
##           Reference
## Prediction    N    Y
##          N 3645  385
##          Y   81  364
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 3645
## 2            Y                                  385
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                   81
## 2                                  364
##           Reference
## Prediction    N    Y
##          N 3687  503
##          Y   39  246
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 3687
## 2            Y                                  503
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                   39
## 2                                  246
##           Reference
## Prediction    N    Y
##          N 3700  644
##          Y   26  105
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 3700
## 2            Y                                  644
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                   26
## 2                                  105
##           Reference
## Prediction    N    Y
##          N 3726  749
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 3726
## 2            Y                                  749
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                    0
## 2                                    0
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.6497653
## 3        0.2 0.7035527
## 4        0.3 0.7191737
## 5        0.4 0.7150611
## 6        0.5 0.6969697
## 7        0.6 0.6727829
## 8        0.7 0.6097152
## 9        0.8 0.4758221
## 10       0.9 0.2386364
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 3483
## 2            Y                                  192
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  243
## 2                                  557
##           Reference
## Prediction    N    Y
##          N 3483  192
##          Y  243  557
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 3483
## 2            Y                                  192
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  243
## 2                                  557
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] Y N
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] Y N
## Levels: N Y
##          Prediction
## Reference    N    Y
##         N 3483  243
##         Y  192  557
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.027933e-01   6.604749e-01   8.937373e-01   9.113196e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   2.150408e-41   1.651565e-02 
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

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-34.png) ![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-35.png) 

```
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 1713  344
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                    0
## 2            Y                                    0
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                 1713
## 2                                  344
##           Reference
## Prediction    N    Y
##          N 1379   26
##          Y  334  318
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 1379
## 2            Y                                   26
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  334
## 2                                  318
##           Reference
## Prediction    N    Y
##          N 1533   59
##          Y  180  285
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 1533
## 2            Y                                   59
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  180
## 2                                  285
##           Reference
## Prediction    N    Y
##          N 1592   86
##          Y  121  258
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 1592
## 2            Y                                   86
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  121
## 2                                  258
##           Reference
## Prediction    N    Y
##          N 1615  102
##          Y   98  242
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 1615
## 2            Y                                  102
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                   98
## 2                                  242
##           Reference
## Prediction    N    Y
##          N 1635  111
##          Y   78  233
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 1635
## 2            Y                                  111
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                   78
## 2                                  233
##           Reference
## Prediction    N    Y
##          N 1645  132
##          Y   68  212
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 1645
## 2            Y                                  132
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                   68
## 2                                  212
##           Reference
## Prediction    N    Y
##          N 1671  164
##          Y   42  180
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 1671
## 2            Y                                  164
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                   42
## 2                                  180
##           Reference
## Prediction    N    Y
##          N 1688  232
##          Y   25  112
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 1688
## 2            Y                                  232
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                   25
## 2                                  112
##           Reference
## Prediction    N    Y
##          N 1702  284
##          Y   11   60
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 1702
## 2            Y                                  284
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                   11
## 2                                   60
##           Reference
## Prediction    N    Y
##          N 1713  344
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 1713
## 2            Y                                  344
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                    0
## 2                                    0
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.6385542
## 3        0.2 0.7045735
## 4        0.3 0.7136929
## 5        0.4 0.7076023
## 6        0.5 0.7114504
## 7        0.6 0.6794872
## 8        0.7 0.6360424
## 9        0.8 0.4656965
## 10       0.9 0.2891566
## 11       1.0 0.0000000
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_0-36.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 1592
## 2            Y                                   86
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  121
## 2                                  258
##           Reference
## Prediction    N    Y
##          N 1592   86
##          Y  121  258
##   Popular.fctr Popular.fctr.predict.Low.cor.X.glm.N
## 1            N                                 1592
## 2            Y                                   86
##   Popular.fctr.predict.Low.cor.X.glm.Y
## 1                                  121
## 2                                  258
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] N Y
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] N Y
## Levels: N Y
##          Prediction
## Reference    N    Y
##         N 1592  121
##         Y   86  258
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.993680e-01   6.528225e-01   8.855497e-01   9.120357e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   4.579965e-18   1.811968e-02 
##        model_id model_method
## 1 Low.cor.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                             feats
## 1 WordCount, PubDate.hour, PubDate.month, .rnorm, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, S.compani, S.time, S.will, S.york, S.new, H.new, S.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, A.num.chars, H.num.words, H.num.words.log, S.num.chars.log, A.num.words, A.num.words.unq, A.num.words.log, A.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      4.791                 1.252
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9372948                    0.3       0.7191737        0.9003357
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8937373             0.9113196      0.619684   0.9326067
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.3       0.7136929         0.899368
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.8855497             0.9120357     0.6528225    2206.319
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.002679281     0.006873523
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
## elapsed9   fit.models                6                0 140.736
## elapsed10  fit.models                6                1 180.884
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
## [1] "    indep_vars: WordCount, PubDate.hour, PubDate.month, .rnorm, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, A.year, S.compani, A.compani, S.time, A.time, S.will, A.will, S.york, A.york, S.new, A.new, H.new, S.week, A.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, H.num.chars.log, A.num.chars, S.num.chars, H.num.words, H.num.words.unq, H.num.words.log, S.num.chars.log, A.num.chars.log, A.num.words, S.num.words, H.num.words.unq.log, A.num.words.unq, S.num.words.unq, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log"
## + Fold1: parameter=none
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
##   297, 4229, 4338
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-1.png) ![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-2.png) 

```
## Warning: not plotting observations with leverage one:
##   297, 4229, 4338
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-3.png) 

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
## -3.3481  -0.3588  -0.2092  -0.0001   3.4697  
## 
## Coefficients: (10 not defined because of singularities)
##                                          Estimate Std. Error z value
## (Intercept)                             6.406e+00  2.002e+00   3.200
## WordCount                               1.723e-03  1.381e-04  12.475
## PubDate.hour                            2.821e-02  1.254e-02   2.250
## PubDate.month                          -9.863e-02  7.364e-02  -1.339
## .rnorm                                 -3.235e-02  5.749e-02  -0.563
## PubDate.date                           -2.133e-03  6.773e-03  -0.315
## PubDate.second                         -8.456e-04  3.347e-03  -0.253
## PubDate.minute                         -3.413e-03  3.289e-03  -1.038
## PubDate.wkday                          -1.282e-01  3.706e-02  -3.459
## S.year                                 -4.140e-01  2.991e-01  -1.384
## A.year                                         NA         NA      NA
## S.compani                              -7.379e-01  1.186e+04   0.000
## A.compani                               4.521e-01  1.186e+04   0.000
## S.time                                  2.207e+01  1.075e+04   0.002
## A.time                                 -2.252e+01  1.075e+04  -0.002
## S.will                                  7.264e-02  5.001e+03   0.000
## A.will                                 -3.097e-01  5.001e+03   0.000
## S.york                                  3.019e-01  3.920e-01   0.770
## A.york                                         NA         NA      NA
## S.new                                   7.876e+00  6.839e+03   0.001
## A.new                                  -7.970e+00  6.839e+03  -0.001
## H.new                                  -6.222e-01  3.212e-01  -1.937
## S.week                                 -2.468e-01  2.500e-01  -0.987
## A.week                                         NA         NA      NA
## SectionName.fctrArts                   -3.447e+00  3.992e-01  -8.636
## `SectionName.fctrBusiness Day`         -5.063e+00  7.101e-01  -7.130
## SectionName.fctrHealth                 -1.001e+00  1.792e+00  -0.559
## SectionName.fctrOpinion                -1.336e+00  1.543e+00  -0.865
## SectionName.fctr                       -2.284e+00  1.047e+00  -2.181
## SectionName.fctrStyle                  -3.025e+00  7.571e+03   0.000
## SectionName.fctrWorld                  -3.298e+00  4.572e+03  -0.001
## SectionName.fctrTechnology             -2.574e+00  4.286e-01  -6.005
## SectionName.fctrMagazine               -2.085e+01  2.207e+03  -0.009
## SectionName.fctrMultimedia             -4.500e+00  1.458e+00  -3.087
## SectionName.fctrTravel                 -4.803e+00  1.064e+00  -4.514
## SectionName.fctrU.S.                    1.744e+01  1.204e+03   0.014
## `SectionName.fctrN.Y. / Region`        -2.845e+00  4.820e-01  -5.903
## SectionName.fctrOpen                   -1.986e+01  7.581e+03  -0.003
## SectionName.fctrSports                 -2.150e+01  1.075e+04  -0.002
## NewsDesk.fctrCulture                           NA         NA      NA
## NewsDesk.fctrScience                    5.345e-01  1.762e+00   0.303
## NewsDesk.fctrOpEd                       1.309e+00  1.522e+00   0.860
## NewsDesk.fctr                          -1.117e+00  1.004e+00  -1.112
## NewsDesk.fctrForeign                   -1.762e+01  9.903e+02  -0.018
## NewsDesk.fctrStyles                    -1.817e+01  1.204e+03  -0.015
## NewsDesk.fctrTStyle                    -3.420e+00  1.084e+00  -3.157
## NewsDesk.fctrMagazine                          NA         NA      NA
## NewsDesk.fctrTravel                            NA         NA      NA
## NewsDesk.fctrMetro                             NA         NA      NA
## NewsDesk.fctrNational                  -1.766e+01  7.539e+03  -0.002
## NewsDesk.fctrSports                            NA         NA      NA
## SubsectionName.fctrDealbook             1.446e+00  6.310e-01   2.291
## `SubsectionName.fctrRoom For Debate`   -6.342e+00  1.558e+00  -4.070
## `SubsectionName.fctrFashion & Style`           NA         NA      NA
## `SubsectionName.fctrAsia Pacific`       1.487e+01  4.464e+03   0.003
## SubsectionName.fctrEducation           -4.828e+01  1.265e+03  -0.038
## `SubsectionName.fctrThe Public Editor`  1.602e+00  1.289e+00   1.243
## `SubsectionName.fctrSmall Business`            NA         NA      NA
## SubsectionName.fctrPolitics            -1.972e+01  1.077e+04  -0.002
## H.num.chars                             5.119e-02  2.025e-02   2.528
## H.num.chars.log                        -1.341e+00  8.375e-01  -1.601
## A.num.chars                             2.096e-01  1.849e-01   1.133
## S.num.chars                            -1.955e-01  1.851e-01  -1.056
## H.num.words                             6.568e-01  1.053e+00   0.623
## H.num.words.unq                        -9.759e-01  1.046e+00  -0.933
## H.num.words.log                        -1.838e+00  6.144e+00  -0.299
## S.num.chars.log                         5.826e+01  5.541e+01   1.052
## A.num.chars.log                        -5.840e+01  5.540e+01  -1.054
## A.num.words                             1.257e+00  4.442e+00   0.283
## S.num.words                            -1.731e+00  4.476e+00  -0.387
## H.num.words.unq.log                     2.559e+00  6.076e+00   0.421
## A.num.words.unq                        -7.306e+00  5.197e+00  -1.406
## S.num.words.unq                         7.678e+00  5.236e+00   1.466
## A.num.words.log                        -3.348e+01  1.080e+02  -0.310
## S.num.words.log                         3.890e+01  1.083e+02   0.359
## A.num.words.unq.log                     1.661e+02  1.165e+02   1.425
## S.num.words.unq.log                    -1.722e+02  1.169e+02  -1.473
##                                        Pr(>|z|)    
## (Intercept)                            0.001375 ** 
## WordCount                               < 2e-16 ***
## PubDate.hour                           0.024461 *  
## PubDate.month                          0.180443    
## .rnorm                                 0.573608    
## PubDate.date                           0.752824    
## PubDate.second                         0.800519    
## PubDate.minute                         0.299431    
## PubDate.wkday                          0.000543 ***
## S.year                                 0.166335    
## A.year                                       NA    
## S.compani                              0.999950    
## A.compani                              0.999970    
## S.time                                 0.998363    
## A.time                                 0.998329    
## S.will                                 0.999988    
## A.will                                 0.999951    
## S.york                                 0.441208    
## A.york                                       NA    
## S.new                                  0.999081    
## A.new                                  0.999070    
## H.new                                  0.052758 .  
## S.week                                 0.323592    
## A.week                                       NA    
## SectionName.fctrArts                    < 2e-16 ***
## `SectionName.fctrBusiness Day`         1.01e-12 ***
## SectionName.fctrHealth                 0.576397    
## SectionName.fctrOpinion                0.386848    
## SectionName.fctr                       0.029194 *  
## SectionName.fctrStyle                  0.999681    
## SectionName.fctrWorld                  0.999425    
## SectionName.fctrTechnology             1.92e-09 ***
## SectionName.fctrMagazine               0.992463    
## SectionName.fctrMultimedia             0.002020 ** 
## SectionName.fctrTravel                 6.35e-06 ***
## SectionName.fctrU.S.                   0.988439    
## `SectionName.fctrN.Y. / Region`        3.58e-09 ***
## SectionName.fctrOpen                   0.997909    
## SectionName.fctrSports                 0.998405    
## NewsDesk.fctrCulture                         NA    
## NewsDesk.fctrScience                   0.761699    
## NewsDesk.fctrOpEd                      0.389789    
## NewsDesk.fctr                          0.266009    
## NewsDesk.fctrForeign                   0.985806    
## NewsDesk.fctrStyles                    0.987956    
## NewsDesk.fctrTStyle                    0.001596 ** 
## NewsDesk.fctrMagazine                        NA    
## NewsDesk.fctrTravel                          NA    
## NewsDesk.fctrMetro                           NA    
## NewsDesk.fctrNational                  0.998131    
## NewsDesk.fctrSports                          NA    
## SubsectionName.fctrDealbook            0.021967 *  
## `SubsectionName.fctrRoom For Debate`   4.70e-05 ***
## `SubsectionName.fctrFashion & Style`         NA    
## `SubsectionName.fctrAsia Pacific`      0.997342    
## SubsectionName.fctrEducation           0.969566    
## `SubsectionName.fctrThe Public Editor` 0.213863    
## `SubsectionName.fctrSmall Business`          NA    
## SubsectionName.fctrPolitics            0.998539    
## H.num.chars                            0.011472 *  
## H.num.chars.log                        0.109308    
## A.num.chars                            0.257005    
## S.num.chars                            0.290872    
## H.num.words                            0.532967    
## H.num.words.unq                        0.351029    
## H.num.words.log                        0.764811    
## S.num.chars.log                        0.293020    
## A.num.chars.log                        0.291740    
## A.num.words                            0.777187    
## S.num.words                            0.698987    
## H.num.words.unq.log                    0.673625    
## A.num.words.unq                        0.159753    
## S.num.words.unq                        0.142594    
## A.num.words.log                        0.756572    
## S.num.words.log                        0.719513    
## A.num.words.unq.log                    0.154158    
## S.num.words.unq.log                    0.140835    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 4042.7  on 4474  degrees of freedom
## Residual deviance: 2078.3  on 4408  degrees of freedom
## AIC: 2212.3
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

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-4.png) ![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-5.png) 

```
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 3726  749
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                        0
## 2            Y                                        0
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                     3726
## 2                                      749
##           Reference
## Prediction    N    Y
##          N 3046   59
##          Y  680  690
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     3046
## 2            Y                                       59
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                      680
## 2                                      690
##           Reference
## Prediction    N    Y
##          N 3362  140
##          Y  364  609
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     3362
## 2            Y                                      140
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                      364
## 2                                      609
##           Reference
## Prediction    N    Y
##          N 3481  190
##          Y  245  559
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     3481
## 2            Y                                      190
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                      245
## 2                                      559
##           Reference
## Prediction    N    Y
##          N 3528  225
##          Y  198  524
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     3528
## 2            Y                                      225
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                      198
## 2                                      524
##           Reference
## Prediction    N    Y
##          N 3571  262
##          Y  155  487
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     3571
## 2            Y                                      262
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                      155
## 2                                      487
##           Reference
## Prediction    N    Y
##          N 3610  310
##          Y  116  439
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     3610
## 2            Y                                      310
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                      116
## 2                                      439
##           Reference
## Prediction    N    Y
##          N 3651  383
##          Y   75  366
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     3651
## 2            Y                                      383
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                       75
## 2                                      366
##           Reference
## Prediction    N    Y
##          N 3687  499
##          Y   39  250
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     3687
## 2            Y                                      499
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                       39
## 2                                      250
##           Reference
## Prediction    N    Y
##          N 3701  635
##          Y   25  114
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     3701
## 2            Y                                      635
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                       25
## 2                                      114
##           Reference
## Prediction    N    Y
##          N 3726  749
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     3726
## 2            Y                                      749
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                        0
## 2                                        0
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.6512506
## 3        0.2 0.7073171
## 4        0.3 0.7198970
## 5        0.4 0.7124405
## 6        0.5 0.7002157
## 7        0.6 0.6733129
## 8        0.7 0.6151261
## 9        0.8 0.4816956
## 10       0.9 0.2567568
## 11       1.0 0.0000000
```

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     3481
## 2            Y                                      190
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                      245
## 2                                      559
##           Reference
## Prediction    N    Y
##          N 3481  190
##          Y  245  559
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     3481
## 2            Y                                      190
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                      245
## 2                                      559
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] Y N
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] Y N
## Levels: N Y
##          Prediction
## Reference    N    Y
##         N 3481  245
##         Y  190  559
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.027933e-01   6.611786e-01   8.937373e-01   9.113196e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   2.150408e-41   9.622663e-03 
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

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-6.png) ![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-7.png) 

```
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 1713  344
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                        0
## 2            Y                                        0
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                     1713
## 2                                      344
##           Reference
## Prediction    N    Y
##          N 1388   27
##          Y  325  317
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     1388
## 2            Y                                       27
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                      325
## 2                                      317
##           Reference
## Prediction    N    Y
##          N 1530   59
##          Y  183  285
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     1530
## 2            Y                                       59
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                      183
## 2                                      285
##           Reference
## Prediction    N    Y
##          N 1590   88
##          Y  123  256
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     1590
## 2            Y                                       88
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                      123
## 2                                      256
##           Reference
## Prediction    N    Y
##          N 1613  101
##          Y  100  243
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     1613
## 2            Y                                      101
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                      100
## 2                                      243
##           Reference
## Prediction    N    Y
##          N 1633  112
##          Y   80  232
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     1633
## 2            Y                                      112
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                       80
## 2                                      232
##           Reference
## Prediction    N    Y
##          N 1646  127
##          Y   67  217
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     1646
## 2            Y                                      127
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                       67
## 2                                      217
##           Reference
## Prediction    N    Y
##          N 1671  159
##          Y   42  185
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     1671
## 2            Y                                      159
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                       42
## 2                                      185
##           Reference
## Prediction    N    Y
##          N 1687  221
##          Y   26  123
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     1687
## 2            Y                                      221
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                       26
## 2                                      123
##           Reference
## Prediction    N    Y
##          N 1701  285
##          Y   12   59
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     1701
## 2            Y                                      285
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                       12
## 2                                       59
##           Reference
## Prediction    N    Y
##          N 1713  344
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     1713
## 2            Y                                      344
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                        0
## 2                                        0
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.6430020
## 3        0.2 0.7019704
## 4        0.3 0.7081604
## 5        0.4 0.7074236
## 6        0.5 0.7073171
## 7        0.6 0.6910828
## 8        0.7 0.6479860
## 9        0.8 0.4989858
## 10       0.9 0.2843373
## 11       1.0 0.0000000
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-8.png) 

```
## [1] "Classifier Probability Threshold: 0.3000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     1590
## 2            Y                                       88
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                      123
## 2                                      256
##           Reference
## Prediction    N    Y
##          N 1590   88
##          Y  123  256
##   Popular.fctr Popular.fctr.predict.Conditional.X.glm.N
## 1            N                                     1590
## 2            Y                                       88
##   Popular.fctr.predict.Conditional.X.glm.Y
## 1                                      123
## 2                                      256
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] N Y
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] N Y
## Levels: N Y
##          Prediction
## Reference    N    Y
##         N 1590  123
##         Y   88  256
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   8.974234e-01   6.461137e-01   8.834974e-01   9.102047e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   4.637200e-17   1.924987e-02 
##            model_id model_method
## 1 Conditional.X.glm          glm
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                feats
## 1 WordCount, PubDate.hour, PubDate.month, .rnorm, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, A.year, S.compani, A.compani, S.time, A.time, S.will, A.will, S.york, A.york, S.new, A.new, H.new, S.week, A.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, H.num.chars.log, A.num.chars, S.num.chars, H.num.words, H.num.words.unq, H.num.words.log, S.num.chars.log, A.num.chars.log, A.num.words, S.num.words, H.num.words.unq.log, A.num.words.unq, S.num.words.unq, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      6.841                 1.746
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9381279                    0.3        0.719897        0.8992181
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8937373             0.9113196     0.6173657   0.9327051
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.3       0.7081604        0.8974234
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB min.aic.fit
## 1             0.8834974             0.9102047     0.6461137    2212.301
##   max.AccuracySD.fit max.KappaSD.fit
## 1        0.002140938      0.00177553
## [1] "fitting model: Conditional.X.rpart"
## [1] "    indep_vars: WordCount, PubDate.hour, PubDate.month, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, A.year, S.compani, A.compani, S.time, A.time, S.will, A.will, S.york, A.york, S.new, A.new, H.new, S.week, A.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, H.num.chars.log, A.num.chars, S.num.chars, H.num.words, H.num.words.unq, H.num.words.log, S.num.chars.log, A.num.chars.log, A.num.words, S.num.words, H.num.words.unq.log, A.num.words.unq, S.num.words.unq, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log"
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

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-9.png) ![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-10.png) 

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
##       NewsDesk.fctrOpEd SectionName.fctrOpinion               WordCount 
##                      32                      26                      12 
##  SectionName.fctrHealth    NewsDesk.fctrScience             A.num.chars 
##                       6                       6                       4 
##         A.num.chars.log             S.num.chars         S.num.chars.log 
##                       4                       4                       4 
##            PubDate.hour 
##                       1 
## 
## Node number 1: 4475 observations,    complexity param=0.259012
##   predicted class=N  expected loss=0.1673743  P(node) =1
##     class counts:  3726   749
##    probabilities: 0.833 0.167 
##   left son=2 (4131 obs) right son=3 (344 obs)
##   Primary splits:
##       NewsDesk.fctrOpEd       < 0.5      to the left,  improve=281.52360, (0 missing)
##       SectionName.fctrOpinion < 0.5      to the left,  improve=244.96950, (0 missing)
##       WordCount               < 683.5    to the left,  improve=113.02630, (0 missing)
##       A.num.chars             < 44.5     to the right, improve= 93.81257, (0 missing)
##       S.num.chars             < 44.5     to the right, improve= 93.81257, (0 missing)
##   Surrogate splits:
##       SectionName.fctrOpinion < 0.5      to the left,  agree=0.986, adj=0.817, (0 split)
##       A.num.chars             < 40.5     to the right, agree=0.933, adj=0.128, (0 split)
##       S.num.chars             < 40.5     to the right, agree=0.933, adj=0.128, (0 split)
##       S.num.chars.log         < 3.725621 to the right, agree=0.933, adj=0.128, (0 split)
##       A.num.chars.log         < 3.725621 to the right, agree=0.933, adj=0.128, (0 split)
## 
## Node number 2: 4131 observations,    complexity param=0.03938585
##   predicted class=N  expected loss=0.1161946  P(node) =0.9231285
##     class counts:  3651   480
##    probabilities: 0.884 0.116 
##   left son=4 (3066 obs) right son=5 (1065 obs)
##   Primary splits:
##       WordCount              < 683.5    to the left,  improve=105.55990, (0 missing)
##       NewsDesk.fctrScience   < 0.5      to the left,  improve= 70.50963, (0 missing)
##       SectionName.fctrHealth < 0.5      to the left,  improve= 69.73481, (0 missing)
##       PubDate.hour           < 21.5     to the left,  improve= 53.45620, (0 missing)
##       NewsDesk.fctrStyles    < 0.5      to the left,  improve= 27.64076, (0 missing)
##   Surrogate splits:
##       SectionName.fctrOpinion              < 0.5      to the left,  agree=0.755, adj=0.048, (0 split)
##       SubsectionName.fctrRoom For Debate   < 0.5      to the left,  agree=0.753, adj=0.041, (0 split)
##       SectionName.fctrHealth               < 0.5      to the left,  agree=0.745, adj=0.012, (0 split)
##       NewsDesk.fctrScience                 < 0.5      to the left,  agree=0.745, adj=0.011, (0 split)
##       SubsectionName.fctrThe Public Editor < 0.5      to the left,  agree=0.744, adj=0.008, (0 split)
## 
## Node number 3: 344 observations
##   predicted class=Y  expected loss=0.2180233  P(node) =0.07687151
##     class counts:    75   269
##    probabilities: 0.218 0.782 
## 
## Node number 4: 3066 observations
##   predicted class=N  expected loss=0.04957599  P(node) =0.6851397
##     class counts:  2914   152
##    probabilities: 0.950 0.050 
## 
## Node number 5: 1065 observations,    complexity param=0.03938585
##   predicted class=N  expected loss=0.3079812  P(node) =0.2379888
##     class counts:   737   328
##    probabilities: 0.692 0.308 
##   left son=10 (994 obs) right son=11 (71 obs)
##   Primary splits:
##       SectionName.fctrHealth < 0.5      to the left,  improve=56.15144, (0 missing)
##       NewsDesk.fctrScience   < 0.5      to the left,  improve=55.08535, (0 missing)
##       PubDate.hour           < 7.5      to the left,  improve=18.57852, (0 missing)
##       NewsDesk.fctrForeign   < 0.5      to the right, improve=11.91201, (0 missing)
##       SectionName.fctrWorld  < 0.5      to the right, improve=11.48588, (0 missing)
##   Surrogate splits:
##       NewsDesk.fctrScience < 0.5      to the left,  agree=0.999, adj=0.986, (0 split)
##       PubDate.hour         < 0.5      to the right, agree=0.939, adj=0.085, (0 split)
## 
## Node number 10: 994 observations
##   predicted class=N  expected loss=0.2645875  P(node) =0.2221229
##     class counts:   731   263
##    probabilities: 0.735 0.265 
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
##      4) WordCount< 683.5 3066 152 N (0.95042401 0.04957599) *
##      5) WordCount>=683.5 1065 328 N (0.69201878 0.30798122)  
##       10) SectionName.fctrHealth< 0.5 994 263 N (0.73541247 0.26458753) *
##       11) SectionName.fctrHealth>=0.5 71   6 Y (0.08450704 0.91549296) *
##    3) NewsDesk.fctrOpEd>=0.5 344  75 Y (0.21802326 0.78197674) *
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-11.png) 

```
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 3726  749
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                          0
## 2            Y                                          0
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                       3726
## 2                                        749
##           Reference
## Prediction    N    Y
##          N 2914  152
##          Y  812  597
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       2914
## 2            Y                                        152
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                        812
## 2                                        597
##           Reference
## Prediction    N    Y
##          N 2914  152
##          Y  812  597
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       2914
## 2            Y                                        152
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                        812
## 2                                        597
##           Reference
## Prediction    N    Y
##          N 3645  415
##          Y   81  334
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       3645
## 2            Y                                        415
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                         81
## 2                                        334
##           Reference
## Prediction    N    Y
##          N 3645  415
##          Y   81  334
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       3645
## 2            Y                                        415
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                         81
## 2                                        334
##           Reference
## Prediction    N    Y
##          N 3645  415
##          Y   81  334
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       3645
## 2            Y                                        415
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                         81
## 2                                        334
##           Reference
## Prediction    N    Y
##          N 3645  415
##          Y   81  334
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       3645
## 2            Y                                        415
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                         81
## 2                                        334
##           Reference
## Prediction    N    Y
##          N 3645  415
##          Y   81  334
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       3645
## 2            Y                                        415
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                         81
## 2                                        334
##           Reference
## Prediction    N    Y
##          N 3720  684
##          Y    6   65
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       3720
## 2            Y                                        684
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                          6
## 2                                         65
##           Reference
## Prediction    N    Y
##          N 3720  684
##          Y    6   65
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       3720
## 2            Y                                        684
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                          6
## 2                                         65
##           Reference
## Prediction    N    Y
##          N 3726  749
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       3726
## 2            Y                                        749
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                          0
## 2                                          0
##    threshold   f.score
## 1        0.0 0.2867534
## 2        0.1 0.5532901
## 3        0.2 0.5532901
## 4        0.3 0.5738832
## 5        0.4 0.5738832
## 6        0.5 0.5738832
## 7        0.6 0.5738832
## 8        0.7 0.5738832
## 9        0.8 0.1585366
## 10       0.9 0.1585366
## 11       1.0 0.0000000
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-12.png) 

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       3645
## 2            Y                                        415
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                         81
## 2                                        334
##           Reference
## Prediction    N    Y
##          N 3645  415
##          Y   81  334
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       3645
## 2            Y                                        415
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                         81
## 2                                        334
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] N Y
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] Y N
## Levels: N Y
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

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-13.png) 

```
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 1713  344
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                          0
## 2            Y                                          0
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                       1713
## 2                                        344
##           Reference
## Prediction    N    Y
##          N 1311   74
##          Y  402  270
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       1311
## 2            Y                                         74
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                        402
## 2                                        270
##           Reference
## Prediction    N    Y
##          N 1311   74
##          Y  402  270
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       1311
## 2            Y                                         74
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                        402
## 2                                        270
##           Reference
## Prediction    N    Y
##          N 1671  178
##          Y   42  166
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       1671
## 2            Y                                        178
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                         42
## 2                                        166
##           Reference
## Prediction    N    Y
##          N 1671  178
##          Y   42  166
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       1671
## 2            Y                                        178
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                         42
## 2                                        166
##           Reference
## Prediction    N    Y
##          N 1671  178
##          Y   42  166
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       1671
## 2            Y                                        178
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                         42
## 2                                        166
##           Reference
## Prediction    N    Y
##          N 1671  178
##          Y   42  166
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       1671
## 2            Y                                        178
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                         42
## 2                                        166
##           Reference
## Prediction    N    Y
##          N 1671  178
##          Y   42  166
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       1671
## 2            Y                                        178
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                         42
## 2                                        166
##           Reference
## Prediction    N    Y
##          N 1709  317
##          Y    4   27
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       1709
## 2            Y                                        317
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                          4
## 2                                         27
##           Reference
## Prediction    N    Y
##          N 1709  317
##          Y    4   27
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       1709
## 2            Y                                        317
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                          4
## 2                                         27
##           Reference
## Prediction    N    Y
##          N 1713  344
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       1713
## 2            Y                                        344
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                          0
## 2                                          0
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.5314961
## 3        0.2 0.5314961
## 4        0.3 0.6014493
## 5        0.4 0.6014493
## 6        0.5 0.6014493
## 7        0.6 0.6014493
## 8        0.7 0.6014493
## 9        0.8 0.1440000
## 10       0.9 0.1440000
## 11       1.0 0.0000000
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-14.png) 

```
## [1] "Classifier Probability Threshold: 0.7000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       1671
## 2            Y                                        178
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                         42
## 2                                        166
##           Reference
## Prediction    N    Y
##          N 1671  178
##          Y   42  166
##   Popular.fctr Popular.fctr.predict.Conditional.X.rpart.N
## 1            N                                       1671
## 2            Y                                        178
##   Popular.fctr.predict.Conditional.X.rpart.Y
## 1                                         42
## 2                                        166
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] N Y
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] N Y
## Levels: N Y
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
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        feats
## 1 WordCount, PubDate.hour, PubDate.month, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, A.year, S.compani, A.compani, S.time, A.time, S.will, A.will, S.york, A.york, S.new, A.new, H.new, S.week, A.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, H.num.chars.log, A.num.chars, S.num.chars, H.num.words, H.num.words.unq, H.num.words.log, S.num.chars.log, A.num.chars.log, A.num.words, S.num.words, H.num.words.unq.log, A.num.words.unq, S.num.words.unq, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                       5.22                 0.912
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1    0.830078                    0.7       0.5738832        0.8893849
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.8795973             0.8982154     0.5222593    0.822503
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.7       0.6014493        0.8930481
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1              0.878885             0.9060795      0.543976
##   max.AccuracySD.fit max.KappaSD.fit
## 1         0.00245876     0.005834195
## [1] "fitting model: Conditional.X.cp.0.rpart"
## [1] "    indep_vars: WordCount, PubDate.hour, PubDate.month, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, A.year, S.compani, A.compani, S.time, A.time, S.will, A.will, S.york, A.york, S.new, A.new, H.new, S.week, A.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, H.num.chars.log, A.num.chars, S.num.chars, H.num.words, H.num.words.unq, H.num.words.log, S.num.chars.log, A.num.chars.log, A.num.words, S.num.words, H.num.words.unq.log, A.num.words.unq, S.num.words.unq, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log"
## Fitting cp = 0 on full training set
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-15.png) 

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
## 4  0.0160213618      4 0.6341789
## 5  0.0140186916      6 0.6021362
## 6  0.0120160214      8 0.5740988
## 7  0.0093457944     10 0.5500668
## 8  0.0080106809     11 0.5407210
## 9  0.0045393858     12 0.5327103
## 10 0.0040053405     17 0.5100134
## 11 0.0033377837     20 0.4979973
## 12 0.0016021362     27 0.4739653
## 13 0.0013351135     49 0.4178905
## 14 0.0004450378     61 0.4018692
## 15 0.0000000000     67 0.3991989
## 
## Variable importance
##                    NewsDesk.fctrOpEd              SectionName.fctrOpinion 
##                                   16                                   14 
##                            WordCount                          A.num.chars 
##                                    8                                    6 
##                          S.num.chars                      S.num.chars.log 
##                                    6                                    5 
##                      A.num.chars.log                         PubDate.hour 
##                                    5                                    5 
##               SectionName.fctrHealth                 NewsDesk.fctrScience 
##                                    4                                    4 
##                          H.num.chars                 SectionName.fctrU.S. 
##                                    2                                    2 
##                       PubDate.minute                  NewsDesk.fctrStyles 
##                                    2                                    2 
##                      H.num.words.unq                          H.num.words 
##                                    2                                    2 
##                        PubDate.wkday                     SectionName.fctr 
##                                    2                                    2 
##                  H.num.words.unq.log                      H.num.words.log 
##                                    1                                    1 
##                      H.num.chars.log                          A.num.words 
##                                    1                                    1 
##                          S.num.words                        PubDate.month 
##                                    1                                    1 
##                       PubDate.second                      A.num.words.unq 
##                                    1                                    1 
##                      S.num.words.unq                         PubDate.date 
##                                    1                                    1 
## SubsectionName.fctrThe Public Editor   SubsectionName.fctrRoom For Debate 
##                                    1                                    1 
## 
## Node number 1: 4475 observations,    complexity param=0.259012
##   predicted class=N  expected loss=0.1673743  P(node) =1
##     class counts:  3726   749
##    probabilities: 0.833 0.167 
##   left son=2 (4131 obs) right son=3 (344 obs)
##   Primary splits:
##       NewsDesk.fctrOpEd       < 0.5       to the left,  improve=281.52360, (0 missing)
##       SectionName.fctrOpinion < 0.5       to the left,  improve=244.96950, (0 missing)
##       WordCount               < 683.5     to the left,  improve=113.02630, (0 missing)
##       A.num.chars             < 44.5      to the right, improve= 93.81257, (0 missing)
##       S.num.chars             < 44.5      to the right, improve= 93.81257, (0 missing)
##   Surrogate splits:
##       SectionName.fctrOpinion < 0.5       to the left,  agree=0.986, adj=0.817, (0 split)
##       A.num.chars             < 40.5      to the right, agree=0.933, adj=0.128, (0 split)
##       S.num.chars             < 40.5      to the right, agree=0.933, adj=0.128, (0 split)
##       S.num.chars.log         < 3.725621  to the right, agree=0.933, adj=0.128, (0 split)
##       A.num.chars.log         < 3.725621  to the right, agree=0.933, adj=0.128, (0 split)
## 
## Node number 2: 4131 observations,    complexity param=0.03938585
##   predicted class=N  expected loss=0.1161946  P(node) =0.9231285
##     class counts:  3651   480
##    probabilities: 0.884 0.116 
##   left son=4 (3066 obs) right son=5 (1065 obs)
##   Primary splits:
##       WordCount              < 683.5     to the left,  improve=105.55990, (0 missing)
##       NewsDesk.fctrScience   < 0.5       to the left,  improve= 70.50963, (0 missing)
##       SectionName.fctrHealth < 0.5       to the left,  improve= 69.73481, (0 missing)
##       PubDate.hour           < 21.5      to the left,  improve= 53.45620, (0 missing)
##       NewsDesk.fctrStyles    < 0.5       to the left,  improve= 27.64076, (0 missing)
##   Surrogate splits:
##       SectionName.fctrOpinion              < 0.5       to the left,  agree=0.755, adj=0.048, (0 split)
##       SubsectionName.fctrRoom For Debate   < 0.5       to the left,  agree=0.753, adj=0.041, (0 split)
##       SectionName.fctrHealth               < 0.5       to the left,  agree=0.745, adj=0.012, (0 split)
##       NewsDesk.fctrScience                 < 0.5       to the left,  agree=0.745, adj=0.011, (0 split)
##       SubsectionName.fctrThe Public Editor < 0.5       to the left,  agree=0.744, adj=0.008, (0 split)
## 
## Node number 3: 344 observations,    complexity param=0.008010681
##   predicted class=Y  expected loss=0.2180233  P(node) =0.07687151
##     class counts:    75   269
##    probabilities: 0.218 0.782 
##   left son=6 (8 obs) right son=7 (336 obs)
##   Primary splits:
##       A.num.chars     < 169.5     to the right, improve=7.070321, (0 missing)
##       S.num.chars     < 169.5     to the right, improve=7.070321, (0 missing)
##       S.num.chars.log < 5.138628  to the right, improve=7.070321, (0 missing)
##       A.num.chars.log < 5.138628  to the right, improve=7.070321, (0 missing)
##       A.num.words     < 15.5      to the right, improve=5.005347, (0 missing)
##   Surrogate splits:
##       S.num.chars     < 169.5     to the right, agree=1.000, adj=1.000, (0 split)
##       S.num.chars.log < 5.138628  to the right, agree=1.000, adj=1.000, (0 split)
##       A.num.chars.log < 5.138628  to the right, agree=1.000, adj=1.000, (0 split)
##       A.num.words     < 17        to the right, agree=0.991, adj=0.625, (0 split)
##       S.num.words     < 17        to the right, agree=0.991, adj=0.625, (0 split)
## 
## Node number 4: 3066 observations,    complexity param=0.01602136
##   predicted class=N  expected loss=0.04957599  P(node) =0.6851397
##     class counts:  2914   152
##    probabilities: 0.950 0.050 
##   left son=8 (2987 obs) right son=9 (79 obs)
##   Primary splits:
##       PubDate.hour        < 21.5      to the left,  improve=23.51779, (0 missing)
##       NewsDesk.fctrStyles < 0.5       to the left,  improve=11.23429, (0 missing)
##       A.num.chars         < 44.5      to the right, improve=10.42832, (0 missing)
##       S.num.chars         < 44.5      to the right, improve=10.42832, (0 missing)
##       S.num.chars.log     < 3.817652  to the right, improve=10.42832, (0 missing)
## 
## Node number 5: 1065 observations,    complexity param=0.03938585
##   predicted class=N  expected loss=0.3079812  P(node) =0.2379888
##     class counts:   737   328
##    probabilities: 0.692 0.308 
##   left son=10 (994 obs) right son=11 (71 obs)
##   Primary splits:
##       SectionName.fctrHealth < 0.5       to the left,  improve=56.15144, (0 missing)
##       NewsDesk.fctrScience   < 0.5       to the left,  improve=55.08535, (0 missing)
##       PubDate.hour           < 7.5       to the left,  improve=18.57852, (0 missing)
##       NewsDesk.fctrForeign   < 0.5       to the right, improve=11.91201, (0 missing)
##       SectionName.fctrWorld  < 0.5       to the right, improve=11.48588, (0 missing)
##   Surrogate splits:
##       NewsDesk.fctrScience < 0.5       to the left,  agree=0.999, adj=0.986, (0 split)
##       PubDate.hour         < 0.5       to the right, agree=0.939, adj=0.085, (0 split)
## 
## Node number 6: 8 observations
##   predicted class=N  expected loss=0.125  P(node) =0.001787709
##     class counts:     7     1
##    probabilities: 0.875 0.125 
## 
## Node number 7: 336 observations,    complexity param=0.00400534
##   predicted class=Y  expected loss=0.202381  P(node) =0.0750838
##     class counts:    68   268
##    probabilities: 0.202 0.798 
##   left son=14 (34 obs) right son=15 (302 obs)
##   Primary splits:
##       WordCount           < 107       to the left,  improve=3.316861, (0 missing)
##       PubDate.wkday       < 3.5       to the right, improve=2.518525, (0 missing)
##       PubDate.hour        < 10.5      to the right, improve=2.041174, (0 missing)
##       H.num.words.unq     < 5.5       to the right, improve=1.827719, (0 missing)
##       H.num.words.unq.log < 1.868835  to the right, improve=1.827719, (0 missing)
##   Surrogate splits:
##       H.num.chars     < 9         to the left,  agree=0.902, adj=0.029, (0 split)
##       H.num.chars.log < 2.29756   to the left,  agree=0.902, adj=0.029, (0 split)
## 
## Node number 8: 2987 observations,    complexity param=0.004539386
##   predicted class=N  expected loss=0.03950452  P(node) =0.667486
##     class counts:  2869   118
##    probabilities: 0.960 0.040 
##   left son=16 (2864 obs) right son=17 (123 obs)
##   Primary splits:
##       NewsDesk.fctrStyles    < 0.5       to the left,  improve=12.492140, (0 missing)
##       NewsDesk.fctrScience   < 0.5       to the left,  improve= 7.607968, (0 missing)
##       SectionName.fctrHealth < 0.5       to the left,  improve= 6.608651, (0 missing)
##       WordCount              < 426.5     to the left,  improve= 4.320240, (0 missing)
##       SectionName.fctrU.S.   < 0.5       to the left,  improve= 4.093264, (0 missing)
##   Surrogate splits:
##       S.week                             < 1.5       to the left,  agree=0.961, adj=0.057, (0 split)
##       A.week                             < 1.5       to the left,  agree=0.961, adj=0.057, (0 split)
##       SectionName.fctrStyle              < 0.5       to the left,  agree=0.959, adj=0.016, (0 split)
##       SubsectionName.fctrFashion & Style < 0.5       to the left,  agree=0.959, adj=0.016, (0 split)
##       A.num.chars                        < 379       to the left,  agree=0.959, adj=0.008, (0 split)
## 
## Node number 9: 79 observations,    complexity param=0.01602136
##   predicted class=N  expected loss=0.4303797  P(node) =0.01765363
##     class counts:    45    34
##    probabilities: 0.570 0.430 
##   left son=18 (45 obs) right son=19 (34 obs)
##   Primary splits:
##       PubDate.minute  < 0.5       to the right, improve=21.31588, (0 missing)
##       A.num.chars     < 62.5      to the right, improve=17.07763, (0 missing)
##       S.num.chars     < 62.5      to the right, improve=17.07763, (0 missing)
##       S.num.chars.log < 4.151009  to the right, improve=17.07763, (0 missing)
##       A.num.chars.log < 4.151009  to the right, improve=17.07763, (0 missing)
##   Surrogate splits:
##       H.num.words         < 4.5       to the right, agree=0.861, adj=0.676, (0 split)
##       H.num.words.unq     < 4.5       to the right, agree=0.861, adj=0.676, (0 split)
##       H.num.words.log     < 1.700599  to the right, agree=0.861, adj=0.676, (0 split)
##       H.num.words.unq.log < 1.700599  to the right, agree=0.861, adj=0.676, (0 split)
##       H.num.chars         < 34.5      to the right, agree=0.848, adj=0.647, (0 split)
## 
## Node number 10: 994 observations,    complexity param=0.02803738
##   predicted class=N  expected loss=0.2645875  P(node) =0.2221229
##     class counts:   731   263
##    probabilities: 0.735 0.265 
##   left son=20 (929 obs) right son=21 (65 obs)
##   Primary splits:
##       PubDate.hour    < 21.5      to the left,  improve=21.91733, (0 missing)
##       A.num.chars     < 78.5      to the right, improve=13.94732, (0 missing)
##       S.num.chars     < 78.5      to the right, improve=13.94732, (0 missing)
##       S.num.chars.log < 4.375737  to the right, improve=13.94732, (0 missing)
##       A.num.chars.log < 4.375737  to the right, improve=13.94732, (0 missing)
##   Surrogate splits:
##       SectionName.fctrArts < 0.5       to the left,  agree=0.943, adj=0.123, (0 split)
##       NewsDesk.fctrCulture < 0.5       to the left,  agree=0.943, adj=0.123, (0 split)
##       A.num.chars          < 44        to the right, agree=0.943, adj=0.123, (0 split)
##       S.num.chars          < 44        to the right, agree=0.943, adj=0.123, (0 split)
##       S.num.chars.log      < 3.806416  to the right, agree=0.943, adj=0.123, (0 split)
## 
## Node number 11: 71 observations
##   predicted class=Y  expected loss=0.08450704  P(node) =0.01586592
##     class counts:     6    65
##    probabilities: 0.085 0.915 
## 
## Node number 14: 34 observations,    complexity param=0.00400534
##   predicted class=Y  expected loss=0.4117647  P(node) =0.007597765
##     class counts:    14    20
##    probabilities: 0.412 0.588 
##   left son=28 (23 obs) right son=29 (11 obs)
##   Primary splits:
##       PubDate.wkday   < 2.5       to the right, improve=3.348059, (0 missing)
##       PubDate.hour    < 17.5      to the right, improve=3.313079, (0 missing)
##       A.num.chars     < 25.5      to the right, improve=2.228164, (0 missing)
##       S.num.chars     < 25.5      to the right, improve=2.228164, (0 missing)
##       S.num.chars.log < 3.276967  to the right, improve=2.228164, (0 missing)
##   Surrogate splits:
##       PubDate.second  < 12        to the right, agree=0.765, adj=0.273, (0 split)
##       A.num.chars     < 19.5      to the right, agree=0.765, adj=0.273, (0 split)
##       S.num.chars     < 19.5      to the right, agree=0.765, adj=0.273, (0 split)
##       S.num.chars.log < 3.020127  to the right, agree=0.765, adj=0.273, (0 split)
##       A.num.chars.log < 3.020127  to the right, agree=0.765, adj=0.273, (0 split)
## 
## Node number 15: 302 observations,    complexity param=0.001602136
##   predicted class=Y  expected loss=0.1788079  P(node) =0.06748603
##     class counts:    54   248
##    probabilities: 0.179 0.821 
##   left son=30 (254 obs) right son=31 (48 obs)
##   Primary splits:
##       A.num.chars     < 38.5      to the right, improve=2.146747, (0 missing)
##       S.num.chars     < 38.5      to the right, improve=2.146747, (0 missing)
##       S.num.chars.log < 3.676221  to the right, improve=2.146747, (0 missing)
##       A.num.chars.log < 3.676221  to the right, improve=2.146747, (0 missing)
##       H.num.chars     < 21.5      to the right, improve=1.604170, (0 missing)
##   Surrogate splits:
##       S.num.chars     < 38.5      to the right, agree=1.000, adj=1.000, (0 split)
##       S.num.chars.log < 3.676221  to the right, agree=1.000, adj=1.000, (0 split)
##       A.num.chars.log < 3.676221  to the right, agree=1.000, adj=1.000, (0 split)
##       A.num.words.unq < 3.5       to the right, agree=0.937, adj=0.604, (0 split)
##       S.num.words.unq < 3.5       to the right, agree=0.937, adj=0.604, (0 split)
## 
## Node number 16: 2864 observations,    complexity param=0.003337784
##   predicted class=N  expected loss=0.03002793  P(node) =0.64
##     class counts:  2778    86
##    probabilities: 0.970 0.030 
##   left son=32 (2806 obs) right son=33 (58 obs)
##   Primary splits:
##       NewsDesk.fctrScience   < 0.5       to the left,  improve=8.194155, (0 missing)
##       SectionName.fctrHealth < 0.5       to the left,  improve=7.308923, (0 missing)
##       WordCount              < 396.5     to the left,  improve=2.632233, (0 missing)
##       PubDate.wkday          < 0.5       to the right, improve=2.360948, (0 missing)
##       A.num.chars            < 44.5      to the right, improve=2.305181, (0 missing)
##   Surrogate splits:
##       SectionName.fctrHealth < 0.5       to the left,  agree=1, adj=0.983, (0 split)
## 
## Node number 17: 123 observations,    complexity param=0.004539386
##   predicted class=N  expected loss=0.2601626  P(node) =0.02748603
##     class counts:    91    32
##    probabilities: 0.740 0.260 
##   left son=34 (61 obs) right son=35 (62 obs)
##   Primary splits:
##       SectionName.fctrU.S. < 0.5       to the left,  improve=16.381850, (0 missing)
##       SectionName.fctr     < 0.5       to the right, improve=14.857290, (0 missing)
##       A.num.chars          < 127.5     to the left,  improve= 5.191844, (0 missing)
##       S.num.chars          < 127.5     to the left,  improve= 5.191844, (0 missing)
##       S.num.chars.log      < 4.855921  to the left,  improve= 5.191844, (0 missing)
##   Surrogate splits:
##       SectionName.fctr < 0.5       to the right, agree=0.976, adj=0.951, (0 split)
##       PubDate.hour     < 9.5       to the left,  agree=0.732, adj=0.459, (0 split)
##       PubDate.month    < 9.5       to the left,  agree=0.699, adj=0.393, (0 split)
##       H.num.chars      < 45.5      to the left,  agree=0.699, adj=0.393, (0 split)
##       H.num.chars.log  < 3.839394  to the left,  agree=0.699, adj=0.393, (0 split)
## 
## Node number 18: 45 observations
##   predicted class=N  expected loss=0.1111111  P(node) =0.01005587
##     class counts:    40     5
##    probabilities: 0.889 0.111 
## 
## Node number 19: 34 observations,    complexity param=0.001335113
##   predicted class=Y  expected loss=0.1470588  P(node) =0.007597765
##     class counts:     5    29
##    probabilities: 0.147 0.853 
##   left son=38 (7 obs) right son=39 (27 obs)
##   Primary splits:
##       WordCount       < 327.5     to the left,  improve=3.174914, (0 missing)
##       H.num.chars     < 29.5      to the right, improve=3.174914, (0 missing)
##       H.num.chars.log < 3.417592  to the right, improve=3.174914, (0 missing)
##       A.num.chars     < 82.5      to the right, improve=3.174914, (0 missing)
##       S.num.chars     < 82.5      to the right, improve=3.174914, (0 missing)
##   Surrogate splits:
##       PubDate.wkday                 < 0.5       to the left,  agree=0.882, adj=0.429, (0 split)
##       SectionName.fctrN.Y. / Region < 0.5       to the right, agree=0.882, adj=0.429, (0 split)
##       NewsDesk.fctrMetro            < 0.5       to the right, agree=0.882, adj=0.429, (0 split)
##       A.num.chars                   < 127       to the right, agree=0.853, adj=0.286, (0 split)
##       S.num.chars                   < 127       to the right, agree=0.853, adj=0.286, (0 split)
## 
## Node number 20: 929 observations,    complexity param=0.01401869
##   predicted class=N  expected loss=0.2368138  P(node) =0.2075978
##     class counts:   709   220
##    probabilities: 0.763 0.237 
##   left son=40 (851 obs) right son=41 (78 obs)
##   Primary splits:
##       NewsDesk.fctrStyles                  < 0.5       to the left,  improve=16.840870, (0 missing)
##       SectionName.fctrU.S.                 < 0.5       to the left,  improve=13.129340, (0 missing)
##       PubDate.hour                         < 7.5       to the left,  improve=12.747800, (0 missing)
##       PubDate.wkday                        < 5.5       to the left,  improve= 8.649968, (0 missing)
##       SubsectionName.fctrThe Public Editor < 0.5       to the left,  improve= 7.524853, (0 missing)
##   Surrogate splits:
##       SectionName.fctrU.S. < 0.5       to the left,  agree=0.963, adj=0.564, (0 split)
## 
## Node number 21: 65 observations,    complexity param=0.01201602
##   predicted class=Y  expected loss=0.3384615  P(node) =0.01452514
##     class counts:    22    43
##    probabilities: 0.338 0.662 
##   left son=42 (25 obs) right son=43 (40 obs)
##   Primary splits:
##       A.num.chars     < 92        to the right, improve=9.477692, (0 missing)
##       S.num.chars     < 92        to the right, improve=9.477692, (0 missing)
##       S.num.chars.log < 4.532368  to the right, improve=9.477692, (0 missing)
##       A.num.chars.log < 4.532368  to the right, improve=9.477692, (0 missing)
##       A.num.words.unq < 9.5       to the right, improve=7.387692, (0 missing)
##   Surrogate splits:
##       S.num.chars     < 92        to the right, agree=1.000, adj=1.00, (0 split)
##       S.num.chars.log < 4.532368  to the right, agree=1.000, adj=1.00, (0 split)
##       A.num.chars.log < 4.532368  to the right, agree=1.000, adj=1.00, (0 split)
##       A.num.words.unq < 9.5       to the right, agree=0.969, adj=0.92, (0 split)
##       S.num.words.unq < 9.5       to the right, agree=0.969, adj=0.92, (0 split)
## 
## Node number 28: 23 observations,    complexity param=0.00400534
##   predicted class=N  expected loss=0.4347826  P(node) =0.005139665
##     class counts:    13    10
##    probabilities: 0.565 0.435 
##   left son=56 (11 obs) right son=57 (12 obs)
##   Primary splits:
##       PubDate.hour   < 15.5      to the right, improve=4.986166, (0 missing)
##       WordCount      < 53        to the right, improve=1.715062, (0 missing)
##       PubDate.second < 31.5      to the left,  improve=1.715062, (0 missing)
##       A.num.chars    < 27.5      to the right, improve=1.590062, (0 missing)
##       S.num.chars    < 27.5      to the right, improve=1.590062, (0 missing)
##   Surrogate splits:
##       A.num.chars     < 25.5      to the right, agree=0.783, adj=0.545, (0 split)
##       S.num.chars     < 25.5      to the right, agree=0.783, adj=0.545, (0 split)
##       S.num.chars.log < 3.276967  to the right, agree=0.783, adj=0.545, (0 split)
##       A.num.chars.log < 3.276967  to the right, agree=0.783, adj=0.545, (0 split)
##       WordCount       < 53        to the right, agree=0.652, adj=0.273, (0 split)
## 
## Node number 29: 11 observations
##   predicted class=Y  expected loss=0.09090909  P(node) =0.002458101
##     class counts:     1    10
##    probabilities: 0.091 0.909 
## 
## Node number 30: 254 observations,    complexity param=0.001602136
##   predicted class=Y  expected loss=0.2047244  P(node) =0.05675978
##     class counts:    52   202
##    probabilities: 0.205 0.795 
##   left son=60 (105 obs) right son=61 (149 obs)
##   Primary splits:
##       PubDate.wkday  < 3.5       to the right, improve=1.373538, (0 missing)
##       PubDate.minute < 32.5      to the left,  improve=1.202827, (0 missing)
##       PubDate.second < 51.5      to the left,  improve=1.149888, (0 missing)
##       WordCount      < 352.5     to the left,  improve=1.099088, (0 missing)
##       H.num.chars    < 21.5      to the right, improve=1.031389, (0 missing)
##   Surrogate splits:
##       WordCount       < 303.5     to the left,  agree=0.618, adj=0.076, (0 split)
##       H.num.chars     < 48.5      to the right, agree=0.610, adj=0.057, (0 split)
##       H.num.chars.log < 3.901922  to the right, agree=0.610, adj=0.057, (0 split)
##       A.num.chars     < 138.5     to the right, agree=0.606, adj=0.048, (0 split)
##       S.num.chars     < 138.5     to the right, agree=0.606, adj=0.048, (0 split)
## 
## Node number 31: 48 observations
##   predicted class=Y  expected loss=0.04166667  P(node) =0.01072626
##     class counts:     2    46
##    probabilities: 0.042 0.958 
## 
## Node number 32: 2806 observations,    complexity param=0.003337784
##   predicted class=N  expected loss=0.02459016  P(node) =0.6270391
##     class counts:  2737    69
##    probabilities: 0.975 0.025 
##   left son=64 (2102 obs) right son=65 (704 obs)
##   Primary splits:
##       WordCount       < 396.5     to the left,  improve=2.907448, (0 missing)
##       PubDate.wkday   < 0.5       to the right, improve=2.650636, (0 missing)
##       A.num.chars     < 44.5      to the right, improve=2.440507, (0 missing)
##       S.num.chars     < 44.5      to the right, improve=2.440507, (0 missing)
##       S.num.chars.log < 3.817652  to the right, improve=2.440507, (0 missing)
##   Surrogate splits:
##       SectionName.fctrBusiness Day      < 0.5       to the left,  agree=0.762, adj=0.053, (0 split)
##       SectionName.fctrWorld             < 0.5       to the left,  agree=0.762, adj=0.053, (0 split)
##       SubsectionName.fctrAsia Pacific   < 0.5       to the left,  agree=0.762, adj=0.053, (0 split)
##       SubsectionName.fctrSmall Business < 0.5       to the left,  agree=0.762, adj=0.050, (0 split)
##       H.num.words.unq                   < 1.5       to the right, agree=0.755, adj=0.023, (0 split)
## 
## Node number 33: 58 observations,    complexity param=0.003337784
##   predicted class=N  expected loss=0.2931034  P(node) =0.01296089
##     class counts:    41    17
##    probabilities: 0.707 0.293 
##   left son=66 (48 obs) right son=67 (10 obs)
##   Primary splits:
##       PubDate.hour    < 9.5       to the right, improve=4.0011490, (0 missing)
##       PubDate.wkday   < 3.5       to the left,  improve=3.0344830, (0 missing)
##       H.num.chars     < 46.5      to the right, improve=1.8137040, (0 missing)
##       H.num.chars.log < 3.860674  to the right, improve=1.8137040, (0 missing)
##       A.num.chars     < 129       to the left,  improve=0.9536747, (0 missing)
##   Surrogate splits:
##       PubDate.second < 2.5       to the right, agree=0.845, adj=0.1, (0 split)
## 
## Node number 34: 61 observations
##   predicted class=N  expected loss=0  P(node) =0.01363128
##     class counts:    61     0
##    probabilities: 1.000 0.000 
## 
## Node number 35: 62 observations,    complexity param=0.004539386
##   predicted class=Y  expected loss=0.483871  P(node) =0.01385475
##     class counts:    30    32
##    probabilities: 0.484 0.516 
##   left son=70 (53 obs) right son=71 (9 obs)
##   Primary splits:
##       PubDate.second  < 8.5       to the right, improve=2.925813, (0 missing)
##       H.num.chars     < 55.5      to the left,  improve=2.669088, (0 missing)
##       H.num.chars.log < 4.034201  to the left,  improve=2.669088, (0 missing)
##       PubDate.minute  < 51.5      to the left,  improve=2.365890, (0 missing)
##       H.num.words     < 5.5       to the left,  improve=2.249904, (0 missing)
##   Surrogate splits:
##       PubDate.minute < 57.5      to the left,  agree=0.871, adj=0.111, (0 split)
## 
## Node number 38: 7 observations
##   predicted class=N  expected loss=0.4285714  P(node) =0.001564246
##     class counts:     4     3
##    probabilities: 0.571 0.429 
## 
## Node number 39: 27 observations
##   predicted class=Y  expected loss=0.03703704  P(node) =0.00603352
##     class counts:     1    26
##    probabilities: 0.037 0.963 
## 
## Node number 40: 851 observations,    complexity param=0.01201602
##   predicted class=N  expected loss=0.2079906  P(node) =0.1901676
##     class counts:   674   177
##    probabilities: 0.792 0.208 
##   left son=80 (834 obs) right son=81 (17 obs)
##   Primary splits:
##       PubDate.wkday                        < 5.5       to the left,  improve=10.752480, (0 missing)
##       SubsectionName.fctrThe Public Editor < 0.5       to the left,  improve= 8.298601, (0 missing)
##       PubDate.hour                         < 7.5       to the left,  improve= 7.895594, (0 missing)
##       H.num.chars                          < 16.5      to the right, improve= 6.622781, (0 missing)
##       H.num.chars.log                      < 2.861793  to the right, improve= 6.622781, (0 missing)
##   Surrogate splits:
##       H.num.chars     < 8.5       to the right, agree=0.982, adj=0.118, (0 split)
##       H.num.chars.log < 2.249905  to the right, agree=0.982, adj=0.118, (0 split)
## 
## Node number 41: 78 observations,    complexity param=0.01401869
##   predicted class=Y  expected loss=0.4487179  P(node) =0.01743017
##     class counts:    35    43
##    probabilities: 0.449 0.551 
##   left son=82 (13 obs) right son=83 (65 obs)
##   Primary splits:
##       SectionName.fctr     < 0.5       to the right, improve=9.482051, (0 missing)
##       SectionName.fctrU.S. < 0.5       to the left,  improve=9.482051, (0 missing)
##       A.num.chars          < 174.5     to the left,  improve=5.128205, (0 missing)
##       S.num.chars          < 174.5     to the left,  improve=5.128205, (0 missing)
##       S.num.chars.log      < 5.167635  to the left,  improve=5.128205, (0 missing)
##   Surrogate splits:
##       SectionName.fctrU.S. < 0.5       to the left,  agree=1.000, adj=1.000, (0 split)
##       S.week               < 0.5       to the right, agree=0.859, adj=0.154, (0 split)
##       A.week               < 0.5       to the right, agree=0.859, adj=0.154, (0 split)
##       WordCount            < 715.5     to the left,  agree=0.846, adj=0.077, (0 split)
##       PubDate.minute       < 57.5      to the right, agree=0.846, adj=0.077, (0 split)
## 
## Node number 42: 25 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.32  P(node) =0.005586592
##     class counts:    17     8
##    probabilities: 0.680 0.320 
##   left son=84 (18 obs) right son=85 (7 obs)
##   Primary splits:
##       PubDate.date    < 11        to the right, improve=1.229206, (0 missing)
##       H.num.words     < 4.5       to the right, improve=1.229206, (0 missing)
##       H.num.words.log < 1.700599  to the right, improve=1.229206, (0 missing)
##       A.num.words     < 16.5      to the left,  improve=1.229206, (0 missing)
##       S.num.words     < 16.5      to the left,  improve=1.229206, (0 missing)
##   Surrogate splits:
##       A.num.chars     < 178.5     to the left,  agree=0.8, adj=0.286, (0 split)
##       S.num.chars     < 178.5     to the left,  agree=0.8, adj=0.286, (0 split)
##       S.num.chars.log < 5.188461  to the left,  agree=0.8, adj=0.286, (0 split)
##       A.num.chars.log < 5.188461  to the left,  agree=0.8, adj=0.286, (0 split)
## 
## Node number 43: 40 observations
##   predicted class=Y  expected loss=0.125  P(node) =0.008938547
##     class counts:     5    35
##    probabilities: 0.125 0.875 
## 
## Node number 56: 11 observations
##   predicted class=N  expected loss=0.09090909  P(node) =0.002458101
##     class counts:    10     1
##    probabilities: 0.909 0.091 
## 
## Node number 57: 12 observations
##   predicted class=Y  expected loss=0.25  P(node) =0.002681564
##     class counts:     3     9
##    probabilities: 0.250 0.750 
## 
## Node number 60: 105 observations,    complexity param=0.001602136
##   predicted class=Y  expected loss=0.2666667  P(node) =0.02346369
##     class counts:    28    77
##    probabilities: 0.267 0.733 
##   left son=120 (48 obs) right son=121 (57 obs)
##   Primary splits:
##       WordCount      < 483       to the left,  improve=2.075439, (0 missing)
##       PubDate.wkday  < 5.5       to the left,  improve=1.747518, (0 missing)
##       PubDate.minute < 12.5      to the right, improve=1.542857, (0 missing)
##       PubDate.second < 47.5      to the left,  improve=1.093178, (0 missing)
##       H.num.words    < 2.5       to the right, improve=1.068339, (0 missing)
##   Surrogate splits:
##       PubDate.minute  < 53.5      to the right, agree=0.629, adj=0.188, (0 split)
##       PubDate.date    < 14.5      to the left,  agree=0.610, adj=0.146, (0 split)
##       PubDate.second  < 54.5      to the right, agree=0.590, adj=0.104, (0 split)
##       H.num.chars     < 54.5      to the right, agree=0.581, adj=0.083, (0 split)
##       H.num.chars.log < 4.016342  to the right, agree=0.581, adj=0.083, (0 split)
## 
## Node number 61: 149 observations
##   predicted class=Y  expected loss=0.1610738  P(node) =0.03329609
##     class counts:    24   125
##    probabilities: 0.161 0.839 
## 
## Node number 64: 2102 observations
##   predicted class=N  expected loss=0.0114177  P(node) =0.4697207
##     class counts:  2078    24
##    probabilities: 0.989 0.011 
## 
## Node number 65: 704 observations,    complexity param=0.003337784
##   predicted class=N  expected loss=0.06392045  P(node) =0.1573184
##     class counts:   659    45
##    probabilities: 0.936 0.064 
##   left son=130 (694 obs) right son=131 (10 obs)
##   Primary splits:
##       A.num.chars     < 56.5      to the right, improve=10.992400, (0 missing)
##       S.num.chars     < 56.5      to the right, improve=10.992400, (0 missing)
##       S.num.chars.log < 4.051747  to the right, improve=10.992400, (0 missing)
##       A.num.chars.log < 4.051747  to the right, improve=10.992400, (0 missing)
##       PubDate.wkday   < 0.5       to the right, improve= 6.557416, (0 missing)
##   Surrogate splits:
##       S.num.chars     < 56.5      to the right, agree=1.00, adj=1.0, (0 split)
##       S.num.chars.log < 4.051747  to the right, agree=1.00, adj=1.0, (0 split)
##       A.num.chars.log < 4.051747  to the right, agree=1.00, adj=1.0, (0 split)
##       A.num.words     < 4.5       to the right, agree=0.99, adj=0.3, (0 split)
##       S.num.words     < 4.5       to the right, agree=0.99, adj=0.3, (0 split)
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
## Node number 70: 53 observations,    complexity param=0.004539386
##   predicted class=N  expected loss=0.4528302  P(node) =0.01184358
##     class counts:    29    24
##    probabilities: 0.547 0.453 
##   left son=140 (16 obs) right son=141 (37 obs)
##   Primary splits:
##       S.week          < 0.5       to the right, improve=3.226989, (0 missing)
##       A.week          < 0.5       to the right, improve=3.226989, (0 missing)
##       H.num.words     < 5.5       to the left,  improve=2.533946, (0 missing)
##       H.num.words.unq < 5.5       to the left,  improve=2.533946, (0 missing)
##       H.num.words.log < 1.868835  to the left,  improve=2.533946, (0 missing)
##   Surrogate splits:
##       A.week          < 0.5       to the right, agree=1.000, adj=1.000, (0 split)
##       WordCount       < 52        to the left,  agree=0.887, adj=0.625, (0 split)
##       PubDate.wkday   < 4.5       to the right, agree=0.849, adj=0.500, (0 split)
##       H.num.chars     < 48        to the left,  agree=0.811, adj=0.375, (0 split)
##       H.num.chars.log < 3.891612  to the left,  agree=0.811, adj=0.375, (0 split)
## 
## Node number 71: 9 observations
##   predicted class=Y  expected loss=0.1111111  P(node) =0.002011173
##     class counts:     1     8
##    probabilities: 0.111 0.889 
## 
## Node number 80: 834 observations,    complexity param=0.009345794
##   predicted class=N  expected loss=0.1966427  P(node) =0.1863687
##     class counts:   670   164
##    probabilities: 0.803 0.197 
##   left son=160 (823 obs) right son=161 (11 obs)
##   Primary splits:
##       SubsectionName.fctrThe Public Editor < 0.5       to the left,  improve=8.612433, (0 missing)
##       PubDate.hour                         < 7.5       to the left,  improve=6.615481, (0 missing)
##       NewsDesk.fctrForeign                 < 0.5       to the right, improve=4.228906, (0 missing)
##       SectionName.fctrWorld                < 0.5       to the right, improve=4.141039, (0 missing)
##       A.num.chars                          < 188.5     to the right, improve=3.859047, (0 missing)
## 
## Node number 81: 17 observations
##   predicted class=Y  expected loss=0.2352941  P(node) =0.003798883
##     class counts:     4    13
##    probabilities: 0.235 0.765 
## 
## Node number 82: 13 observations
##   predicted class=N  expected loss=0  P(node) =0.002905028
##     class counts:    13     0
##    probabilities: 1.000 0.000 
## 
## Node number 83: 65 observations,    complexity param=0.003337784
##   predicted class=Y  expected loss=0.3384615  P(node) =0.01452514
##     class counts:    22    43
##    probabilities: 0.338 0.662 
##   left son=166 (40 obs) right son=167 (25 obs)
##   Primary splits:
##       A.num.chars     < 174.5     to the left,  improve=2.587692, (0 missing)
##       S.num.chars     < 174.5     to the left,  improve=2.587692, (0 missing)
##       S.num.chars.log < 5.167635  to the left,  improve=2.587692, (0 missing)
##       A.num.chars.log < 5.167635  to the left,  improve=2.587692, (0 missing)
##       S.new           < 0.5       to the right, improve=2.216067, (0 missing)
##   Surrogate splits:
##       S.num.chars     < 174.5     to the left,  agree=1.000, adj=1.00, (0 split)
##       S.num.chars.log < 5.167635  to the left,  agree=1.000, adj=1.00, (0 split)
##       A.num.chars.log < 5.167635  to the left,  agree=1.000, adj=1.00, (0 split)
##       A.num.words     < 15.5      to the left,  agree=0.862, adj=0.64, (0 split)
##       S.num.words     < 15.5      to the left,  agree=0.862, adj=0.64, (0 split)
## 
## Node number 84: 18 observations
##   predicted class=N  expected loss=0.2222222  P(node) =0.004022346
##     class counts:    14     4
##    probabilities: 0.778 0.222 
## 
## Node number 85: 7 observations
##   predicted class=Y  expected loss=0.4285714  P(node) =0.001564246
##     class counts:     3     4
##    probabilities: 0.429 0.571 
## 
## Node number 120: 48 observations,    complexity param=0.001602136
##   predicted class=Y  expected loss=0.375  P(node) =0.01072626
##     class counts:    18    30
##    probabilities: 0.375 0.625 
##   left son=240 (41 obs) right son=241 (7 obs)
##   Primary splits:
##       A.num.chars     < 56        to the right, improve=2.304878, (0 missing)
##       S.num.chars     < 56        to the right, improve=2.304878, (0 missing)
##       S.num.chars.log < 4.042897  to the right, improve=2.304878, (0 missing)
##       A.num.chars.log < 4.042897  to the right, improve=2.304878, (0 missing)
##       PubDate.date    < 24.5      to the right, improve=1.886760, (0 missing)
##   Surrogate splits:
##       S.num.chars     < 56        to the right, agree=1.000, adj=1.000, (0 split)
##       S.num.chars.log < 4.042897  to the right, agree=1.000, adj=1.000, (0 split)
##       A.num.chars.log < 4.042897  to the right, agree=1.000, adj=1.000, (0 split)
##       A.num.words.unq < 5.5       to the right, agree=0.938, adj=0.571, (0 split)
##       S.num.words.unq < 5.5       to the right, agree=0.938, adj=0.571, (0 split)
## 
## Node number 121: 57 observations
##   predicted class=Y  expected loss=0.1754386  P(node) =0.01273743
##     class counts:    10    47
##    probabilities: 0.175 0.825 
## 
## Node number 130: 694 observations,    complexity param=0.0004450378
##   predicted class=N  expected loss=0.05331412  P(node) =0.1550838
##     class counts:   657    37
##    probabilities: 0.947 0.053 
##   left son=260 (250 obs) right son=261 (444 obs)
##   Primary splits:
##       SectionName.fctrBusiness Day < 0.5       to the right, improve=1.9005930, (0 missing)
##       SubsectionName.fctrDealbook  < 0.5       to the right, improve=1.3867990, (0 missing)
##       PubDate.hour                 < 18.5      to the left,  improve=1.2015070, (0 missing)
##       SectionName.fctrTechnology   < 0.5       to the left,  improve=1.1521010, (0 missing)
##       PubDate.wkday                < 0.5       to the right, improve=0.8343778, (0 missing)
##   Surrogate splits:
##       SubsectionName.fctrDealbook       < 0.5       to the right, agree=0.938, adj=0.828, (0 split)
##       SubsectionName.fctrSmall Business < 0.5       to the right, agree=0.702, adj=0.172, (0 split)
##       S.compani                         < 0.5       to the right, agree=0.689, adj=0.136, (0 split)
##       A.compani                         < 0.5       to the right, agree=0.689, adj=0.136, (0 split)
##       WordCount                         < 411.5     to the left,  agree=0.641, adj=0.004, (0 split)
## 
## Node number 131: 10 observations
##   predicted class=Y  expected loss=0.2  P(node) =0.002234637
##     class counts:     2     8
##    probabilities: 0.200 0.800 
## 
## Node number 140: 16 observations
##   predicted class=N  expected loss=0.1875  P(node) =0.003575419
##     class counts:    13     3
##    probabilities: 0.812 0.188 
## 
## Node number 141: 37 observations,    complexity param=0.004539386
##   predicted class=Y  expected loss=0.4324324  P(node) =0.008268156
##     class counts:    16    21
##    probabilities: 0.432 0.568 
##   left son=282 (9 obs) right son=283 (28 obs)
##   Primary splits:
##       PubDate.second < 21.5      to the left,  improve=2.836765, (0 missing)
##       WordCount      < 310       to the left,  improve=1.305019, (0 missing)
##       PubDate.month  < 10.5      to the right, improve=1.305019, (0 missing)
##       A.num.chars    < 199       to the right, improve=1.305019, (0 missing)
##       S.num.chars    < 199       to the right, improve=1.305019, (0 missing)
##   Surrogate splits:
##       WordCount       < 190       to the left,  agree=0.811, adj=0.222, (0 split)
##       PubDate.minute  < 33.5      to the right, agree=0.811, adj=0.222, (0 split)
##       A.num.words     < 24.5      to the right, agree=0.811, adj=0.222, (0 split)
##       S.num.words     < 23.5      to the right, agree=0.811, adj=0.222, (0 split)
##       A.num.words.log < 3.238486  to the right, agree=0.811, adj=0.222, (0 split)
## 
## Node number 160: 823 observations,    complexity param=0.001602136
##   predicted class=N  expected loss=0.1883354  P(node) =0.1839106
##     class counts:   668   155
##    probabilities: 0.812 0.188 
##   left son=320 (267 obs) right son=321 (556 obs)
##   Primary splits:
##       PubDate.hour          < 7.5       to the left,  improve=5.506684, (0 missing)
##       SectionName.fctr      < 0.5       to the left,  improve=4.475767, (0 missing)
##       WordCount             < 3668      to the left,  improve=3.905955, (0 missing)
##       NewsDesk.fctrForeign  < 0.5       to the right, improve=3.824819, (0 missing)
##       SectionName.fctrWorld < 0.5       to the right, improve=3.744282, (0 missing)
##   Surrogate splits:
##       SectionName.fctrN.Y. / Region     < 0.5       to the right, agree=0.724, adj=0.150, (0 split)
##       NewsDesk.fctrMetro                < 0.5       to the right, agree=0.724, adj=0.150, (0 split)
##       SectionName.fctrMultimedia        < 0.5       to the right, agree=0.713, adj=0.116, (0 split)
##       H.new                             < 0.5       to the right, agree=0.707, adj=0.097, (0 split)
##       SubsectionName.fctrSmall Business < 0.5       to the right, agree=0.706, adj=0.094, (0 split)
## 
## Node number 161: 11 observations
##   predicted class=Y  expected loss=0.1818182  P(node) =0.002458101
##     class counts:     2     9
##    probabilities: 0.182 0.818 
## 
## Node number 166: 40 observations,    complexity param=0.003337784
##   predicted class=Y  expected loss=0.45  P(node) =0.008938547
##     class counts:    18    22
##    probabilities: 0.450 0.550 
##   left son=332 (17 obs) right son=333 (23 obs)
##   Primary splits:
##       PubDate.date        < 17.5      to the right, improve=2.296164, (0 missing)
##       A.num.words.unq     < 10.5      to the right, improve=2.182445, (0 missing)
##       S.num.words.unq     < 10.5      to the right, improve=2.182445, (0 missing)
##       A.num.words.unq.log < 2.441401  to the right, improve=2.182445, (0 missing)
##       S.num.words.unq.log < 2.441401  to the right, improve=2.182445, (0 missing)
##   Surrogate splits:
##       A.num.chars     < 141.5     to the right, agree=0.700, adj=0.294, (0 split)
##       S.num.chars     < 141.5     to the right, agree=0.700, adj=0.294, (0 split)
##       S.num.chars.log < 4.959336  to the right, agree=0.700, adj=0.294, (0 split)
##       A.num.chars.log < 4.959336  to the right, agree=0.700, adj=0.294, (0 split)
##       PubDate.second  < 29        to the left,  agree=0.675, adj=0.235, (0 split)
## 
## Node number 167: 25 observations
##   predicted class=Y  expected loss=0.16  P(node) =0.005586592
##     class counts:     4    21
##    probabilities: 0.160 0.840 
## 
## Node number 240: 41 observations,    complexity param=0.001602136
##   predicted class=Y  expected loss=0.4390244  P(node) =0.009162011
##     class counts:    18    23
##    probabilities: 0.439 0.561 
##   left son=480 (10 obs) right son=481 (31 obs)
##   Primary splits:
##       A.num.chars     < 70.5      to the left,  improve=3.446735, (0 missing)
##       S.num.chars     < 70.5      to the left,  improve=3.446735, (0 missing)
##       S.num.chars.log < 4.268498  to the left,  improve=3.446735, (0 missing)
##       A.num.chars.log < 4.268498  to the left,  improve=3.446735, (0 missing)
##       H.num.words.unq < 4.5       to the left,  improve=2.513749, (0 missing)
##   Surrogate splits:
##       S.num.chars     < 70.5      to the left,  agree=1.000, adj=1.0, (0 split)
##       S.num.chars.log < 4.268498  to the left,  agree=1.000, adj=1.0, (0 split)
##       A.num.chars.log < 4.268498  to the left,  agree=1.000, adj=1.0, (0 split)
##       A.num.words     < 6.5       to the left,  agree=0.854, adj=0.4, (0 split)
##       S.num.words     < 6.5       to the left,  agree=0.854, adj=0.4, (0 split)
## 
## Node number 241: 7 observations
##   predicted class=Y  expected loss=0  P(node) =0.001564246
##     class counts:     0     7
##    probabilities: 0.000 1.000 
## 
## Node number 260: 250 observations
##   predicted class=N  expected loss=0.004  P(node) =0.05586592
##     class counts:   249     1
##    probabilities: 0.996 0.004 
## 
## Node number 261: 444 observations,    complexity param=0.0004450378
##   predicted class=N  expected loss=0.08108108  P(node) =0.09921788
##     class counts:   408    36
##    probabilities: 0.919 0.081 
##   left son=522 (415 obs) right son=523 (29 obs)
##   Primary splits:
##       PubDate.hour         < 18.5      to the left,  improve=1.5944850, (0 missing)
##       PubDate.date         < 23.5      to the left,  improve=1.1176040, (0 missing)
##       NewsDesk.fctrTStyle  < 0.5       to the right, improve=1.0193050, (0 missing)
##       PubDate.wkday        < 0.5       to the right, improve=0.8285489, (0 missing)
##       NewsDesk.fctrForeign < 0.5       to the right, improve=0.8254012, (0 missing)
## 
## Node number 282: 9 observations
##   predicted class=N  expected loss=0.2222222  P(node) =0.002011173
##     class counts:     7     2
##    probabilities: 0.778 0.222 
## 
## Node number 283: 28 observations
##   predicted class=Y  expected loss=0.3214286  P(node) =0.006256983
##     class counts:     9    19
##    probabilities: 0.321 0.679 
## 
## Node number 320: 267 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.1048689  P(node) =0.0596648
##     class counts:   239    28
##    probabilities: 0.895 0.105 
##   left son=640 (252 obs) right son=641 (15 obs)
##   Primary splits:
##       S.compani        < 0.5       to the left,  improve=1.659087, (0 missing)
##       A.compani        < 0.5       to the left,  improve=1.659087, (0 missing)
##       SectionName.fctr < 0.5       to the left,  improve=1.400771, (0 missing)
##       PubDate.hour     < 2.5       to the right, improve=1.300517, (0 missing)
##       PubDate.minute   < 5.5       to the left,  improve=1.050223, (0 missing)
##   Surrogate splits:
##       A.compani    < 0.5       to the left,  agree=1.000, adj=1.000, (0 split)
##       PubDate.hour < 0.5       to the right, agree=0.948, adj=0.067, (0 split)
## 
## Node number 321: 556 observations,    complexity param=0.001602136
##   predicted class=N  expected loss=0.2284173  P(node) =0.1242458
##     class counts:   429   127
##    probabilities: 0.772 0.228 
##   left son=642 (94 obs) right son=643 (462 obs)
##   Primary splits:
##       A.num.chars     < 188.5     to the right, improve=3.982475, (0 missing)
##       S.num.chars     < 188.5     to the right, improve=3.982475, (0 missing)
##       S.num.chars.log < 5.244386  to the right, improve=3.982475, (0 missing)
##       A.num.chars.log < 5.244386  to the right, improve=3.982475, (0 missing)
##       A.num.words.unq < 17.5      to the right, improve=3.822076, (0 missing)
##   Surrogate splits:
##       S.num.chars     < 188.5     to the right, agree=1.000, adj=1.000, (0 split)
##       S.num.chars.log < 5.244386  to the right, agree=1.000, adj=1.000, (0 split)
##       A.num.chars.log < 5.244386  to the right, agree=1.000, adj=1.000, (0 split)
##       A.num.words     < 19.5      to the right, agree=0.928, adj=0.574, (0 split)
##       A.num.words.log < 3.020127  to the right, agree=0.928, adj=0.574, (0 split)
## 
## Node number 332: 17 observations
##   predicted class=N  expected loss=0.3529412  P(node) =0.003798883
##     class counts:    11     6
##    probabilities: 0.647 0.353 
## 
## Node number 333: 23 observations,    complexity param=0.003337784
##   predicted class=Y  expected loss=0.3043478  P(node) =0.005139665
##     class counts:     7    16
##    probabilities: 0.304 0.696 
##   left son=666 (7 obs) right son=667 (16 obs)
##   Primary splits:
##       PubDate.minute      < 38        to the right, improve=3.381988, (0 missing)
##       PubDate.second      < 33.5      to the left,  improve=3.092977, (0 missing)
##       A.num.words.unq     < 10.5      to the right, improve=2.272464, (0 missing)
##       S.num.words.unq     < 10.5      to the right, improve=2.272464, (0 missing)
##       A.num.words.unq.log < 2.441401  to the right, improve=2.272464, (0 missing)
##   Surrogate splits:
##       PubDate.hour        < 17.5      to the right, agree=0.783, adj=0.286, (0 split)
##       H.num.words         < 2.5       to the left,  agree=0.783, adj=0.286, (0 split)
##       H.num.words.unq     < 2.5       to the left,  agree=0.783, adj=0.286, (0 split)
##       H.num.words.log     < 1.242453  to the left,  agree=0.783, adj=0.286, (0 split)
##       H.num.words.unq.log < 1.242453  to the left,  agree=0.783, adj=0.286, (0 split)
## 
## Node number 480: 10 observations
##   predicted class=N  expected loss=0.2  P(node) =0.002234637
##     class counts:     8     2
##    probabilities: 0.800 0.200 
## 
## Node number 481: 31 observations,    complexity param=0.001602136
##   predicted class=Y  expected loss=0.3225806  P(node) =0.006927374
##     class counts:    10    21
##    probabilities: 0.323 0.677 
##   left son=962 (12 obs) right son=963 (19 obs)
##   Primary splits:
##       H.num.chars     < 39        to the left,  improve=2.662422, (0 missing)
##       H.num.chars.log < 3.688567  to the left,  improve=2.662422, (0 missing)
##       H.num.words     < 4.5       to the left,  improve=2.662422, (0 missing)
##       H.num.words.unq < 4.5       to the left,  improve=2.662422, (0 missing)
##       H.num.words.log < 1.700599  to the left,  improve=2.662422, (0 missing)
##   Surrogate splits:
##       H.num.chars.log     < 3.688567  to the left,  agree=1.000, adj=1.000, (0 split)
##       H.num.words         < 4.5       to the left,  agree=0.935, adj=0.833, (0 split)
##       H.num.words.unq     < 4.5       to the left,  agree=0.935, adj=0.833, (0 split)
##       H.num.words.log     < 1.700599  to the left,  agree=0.935, adj=0.833, (0 split)
##       H.num.words.unq.log < 1.700599  to the left,  agree=0.935, adj=0.833, (0 split)
## 
## Node number 522: 415 observations,    complexity param=0.0004450378
##   predicted class=N  expected loss=0.06987952  P(node) =0.09273743
##     class counts:   386    29
##    probabilities: 0.930 0.070 
##   left son=1044 (316 obs) right son=1045 (99 obs)
##   Primary splits:
##       PubDate.date               < 23.5      to the left,  improve=1.3306350, (0 missing)
##       NewsDesk.fctrTStyle        < 0.5       to the right, improve=0.7390107, (0 missing)
##       PubDate.second             < 9.5       to the right, improve=0.7042069, (0 missing)
##       SectionName.fctrTechnology < 0.5       to the left,  improve=0.6535238, (0 missing)
##       NewsDesk.fctrForeign       < 0.5       to the right, improve=0.5933968, (0 missing)
##   Surrogate splits:
##       H.num.chars     < 13.5      to the right, agree=0.764, adj=0.01, (0 split)
##       H.num.chars.log < 2.65906   to the right, agree=0.764, adj=0.01, (0 split)
##       H.num.words     < 11        to the left,  agree=0.764, adj=0.01, (0 split)
##       H.num.words.unq < 11        to the left,  agree=0.764, adj=0.01, (0 split)
##       H.num.words.log < 2.481422  to the left,  agree=0.764, adj=0.01, (0 split)
## 
## Node number 523: 29 observations
##   predicted class=N  expected loss=0.2413793  P(node) =0.006480447
##     class counts:    22     7
##    probabilities: 0.759 0.241 
## 
## Node number 640: 252 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.09126984  P(node) =0.05631285
##     class counts:   229    23
##    probabilities: 0.909 0.091 
##   left son=1280 (192 obs) right son=1281 (60 obs)
##   Primary splits:
##       SectionName.fctr < 0.5       to the left,  improve=1.8620040, (0 missing)
##       PubDate.minute   < 5.5       to the left,  improve=0.9876580, (0 missing)
##       A.num.chars      < 101       to the left,  improve=0.6567234, (0 missing)
##       S.num.chars      < 101       to the left,  improve=0.6567234, (0 missing)
##       S.num.chars.log  < 4.624925  to the left,  improve=0.6567234, (0 missing)
##   Surrogate splits:
##       H.num.words         < 2.5       to the right, agree=0.889, adj=0.533, (0 split)
##       H.num.words.unq     < 2.5       to the right, agree=0.889, adj=0.533, (0 split)
##       H.num.words.log     < 1.242453  to the right, agree=0.889, adj=0.533, (0 split)
##       H.num.words.unq.log < 1.242453  to the right, agree=0.889, adj=0.533, (0 split)
##       H.num.chars         < 23        to the right, agree=0.885, adj=0.517, (0 split)
## 
## Node number 641: 15 observations
##   predicted class=N  expected loss=0.3333333  P(node) =0.003351955
##     class counts:    10     5
##    probabilities: 0.667 0.333 
## 
## Node number 642: 94 observations
##   predicted class=N  expected loss=0.09574468  P(node) =0.02100559
##     class counts:    85     9
##    probabilities: 0.904 0.096 
## 
## Node number 643: 462 observations,    complexity param=0.001602136
##   predicted class=N  expected loss=0.2554113  P(node) =0.1032402
##     class counts:   344   118
##    probabilities: 0.745 0.255 
##   left son=1286 (39 obs) right son=1287 (423 obs)
##   Primary splits:
##       SubsectionName.fctrRoom For Debate < 0.5       to the right, improve=4.497630, (0 missing)
##       PubDate.hour                       < 12.5      to the right, improve=4.284205, (0 missing)
##       PubDate.wkday                      < 2.5       to the right, improve=3.982380, (0 missing)
##       SectionName.fctrOpinion            < 0.5       to the right, improve=3.695456, (0 missing)
##       PubDate.second                     < 53.5      to the left,  improve=3.028558, (0 missing)
##   Surrogate splits:
##       SectionName.fctrOpinion < 0.5       to the right, agree=0.998, adj=0.974, (0 split)
##       WordCount               < 1909.5    to the right, agree=0.935, adj=0.231, (0 split)
##       A.num.chars             < 50        to the left,  agree=0.918, adj=0.026, (0 split)
##       S.num.chars             < 50        to the left,  agree=0.918, adj=0.026, (0 split)
##       S.num.chars.log         < 3.931633  to the left,  agree=0.918, adj=0.026, (0 split)
## 
## Node number 666: 7 observations
##   predicted class=N  expected loss=0.2857143  P(node) =0.001564246
##     class counts:     5     2
##    probabilities: 0.714 0.286 
## 
## Node number 667: 16 observations
##   predicted class=Y  expected loss=0.125  P(node) =0.003575419
##     class counts:     2    14
##    probabilities: 0.125 0.875 
## 
## Node number 962: 12 observations
##   predicted class=N  expected loss=0.4166667  P(node) =0.002681564
##     class counts:     7     5
##    probabilities: 0.583 0.417 
## 
## Node number 963: 19 observations
##   predicted class=Y  expected loss=0.1578947  P(node) =0.00424581
##     class counts:     3    16
##    probabilities: 0.158 0.842 
## 
## Node number 1044: 316 observations
##   predicted class=N  expected loss=0.04746835  P(node) =0.07061453
##     class counts:   301    15
##    probabilities: 0.953 0.047 
## 
## Node number 1045: 99 observations,    complexity param=0.0004450378
##   predicted class=N  expected loss=0.1414141  P(node) =0.02212291
##     class counts:    85    14
##    probabilities: 0.859 0.141 
##   left son=2090 (65 obs) right son=2091 (34 obs)
##   Primary splits:
##       PubDate.month              < 9.5       to the right, improve=1.574341, (0 missing)
##       PubDate.second             < 25.5      to the right, improve=1.247751, (0 missing)
##       SectionName.fctrTechnology < 0.5       to the left,  improve=0.975469, (0 missing)
##       A.num.chars                < 175.5     to the right, improve=0.940404, (0 missing)
##       S.num.chars                < 175.5     to the right, improve=0.940404, (0 missing)
##   Surrogate splits:
##       H.num.chars     < 65.5      to the left,  agree=0.687, adj=0.088, (0 split)
##       H.num.chars.log < 4.197174  to the left,  agree=0.687, adj=0.088, (0 split)
##       PubDate.hour    < 17.5      to the left,  agree=0.667, adj=0.029, (0 split)
##       PubDate.date    < 26.5      to the right, agree=0.667, adj=0.029, (0 split)
##       A.num.chars     < 72.5      to the right, agree=0.667, adj=0.029, (0 split)
## 
## Node number 1280: 192 observations
##   predicted class=N  expected loss=0.05729167  P(node) =0.04290503
##     class counts:   181    11
##    probabilities: 0.943 0.057 
## 
## Node number 1281: 60 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.2  P(node) =0.01340782
##     class counts:    48    12
##    probabilities: 0.800 0.200 
##   left son=2562 (31 obs) right son=2563 (29 obs)
##   Primary splits:
##       WordCount       < 1184.5    to the right, improve=5.131034, (0 missing)
##       PubDate.hour    < 6.5       to the right, improve=5.131034, (0 missing)
##       H.num.chars     < 18        to the left,  improve=4.490323, (0 missing)
##       H.num.chars.log < 2.943052  to the left,  improve=4.490323, (0 missing)
##       H.num.words     < 2.5       to the left,  improve=1.745455, (0 missing)
##   Surrogate splits:
##       H.num.chars     < 18        to the left,  agree=0.867, adj=0.724, (0 split)
##       H.num.chars.log < 2.943052  to the left,  agree=0.867, adj=0.724, (0 split)
##       PubDate.hour    < 6.5       to the right, agree=0.833, adj=0.655, (0 split)
##       H.num.words     < 2.5       to the left,  agree=0.833, adj=0.655, (0 split)
##       H.num.words.unq < 2.5       to the left,  agree=0.833, adj=0.655, (0 split)
## 
## Node number 1286: 39 observations
##   predicted class=N  expected loss=0.02564103  P(node) =0.008715084
##     class counts:    38     1
##    probabilities: 0.974 0.026 
## 
## Node number 1287: 423 observations,    complexity param=0.001602136
##   predicted class=N  expected loss=0.2765957  P(node) =0.09452514
##     class counts:   306   117
##    probabilities: 0.723 0.277 
##   left son=2574 (287 obs) right son=2575 (136 obs)
##   Primary splits:
##       WordCount           < 1152.5    to the left,  improve=4.483810, (0 missing)
##       PubDate.wkday       < 2.5       to the right, improve=4.393720, (0 missing)
##       NewsDesk.fctrTStyle < 0.5       to the right, improve=3.860776, (0 missing)
##       H.num.words.unq     < 4.5       to the right, improve=3.708937, (0 missing)
##       H.num.words.unq.log < 1.700599  to the right, improve=3.708937, (0 missing)
##   Surrogate splits:
##       PubDate.hour    < 8.5       to the right, agree=0.704, adj=0.081, (0 split)
##       H.num.chars     < 25.5      to the right, agree=0.702, adj=0.074, (0 split)
##       H.num.chars.log < 3.276967  to the right, agree=0.702, adj=0.074, (0 split)
##       PubDate.minute  < 1.5       to the right, agree=0.697, adj=0.059, (0 split)
##       A.num.chars     < 186.5     to the left,  agree=0.686, adj=0.022, (0 split)
## 
## Node number 2090: 65 observations
##   predicted class=N  expected loss=0.07692308  P(node) =0.01452514
##     class counts:    60     5
##    probabilities: 0.923 0.077 
## 
## Node number 2091: 34 observations,    complexity param=0.0004450378
##   predicted class=N  expected loss=0.2647059  P(node) =0.007597765
##     class counts:    25     9
##    probabilities: 0.735 0.265 
##   left son=4182 (8 obs) right son=4183 (26 obs)
##   Primary splits:
##       A.num.words         < 16.5      to the right, improve=1.466063, (0 missing)
##       S.num.words         < 16.5      to the right, improve=1.466063, (0 missing)
##       A.num.words.log     < 2.861793  to the right, improve=1.466063, (0 missing)
##       S.num.words.log     < 2.861793  to the right, improve=1.466063, (0 missing)
##       NewsDesk.fctrTStyle < 0.5       to the right, improve=1.235294, (0 missing)
##   Surrogate splits:
##       S.num.words     < 16.5      to the right, agree=1.000, adj=1.000, (0 split)
##       A.num.words.log < 2.861793  to the right, agree=1.000, adj=1.000, (0 split)
##       S.num.words.log < 2.861793  to the right, agree=1.000, adj=1.000, (0 split)
##       A.num.words.unq < 16.5      to the right, agree=0.971, adj=0.875, (0 split)
##       S.num.words.unq < 16.5      to the right, agree=0.971, adj=0.875, (0 split)
## 
## Node number 2562: 31 observations
##   predicted class=N  expected loss=0  P(node) =0.006927374
##     class counts:    31     0
##    probabilities: 1.000 0.000 
## 
## Node number 2563: 29 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.4137931  P(node) =0.006480447
##     class counts:    17    12
##    probabilities: 0.586 0.414 
##   left son=5126 (19 obs) right son=5127 (10 obs)
##   Primary splits:
##       PubDate.wkday  < 3.5       to the left,  improve=2.5005440, (0 missing)
##       PubDate.minute < 26        to the right, improve=1.8427750, (0 missing)
##       PubDate.second < 23.5      to the right, improve=1.0584390, (0 missing)
##       WordCount      < 900       to the left,  improve=0.7053292, (0 missing)
##       PubDate.date   < 9.5       to the left,  improve=0.7053292, (0 missing)
##   Surrogate splits:
##       PubDate.date < 15.5      to the left,  agree=0.724, adj=0.2, (0 split)
##       S.new        < 0.5       to the left,  agree=0.724, adj=0.2, (0 split)
##       A.new        < 0.5       to the left,  agree=0.724, adj=0.2, (0 split)
##       A.num.chars  < 200.5     to the left,  agree=0.724, adj=0.2, (0 split)
##       S.num.chars  < 200.5     to the left,  agree=0.724, adj=0.2, (0 split)
## 
## Node number 2574: 287 observations,    complexity param=0.001602136
##   predicted class=N  expected loss=0.2264808  P(node) =0.06413408
##     class counts:   222    65
##    probabilities: 0.774 0.226 
##   left son=5148 (214 obs) right son=5149 (73 obs)
##   Primary splits:
##       PubDate.wkday       < 1.5       to the right, improve=4.025421, (0 missing)
##       PubDate.second      < 54.5      to the left,  improve=3.159319, (0 missing)
##       NewsDesk.fctr       < 0.5       to the left,  improve=2.540345, (0 missing)
##       NewsDesk.fctrTStyle < 0.5       to the right, improve=2.378395, (0 missing)
##       H.num.words.unq     < 4.5       to the right, improve=2.280619, (0 missing)
##   Surrogate splits:
##       PubDate.hour    < 20.5      to the left,  agree=0.770, adj=0.096, (0 split)
##       H.num.chars     < 14        to the right, agree=0.753, adj=0.027, (0 split)
##       H.num.chars.log < 2.705823  to the right, agree=0.753, adj=0.027, (0 split)
##       A.num.chars     < 47        to the right, agree=0.753, adj=0.027, (0 split)
##       S.num.chars     < 47        to the right, agree=0.753, adj=0.027, (0 split)
## 
## Node number 2575: 136 observations,    complexity param=0.001602136
##   predicted class=N  expected loss=0.3823529  P(node) =0.03039106
##     class counts:    84    52
##    probabilities: 0.618 0.382 
##   left son=5150 (126 obs) right son=5151 (10 obs)
##   Primary splits:
##       H.num.chars         < 21.5      to the right, improve=3.765453, (0 missing)
##       H.num.chars.log     < 3.113268  to the right, improve=3.765453, (0 missing)
##       PubDate.wkday       < 3.5       to the right, improve=3.432863, (0 missing)
##       NewsDesk.fctrTStyle < 0.5       to the right, improve=2.405360, (0 missing)
##       PubDate.hour        < 8.5       to the right, improve=2.171264, (0 missing)
##   Surrogate splits:
##       H.num.chars.log     < 3.113268  to the right, agree=1.000, adj=1.0, (0 split)
##       H.num.words         < 1.5       to the right, agree=0.941, adj=0.2, (0 split)
##       H.num.words.unq     < 1.5       to the right, agree=0.941, adj=0.2, (0 split)
##       H.num.words.log     < 0.8958797 to the right, agree=0.941, adj=0.2, (0 split)
##       H.num.words.unq.log < 0.8958797 to the right, agree=0.941, adj=0.2, (0 split)
## 
## Node number 4182: 8 observations
##   predicted class=N  expected loss=0  P(node) =0.001787709
##     class counts:     8     0
##    probabilities: 1.000 0.000 
## 
## Node number 4183: 26 observations,    complexity param=0.0004450378
##   predicted class=N  expected loss=0.3461538  P(node) =0.005810056
##     class counts:    17     9
##    probabilities: 0.654 0.346 
##   left son=8366 (18 obs) right son=8367 (8 obs)
##   Primary splits:
##       SectionName.fctrTechnology < 0.5       to the left,  improve=1.797009, (0 missing)
##       A.num.words                < 13.5      to the left,  improve=1.207139, (0 missing)
##       S.num.words                < 13.5      to the left,  improve=1.207139, (0 missing)
##       A.num.words.log            < 2.673554  to the left,  improve=1.207139, (0 missing)
##       S.num.words.log            < 2.673554  to the left,  improve=1.207139, (0 missing)
##   Surrogate splits:
##       WordCount       < 591.5     to the left,  agree=0.769, adj=0.25, (0 split)
##       PubDate.minute  < 57        to the left,  agree=0.769, adj=0.25, (0 split)
##       A.num.chars     < 161       to the left,  agree=0.769, adj=0.25, (0 split)
##       S.num.chars     < 161       to the left,  agree=0.769, adj=0.25, (0 split)
##       S.num.chars.log < 5.085687  to the left,  agree=0.769, adj=0.25, (0 split)
## 
## Node number 5126: 19 observations
##   predicted class=N  expected loss=0.2631579  P(node) =0.00424581
##     class counts:    14     5
##    probabilities: 0.737 0.263 
## 
## Node number 5127: 10 observations
##   predicted class=Y  expected loss=0.3  P(node) =0.002234637
##     class counts:     3     7
##    probabilities: 0.300 0.700 
## 
## Node number 5148: 214 observations,    complexity param=0.001602136
##   predicted class=N  expected loss=0.1775701  P(node) =0.04782123
##     class counts:   176    38
##    probabilities: 0.822 0.178 
##   left son=10296 (192 obs) right son=10297 (22 obs)
##   Primary splits:
##       PubDate.second < 53.5      to the left,  improve=2.628726, (0 missing)
##       PubDate.hour   < 12.5      to the right, improve=1.833210, (0 missing)
##       NewsDesk.fctr  < 0.5       to the left,  improve=1.525789, (0 missing)
##       WordCount      < 992       to the left,  improve=1.427800, (0 missing)
##       PubDate.date   < 24.5      to the left,  improve=1.098137, (0 missing)
##   Surrogate splits:
##       A.num.chars     < 186       to the left,  agree=0.907, adj=0.091, (0 split)
##       S.num.chars     < 186       to the left,  agree=0.907, adj=0.091, (0 split)
##       S.num.chars.log < 5.231094  to the left,  agree=0.907, adj=0.091, (0 split)
##       A.num.chars.log < 5.231094  to the left,  agree=0.907, adj=0.091, (0 split)
##       PubDate.minute  < 56.5      to the left,  agree=0.902, adj=0.045, (0 split)
## 
## Node number 5149: 73 observations,    complexity param=0.001602136
##   predicted class=N  expected loss=0.369863  P(node) =0.01631285
##     class counts:    46    27
##    probabilities: 0.630 0.370 
##   left son=10298 (50 obs) right son=10299 (23 obs)
##   Primary splits:
##       H.num.words.unq     < 4.5       to the right, improve=3.830876, (0 missing)
##       H.num.words.unq.log < 1.700599  to the right, improve=3.830876, (0 missing)
##       H.num.words         < 4.5       to the right, improve=3.077308, (0 missing)
##       H.num.words.log     < 1.700599  to the right, improve=3.077308, (0 missing)
##       H.num.chars         < 31.5      to the right, improve=2.596628, (0 missing)
##   Surrogate splits:
##       H.num.words.unq.log < 1.700599  to the right, agree=1.000, adj=1.000, (0 split)
##       H.num.words         < 4.5       to the right, agree=0.986, adj=0.957, (0 split)
##       H.num.words.log     < 1.700599  to the right, agree=0.986, adj=0.957, (0 split)
##       H.num.chars         < 44.5      to the right, agree=0.918, adj=0.739, (0 split)
##       H.num.chars.log     < 3.817652  to the right, agree=0.918, adj=0.739, (0 split)
## 
## Node number 5150: 126 observations,    complexity param=0.001602136
##   predicted class=N  expected loss=0.3492063  P(node) =0.02815642
##     class counts:    82    44
##    probabilities: 0.651 0.349 
##   left son=10300 (84 obs) right son=10301 (42 obs)
##   Primary splits:
##       PubDate.second  < 20.5      to the right, improve=2.865079, (0 missing)
##       PubDate.wkday   < 3.5       to the right, improve=2.293651, (0 missing)
##       PubDate.date    < 29.5      to the right, improve=2.083401, (0 missing)
##       PubDate.hour    < 20.5      to the left,  improve=1.953602, (0 missing)
##       A.num.words.unq < 16.5      to the right, improve=1.848966, (0 missing)
##   Surrogate splits:
##       PubDate.minute  < 51.5      to the left,  agree=0.690, adj=0.071, (0 split)
##       WordCount       < 1156.5    to the right, agree=0.683, adj=0.048, (0 split)
##       H.num.words.unq < 7.5       to the left,  agree=0.683, adj=0.048, (0 split)
##       A.num.words     < 20.5      to the left,  agree=0.683, adj=0.048, (0 split)
##       S.num.words     < 20.5      to the left,  agree=0.683, adj=0.048, (0 split)
## 
## Node number 5151: 10 observations
##   predicted class=Y  expected loss=0.2  P(node) =0.002234637
##     class counts:     2     8
##    probabilities: 0.200 0.800 
## 
## Node number 8366: 18 observations
##   predicted class=N  expected loss=0.2222222  P(node) =0.004022346
##     class counts:    14     4
##    probabilities: 0.778 0.222 
## 
## Node number 8367: 8 observations
##   predicted class=Y  expected loss=0.375  P(node) =0.001787709
##     class counts:     3     5
##    probabilities: 0.375 0.625 
## 
## Node number 10296: 192 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.1510417  P(node) =0.04290503
##     class counts:   163    29
##    probabilities: 0.849 0.151 
##   left son=20592 (158 obs) right son=20593 (34 obs)
##   Primary splits:
##       PubDate.date    < 24.5      to the left,  improve=1.6915570, (0 missing)
##       WordCount       < 992       to the left,  improve=1.6515280, (0 missing)
##       A.num.words     < 17.5      to the right, improve=0.9621267, (0 missing)
##       S.num.words     < 17.5      to the right, improve=0.9621267, (0 missing)
##       A.num.words.log < 2.917405  to the right, improve=0.9621267, (0 missing)
##   Surrogate splits:
##       A.num.chars     < 184.5     to the left,  agree=0.839, adj=0.088, (0 split)
##       S.num.chars     < 184.5     to the left,  agree=0.839, adj=0.088, (0 split)
##       S.num.chars.log < 5.223051  to the left,  agree=0.839, adj=0.088, (0 split)
##       A.num.chars.log < 5.223051  to the left,  agree=0.839, adj=0.088, (0 split)
##       H.num.chars     < 90        to the left,  agree=0.828, adj=0.029, (0 split)
## 
## Node number 10297: 22 observations,    complexity param=0.001602136
##   predicted class=N  expected loss=0.4090909  P(node) =0.004916201
##     class counts:    13     9
##    probabilities: 0.591 0.409 
##   left son=20594 (12 obs) right son=20595 (10 obs)
##   Primary splits:
##       PubDate.hour   < 12.5      to the right, improve=3.1030300, (0 missing)
##       PubDate.second < 57.5      to the right, improve=2.0292210, (0 missing)
##       PubDate.minute < 28        to the right, improve=1.3363640, (0 missing)
##       WordCount      < 844       to the left,  improve=0.8181818, (0 missing)
##       PubDate.wkday  < 3.5       to the left,  improve=0.8181818, (0 missing)
##   Surrogate splits:
##       A.num.chars     < 163       to the left,  agree=0.727, adj=0.4, (0 split)
##       S.num.chars     < 163       to the left,  agree=0.727, adj=0.4, (0 split)
##       S.num.chars.log < 5.099848  to the left,  agree=0.727, adj=0.4, (0 split)
##       A.num.chars.log < 5.099848  to the left,  agree=0.727, adj=0.4, (0 split)
##       PubDate.minute  < 25.5      to the right, agree=0.682, adj=0.3, (0 split)
## 
## Node number 10298: 50 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.26  P(node) =0.01117318
##     class counts:    37    13
##    probabilities: 0.740 0.260 
##   left son=20596 (39 obs) right son=20597 (11 obs)
##   Primary splits:
##       H.num.words         < 7.5       to the left,  improve=2.298275, (0 missing)
##       H.num.words.unq     < 7.5       to the left,  improve=2.298275, (0 missing)
##       H.num.words.log     < 2.138333  to the left,  improve=2.298275, (0 missing)
##       H.num.words.unq.log < 2.138333  to the left,  improve=2.298275, (0 missing)
##       WordCount           < 988       to the right, improve=2.084920, (0 missing)
##   Surrogate splits:
##       H.num.words.unq     < 7.5       to the left,  agree=1.00, adj=1.000, (0 split)
##       H.num.words.log     < 2.138333  to the left,  agree=1.00, adj=1.000, (0 split)
##       H.num.words.unq.log < 2.138333  to the left,  agree=1.00, adj=1.000, (0 split)
##       H.num.chars         < 63.5      to the left,  agree=0.84, adj=0.273, (0 split)
##       H.num.chars.log     < 4.166635  to the left,  agree=0.84, adj=0.273, (0 split)
## 
## Node number 10299: 23 observations
##   predicted class=Y  expected loss=0.3913043  P(node) =0.005139665
##     class counts:     9    14
##    probabilities: 0.391 0.609 
## 
## Node number 10300: 84 observations,    complexity param=0.001602136
##   predicted class=N  expected loss=0.2738095  P(node) =0.01877095
##     class counts:    61    23
##    probabilities: 0.726 0.274 
##   left son=20600 (62 obs) right son=20601 (22 obs)
##   Primary splits:
##       PubDate.month                < 10.5      to the left,  improve=3.049923, (0 missing)
##       PubDate.hour                 < 20.5      to the left,  improve=2.963203, (0 missing)
##       NewsDesk.fctrTStyle          < 0.5       to the right, improve=2.738095, (0 missing)
##       SectionName.fctrBusiness Day < 0.5       to the left,  improve=2.460020, (0 missing)
##       SubsectionName.fctrDealbook  < 0.5       to the left,  improve=2.460020, (0 missing)
##   Surrogate splits:
##       PubDate.second < 53.5      to the left,  agree=0.762, adj=0.091, (0 split)
##       H.new          < 0.5       to the left,  agree=0.762, adj=0.091, (0 split)
## 
## Node number 10301: 42 observations,    complexity param=0.001602136
##   predicted class=N  expected loss=0.5  P(node) =0.009385475
##     class counts:    21    21
##    probabilities: 0.500 0.500 
##   left son=20602 (25 obs) right son=20603 (17 obs)
##   Primary splits:
##       A.num.chars     < 128.5     to the right, improve=4.002353, (0 missing)
##       S.num.chars     < 128.5     to the right, improve=4.002353, (0 missing)
##       S.num.chars.log < 4.863673  to the right, improve=4.002353, (0 missing)
##       A.num.chars.log < 4.863673  to the right, improve=4.002353, (0 missing)
##       A.num.words     < 12.5      to the right, improve=3.428571, (0 missing)
##   Surrogate splits:
##       S.num.chars     < 128.5     to the right, agree=1.000, adj=1.000, (0 split)
##       S.num.chars.log < 4.863673  to the right, agree=1.000, adj=1.000, (0 split)
##       A.num.chars.log < 4.863673  to the right, agree=1.000, adj=1.000, (0 split)
##       A.num.words     < 12.5      to the right, agree=0.929, adj=0.824, (0 split)
##       S.num.words     < 12.5      to the right, agree=0.929, adj=0.824, (0 split)
## 
## Node number 20592: 158 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.1202532  P(node) =0.03530726
##     class counts:   139    19
##    probabilities: 0.880 0.120 
##   left son=41184 (116 obs) right son=41185 (42 obs)
##   Primary splits:
##       WordCount      < 992       to the left,  improve=1.5888360, (0 missing)
##       PubDate.minute < 17.5      to the left,  improve=0.8515919, (0 missing)
##       PubDate.hour   < 12.5      to the right, improve=0.7468218, (0 missing)
##       NewsDesk.fctr  < 0.5       to the left,  improve=0.7468218, (0 missing)
##       S.new          < 0.5       to the right, improve=0.6246244, (0 missing)
##   Surrogate splits:
##       H.num.chars     < 27.5      to the right, agree=0.753, adj=0.071, (0 split)
##       H.num.chars.log < 3.34975   to the right, agree=0.753, adj=0.071, (0 split)
## 
## Node number 20593: 34 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.2941176  P(node) =0.007597765
##     class counts:    24    10
##    probabilities: 0.706 0.294 
##   left son=41186 (24 obs) right son=41187 (10 obs)
##   Primary splits:
##       PubDate.wkday   < 2.5       to the right, improve=2.650980, (0 missing)
##       A.num.words     < 14.5      to the right, improve=2.360504, (0 missing)
##       S.num.words     < 14.5      to the right, improve=2.360504, (0 missing)
##       A.num.words.log < 2.740319  to the right, improve=2.360504, (0 missing)
##       S.num.words.log < 2.740319  to the right, improve=2.360504, (0 missing)
##   Surrogate splits:
##       PubDate.hour    < 17.5      to the left,  agree=0.765, adj=0.2, (0 split)
##       A.num.chars     < 81.5      to the right, agree=0.735, adj=0.1, (0 split)
##       S.num.chars     < 81.5      to the right, agree=0.735, adj=0.1, (0 split)
##       H.num.words     < 9.5       to the left,  agree=0.735, adj=0.1, (0 split)
##       H.num.words.unq < 9.5       to the left,  agree=0.735, adj=0.1, (0 split)
## 
## Node number 20594: 12 observations
##   predicted class=N  expected loss=0.1666667  P(node) =0.002681564
##     class counts:    10     2
##    probabilities: 0.833 0.167 
## 
## Node number 20595: 10 observations
##   predicted class=Y  expected loss=0.3  P(node) =0.002234637
##     class counts:     3     7
##    probabilities: 0.300 0.700 
## 
## Node number 20596: 39 observations
##   predicted class=N  expected loss=0.1794872  P(node) =0.008715084
##     class counts:    32     7
##    probabilities: 0.821 0.179 
## 
## Node number 20597: 11 observations
##   predicted class=Y  expected loss=0.4545455  P(node) =0.002458101
##     class counts:     5     6
##    probabilities: 0.455 0.545 
## 
## Node number 20600: 62 observations,    complexity param=0.001602136
##   predicted class=N  expected loss=0.1935484  P(node) =0.01385475
##     class counts:    50    12
##    probabilities: 0.806 0.194 
##   left son=41200 (28 obs) right son=41201 (34 obs)
##   Primary splits:
##       PubDate.month   < 9.5       to the right, improve=2.543914, (0 missing)
##       A.num.chars     < 178.5     to the left,  improve=2.253540, (0 missing)
##       S.num.chars     < 178.5     to the left,  improve=2.253540, (0 missing)
##       S.num.chars.log < 5.190171  to the left,  improve=2.253540, (0 missing)
##       A.num.chars.log < 5.190171  to the left,  improve=2.253540, (0 missing)
##   Surrogate splits:
##       PubDate.date        < 26        to the right, agree=0.677, adj=0.286, (0 split)
##       WordCount           < 1863      to the right, agree=0.661, adj=0.250, (0 split)
##       PubDate.hour        < 12.5      to the left,  agree=0.661, adj=0.250, (0 split)
##       NewsDesk.fctrTStyle < 0.5       to the right, agree=0.645, adj=0.214, (0 split)
##       PubDate.second      < 34.5      to the left,  agree=0.613, adj=0.143, (0 split)
## 
## Node number 20601: 22 observations,    complexity param=0.001602136
##   predicted class=N  expected loss=0.5  P(node) =0.004916201
##     class counts:    11    11
##    probabilities: 0.500 0.500 
##   left son=41202 (9 obs) right son=41203 (13 obs)
##   Primary splits:
##       PubDate.date    < 10.5      to the left,  improve=2.350427, (0 missing)
##       H.num.words     < 6.5       to the left,  improve=1.571429, (0 missing)
##       H.num.words.unq < 6.5       to the left,  improve=1.571429, (0 missing)
##       H.num.words.log < 2.012676  to the left,  improve=1.571429, (0 missing)
##       A.num.words     < 15.5      to the right, improve=1.571429, (0 missing)
##   Surrogate splits:
##       A.num.words     < 15.5      to the right, agree=0.773, adj=0.444, (0 split)
##       S.num.words     < 15.5      to the right, agree=0.773, adj=0.444, (0 split)
##       A.num.words.unq < 15.5      to the right, agree=0.773, adj=0.444, (0 split)
##       S.num.words.unq < 15.5      to the right, agree=0.773, adj=0.444, (0 split)
##       A.num.words.log < 2.802901  to the right, agree=0.773, adj=0.444, (0 split)
## 
## Node number 20602: 25 observations,    complexity param=0.001602136
##   predicted class=N  expected loss=0.32  P(node) =0.005586592
##     class counts:    17     8
##    probabilities: 0.680 0.320 
##   left son=41204 (17 obs) right son=41205 (8 obs)
##   Primary splits:
##       PubDate.month   < 10.5      to the left,  improve=2.188824, (0 missing)
##       PubDate.minute  < 4.5       to the right, improve=1.560556, (0 missing)
##       H.num.chars     < 64.5      to the left,  improve=1.229206, (0 missing)
##       H.num.chars.log < 4.182021  to the left,  improve=1.229206, (0 missing)
##       PubDate.wkday   < 3.5       to the right, improve=1.227222, (0 missing)
##   Surrogate splits:
##       WordCount           < 1166      to the right, agree=0.76, adj=0.250, (0 split)
##       PubDate.hour        < 10        to the right, agree=0.76, adj=0.250, (0 split)
##       H.num.chars         < 71.5      to the left,  agree=0.76, adj=0.250, (0 split)
##       H.num.chars.log     < 4.283563  to the left,  agree=0.76, adj=0.250, (0 split)
##       NewsDesk.fctrTStyle < 0.5       to the left,  agree=0.72, adj=0.125, (0 split)
## 
## Node number 20603: 17 observations
##   predicted class=Y  expected loss=0.2352941  P(node) =0.003798883
##     class counts:     4    13
##    probabilities: 0.235 0.765 
## 
## Node number 41184: 116 observations
##   predicted class=N  expected loss=0.07758621  P(node) =0.02592179
##     class counts:   107     9
##    probabilities: 0.922 0.078 
## 
## Node number 41185: 42 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.2380952  P(node) =0.009385475
##     class counts:    32    10
##    probabilities: 0.762 0.238 
##   left son=82370 (21 obs) right son=82371 (21 obs)
##   Primary splits:
##       WordCount      < 1073.5    to the right, improve=3.047619, (0 missing)
##       PubDate.second < 44.5      to the left,  improve=1.866667, (0 missing)
##       PubDate.wkday  < 2.5       to the left,  improve=1.689708, (0 missing)
##       PubDate.minute < 43.5      to the left,  improve=1.355742, (0 missing)
##       H.num.words    < 3.5       to the left,  improve=1.298701, (0 missing)
##   Surrogate splits:
##       PubDate.second  < 31        to the left,  agree=0.69, adj=0.381, (0 split)
##       A.num.chars     < 155.5     to the left,  agree=0.69, adj=0.381, (0 split)
##       S.num.chars     < 155.5     to the left,  agree=0.69, adj=0.381, (0 split)
##       S.num.chars.log < 5.05301   to the left,  agree=0.69, adj=0.381, (0 split)
##       A.num.chars.log < 5.05301   to the left,  agree=0.69, adj=0.381, (0 split)
## 
## Node number 41186: 24 observations
##   predicted class=N  expected loss=0.1666667  P(node) =0.005363128
##     class counts:    20     4
##    probabilities: 0.833 0.167 
## 
## Node number 41187: 10 observations
##   predicted class=Y  expected loss=0.4  P(node) =0.002234637
##     class counts:     4     6
##    probabilities: 0.400 0.600 
## 
## Node number 41200: 28 observations
##   predicted class=N  expected loss=0.03571429  P(node) =0.006256983
##     class counts:    27     1
##    probabilities: 0.964 0.036 
## 
## Node number 41201: 34 observations,    complexity param=0.001602136
##   predicted class=N  expected loss=0.3235294  P(node) =0.007597765
##     class counts:    23    11
##    probabilities: 0.676 0.324 
##   left son=82402 (27 obs) right son=82403 (7 obs)
##   Primary splits:
##       A.num.chars     < 172       to the left,  improve=2.691877, (0 missing)
##       S.num.chars     < 172       to the left,  improve=2.691877, (0 missing)
##       S.num.chars.log < 5.153024  to the left,  improve=2.691877, (0 missing)
##       A.num.chars.log < 5.153024  to the left,  improve=2.691877, (0 missing)
##       PubDate.date    < 9.5       to the right, improve=1.901584, (0 missing)
##   Surrogate splits:
##       S.num.chars     < 172       to the left,  agree=1.000, adj=1.000, (0 split)
##       S.num.chars.log < 5.153024  to the left,  agree=1.000, adj=1.000, (0 split)
##       A.num.chars.log < 5.153024  to the left,  agree=1.000, adj=1.000, (0 split)
##       A.num.words     < 14.5      to the left,  agree=0.853, adj=0.286, (0 split)
##       S.num.words     < 14.5      to the left,  agree=0.853, adj=0.286, (0 split)
## 
## Node number 41202: 9 observations
##   predicted class=N  expected loss=0.2222222  P(node) =0.002011173
##     class counts:     7     2
##    probabilities: 0.778 0.222 
## 
## Node number 41203: 13 observations
##   predicted class=Y  expected loss=0.3076923  P(node) =0.002905028
##     class counts:     4     9
##    probabilities: 0.308 0.692 
## 
## Node number 41204: 17 observations
##   predicted class=N  expected loss=0.1764706  P(node) =0.003798883
##     class counts:    14     3
##    probabilities: 0.824 0.176 
## 
## Node number 41205: 8 observations
##   predicted class=Y  expected loss=0.375  P(node) =0.001787709
##     class counts:     3     5
##    probabilities: 0.375 0.625 
## 
## Node number 82370: 21 observations
##   predicted class=N  expected loss=0.04761905  P(node) =0.004692737
##     class counts:    20     1
##    probabilities: 0.952 0.048 
## 
## Node number 82371: 21 observations,    complexity param=0.001335113
##   predicted class=N  expected loss=0.4285714  P(node) =0.004692737
##     class counts:    12     9
##    probabilities: 0.571 0.429 
##   left son=164742 (14 obs) right son=164743 (7 obs)
##   Primary splits:
##       PubDate.minute  < 34        to the left,  improve=1.714286, (0 missing)
##       A.num.chars     < 140.5     to the right, improve=1.714286, (0 missing)
##       S.num.chars     < 140.5     to the right, improve=1.714286, (0 missing)
##       S.num.chars.log < 4.952244  to the right, improve=1.714286, (0 missing)
##       A.num.chars.log < 4.952244  to the right, improve=1.714286, (0 missing)
##   Surrogate splits:
##       WordCount                    < 1041      to the left,  agree=0.810, adj=0.429, (0 split)
##       PubDate.date                 < 3.5       to the right, agree=0.810, adj=0.429, (0 split)
##       SectionName.fctrBusiness Day < 0.5       to the left,  agree=0.810, adj=0.429, (0 split)
##       SubsectionName.fctrDealbook  < 0.5       to the left,  agree=0.810, adj=0.429, (0 split)
##       PubDate.hour                 < 18.5      to the left,  agree=0.714, adj=0.143, (0 split)
## 
## Node number 82402: 27 observations
##   predicted class=N  expected loss=0.2222222  P(node) =0.00603352
##     class counts:    21     6
##    probabilities: 0.778 0.222 
## 
## Node number 82403: 7 observations
##   predicted class=Y  expected loss=0.2857143  P(node) =0.001564246
##     class counts:     2     5
##    probabilities: 0.286 0.714 
## 
## Node number 164742: 14 observations
##   predicted class=N  expected loss=0.2857143  P(node) =0.003128492
##     class counts:    10     4
##    probabilities: 0.714 0.286 
## 
## Node number 164743: 7 observations
##   predicted class=Y  expected loss=0.2857143  P(node) =0.001564246
##     class counts:     2     5
##    probabilities: 0.286 0.714 
## 
## n= 4475 
## 
## node), split, n, loss, yval, (yprob)
##       * denotes terminal node
## 
##      1) root 4475 749 N (0.83262570 0.16737430)  
##        2) NewsDesk.fctrOpEd< 0.5 4131 480 N (0.88380537 0.11619463)  
##          4) WordCount< 683.5 3066 152 N (0.95042401 0.04957599)  
##            8) PubDate.hour< 21.5 2987 118 N (0.96049548 0.03950452)  
##             16) NewsDesk.fctrStyles< 0.5 2864  86 N (0.96997207 0.03002793)  
##               32) NewsDesk.fctrScience< 0.5 2806  69 N (0.97540984 0.02459016)  
##                 64) WordCount< 396.5 2102  24 N (0.98858230 0.01141770) *
##                 65) WordCount>=396.5 704  45 N (0.93607955 0.06392045)  
##                  130) A.num.chars>=56.5 694  37 N (0.94668588 0.05331412)  
##                    260) SectionName.fctrBusiness Day>=0.5 250   1 N (0.99600000 0.00400000) *
##                    261) SectionName.fctrBusiness Day< 0.5 444  36 N (0.91891892 0.08108108)  
##                      522) PubDate.hour< 18.5 415  29 N (0.93012048 0.06987952)  
##                       1044) PubDate.date< 23.5 316  15 N (0.95253165 0.04746835) *
##                       1045) PubDate.date>=23.5 99  14 N (0.85858586 0.14141414)  
##                         2090) PubDate.month>=9.5 65   5 N (0.92307692 0.07692308) *
##                         2091) PubDate.month< 9.5 34   9 N (0.73529412 0.26470588)  
##                           4182) A.num.words>=16.5 8   0 N (1.00000000 0.00000000) *
##                           4183) A.num.words< 16.5 26   9 N (0.65384615 0.34615385)  
##                             8366) SectionName.fctrTechnology< 0.5 18   4 N (0.77777778 0.22222222) *
##                             8367) SectionName.fctrTechnology>=0.5 8   3 Y (0.37500000 0.62500000) *
##                      523) PubDate.hour>=18.5 29   7 N (0.75862069 0.24137931) *
##                  131) A.num.chars< 56.5 10   2 Y (0.20000000 0.80000000) *
##               33) NewsDesk.fctrScience>=0.5 58  17 N (0.70689655 0.29310345)  
##                 66) PubDate.hour>=9.5 48  10 N (0.79166667 0.20833333) *
##                 67) PubDate.hour< 9.5 10   3 Y (0.30000000 0.70000000) *
##             17) NewsDesk.fctrStyles>=0.5 123  32 N (0.73983740 0.26016260)  
##               34) SectionName.fctrU.S.< 0.5 61   0 N (1.00000000 0.00000000) *
##               35) SectionName.fctrU.S.>=0.5 62  30 Y (0.48387097 0.51612903)  
##                 70) PubDate.second>=8.5 53  24 N (0.54716981 0.45283019)  
##                  140) S.week>=0.5 16   3 N (0.81250000 0.18750000) *
##                  141) S.week< 0.5 37  16 Y (0.43243243 0.56756757)  
##                    282) PubDate.second< 21.5 9   2 N (0.77777778 0.22222222) *
##                    283) PubDate.second>=21.5 28   9 Y (0.32142857 0.67857143) *
##                 71) PubDate.second< 8.5 9   1 Y (0.11111111 0.88888889) *
##            9) PubDate.hour>=21.5 79  34 N (0.56962025 0.43037975)  
##             18) PubDate.minute>=0.5 45   5 N (0.88888889 0.11111111) *
##             19) PubDate.minute< 0.5 34   5 Y (0.14705882 0.85294118)  
##               38) WordCount< 327.5 7   3 N (0.57142857 0.42857143) *
##               39) WordCount>=327.5 27   1 Y (0.03703704 0.96296296) *
##          5) WordCount>=683.5 1065 328 N (0.69201878 0.30798122)  
##           10) SectionName.fctrHealth< 0.5 994 263 N (0.73541247 0.26458753)  
##             20) PubDate.hour< 21.5 929 220 N (0.76318622 0.23681378)  
##               40) NewsDesk.fctrStyles< 0.5 851 177 N (0.79200940 0.20799060)  
##                 80) PubDate.wkday< 5.5 834 164 N (0.80335731 0.19664269)  
##                  160) SubsectionName.fctrThe Public Editor< 0.5 823 155 N (0.81166464 0.18833536)  
##                    320) PubDate.hour< 7.5 267  28 N (0.89513109 0.10486891)  
##                      640) S.compani< 0.5 252  23 N (0.90873016 0.09126984)  
##                       1280) SectionName.fctr< 0.5 192  11 N (0.94270833 0.05729167) *
##                       1281) SectionName.fctr>=0.5 60  12 N (0.80000000 0.20000000)  
##                         2562) WordCount>=1184.5 31   0 N (1.00000000 0.00000000) *
##                         2563) WordCount< 1184.5 29  12 N (0.58620690 0.41379310)  
##                           5126) PubDate.wkday< 3.5 19   5 N (0.73684211 0.26315789) *
##                           5127) PubDate.wkday>=3.5 10   3 Y (0.30000000 0.70000000) *
##                      641) S.compani>=0.5 15   5 N (0.66666667 0.33333333) *
##                    321) PubDate.hour>=7.5 556 127 N (0.77158273 0.22841727)  
##                      642) A.num.chars>=188.5 94   9 N (0.90425532 0.09574468) *
##                      643) A.num.chars< 188.5 462 118 N (0.74458874 0.25541126)  
##                       1286) SubsectionName.fctrRoom For Debate>=0.5 39   1 N (0.97435897 0.02564103) *
##                       1287) SubsectionName.fctrRoom For Debate< 0.5 423 117 N (0.72340426 0.27659574)  
##                         2574) WordCount< 1152.5 287  65 N (0.77351916 0.22648084)  
##                           5148) PubDate.wkday>=1.5 214  38 N (0.82242991 0.17757009)  
##                            10296) PubDate.second< 53.5 192  29 N (0.84895833 0.15104167)  
##                              20592) PubDate.date< 24.5 158  19 N (0.87974684 0.12025316)  
##                                41184) WordCount< 992 116   9 N (0.92241379 0.07758621) *
##                                41185) WordCount>=992 42  10 N (0.76190476 0.23809524)  
##                                  82370) WordCount>=1073.5 21   1 N (0.95238095 0.04761905) *
##                                  82371) WordCount< 1073.5 21   9 N (0.57142857 0.42857143)  
##                                   164742) PubDate.minute< 34 14   4 N (0.71428571 0.28571429) *
##                                   164743) PubDate.minute>=34 7   2 Y (0.28571429 0.71428571) *
##                              20593) PubDate.date>=24.5 34  10 N (0.70588235 0.29411765)  
##                                41186) PubDate.wkday>=2.5 24   4 N (0.83333333 0.16666667) *
##                                41187) PubDate.wkday< 2.5 10   4 Y (0.40000000 0.60000000) *
##                            10297) PubDate.second>=53.5 22   9 N (0.59090909 0.40909091)  
##                              20594) PubDate.hour>=12.5 12   2 N (0.83333333 0.16666667) *
##                              20595) PubDate.hour< 12.5 10   3 Y (0.30000000 0.70000000) *
##                           5149) PubDate.wkday< 1.5 73  27 N (0.63013699 0.36986301)  
##                            10298) H.num.words.unq>=4.5 50  13 N (0.74000000 0.26000000)  
##                              20596) H.num.words< 7.5 39   7 N (0.82051282 0.17948718) *
##                              20597) H.num.words>=7.5 11   5 Y (0.45454545 0.54545455) *
##                            10299) H.num.words.unq< 4.5 23   9 Y (0.39130435 0.60869565) *
##                         2575) WordCount>=1152.5 136  52 N (0.61764706 0.38235294)  
##                           5150) H.num.chars>=21.5 126  44 N (0.65079365 0.34920635)  
##                            10300) PubDate.second>=20.5 84  23 N (0.72619048 0.27380952)  
##                              20600) PubDate.month< 10.5 62  12 N (0.80645161 0.19354839)  
##                                41200) PubDate.month>=9.5 28   1 N (0.96428571 0.03571429) *
##                                41201) PubDate.month< 9.5 34  11 N (0.67647059 0.32352941)  
##                                  82402) A.num.chars< 172 27   6 N (0.77777778 0.22222222) *
##                                  82403) A.num.chars>=172 7   2 Y (0.28571429 0.71428571) *
##                              20601) PubDate.month>=10.5 22  11 N (0.50000000 0.50000000)  
##                                41202) PubDate.date< 10.5 9   2 N (0.77777778 0.22222222) *
##                                41203) PubDate.date>=10.5 13   4 Y (0.30769231 0.69230769) *
##                            10301) PubDate.second< 20.5 42  21 N (0.50000000 0.50000000)  
##                              20602) A.num.chars>=128.5 25   8 N (0.68000000 0.32000000)  
##                                41204) PubDate.month< 10.5 17   3 N (0.82352941 0.17647059) *
##                                41205) PubDate.month>=10.5 8   3 Y (0.37500000 0.62500000) *
##                              20603) A.num.chars< 128.5 17   4 Y (0.23529412 0.76470588) *
##                           5151) H.num.chars< 21.5 10   2 Y (0.20000000 0.80000000) *
##                  161) SubsectionName.fctrThe Public Editor>=0.5 11   2 Y (0.18181818 0.81818182) *
##                 81) PubDate.wkday>=5.5 17   4 Y (0.23529412 0.76470588) *
##               41) NewsDesk.fctrStyles>=0.5 78  35 Y (0.44871795 0.55128205)  
##                 82) SectionName.fctr>=0.5 13   0 N (1.00000000 0.00000000) *
##                 83) SectionName.fctr< 0.5 65  22 Y (0.33846154 0.66153846)  
##                  166) A.num.chars< 174.5 40  18 Y (0.45000000 0.55000000)  
##                    332) PubDate.date>=17.5 17   6 N (0.64705882 0.35294118) *
##                    333) PubDate.date< 17.5 23   7 Y (0.30434783 0.69565217)  
##                      666) PubDate.minute>=38 7   2 N (0.71428571 0.28571429) *
##                      667) PubDate.minute< 38 16   2 Y (0.12500000 0.87500000) *
##                  167) A.num.chars>=174.5 25   4 Y (0.16000000 0.84000000) *
##             21) PubDate.hour>=21.5 65  22 Y (0.33846154 0.66153846)  
##               42) A.num.chars>=92 25   8 N (0.68000000 0.32000000)  
##                 84) PubDate.date>=11 18   4 N (0.77777778 0.22222222) *
##                 85) PubDate.date< 11 7   3 Y (0.42857143 0.57142857) *
##               43) A.num.chars< 92 40   5 Y (0.12500000 0.87500000) *
##           11) SectionName.fctrHealth>=0.5 71   6 Y (0.08450704 0.91549296) *
##        3) NewsDesk.fctrOpEd>=0.5 344  75 Y (0.21802326 0.78197674)  
##          6) A.num.chars>=169.5 8   1 N (0.87500000 0.12500000) *
##          7) A.num.chars< 169.5 336  68 Y (0.20238095 0.79761905)  
##           14) WordCount< 107 34  14 Y (0.41176471 0.58823529)  
##             28) PubDate.wkday>=2.5 23  10 N (0.56521739 0.43478261)  
##               56) PubDate.hour>=15.5 11   1 N (0.90909091 0.09090909) *
##               57) PubDate.hour< 15.5 12   3 Y (0.25000000 0.75000000) *
##             29) PubDate.wkday< 2.5 11   1 Y (0.09090909 0.90909091) *
##           15) WordCount>=107 302  54 Y (0.17880795 0.82119205)  
##             30) A.num.chars>=38.5 254  52 Y (0.20472441 0.79527559)  
##               60) PubDate.wkday>=3.5 105  28 Y (0.26666667 0.73333333)  
##                120) WordCount< 483 48  18 Y (0.37500000 0.62500000)  
##                  240) A.num.chars>=56 41  18 Y (0.43902439 0.56097561)  
##                    480) A.num.chars< 70.5 10   2 N (0.80000000 0.20000000) *
##                    481) A.num.chars>=70.5 31  10 Y (0.32258065 0.67741935)  
##                      962) H.num.chars< 39 12   5 N (0.58333333 0.41666667) *
##                      963) H.num.chars>=39 19   3 Y (0.15789474 0.84210526) *
##                  241) A.num.chars< 56 7   0 Y (0.00000000 1.00000000) *
##                121) WordCount>=483 57  10 Y (0.17543860 0.82456140) *
##               61) PubDate.wkday< 3.5 149  24 Y (0.16107383 0.83892617) *
##             31) A.num.chars< 38.5 48   2 Y (0.04166667 0.95833333) *
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-16.png) 

```
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 3726  749
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                               0
## 2            Y                                               0
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                            3726
## 2                                             749
##           Reference
## Prediction    N    Y
##          N 3269   78
##          Y  457  671
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            3269
## 2            Y                                              78
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                             457
## 2                                             671
##           Reference
## Prediction    N    Y
##          N 3413  105
##          Y  313  644
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            3413
## 2            Y                                             105
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                             313
## 2                                             644
##           Reference
## Prediction    N    Y
##          N 3565  151
##          Y  161  598
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            3565
## 2            Y                                             151
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                             161
## 2                                             598
##           Reference
## Prediction    N    Y
##          N 3586  162
##          Y  140  587
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            3586
## 2            Y                                             162
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                             140
## 2                                             587
##           Reference
## Prediction    N    Y
##          N 3597  170
##          Y  129  579
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            3597
## 2            Y                                             170
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                             129
## 2                                             579
##           Reference
## Prediction    N    Y
##          N 3609  186
##          Y  117  563
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            3609
## 2            Y                                             186
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                             117
## 2                                             563
##           Reference
## Prediction    N    Y
##          N 3637  238
##          Y   89  511
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            3637
## 2            Y                                             238
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                              89
## 2                                             511
##           Reference
## Prediction    N    Y
##          N 3665  320
##          Y   61  429
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            3665
## 2            Y                                             320
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                              61
## 2                                             429
##           Reference
## Prediction    N    Y
##          N 3716  595
##          Y   10  154
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            3716
## 2            Y                                             595
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                              10
## 2                                             154
##           Reference
## Prediction    N    Y
##          N 3726  742
##          Y    0    7
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            3726
## 2            Y                                             742
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                               0
## 2                                               7
##    threshold    f.score
## 1        0.0 0.28675345
## 2        0.1 0.71497070
## 3        0.2 0.75498242
## 4        0.3 0.79310345
## 5        0.4 0.79539295
## 6        0.5 0.79478380
## 7        0.6 0.78796361
## 8        0.7 0.75759822
## 9        0.8 0.69249395
## 10       0.9 0.33734940
## 11       1.0 0.01851852
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-17.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            3586
## 2            Y                                             162
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                             140
## 2                                             587
##           Reference
## Prediction    N    Y
##          N 3586  162
##          Y  140  587
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            3586
## 2            Y                                             162
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                             140
## 2                                             587
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] Y N
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] Y N
## Levels: N Y
##          Prediction
## Reference    N    Y
##         N 3586  140
##         Y  162  587
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.325140e-01   7.549970e-01   9.247659e-01   9.396915e-01   8.326257e-01 
## AccuracyPValue  McnemarPValue 
##   2.322542e-88   2.268880e-01 
## [1] "    calling mypredict_mdl for OOB:"
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-18.png) 

```
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 1713  344
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                               0
## 2            Y                                               0
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                            1713
## 2                                             344
##           Reference
## Prediction    N    Y
##          N 1458   77
##          Y  255  267
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            1458
## 2            Y                                              77
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                             255
## 2                                             267
##           Reference
## Prediction    N    Y
##          N 1520   93
##          Y  193  251
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            1520
## 2            Y                                              93
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                             193
## 2                                             251
##           Reference
## Prediction    N    Y
##          N 1583  116
##          Y  130  228
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            1583
## 2            Y                                             116
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                             130
## 2                                             228
##           Reference
## Prediction    N    Y
##          N 1594  119
##          Y  119  225
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            1594
## 2            Y                                             119
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                             119
## 2                                             225
##           Reference
## Prediction    N    Y
##          N 1597  124
##          Y  116  220
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            1597
## 2            Y                                             124
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                             116
## 2                                             220
##           Reference
## Prediction    N    Y
##          N 1608  125
##          Y  105  219
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            1608
## 2            Y                                             125
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                             105
## 2                                             219
##           Reference
## Prediction    N    Y
##          N 1629  137
##          Y   84  207
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            1629
## 2            Y                                             137
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                              84
## 2                                             207
##           Reference
## Prediction    N    Y
##          N 1666  153
##          Y   47  191
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            1666
## 2            Y                                             153
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                              47
## 2                                             191
##           Reference
## Prediction    N    Y
##          N 1703  277
##          Y   10   67
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            1703
## 2            Y                                             277
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                              10
## 2                                              67
##           Reference
## Prediction    N    Y
##          N 1712  340
##          Y    1    4
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            1712
## 2            Y                                             340
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                               1
## 2                                               4
##    threshold    f.score
## 1        0.0 0.28654727
## 2        0.1 0.61662818
## 3        0.2 0.63705584
## 4        0.3 0.64957265
## 5        0.4 0.65406977
## 6        0.5 0.64705882
## 7        0.6 0.65568862
## 8        0.7 0.65196850
## 9        0.8 0.65635739
## 10       0.9 0.31828979
## 11       1.0 0.02292264
```

```
## [1] "Classifier Probability Threshold: 0.8000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            1666
## 2            Y                                             153
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                              47
## 2                                             191
##           Reference
## Prediction    N    Y
##          N 1666  153
##          Y   47  191
##   Popular.fctr Popular.fctr.predict.Conditional.X.cp.0.rpart.N
## 1            N                                            1666
## 2            Y                                             153
##   Popular.fctr.predict.Conditional.X.cp.0.rpart.Y
## 1                                              47
## 2                                             191
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] N Y
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] N Y
## Levels: N Y
##          Prediction
## Reference    N    Y
##         N 1666   47
##         Y  153  191
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.027710e-01   6.019082e-01   8.891448e-01   9.152364e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   6.489691e-20   1.131031e-13 
##                   model_id model_method
## 1 Conditional.X.cp.0.rpart        rpart
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        feats
## 1 WordCount, PubDate.hour, PubDate.month, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, A.year, S.compani, A.compani, S.time, A.time, S.will, A.will, S.york, A.york, S.new, A.new, H.new, S.week, A.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, H.num.chars.log, A.num.chars, S.num.chars, H.num.words, H.num.words.unq, H.num.words.log, S.num.chars.log, A.num.chars.log, A.num.words, S.num.words, H.num.words.unq.log, A.num.words.unq, S.num.words.unq, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                      1.525                 0.897
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1   0.9468583                    0.4        0.795393         0.932514
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1             0.9247659             0.9396915      0.754997   0.8910062
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.8       0.6563574         0.902771
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8891448             0.9152364     0.6019082
## [1] "fitting model: Conditional.X.rf"
## [1] "    indep_vars: WordCount, PubDate.hour, PubDate.month, .rnorm, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, A.year, S.compani, A.compani, S.time, A.time, S.will, A.will, S.york, A.york, S.new, A.new, H.new, S.week, A.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, H.num.chars.log, A.num.chars, S.num.chars, H.num.words, H.num.words.unq, H.num.words.log, S.num.chars.log, A.num.chars.log, A.num.words, S.num.words, H.num.words.unq.log, A.num.words.unq, S.num.words.unq, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log"
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-19.png) 

```
## + : mtry= 2 
## - : mtry= 2 
## + : mtry=39 
## - : mtry=39 
## + : mtry=76 
## - : mtry=76 
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 39 on full training set
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-20.png) ![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-21.png) 

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
## importance        76   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y               4475   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            76   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-22.png) 

```
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 3726  749
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                       0
## 2            Y                                       0
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                    3726
## 2                                     749
##           Reference
## Prediction    N    Y
##          N 3341    0
##          Y  385  749
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    3341
## 2            Y                                       0
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                     385
## 2                                     749
##           Reference
## Prediction    N    Y
##          N 3607    0
##          Y  119  749
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    3607
## 2            Y                                       0
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                     119
## 2                                     749
##           Reference
## Prediction    N    Y
##          N 3690    0
##          Y   36  749
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    3690
## 2            Y                                       0
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                      36
## 2                                     749
##           Reference
## Prediction    N    Y
##          N 3726    0
##          Y    0  749
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    3726
## 2            Y                                       0
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                       0
## 2                                     749
##           Reference
## Prediction    N    Y
##          N 3726    0
##          Y    0  749
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    3726
## 2            Y                                       0
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                       0
## 2                                     749
##           Reference
## Prediction    N    Y
##          N 3726    2
##          Y    0  747
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    3726
## 2            Y                                       2
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                       0
## 2                                     747
##           Reference
## Prediction    N    Y
##          N 3726   94
##          Y    0  655
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    3726
## 2            Y                                      94
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                       0
## 2                                     655
##           Reference
## Prediction    N    Y
##          N 3726  247
##          Y    0  502
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    3726
## 2            Y                                     247
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                       0
## 2                                     502
##           Reference
## Prediction    N    Y
##          N 3726  409
##          Y    0  340
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    3726
## 2            Y                                     409
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                       0
## 2                                     340
##           Reference
## Prediction    N    Y
##          N 3726  747
##          Y    0    2
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    3726
## 2            Y                                     747
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                       0
## 2                                       2
##    threshold     f.score
## 1        0.0 0.286753446
## 2        0.1 0.795539033
## 3        0.2 0.926406926
## 4        0.3 0.976531943
## 5        0.4 1.000000000
## 6        0.5 1.000000000
## 7        0.6 0.998663102
## 8        0.7 0.933048433
## 9        0.8 0.802557954
## 10       0.9 0.624426079
## 11       1.0 0.005326232
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-23.png) 

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    3726
## 2            Y                                      NA
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                      NA
## 2                                     749
##           Reference
## Prediction    N    Y
##          N 3726    0
##          Y    0  749
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    3726
## 2            Y                                       0
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                       0
## 2                                     749
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] Y N
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] Y N
## Levels: N Y
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

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-24.png) 

```
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 1713  344
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                       0
## 2            Y                                       0
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                    1713
## 2                                     344
##           Reference
## Prediction    N    Y
##          N 1271   24
##          Y  442  320
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    1271
## 2            Y                                      24
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                     442
## 2                                     320
##           Reference
## Prediction    N    Y
##          N 1449   50
##          Y  264  294
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    1449
## 2            Y                                      50
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                     264
## 2                                     294
##           Reference
## Prediction    N    Y
##          N 1564   72
##          Y  149  272
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    1564
## 2            Y                                      72
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                     149
## 2                                     272
##           Reference
## Prediction    N    Y
##          N 1621   99
##          Y   92  245
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    1621
## 2            Y                                      99
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                      92
## 2                                     245
##           Reference
## Prediction    N    Y
##          N 1653  120
##          Y   60  224
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    1653
## 2            Y                                     120
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                      60
## 2                                     224
##           Reference
## Prediction    N    Y
##          N 1667  149
##          Y   46  195
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    1667
## 2            Y                                     149
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                      46
## 2                                     195
##           Reference
## Prediction    N    Y
##          N 1682  181
##          Y   31  163
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    1682
## 2            Y                                     181
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                      31
## 2                                     163
##           Reference
## Prediction    N    Y
##          N 1697  225
##          Y   16  119
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    1697
## 2            Y                                     225
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                      16
## 2                                     119
##           Reference
## Prediction    N    Y
##          N 1707  290
##          Y    6   54
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    1707
## 2            Y                                     290
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                       6
## 2                                      54
##           Reference
## Prediction    N    Y
##          N 1713  344
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    1713
## 2            Y                                     344
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                       0
## 2                                       0
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.5786618
## 3        0.2 0.6518847
## 4        0.3 0.7111111
## 5        0.4 0.7195301
## 6        0.5 0.7133758
## 7        0.6 0.6666667
## 8        0.7 0.6059480
## 9        0.8 0.4968685
## 10       0.9 0.2673267
## 11       1.0 0.0000000
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-25.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    1621
## 2            Y                                      99
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                      92
## 2                                     245
##           Reference
## Prediction    N    Y
##          N 1621   99
##          Y   92  245
##   Popular.fctr Popular.fctr.predict.Conditional.X.rf.N
## 1            N                                    1621
## 2            Y                                      99
##   Popular.fctr.predict.Conditional.X.rf.Y
## 1                                      92
## 2                                     245
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] N Y
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] N Y
## Levels: N Y
##          Prediction
## Reference    N    Y
##         N 1621   92
##         Y   99  245
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.071463e-01   6.639007e-01   8.937741e-01   9.193445e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   1.834204e-22   6.641833e-01 
##           model_id model_method
## 1 Conditional.X.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                feats
## 1 WordCount, PubDate.hour, PubDate.month, .rnorm, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, A.year, S.compani, A.compani, S.time, A.time, S.will, A.will, S.york, A.york, S.new, A.new, H.new, S.week, A.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, H.num.chars.log, A.num.chars, S.num.chars, H.num.words, H.num.words.unq, H.num.words.log, S.num.chars.log, A.num.chars.log, A.num.words, S.num.words, H.num.words.unq.log, A.num.words.unq, S.num.words.unq, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                    105.229                25.835
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1        0.9086034
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.999176                     1     0.6478558   0.9287358
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7195301        0.9071463
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1             0.8937741             0.9193445     0.6639007
## [1] "fitting model: Conditional.X.no.rnorm.rf"
## [1] "    indep_vars: WordCount, PubDate.hour, PubDate.month, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, A.year, S.compani, A.compani, S.time, A.time, S.will, A.will, S.york, A.york, S.new, A.new, H.new, S.week, A.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, H.num.chars.log, A.num.chars, S.num.chars, H.num.words, H.num.words.unq, H.num.words.log, S.num.chars.log, A.num.chars.log, A.num.words, S.num.words, H.num.words.unq.log, A.num.words.unq, S.num.words.unq, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log"
## + : mtry= 2 
## - : mtry= 2 
## + : mtry=38 
## - : mtry=38 
## + : mtry=75 
## - : mtry=75 
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 38 on full training set
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-26.png) ![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-27.png) 

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
## importance        75   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y               4475   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            75   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-28.png) 

```
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 3726  749
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                                0
## 2            Y                                                0
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                             3726
## 2                                              749
##           Reference
## Prediction    N    Y
##          N 3346    0
##          Y  380  749
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             3346
## 2            Y                                                0
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                              380
## 2                                              749
##           Reference
## Prediction    N    Y
##          N 3605    0
##          Y  121  749
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             3605
## 2            Y                                                0
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                              121
## 2                                              749
##           Reference
## Prediction    N    Y
##          N 3689    0
##          Y   37  749
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             3689
## 2            Y                                                0
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                               37
## 2                                              749
##           Reference
## Prediction    N    Y
##          N 3725    0
##          Y    1  749
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             3725
## 2            Y                                                0
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                                1
## 2                                              749
##           Reference
## Prediction    N    Y
##          N 3726    0
##          Y    0  749
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             3726
## 2            Y                                                0
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                                0
## 2                                              749
##           Reference
## Prediction    N    Y
##          N 3726    0
##          Y    0  749
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             3726
## 2            Y                                                0
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                                0
## 2                                              749
##           Reference
## Prediction    N    Y
##          N 3726   96
##          Y    0  653
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             3726
## 2            Y                                               96
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                                0
## 2                                              653
##           Reference
## Prediction    N    Y
##          N 3726  252
##          Y    0  497
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             3726
## 2            Y                                              252
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                                0
## 2                                              497
##           Reference
## Prediction    N    Y
##          N 3726  408
##          Y    0  341
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             3726
## 2            Y                                              408
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                                0
## 2                                              341
##           Reference
## Prediction    N    Y
##          N 3726  748
##          Y    0    1
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             3726
## 2            Y                                              748
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                                0
## 2                                                1
##    threshold     f.score
## 1        0.0 0.286753446
## 2        0.1 0.797657082
## 3        0.2 0.925262508
## 4        0.3 0.975895765
## 5        0.4 0.999332889
## 6        0.5 1.000000000
## 7        0.6 1.000000000
## 8        0.7 0.931526391
## 9        0.8 0.797752809
## 10       0.9 0.625688073
## 11       1.0 0.002666667
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-29.png) 

```
## [1] "Classifier Probability Threshold: 0.6000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             3726
## 2            Y                                               NA
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                               NA
## 2                                              749
##           Reference
## Prediction    N    Y
##          N 3726    0
##          Y    0  749
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             3726
## 2            Y                                                0
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                                0
## 2                                              749
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] Y N
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] Y N
## Levels: N Y
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

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-30.png) 

```
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 1713  344
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                                0
## 2            Y                                                0
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                             1713
## 2                                              344
##           Reference
## Prediction    N    Y
##          N 1277   25
##          Y  436  319
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             1277
## 2            Y                                               25
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                              436
## 2                                              319
##           Reference
## Prediction    N    Y
##          N 1454   50
##          Y  259  294
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             1454
## 2            Y                                               50
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                              259
## 2                                              294
##           Reference
## Prediction    N    Y
##          N 1569   71
##          Y  144  273
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             1569
## 2            Y                                               71
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                              144
## 2                                              273
##           Reference
## Prediction    N    Y
##          N 1621   98
##          Y   92  246
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             1621
## 2            Y                                               98
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                               92
## 2                                              246
##           Reference
## Prediction    N    Y
##          N 1653  121
##          Y   60  223
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             1653
## 2            Y                                              121
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                               60
## 2                                              223
##           Reference
## Prediction    N    Y
##          N 1668  150
##          Y   45  194
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             1668
## 2            Y                                              150
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                               45
## 2                                              194
##           Reference
## Prediction    N    Y
##          N 1679  180
##          Y   34  164
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             1679
## 2            Y                                              180
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                               34
## 2                                              164
##           Reference
## Prediction    N    Y
##          N 1698  224
##          Y   15  120
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             1698
## 2            Y                                              224
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                               15
## 2                                              120
##           Reference
## Prediction    N    Y
##          N 1707  282
##          Y    6   62
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             1707
## 2            Y                                              282
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                                6
## 2                                               62
##           Reference
## Prediction    N    Y
##          N 1713  344
##          Y    0    0
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             1713
## 2            Y                                              344
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                                0
## 2                                                0
##    threshold   f.score
## 1        0.0 0.2865473
## 2        0.1 0.5805278
## 3        0.2 0.6555184
## 4        0.3 0.7174770
## 5        0.4 0.7214076
## 6        0.5 0.7113238
## 7        0.6 0.6655232
## 8        0.7 0.6051661
## 9        0.8 0.5010438
## 10       0.9 0.3009709
## 11       1.0 0.0000000
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_1-31.png) 

```
## [1] "Classifier Probability Threshold: 0.4000 to maximize f.score.OOB"
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             1621
## 2            Y                                               98
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                               92
## 2                                              246
##           Reference
## Prediction    N    Y
##          N 1621   98
##          Y   92  246
##   Popular.fctr Popular.fctr.predict.Conditional.X.no.rnorm.rf.N
## 1            N                                             1621
## 2            Y                                               98
##   Popular.fctr.predict.Conditional.X.no.rnorm.rf.Y
## 1                                               92
## 2                                              246
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] N Y
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] N Y
## Levels: N Y
##          Prediction
## Reference    N    Y
##         N 1621   92
##         Y   98  246
##       Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull 
##   9.076325e-01   6.660515e-01   8.942890e-01   9.198004e-01   8.327662e-01 
## AccuracyPValue  McnemarPValue 
##   9.289451e-23   7.168005e-01 
##                    model_id model_method
## 1 Conditional.X.no.rnorm.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        feats
## 1 WordCount, PubDate.hour, PubDate.month, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, A.year, S.compani, A.compani, S.time, A.time, S.will, A.will, S.york, A.york, S.new, A.new, H.new, S.week, A.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, H.num.chars.log, A.num.chars, S.num.chars, H.num.words, H.num.words.unq, H.num.words.log, S.num.chars.log, A.num.chars.log, A.num.words, S.num.words, H.num.words.unq.log, A.num.words.unq, S.num.words.unq, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                    100.692                25.241
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.6               1         0.907486
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.999176                     1     0.6429373   0.9283871
##   opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                    0.4       0.7214076        0.9076325
##   max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1              0.894289             0.9198004     0.6660515
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
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 feats
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .rnorm
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .rnorm
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           WordCount
## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           WordCount
## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           WordCount
## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           WordCount
## 7                                                                                                                                                                                                                                WordCount, WordCount:S.year, WordCount:S.compani, WordCount:S.time, WordCount:S.will, WordCount:S.york, WordCount:S.new, WordCount:S.week, WordCount:H.num.chars, WordCount:A.num.chars, WordCount:H.num.words, WordCount:S.num.chars.log, WordCount:A.num.words, WordCount:H.num.chars.log, WordCount:S.num.words, WordCount:A.num.words.log, WordCount:S.num.chars
## 8                                                                                                                                                                                                                     WordCount, PubDate.hour, PubDate.month, .rnorm, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, S.compani, S.time, S.will, S.york, S.new, H.new, S.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, A.num.chars, H.num.words, H.num.words.log, S.num.chars.log, A.num.words, A.num.words.unq, A.num.words.log, A.num.words.unq.log
## 9  WordCount, PubDate.hour, PubDate.month, .rnorm, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, A.year, S.compani, A.compani, S.time, A.time, S.will, A.will, S.york, A.york, S.new, A.new, H.new, S.week, A.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, H.num.chars.log, A.num.chars, S.num.chars, H.num.words, H.num.words.unq, H.num.words.log, S.num.chars.log, A.num.chars.log, A.num.words, S.num.words, H.num.words.unq.log, A.num.words.unq, S.num.words.unq, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 10         WordCount, PubDate.hour, PubDate.month, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, A.year, S.compani, A.compani, S.time, A.time, S.will, A.will, S.york, A.york, S.new, A.new, H.new, S.week, A.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, H.num.chars.log, A.num.chars, S.num.chars, H.num.words, H.num.words.unq, H.num.words.log, S.num.chars.log, A.num.chars.log, A.num.words, S.num.words, H.num.words.unq.log, A.num.words.unq, S.num.words.unq, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 11         WordCount, PubDate.hour, PubDate.month, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, A.year, S.compani, A.compani, S.time, A.time, S.will, A.will, S.york, A.york, S.new, A.new, H.new, S.week, A.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, H.num.chars.log, A.num.chars, S.num.chars, H.num.words, H.num.words.unq, H.num.words.log, S.num.chars.log, A.num.chars.log, A.num.words, S.num.words, H.num.words.unq.log, A.num.words.unq, S.num.words.unq, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 12 WordCount, PubDate.hour, PubDate.month, .rnorm, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, A.year, S.compani, A.compani, S.time, A.time, S.will, A.will, S.york, A.york, S.new, A.new, H.new, S.week, A.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, H.num.chars.log, A.num.chars, S.num.chars, H.num.words, H.num.words.unq, H.num.words.log, S.num.chars.log, A.num.chars.log, A.num.words, S.num.words, H.num.words.unq.log, A.num.words.unq, S.num.words.unq, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 13         WordCount, PubDate.hour, PubDate.month, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, A.year, S.compani, A.compani, S.time, A.time, S.will, A.will, S.york, A.york, S.new, A.new, H.new, S.week, A.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, H.num.chars.log, A.num.chars, S.num.chars, H.num.words, H.num.words.unq, H.num.words.log, S.num.chars.log, A.num.chars.log, A.num.words, S.num.words, H.num.words.unq.log, A.num.words.unq, S.num.words.unq, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##    max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1                0                      0.460                 0.003
## 2                0                      0.336                 0.002
## 3                0                      0.753                 0.063
## 4                0                      0.599                 0.054
## 5                3                      1.265                 0.063
## 6                1                      1.126                 0.062
## 7                1                      1.834                 0.278
## 8                1                      4.791                 1.252
## 9                1                      6.841                 1.746
## 10               3                      5.220                 0.912
## 11               0                      1.525                 0.897
## 12               3                    105.229                25.835
## 13               3                    100.692                25.241
##    max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1    0.5000000                    0.5       0.0000000        0.8326257
## 2    0.4975446                    0.1       0.2867534        0.1673743
## 3    0.5000000                    0.5       0.0000000        0.8326257
## 4    0.7072558                    0.2       0.4562462        0.7986592
## 5    0.5000000                    0.5       0.0000000        0.8192183
## 6    0.7350785                    0.2       0.4220705        0.8225702
## 7    0.8062116                    0.2       0.5124688        0.8359773
## 8    0.9372948                    0.3       0.7191737        0.9003357
## 9    0.9381279                    0.3       0.7198970        0.8992181
## 10   0.8300780                    0.7       0.5738832        0.8893849
## 11   0.9468583                    0.4       0.7953930        0.9325140
## 12   1.0000000                    0.5       1.0000000        0.9086034
## 13   1.0000000                    0.6       1.0000000        0.9074860
##    max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit max.auc.OOB
## 1              0.8213602             0.8434553    0.00000000   0.5000000
## 2              0.1565447             0.1786398    0.00000000   0.4821958
## 3              0.8213602             0.8434553    0.00000000   0.5000000
## 4              0.7866053             0.8103220    0.33409619   0.6552144
## 5              0.8213602             0.8434553    0.07367080   0.5000000
## 6              0.7603993             0.7851652    0.01764536   0.7378291
## 7              0.8138022             0.8362745    0.16186063   0.8156980
## 8              0.8937373             0.9113196    0.61968400   0.9326067
## 9              0.8937373             0.9113196    0.61736565   0.9327051
## 10             0.8795973             0.8982154    0.52225926   0.8225030
## 11             0.9247659             0.9396915    0.75499696   0.8910062
## 12             0.9991760             1.0000000    0.64785575   0.9287358
## 13             0.9991760             1.0000000    0.64293726   0.9283871
##    opt.prob.threshold.OOB max.f.score.OOB max.Accuracy.OOB
## 1                     0.5       0.0000000        0.8327662
## 2                     0.1       0.2865473        0.1672338
## 3                     0.5       0.0000000        0.8327662
## 4                     0.1       0.4058625        0.7438017
## 5                     0.5       0.0000000        0.8327662
## 6                     0.2       0.4044944        0.7681089
## 7                     0.2       0.5000000        0.8142927
## 8                     0.3       0.7136929        0.8993680
## 9                     0.3       0.7081604        0.8974234
## 10                    0.7       0.6014493        0.8930481
## 11                    0.8       0.6563574        0.9027710
## 12                    0.4       0.7195301        0.9071463
## 13                    0.4       0.7214076        0.9076325
##    max.AccuracyLower.OOB max.AccuracyUpper.OOB max.Kappa.OOB
## 1              0.8159247             0.8486533     0.0000000
## 2              0.1513467             0.1840753     0.0000000
## 3              0.8159247             0.8486533     0.0000000
## 4              0.7243549             0.7625511     0.2528893
## 5              0.8159247             0.8486533     0.0000000
## 6              0.7492527             0.7861979     0.2640573
## 7              0.7968011             0.8308835     0.3873528
## 8              0.8855497             0.9120357     0.6528225
## 9              0.8834974             0.9102047     0.6461137
## 10             0.8788850             0.9060795     0.5439760
## 11             0.8891448             0.9152364     0.6019082
## 12             0.8937741             0.9193445     0.6639007
## 13             0.8942890             0.9198004     0.6660515
##    max.AccuracySD.fit max.KappaSD.fit min.aic.fit
## 1                  NA              NA          NA
## 2                  NA              NA          NA
## 3                  NA              NA          NA
## 4                  NA              NA          NA
## 5         0.003061652     0.025245020          NA
## 6         0.003663409     0.012343629    3778.810
## 7         0.003949821     0.031367400    3582.118
## 8         0.002679281     0.006873523    2206.319
## 9         0.002140938     0.001775530    2212.301
## 10        0.002458760     0.005834195          NA
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
## elapsed10  fit.models                6                1 180.884
## elapsed11  fit.models                6                2 433.449
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
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 feats
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .rnorm
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              .rnorm
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           WordCount
## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           WordCount
## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           WordCount
## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           WordCount
## 7                                                                                                                                                                                                                                WordCount, WordCount:S.year, WordCount:S.compani, WordCount:S.time, WordCount:S.will, WordCount:S.york, WordCount:S.new, WordCount:S.week, WordCount:H.num.chars, WordCount:A.num.chars, WordCount:H.num.words, WordCount:S.num.chars.log, WordCount:A.num.words, WordCount:H.num.chars.log, WordCount:S.num.words, WordCount:A.num.words.log, WordCount:S.num.chars
## 8                                                                                                                                                                                                                     WordCount, PubDate.hour, PubDate.month, .rnorm, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, S.compani, S.time, S.will, S.york, S.new, H.new, S.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, A.num.chars, H.num.words, H.num.words.log, S.num.chars.log, A.num.words, A.num.words.unq, A.num.words.log, A.num.words.unq.log
## 9  WordCount, PubDate.hour, PubDate.month, .rnorm, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, A.year, S.compani, A.compani, S.time, A.time, S.will, A.will, S.york, A.york, S.new, A.new, H.new, S.week, A.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, H.num.chars.log, A.num.chars, S.num.chars, H.num.words, H.num.words.unq, H.num.words.log, S.num.chars.log, A.num.chars.log, A.num.words, S.num.words, H.num.words.unq.log, A.num.words.unq, S.num.words.unq, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 10         WordCount, PubDate.hour, PubDate.month, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, A.year, S.compani, A.compani, S.time, A.time, S.will, A.will, S.york, A.york, S.new, A.new, H.new, S.week, A.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, H.num.chars.log, A.num.chars, S.num.chars, H.num.words, H.num.words.unq, H.num.words.log, S.num.chars.log, A.num.chars.log, A.num.words, S.num.words, H.num.words.unq.log, A.num.words.unq, S.num.words.unq, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 11         WordCount, PubDate.hour, PubDate.month, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, A.year, S.compani, A.compani, S.time, A.time, S.will, A.will, S.york, A.york, S.new, A.new, H.new, S.week, A.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, H.num.chars.log, A.num.chars, S.num.chars, H.num.words, H.num.words.unq, H.num.words.log, S.num.chars.log, A.num.chars.log, A.num.words, S.num.words, H.num.words.unq.log, A.num.words.unq, S.num.words.unq, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 12 WordCount, PubDate.hour, PubDate.month, .rnorm, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, A.year, S.compani, A.compani, S.time, A.time, S.will, A.will, S.york, A.york, S.new, A.new, H.new, S.week, A.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, H.num.chars.log, A.num.chars, S.num.chars, H.num.words, H.num.words.unq, H.num.words.log, S.num.chars.log, A.num.chars.log, A.num.words, S.num.words, H.num.words.unq.log, A.num.words.unq, S.num.words.unq, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
## 13         WordCount, PubDate.hour, PubDate.month, PubDate.date, PubDate.second, PubDate.minute, PubDate.wkday, S.year, A.year, S.compani, A.compani, S.time, A.time, S.will, A.will, S.york, A.york, S.new, A.new, H.new, S.week, A.week, SectionName.fctr, NewsDesk.fctr, SubsectionName.fctr, H.num.chars, H.num.chars.log, A.num.chars, S.num.chars, H.num.words, H.num.words.unq, H.num.words.log, S.num.chars.log, A.num.chars.log, A.num.words, S.num.words, H.num.words.unq.log, A.num.words.unq, S.num.words.unq, A.num.words.log, S.num.words.log, A.num.words.unq.log, S.num.words.unq.log
##    max.nTuningRuns max.auc.fit opt.prob.threshold.fit max.f.score.fit
## 1                0   0.5000000                    0.5       0.0000000
## 2                0   0.4975446                    0.1       0.2867534
## 3                0   0.5000000                    0.5       0.0000000
## 4                0   0.7072558                    0.2       0.4562462
## 5                3   0.5000000                    0.5       0.0000000
## 6                1   0.7350785                    0.2       0.4220705
## 7                1   0.8062116                    0.2       0.5124688
## 8                1   0.9372948                    0.3       0.7191737
## 9                1   0.9381279                    0.3       0.7198970
## 10               3   0.8300780                    0.7       0.5738832
## 11               0   0.9468583                    0.4       0.7953930
## 12               3   1.0000000                    0.5       1.0000000
## 13               3   1.0000000                    0.6       1.0000000
##    max.Accuracy.fit max.Kappa.fit max.auc.OOB opt.prob.threshold.OOB
## 1         0.8326257    0.00000000   0.5000000                    0.5
## 2         0.1673743    0.00000000   0.4821958                    0.1
## 3         0.8326257    0.00000000   0.5000000                    0.5
## 4         0.7986592    0.33409619   0.6552144                    0.1
## 5         0.8192183    0.07367080   0.5000000                    0.5
## 6         0.8225702    0.01764536   0.7378291                    0.2
## 7         0.8359773    0.16186063   0.8156980                    0.2
## 8         0.9003357    0.61968400   0.9326067                    0.3
## 9         0.8992181    0.61736565   0.9327051                    0.3
## 10        0.8893849    0.52225926   0.8225030                    0.7
## 11        0.9325140    0.75499696   0.8910062                    0.8
## 12        0.9086034    0.64785575   0.9287358                    0.4
## 13        0.9074860    0.64293726   0.9283871                    0.4
##    max.f.score.OOB max.Accuracy.OOB max.Kappa.OOB
## 1        0.0000000        0.8327662     0.0000000
## 2        0.2865473        0.1672338     0.0000000
## 3        0.0000000        0.8327662     0.0000000
## 4        0.4058625        0.7438017     0.2528893
## 5        0.0000000        0.8327662     0.0000000
## 6        0.4044944        0.7681089     0.2640573
## 7        0.5000000        0.8142927     0.3873528
## 8        0.7136929        0.8993680     0.6528225
## 9        0.7081604        0.8974234     0.6461137
## 10       0.6014493        0.8930481     0.5439760
## 11       0.6563574        0.9027710     0.6019082
## 12       0.7195301        0.9071463     0.6639007
## 13       0.7214076        0.9076325     0.6660515
##    inv.elapsedtime.everything inv.elapsedtime.final  inv.aic.fit
## 1                 2.173913043          333.33333333           NA
## 2                 2.976190476          500.00000000           NA
## 3                 1.328021248           15.87301587           NA
## 4                 1.669449082           18.51851852           NA
## 5                 0.790513834           15.87301587           NA
## 6                 0.888099467           16.12903226 0.0002646336
## 7                 0.545256270            3.59712230 0.0002791645
## 8                 0.208724692            0.79872204 0.0004532437
## 9                 0.146177459            0.57273769 0.0004520180
## 10                0.191570881            1.09649123           NA
## 11                0.655737705            1.11482720           NA
## 12                0.009503084            0.03870718           NA
## 13                0.009931276            0.03961808           NA
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

![](NYTimes_Blogs_Base_files/figure-html/fit.models_2-1.png) 

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
            mltd_models_df[row_ix, "variable"]), paste0(".", mltd_models_df[row_ix, "data"]))), 1))
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
            mltdCI_models_df[row_ix, "data"] <- unlist(strsplit(var_components[2], "[.]"))[2]
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
        if (var_type %in% unique(paste(mltd_models_df$label, mltd_models_df$data, sep=".")))
            next
        #print(sprintf("var_type:%s", var_type))
        goback_vars <- c(goback_vars, var_type)
    }
}

if (length(goback_vars) > 0) {
    mltd_goback_df <- build_statsCI_data(glb_models_df[, c("model_id", goback_vars)])
    mltd_models_df <- rbind(mltd_models_df, mltd_goback_df)
}

mltd_models_df <- merge(mltd_models_df, glb_models_df[, c("model_id", "model_method")], all.x=TRUE)

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

![](NYTimes_Blogs_Base_files/figure-html/fit.models_2-2.png) 

```r
# used for console inspection
model_evl_terms <- c(NULL)
for (metric in glb_model_evl_criteria)
    model_evl_terms <- c(model_evl_terms, 
                         ifelse(length(grep("max", metric)) > 0, "-", "+"), metric)
model_sel_frmla <- as.formula(paste(c("~ ", model_evl_terms), collapse=" "))
print(tmp_models_df <- orderBy(model_sel_frmla, glb_models_df)[, c("model_id", glb_model_evl_criteria)])
```

```
##                     model_id max.Accuracy.OOB max.Kappa.OOB min.aic.fit
## 13 Conditional.X.no.rnorm.rf        0.9076325     0.6660515          NA
## 12          Conditional.X.rf        0.9071463     0.6639007          NA
## 11  Conditional.X.cp.0.rpart        0.9027710     0.6019082          NA
## 8              Low.cor.X.glm        0.8993680     0.6528225    2206.319
## 9          Conditional.X.glm        0.8974234     0.6461137    2212.301
## 10       Conditional.X.rpart        0.8930481     0.5439760          NA
## 1          MFO.myMFO_classfr        0.8327662     0.0000000          NA
## 3       Max.cor.Y.cv.0.rpart        0.8327662     0.0000000          NA
## 5            Max.cor.Y.rpart        0.8327662     0.0000000          NA
## 7    Interact.High.cor.Y.glm        0.8142927     0.3873528    3582.118
## 6              Max.cor.Y.glm        0.7681089     0.2640573    3778.810
## 4  Max.cor.Y.cv.0.cp.0.rpart        0.7438017     0.2528893          NA
## 2    Random.myrandom_classfr        0.1672338     0.0000000          NA
```

```r
print(myplot_radar(radar_inp_df=tmp_models_df))
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
## Warning: Removed 27 rows containing missing values (geom_point).
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

![](NYTimes_Blogs_Base_files/figure-html/fit.models_2-3.png) 

```r
print("Metrics used for model selection:"); print(model_sel_frmla)
```

```
## [1] "Metrics used for model selection:"
```

```
## ~-max.Accuracy.OOB - max.Kappa.OOB + min.aic.fit
```

```r
print(sprintf("Best model id: %s", tmp_models_df[1, "model_id"]))
```

```
## [1] "Best model id: Conditional.X.no.rnorm.rf"
```

```r
if (is.null(glb_sel_mdl_id)) 
    { glb_sel_mdl_id <- tmp_models_df[1, "model_id"] } else 
        print(sprintf("User specified selection: %s", glb_sel_mdl_id))   
    
myprint_mdl(glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]])
```

![](NYTimes_Blogs_Base_files/figure-html/fit.models_2-4.png) 

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
## importance        75   -none-     numeric  
## importanceSD       0   -none-     NULL     
## localImportance    0   -none-     NULL     
## proximity          0   -none-     NULL     
## ntree              1   -none-     numeric  
## mtry               1   -none-     numeric  
## forest            14   -none-     list     
## y               4475   factor     numeric  
## test               0   -none-     NULL     
## inbag              0   -none-     NULL     
## xNames            75   -none-     character
## problemType        1   -none-     character
## tuneValue          1   data.frame list     
## obsLevels          2   -none-     character
```

```
## [1] TRUE
```

```r
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

![](NYTimes_Blogs_Base_files/figure-html/fit.models_2-5.png) 

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
## elapsed11        fit.models                6                2 433.449
## elapsed12 fit.data.training                7                0 447.799
```

## Step `7`: fit.data.training

```r
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
##                                      id  importance
## NewsDesk.fctr1            NewsDesk.fctr 100.0000000
## WordCount                     WordCount  92.2672600
## PubDate.hour               PubDate.hour  45.0890753
## SectionName.fctr1      SectionName.fctr  38.4932377
## PubDate.minute           PubDate.minute  28.0970381
## PubDate.second           PubDate.second  23.6097109
## H.num.chars                 H.num.chars  20.4791833
## H.num.chars.log         H.num.chars.log  20.3585154
## PubDate.date               PubDate.date  20.0148418
## A.num.chars.log         A.num.chars.log  17.7059570
## S.num.chars.log         S.num.chars.log  17.1895057
## A.num.chars                 A.num.chars  17.0681326
## PubDate.wkday             PubDate.wkday  17.0150952
## S.num.chars                 S.num.chars  16.9976533
## PubDate.month             PubDate.month   7.4612116
## SectionName.fctr       SectionName.fctr   7.3351215
## H.num.words                 H.num.words   6.5844434
## NewsDesk.fctr             NewsDesk.fctr   6.5568902
## H.num.words.unq.log H.num.words.unq.log   6.4062094
## H.num.words.log         H.num.words.log   6.3910724
## H.num.words.unq         H.num.words.unq   6.3128616
## A.num.words.unq         A.num.words.unq   5.3099741
## S.num.words.unq         S.num.words.unq   5.2607201
## S.num.words.unq.log S.num.words.unq.log   5.1258698
## A.num.words.unq.log A.num.words.unq.log   5.1216312
## A.num.words                 A.num.words   4.9427359
## S.num.words                 S.num.words   4.7636974
## A.num.words.log         A.num.words.log   4.7489550
## S.num.words.log         S.num.words.log   4.6903604
## SubsectionName.fctr SubsectionName.fctr   4.2589466
## A.time                           A.time   1.7015104
## A.new                             A.new   1.6751538
## S.new                             S.new   1.6027706
## S.time                           S.time   1.4996134
## S.will                           S.will   1.4712946
## A.will                           A.will   1.4665449
## A.year                           A.year   1.3544952
## A.compani                     A.compani   1.3514317
## S.year                           S.year   1.3291077
## S.week                           S.week   1.2989863
## A.week                           A.week   1.2723817
## S.compani                     S.compani   1.2299062
## H.new                             H.new   0.9587475
## A.york                           A.york   0.4698017
## S.york                           S.york   0.4182270
## [1] "fitting model: Final.rf"
## [1] "    indep_vars: NewsDesk.fctr, WordCount, PubDate.hour, SectionName.fctr, PubDate.minute, PubDate.second, H.num.chars, H.num.chars.log, PubDate.date, A.num.chars.log, S.num.chars.log, A.num.chars, PubDate.wkday, S.num.chars, PubDate.month, SectionName.fctr, H.num.words, NewsDesk.fctr, H.num.words.unq.log, H.num.words.log, H.num.words.unq, A.num.words.unq, S.num.words.unq, S.num.words.unq.log, A.num.words.unq.log, A.num.words, S.num.words, A.num.words.log, S.num.words.log, SubsectionName.fctr, A.time, A.new, S.new, S.time, S.will, A.will, A.year, A.compani, S.year, S.week, A.week, S.compani, H.new, A.york, S.york"
## + : mtry=38 
## - : mtry=38 
## Aggregating results
## Fitting final model on full training set
```

![](NYTimes_Blogs_Base_files/figure-html/fit.data.training_0-1.png) 

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
## importance         75  -none-     numeric  
## importanceSD        0  -none-     NULL     
## localImportance     0  -none-     NULL     
## proximity           0  -none-     NULL     
## ntree               1  -none-     numeric  
## mtry                1  -none-     numeric  
## forest             14  -none-     list     
## y                6532  factor     numeric  
## test                0  -none-     NULL     
## inbag               0  -none-     NULL     
## xNames             75  -none-     character
## problemType         1  -none-     character
## tuneValue           1  data.frame list     
## obsLevels           2  -none-     character
## [1] "    calling mypredict_mdl for fit:"
```

![](NYTimes_Blogs_Base_files/figure-html/fit.data.training_0-2.png) 

```
##           Reference
## Prediction    N    Y
##          N    0    0
##          Y 5439 1093
##   Popular.fctr Popular.fctr.predict.Final.rf.N
## 1            N                               0
## 2            Y                               0
##   Popular.fctr.predict.Final.rf.Y
## 1                            5439
## 2                            1093
##           Reference
## Prediction    N    Y
##          N 4882    0
##          Y  557 1093
##   Popular.fctr Popular.fctr.predict.Final.rf.N
## 1            N                            4882
## 2            Y                               0
##   Popular.fctr.predict.Final.rf.Y
## 1                             557
## 2                            1093
##           Reference
## Prediction    N    Y
##          N 5278    0
##          Y  161 1093
##   Popular.fctr Popular.fctr.predict.Final.rf.N
## 1            N                            5278
## 2            Y                               0
##   Popular.fctr.predict.Final.rf.Y
## 1                             161
## 2                            1093
##           Reference
## Prediction    N    Y
##          N 5392    0
##          Y   47 1093
##   Popular.fctr Popular.fctr.predict.Final.rf.N
## 1            N                            5392
## 2            Y                               0
##   Popular.fctr.predict.Final.rf.Y
## 1                              47
## 2                            1093
##           Reference
## Prediction    N    Y
##          N 5439    0
##          Y    0 1093
##   Popular.fctr Popular.fctr.predict.Final.rf.N
## 1            N                            5439
## 2            Y                               0
##   Popular.fctr.predict.Final.rf.Y
## 1                               0
## 2                            1093
##           Reference
## Prediction    N    Y
##          N 5439    0
##          Y    0 1093
##   Popular.fctr Popular.fctr.predict.Final.rf.N
## 1            N                            5439
## 2            Y                               0
##   Popular.fctr.predict.Final.rf.Y
## 1                               0
## 2                            1093
##           Reference
## Prediction    N    Y
##          N 5439    3
##          Y    0 1090
##   Popular.fctr Popular.fctr.predict.Final.rf.N
## 1            N                            5439
## 2            Y                               3
##   Popular.fctr.predict.Final.rf.Y
## 1                               0
## 2                            1090
##           Reference
## Prediction    N    Y
##          N 5439  151
##          Y    0  942
##   Popular.fctr Popular.fctr.predict.Final.rf.N
## 1            N                            5439
## 2            Y                             151
##   Popular.fctr.predict.Final.rf.Y
## 1                               0
## 2                             942
##           Reference
## Prediction    N    Y
##          N 5439  350
##          Y    0  743
##   Popular.fctr Popular.fctr.predict.Final.rf.N
## 1            N                            5439
## 2            Y                             350
##   Popular.fctr.predict.Final.rf.Y
## 1                               0
## 2                             743
##           Reference
## Prediction    N    Y
##          N 5439  602
##          Y    0  491
##   Popular.fctr Popular.fctr.predict.Final.rf.N
## 1            N                            5439
## 2            Y                             602
##   Popular.fctr.predict.Final.rf.Y
## 1                               0
## 2                             491
##           Reference
## Prediction    N    Y
##          N 5439 1084
##          Y    0    9
##   Popular.fctr Popular.fctr.predict.Final.rf.N
## 1            N                            5439
## 2            Y                            1084
##   Popular.fctr.predict.Final.rf.Y
## 1                               0
## 2                               9
##    threshold    f.score
## 1        0.0 0.28668852
## 2        0.1 0.79693766
## 3        0.2 0.93140179
## 4        0.3 0.97895208
## 5        0.4 1.00000000
## 6        0.5 1.00000000
## 7        0.6 0.99862574
## 8        0.7 0.92579853
## 9        0.8 0.80936819
## 10       0.9 0.61994949
## 11       1.0 0.01633394
```

```
## [1] "Classifier Probability Threshold: 0.5000 to maximize f.score.fit"
##   Popular.fctr Popular.fctr.predict.Final.rf.N
## 1            N                            5439
## 2            Y                              NA
##   Popular.fctr.predict.Final.rf.Y
## 1                              NA
## 2                            1093
##           Reference
## Prediction    N    Y
##          N 5439    0
##          Y    0 1093
##   Popular.fctr Popular.fctr.predict.Final.rf.N
## 1            N                            5439
## 2            Y                               0
##   Popular.fctr.predict.Final.rf.Y
## 1                               0
## 2                            1093
## [1] "in mypredict_mdl: calling confusionMatrix..."
## [1] "    unique(df[, rsp_var_out]: "
## [1] Y N
## Levels: N Y
## [1] "    unique(df[, rsp_var    ]: "
## [1] Y N
## Levels: N Y
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

![](NYTimes_Blogs_Base_files/figure-html/fit.data.training_0-3.png) 

```
##   model_id model_method
## 1 Final.rf           rf
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         feats
## 1 NewsDesk.fctr, WordCount, PubDate.hour, SectionName.fctr, PubDate.minute, PubDate.second, H.num.chars, H.num.chars.log, PubDate.date, A.num.chars.log, S.num.chars.log, A.num.chars, PubDate.wkday, S.num.chars, PubDate.month, SectionName.fctr, H.num.words, NewsDesk.fctr, H.num.words.unq.log, H.num.words.log, H.num.words.unq, A.num.words.unq, S.num.words.unq, S.num.words.unq.log, A.num.words.unq.log, A.num.words, S.num.words, A.num.words.log, S.num.words.log, SubsectionName.fctr, A.time, A.new, S.new, S.time, S.will, A.will, A.year, A.compani, S.year, S.week, A.week, S.compani, H.new, A.york, S.york
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                     87.291                42.923
##   max.auc.fit opt.prob.threshold.fit max.f.score.fit max.Accuracy.fit
## 1           1                    0.5               1        0.9102878
##   max.AccuracyLower.fit max.AccuracyUpper.fit max.Kappa.fit
## 1             0.9994354                     1     0.6532154
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
##                 chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed12 fit.data.training                7                0 447.799
## elapsed13 fit.data.training                7                1 539.919
```


```r
glb_rsp_var_out <- paste0(glb_rsp_var_out, tail(names(glb_models_lst), 1))

# Used again in predict.data.new chunk
glb_get_predictions <- function(df) {
    if (glb_is_regression) {
        df[, glb_rsp_var_out] <- predict(glb_fin_mdl, newdata=df, type="raw")
        print(myplot_scatter(df, glb_rsp_var, glb_rsp_var_out, 
                             smooth=TRUE))
        df[, paste0(glb_rsp_var_out, ".err")] <- 
            abs(df[, glb_rsp_var_out] - df[, glb_rsp_var])
        print(head(orderBy(reformulate(c("-", paste0(glb_rsp_var_out, ".err"))), 
                           df)))                             
    }

    if (glb_is_classification && glb_is_binomial) {
        # incorporate glb_clf_proba_threshold
        #   shd it only be for glb_fin_mdl or for earlier models ?
        # for glb_trnent_df it shd opt; vs. assume for glb_newent_df
        finmdl_prob_fit <- glb_models_df[glb_models_df$model_id == glb_fin_mdl_id, 
                                        "opt.prob.threshold.fit"]
        selmdl_prob_fit <- glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                        "opt.prob.threshold.fit"]
        if (finmdl_prob_fit != selmdl_prob_fit)
            warning("opt.prob.threshold.fit differs for fin_mdl: ", finmdl_prob_fit,
                    " vs. sel_mdl: ", selmdl_prob_fit)
        prob_threshold <- glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                        "opt.prob.threshold.OOB"]
        
        df[, paste0(glb_rsp_var_out, ".prob")] <- 
            predict(glb_fin_mdl, newdata=df, type="prob")[, 2]
        df[, glb_rsp_var_out] <- 
    			factor(levels(df[, glb_rsp_var])[
    				(df[, paste0(glb_rsp_var_out, ".prob")] >=
    					prob_threshold) * 1 + 1], levels(df[, glb_rsp_var]))
    
        # prediction stats already reported by myfit_mdl ???
    }    
    
    if (glb_is_classification && !glb_is_binomial) {
        df[, glb_rsp_var_out] <- predict(glb_fin_mdl, newdata=df, type="raw")
    }

    return(df)
}    
glb_trnent_df <- glb_get_predictions(df=glb_trnent_df)
```

```
## Warning in glb_get_predictions(df = glb_trnent_df): opt.prob.threshold.fit
## differs for fin_mdl: 0.5 vs. sel_mdl: 0.6
```

```r
print(glb_feats_df <- mymerge_feats_importance(feats_df=glb_feats_df, sel_mdl=glb_fin_mdl, 
                                               entity_df=glb_trnent_df))
```

```
##                     id        cor.y exclude.as.feat   cor.y.abs
## 25       NewsDesk.fctr -0.115969991               0 0.115969991
## 52           WordCount  0.257526549               0 0.257526549
## 48    SectionName.fctr -0.089486409               0 0.089486409
## 28        PubDate.hour  0.159167673               0 0.159167673
## 29      PubDate.minute -0.031469083               0 0.031469083
## 31      PubDate.second -0.012253600               0 0.012253600
## 19     H.num.chars.log -0.171062360               0 0.171062360
## 18         H.num.chars -0.147211183               0 0.147211183
## 27        PubDate.date -0.012014100               0 0.012014100
## 37         S.num.chars -0.178433540               0 0.178433540
## 5          A.num.chars -0.176187472               0 0.176187472
## 6      A.num.chars.log -0.205487403               0 0.205487403
## 38     S.num.chars.log -0.205402971               0 0.205402971
## 32       PubDate.wkday -0.039801288               0 0.039801288
## 49    SectionName.fctr -0.089486409               0 0.089486409
## 24       NewsDesk.fctr -0.115969991               0 0.115969991
## 30       PubDate.month  0.019148739               0 0.019148739
## 23 H.num.words.unq.log -0.208052027               0 0.208052027
## 20         H.num.words -0.190083728               0 0.190083728
## 22     H.num.words.unq -0.193599112               0 0.193599112
## 21     H.num.words.log -0.204378687               0 0.204378687
## 41     S.num.words.unq -0.213378613               0 0.213378613
## 42 S.num.words.unq.log -0.244677426               0 0.244677426
## 9      A.num.words.unq -0.211522561               0 0.211522561
## 8      A.num.words.log -0.239527732               0 0.239527732
## 10 A.num.words.unq.log -0.244569301               0 0.244569301
## 40     S.num.words.log -0.239719045               0 0.239719045
## 39         S.num.words -0.207846930               0 0.207846930
## 7          A.num.words -0.205674056               0 0.205674056
## 50 SubsectionName.fctr -0.137898635               0 0.137898635
## 45              S.will -0.060392694               0 0.060392694
## 36               S.new -0.069088180               0 0.069088180
## 4                A.new -0.069393513               0 0.069393513
## 13              A.will -0.060842964               0 0.060842964
## 43              S.time -0.057595102               0 0.057595102
## 11              A.time -0.057790617               0 0.057790617
## 44              S.week -0.084814939               0 0.084814939
## 12              A.week -0.084814939               0 0.084814939
## 46              S.year -0.051235765               0 0.051235765
## 14              A.year -0.051235765               0 0.051235765
## 34           S.compani -0.053012962               0 0.053012962
## 2            A.compani -0.053099633               0 0.053099633
## 17               H.new -0.079839251               0 0.079839251
## 15              A.york -0.064751157               0 0.064751157
## 47              S.york -0.064751157               0 0.064751157
## 1               .rnorm -0.008703337               0 0.008703337
## 3           A.has.http -0.013592603               0 0.013592603
## 16          H.has.http           NA               0          NA
## 26             Popular  1.000000000               1 1.000000000
## 33        PubDate.year           NA               0          NA
## 35          S.has.http           NA               0          NA
## 51            UniqueID  0.011824920               1 0.011824920
##         cor.high.X is.ConditionalX.y is.cor.y.abs.low  importance
## 25            <NA>              TRUE            FALSE 100.0000000
## 52            <NA>              TRUE            FALSE  90.9361320
## 48            <NA>              TRUE            FALSE  46.2256577
## 28            <NA>              TRUE            FALSE  41.1964054
## 29            <NA>              TRUE            FALSE  28.4647014
## 31            <NA>              TRUE            FALSE  23.2904197
## 19     H.num.chars              TRUE            FALSE  22.0807744
## 18            <NA>              TRUE            FALSE  21.9376223
## 27            <NA>              TRUE            FALSE  21.4302834
## 37     A.num.chars              TRUE            FALSE  18.1218961
## 5             <NA>              TRUE            FALSE  16.9166846
## 6  S.num.chars.log              TRUE            FALSE  16.4786255
## 38            <NA>              TRUE            FALSE  15.7366530
## 32            <NA>              TRUE            FALSE  15.0538430
## 49            <NA>              TRUE            FALSE   8.7006298
## 24            <NA>              TRUE            FALSE   6.9410469
## 30            <NA>              TRUE            FALSE   6.9002002
## 23 H.num.chars.log              TRUE            FALSE   6.3970600
## 20            <NA>              TRUE            FALSE   6.3225781
## 22     H.num.words              TRUE            FALSE   6.3166418
## 21            <NA>              TRUE            FALSE   6.1230165
## 41     S.num.words              TRUE            FALSE   5.4969604
## 42     S.num.chars              TRUE            FALSE   5.4344611
## 9             <NA>              TRUE            FALSE   5.3449758
## 8             <NA>              TRUE            FALSE   5.2480939
## 10            <NA>              TRUE            FALSE   5.1970811
## 40 A.num.words.log              TRUE            FALSE   5.0951985
## 39     A.num.words              TRUE            FALSE   5.0572216
## 7             <NA>              TRUE            FALSE   4.9203295
## 50            <NA>              TRUE            FALSE   4.8308148
## 45            <NA>              TRUE            FALSE   1.6633510
## 36            <NA>              TRUE            FALSE   1.6437976
## 4            S.new              TRUE            FALSE   1.6122236
## 13          S.will              TRUE            FALSE   1.6036419
## 43            <NA>              TRUE            FALSE   1.4513837
## 11          S.time              TRUE            FALSE   1.4330261
## 44            <NA>              TRUE            FALSE   1.4094539
## 12          S.week              TRUE            FALSE   1.2975671
## 46            <NA>              TRUE            FALSE   1.2405571
## 14          S.year              TRUE            FALSE   1.1671384
## 34            <NA>              TRUE            FALSE   1.1157192
## 2        S.compani              TRUE            FALSE   1.0714318
## 17            <NA>              TRUE            FALSE   1.0122754
## 15          S.york              TRUE            FALSE   0.4472829
## 47            <NA>              TRUE            FALSE   0.4339125
## 1             <NA>              TRUE            FALSE          NA
## 3             <NA>             FALSE            FALSE          NA
## 16            <NA>             FALSE               NA          NA
## 26            <NA>                NA            FALSE          NA
## 33            <NA>             FALSE               NA          NA
## 35            <NA>             FALSE               NA          NA
## 51            <NA>                NA            FALSE          NA
```

```r
# Used again in predict.data.new chunk
glb_analytics_diag_plots <- function(obs_df) {
    if (length(vars <- subset(glb_feats_df, importance > 0)$id) > 5) {
        warning("Limiting important feature scatter plots to 5 out of ", length(vars))
        vars <- vars[1:5]
    }
    for (var in vars) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_rsp_var, glb_rsp_var_out))
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
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
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
                     rsp_var_out=glb_rsp_var_out, 
                     id_vars=glb_id_vars)
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
glb_analytics_diag_plots(obs_df=glb_trnent_df)
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_trnent_df): Limiting
## important feature scatter plots to 5 out of 45
```

![](NYTimes_Blogs_Base_files/figure-html/fit.data.training_1-1.png) ![](NYTimes_Blogs_Base_files/figure-html/fit.data.training_1-2.png) ![](NYTimes_Blogs_Base_files/figure-html/fit.data.training_1-3.png) ![](NYTimes_Blogs_Base_files/figure-html/fit.data.training_1-4.png) ![](NYTimes_Blogs_Base_files/figure-html/fit.data.training_1-5.png) 

```
##      UniqueID NewsDesk  SectionName SubsectionName
## 96         96 Business Business Day       Dealbook
## 6370     6370                                     
##                                                                       Headline
## 96                                       Morning Agenda: Evans Bank Under Fire
## 6370 Latest Updates: Protests Nationwide as More Troops Are Called to Ferguson
##                                                                                                                                                                                    Snippet
## 96   New York set to accuse Evans Bank of redlining. | Dollar General raises bid for Family Dollar. | Public pension funds staying mum on inversions. | Banks pursuing military customers.
## 6370                                               Following the events the day and evening after the release of the grand jury decision in the shooting of Michael Brown in Ferguson. Mo.
##                                                                                                                                                                                   Abstract
## 96   New York set to accuse Evans Bank of redlining. | Dollar General raises bid for Family Dollar. | Public pension funds staying mum on inversions. | Banks pursuing military customers.
## 6370                                               Following the events the day and evening after the release of the grand jury decision in the shooting of Michael Brown in Ferguson. Mo.
##      WordCount             PubDate Popular PubDate.year PubDate.month
## 96           0 2014-09-02 07:57:53       0         2014             9
## 6370     10912 2014-11-25 07:49:49       1         2014            11
##      PubDate.date PubDate.wkday PubDate.hour PubDate.minute PubDate.second
## 96              2             2            7             57             53
## 6370           25             2            7             49             49
##      NewsDesk.fctr SectionName.fctr SubsectionName.fctr     .rnorm
## 96        Business     Business Day            Dealbook  0.1410843
## 6370                                                    -0.4313549
##      Popular.fctr H.new H.has.http H.num.chars H.num.words H.num.words.unq
## 96              N     0          0          37           5               5
## 6370            Y     0          0          73           7               7
##      H.num.chars.log H.num.words.log H.num.words.unq.log S.compani S.new
## 96          3.637586        1.791759            1.791759         0     1
## 6370        4.304065        2.079442            2.079442         0     0
##      S.time S.week S.will S.year S.york S.has.http S.num.chars S.num.words
## 96        0      0      0      0      1          0         181          23
## 6370      0      0      0      0      0          0         135          12
##      S.num.words.unq S.num.chars.log S.num.words.log S.num.words.unq.log
## 96                21        5.204007        3.178054            3.091042
## 6370              12        4.912655        2.564949            2.564949
##      A.compani A.new A.time A.week A.will A.year A.york A.has.http
## 96           0     1      0      0      0      0      1          0
## 6370         0     0      0      0      0      0      0          0
##      A.num.chars A.num.words A.num.words.unq A.num.chars.log
## 96           181          23              21        5.204007
## 6370         135          12              12        4.912655
##      A.num.words.log A.num.words.unq.log
## 96          3.178054            3.091042
## 6370        2.564949            2.564949
##      Popular.fctr.predict.Final.rf.prob Popular.fctr.predict.Final.rf
## 96                                0.000                             N
## 6370                              0.746                             Y
##      Popular.fctr.predict.Final.rf.accurate .label
## 96                                     TRUE     96
## 6370                                   TRUE   6370
```

![](NYTimes_Blogs_Base_files/figure-html/fit.data.training_1-6.png) 

```r
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

![](NYTimes_Blogs_Base_files/figure-html/fit.data.training_1-7.png) 

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="predict.data.new", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                 chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed13 fit.data.training                7                1 539.919
## elapsed14  predict.data.new                8                0 550.030
```

## Step `8`: predict data.new

```r
# Compute final model predictions
glb_newent_df <- glb_get_predictions(glb_newent_df)
```

```
## Warning in glb_get_predictions(glb_newent_df): opt.prob.threshold.fit
## differs for fin_mdl: 0.5 vs. sel_mdl: 0.6
```

```r
glb_analytics_diag_plots(obs_df=glb_newent_df)
```

```
## Warning in glb_analytics_diag_plots(obs_df = glb_newent_df): Limiting
## important feature scatter plots to 5 out of 45
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

![](NYTimes_Blogs_Base_files/figure-html/predict.data.new-1.png) 

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

![](NYTimes_Blogs_Base_files/figure-html/predict.data.new-2.png) 

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

![](NYTimes_Blogs_Base_files/figure-html/predict.data.new-3.png) 

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

![](NYTimes_Blogs_Base_files/figure-html/predict.data.new-4.png) 

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

![](NYTimes_Blogs_Base_files/figure-html/predict.data.new-5.png) 

```
##      UniqueID NewsDesk   SectionName SubsectionName
## 6563     6563                                      
## 6753     6753    Metro N.Y. / Region               
##                                                Headline
## 6563 First Draft Focus: A Red Ribbon on the White House
## 6753        Reaction to Eric Garner Grand Jury Decision
##                                                                                                                                              Snippet
## 6563                                                          A large red ribbon was hung from the White House on Monday in honor of World AIDS Day.
## 6753 Reactions to a grand jurys decision not to indict the police officer who caused the death of a Staten Island man by putting him in a chokehold.
##                                                                                                                                             Abstract
## 6563                                                          A large red ribbon was hung from the White House on Monday in honor of World AIDS Day.
## 6753 Reactions to a grand jurys decision not to indict the police officer who caused the death of a Staten Island man by putting him in a chokehold.
##      WordCount             PubDate Popular PubDate.year PubDate.month
## 6563         0 2014-12-01 15:56:36      NA         2014            12
## 6753      5956 2014-12-03 14:22:22      NA         2014            12
##      PubDate.date PubDate.wkday PubDate.hour PubDate.minute PubDate.second
## 6563            1             1           15             56             36
## 6753            3             3           14             22             22
##      NewsDesk.fctr SectionName.fctr SubsectionName.fctr    .rnorm
## 6563                                                    0.7064699
## 6753         Metro    N.Y. / Region                     0.7189705
##      Popular.fctr H.new H.has.http H.num.chars H.num.words H.num.words.unq
## 6563         <NA>     0          0          50           7               7
## 6753         <NA>     0          0          43           6               6
##      H.num.chars.log H.num.words.log H.num.words.unq.log S.compani S.new
## 6563        3.931826        2.079442            2.079442         0     0
## 6753        3.784190        1.945910            1.945910         0     0
##      S.time S.week S.will S.year S.york S.has.http S.num.chars S.num.words
## 6563      0      0      0      0      0          0          86          11
## 6753      0      0      0      0      0          0         143          14
##      S.num.words.unq S.num.chars.log S.num.words.log S.num.words.unq.log
## 6563              11        4.465908        2.484907            2.484907
## 6753              14        4.969813        2.708050            2.708050
##      A.compani A.new A.time A.week A.will A.year A.york A.has.http
## 6563         0     0      0      0      0      0      0          0
## 6753         0     0      0      0      0      0      0          0
##      A.num.chars A.num.words A.num.words.unq A.num.chars.log
## 6563          86          11              11        4.465908
## 6753         143          14              14        4.969813
##      A.num.words.log A.num.words.unq.log
## 6563        2.484907            2.484907
## 6753        2.708050            2.708050
##      Popular.fctr.predict.Final.rf.prob Popular.fctr.predict.Final.rf
## 6563                              0.002                             N
## 6753                              0.460                             Y
##      Popular.fctr.predict.Final.rf.accurate .label
## 6563                                     NA   6563
## 6753                                     NA   6753
```

```
## Warning: Removed 1870 rows containing missing values (geom_point).
```

![](NYTimes_Blogs_Base_files/figure-html/predict.data.new-6.png) 

```r
submit_df <- glb_newent_df[, c(glb_id_vars, paste0(glb_rsp_var_out, ".prob"))]
names(submit_df)[2] <- "Probability1"
write.csv(submit_df, paste0(glb_out_pfx, "submit.csv"), row.names=FALSE)

print(orderBy(~ -max.auc.OOB, glb_models_df[, c("model_id", 
            "max.auc.OOB", "max.Accuracy.OOB")]))
```

```
##                     model_id max.auc.OOB max.Accuracy.OOB
## 9          Conditional.X.glm   0.9327051        0.8974234
## 8              Low.cor.X.glm   0.9326067        0.8993680
## 12          Conditional.X.rf   0.9287358        0.9071463
## 13 Conditional.X.no.rnorm.rf   0.9283871        0.9076325
## 11  Conditional.X.cp.0.rpart   0.8910062        0.9027710
## 10       Conditional.X.rpart   0.8225030        0.8930481
## 7    Interact.High.cor.Y.glm   0.8156980        0.8142927
## 6              Max.cor.Y.glm   0.7378291        0.7681089
## 4  Max.cor.Y.cv.0.cp.0.rpart   0.6552144        0.7438017
## 1          MFO.myMFO_classfr   0.5000000        0.8327662
## 3       Max.cor.Y.cv.0.rpart   0.5000000        0.8327662
## 5            Max.cor.Y.rpart   0.5000000        0.8327662
## 2    Random.myrandom_classfr   0.4821958        0.1672338
## 14                  Final.rf          NA               NA
```

```r
print(sprintf("glb_sel_mdl_id: %s", glb_sel_mdl_id))
```

```
## [1] "glb_sel_mdl_id: Conditional.X.no.rnorm.rf"
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
## [1] 4475   58
```

```r
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
##                   chunk_label chunk_step_major chunk_step_minor elapsed
## 12                 fit.models                6                2 433.449
## 14          fit.data.training                7                1 539.919
## 7             select.features                4                0  85.848
## 11                 fit.models                6                1 180.884
## 9     partition.data.training                5                0 124.117
## 10                 fit.models                6                0 140.736
## 13          fit.data.training                7                0 447.799
## 15           predict.data.new                8                0 550.030
## 6            extract.features                3                0   5.551
## 8  remove.correlated.features                4                1  88.177
## 4         manage_missing_data                2                2   2.831
## 2                cleanse_data                2                0   0.864
## 5         encodeORretype.data                2                3   3.123
## 3       inspectORexplore.data                2                1   0.903
## 1                 import_data                1                0   0.002
##    elapsed_diff
## 12      252.565
## 14       92.120
## 7        80.297
## 11       40.148
## 9        35.940
## 10       16.619
## 13       14.350
## 15       10.111
## 6         2.428
## 8         2.329
## 4         1.928
## 2         0.862
## 5         0.292
## 3         0.039
## 1         0.000
```

```
## [1] "Total Elapsed Time: 550.03 secs"
```

![](NYTimes_Blogs_Base_files/figure-html/print_sessionInfo-1.png) 

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
##  [7] caret_6.0-41        lattice_0.20-31     tm_0.6             
## [10] NLP_0.1-6           sqldf_0.4-10        RSQLite_1.0.0      
## [13] DBI_0.3.1           gsubfn_0.6-6        proto_0.3-10       
## [16] reshape2_1.4.1      plyr_1.8.1          doBy_4.5-13        
## [19] survival_2.38-1     ggplot2_1.0.1      
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
## [34] quantreg_5.11       RColorBrewer_1.1-2  Rcpp_0.11.5        
## [37] rmarkdown_0.5.1     scales_0.2.4        slam_0.1-32        
## [40] SparseM_1.6         splines_3.1.3       stringr_0.6.2      
## [43] tools_3.1.3         yaml_2.1.13
```
