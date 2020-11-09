  
    #------------------------------#
  #       Statistical Modeling       #
    #------------------------------#
  
  # Author: David Brillantes (dsbrillantes@up.edu.ph)
  
  
  # << Set Options >>
  options(java.parameters = "-Xmx32g",
          scipen = 999999)
  
  
  # << Load Libraries >>
  suppressWarnings({
    suppressPackageStartupMessages({
      library(xml2)
      library(rvest)
      library(viridis)
      library(DBI)
      library(randomForest)
      library(ggplot2)
      library(ggridges)
      library(factoextra)
      library(FactoMineR)
      library(fastmatch)
      library(lubridate)
      library(data.table)
      library(dplyr)
      library(tidytable)
      library(stringr)
      library(stringi)
      library(glmnet)
      library(ROCR)
      library(pROC)
      library(caret)
    })
  })

    
# 0. Initializations ---------------------------------------------------------
  
  
  # << Path Declarations >>
  
  # Home Folders
  loc.main     <- "./"
  loc.input    <- paste0(loc.main, "Input/")
  loc.output   <- paste0(loc.main, "Output/")
  
  # Data Marts
  loc.imdb     <- paste0(loc.input, "Data/IMDB/")
  loc.imdb_db  <- paste0(loc.imdb, "imdb_movies.sqlite")
  loc.kaggle   <- paste0(loc.input, "Data/Kaggle/")
  
  # << Utility Functions >>
  pipe_message <- function(df, message = NULL, fill = 2, crayon_f = NULL) {
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- 
    # --- For including messages in pipes
    # --- --- --- --- --- --- --- --- --- --- --- --- ---
    
    if (!is.null(message)) {
      if (!is.null(crayon_f)) message <- do.call(getExportedValue("crayon", crayon_f), list(message))
      cat(message, fill = fill)
    } 
    return(df)
  }
  
  gcQuiet <- function(quiet = TRUE, ...) {
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- 
    # --- Quiet version of gc()
    # --- --- --- --- --- --- --- --- --- --- --- --- ---
    
    if (quiet) invisible(gc()) else gc(...)
  }
  
  get_budget_gross <- function(html) {
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- 
    # --- Get Budget and Gross
    # --- --- --- --- --- --- --- --- --- --- --- --- ---
    
    chk <- title_html %>% html_nodes(".txt-block") 
    
    if (length(chk) != 0) {
      res <- chk %>% 
        html_text(trim = T) %>% 
        tidytable(Budget = ifelse(length(.[stri_detect_fixed(., "Budget")]) == 0, NA_character_, 
                                  stri_replace_all_regex(.[stri_detect_fixed(., "Budget")], "\\n|Budget:|\\(estimated\\)", "")), 
                  WW_Gross = ifelse(length(.[stri_detect_fixed(., "Worldwide Gross")]) == 0, NA_character_,
                                    stri_replace_all_regex(.[stri_detect_fixed(., "Worldwide Gross")], "\\n|Cumulative Worldwide Gross:|\\(estimated\\)", ""))) %>% 
        select.(Budget, WW_Gross) %>% 
        slice.(1) %>% 
        mutate_across.(everything(), str_trim)
    } else {
      res <- NA
    }
    return(res)
  }
  
  # RMSE <- function(fitted, observed){
  #   
  #   # --- --- --- --- --- --- --- --- --- --- --- --- --- 
  #   # --- Computing Root Mean Square Error
  #   # --- --- --- --- --- --- --- --- --- --- --- --- ---
  #   
  #   return(sqrt(mean((fitted - observed)^2)))
  # }
  
# 1. Load IMDB Data ----------------------------------------------------------
  
  
  # Establish Connection to SQLite DB
  imdb_db   <- dbConnect(RSQLite::SQLite(), loc.imdb_db)
  
  # List All Tables and Fields
  imdb_tabs <- lapply(dbListTables(imdb_db), function(tab) {
    data.frame(Table = tab, Field = dbListFields(imdb_db, tab))
  }) %>% 
    rbindlist() %>% 
    summarise.(Fields = toString(Field), .by = Table)
  
  # Load into Global Env
  list_tabs <- lapply(imdb_tabs$Table, function(tab) dbGetQuery(imdb_db, paste("SELECT * FROM", tab))); names(list_tabs) <- imdb_tabs$Table
  list2env(list_tabs, envir = .GlobalEnv); rm(list_tabs)
  
  
# 2. Create Movies Data ------------------------------------------------------

  
  # 2.1. Filtering Movies ----
  movies <- inner_join.(
    
    # Titles in the Past 5 Years
    title_basics %>% 
      filter.(startYear >= 2016),
    
    # Ratings and Votes
    title_rating %>% 
      filter.(!is.na(averageRating) & !is.na(numVotes)),
    
  by = "tconst") %>% 
    
    # Filter to Reasonably Popular Movies
    filter.(numVotes >= 10000)

  
  # 2.2. Getting Budget and Gross ----
  
  # Main Loop
  link.title   <- "https://www.imdb.com/title/"
  title_tconst <- c(unique(na.omit(movies$tconst)))
  title_page   <- data.frame() # Initialize
  prog         <- txtProgressBar(min = 0, max = length(title_tconst), initial = 0)   

  for (i in seq_along(title_tconst)) {
    
    setTxtProgressBar(prog, i)
    
    # Access HTML File
    repeat {
      title_html <- try(read_html(paste0(link.title, title_tconst[i])), silent = T)
      if(!"try-error" %fin% class(title_html)) break
    }
    
    title_page <- bind_rows.(
      title_page,
      bind_cols.(data.frame(tconst = title_tconst[i]), get_budget_gross(title_html))
    )
  }
  
  title_bg <- title_page %>% 
    mutate.(Budget = as.numeric(str_extract(Budget, "\\d+")),
            WW_Gross = as.numeric(str_extract(WW_Gross, "\\d+"))) %>% 
    filter.(!is.na(Budget) & !is.na(WW_Gross)) %>% 
    distinct.()
  
  # Join Back to Base
  movies <- inner_join.(movies, title_bg, by = "tconst")
  saveRDS(title_bg, paste0(loc.output, "title_bg.RDS"))
  
  
  # EDIT: From numVotes >= 5000 Run
  title_bg_5000 <- readRDS(paste0(loc.output, "title_bg_5000votes.RDS")) %>% 
    filter.(!is.na(Budget), !is.na(WW_Gross)) %>% 
    mutate.(Budget = stri_replace_all_fixed(Budget, "RUR", "RUB"),
            Budget = stri_replace_all_fixed(Budget, "TRL", "TRY"),
            Currency = ifelse(stri_detect_regex(Budget, "^[[:alpha:]]"), str_sub(Budget, 1, 3), NA_character_))
  
  # Get All Foreign Currency
  currencies <- paste0(unique(na.omit(title_bg_5000$Currency)), "USD=X")
  fx_rates   <- quantmod::getQuote(currencies) %>% 
    mutate.(Currency = str_sub(rownames(.), 1, 3)) %>% 
    select.(Currency, Last)
  
  # Convert Budgets in Foreign Currency to USD
  title_bg_5000 <- title_bg_5000 %>% 
    left_join.(fx_rates, by = "Currency") %>% 
    mutate_across.(c(Budget, WW_Gross), function(col) stri_replace_all_regex(col, "[[:alpha:][:punct:]]", "")) %>% 
    mutate.(Budget = as.numeric(str_extract(Budget, "\\d+")),
            Budget = ifelse(!is.na(Currency), Budget*Last, Budget),
            WW_Gross = as.numeric(str_extract(WW_Gross, "\\d+"))) %>% 
    select.(-Currency, -Last)
  
  # Join Back to Base
  movies <- movies %>% 
    select.(-Budget, -WW_Gross) %>% 
    left_join.(title_bg_5000, by = "tconst")
  
  rm(fx_rates, title_bg_5000); gcQuiet()

  
  # 2.3. Adding Features from Other Tables ----
  
  # Number of Countries and Languages
  movies <- inner_join.(movies,
    title_aka %>% 
      filter.(titleId %fin% movies$tconst) %>% 
      summarise.(num_countries = n_distinct(country_name, na.rm = T),
                 num_languages = n_distinct(language, na.rm = T), 
                 .by = titleId) %>% 
      filter.(num_countries > 0 & num_languages > 0),
    by = c("tconst" = "titleId"))
  
  # Get Lead Actor/Actress Info.
  movies <- inner_join.(movies,
    title_prin %>% 
      filter.(tconst %fin% movies$tconst) %>% 
      
      # Get Name Basic
      left_join.(select.(name_basics, birthYear, mainProfession, nconst),
                 by = "nconst") %>% 
      
      # Filter to Films where they were Lead Actors/Actresses
      filter.(stri_detect_regex(category, "actor|actress")
              & stri_detect_regex(mainProfession, "actor|actress")) %>% 
      filter.(ordering == min(ordering), .by = tconst) %>% 
      mutate(Category = str_to_title(category)) %>% 
      select.(tconst, Category, birthYear) %>% 
      drop_na.(),
  by = "tconst")
  
  # 2.4. Other Variables and Cleaning
  movies_fin <- movies %>% 
    
    mutate.(lead_age = startYear - birthYear,
            years_from_2020 = 2020 - startYear,
            Earning = WW_Gross - Budget) %>% 
    select.(tconst, 
            years_from_2020,
            runtimeMinutes,
            mainGenre,
            NumGenres,
            averageRating,
            numVotes,
            Budget,
            WW_Gross,
            Earning,
            num_countries,
            num_languages,
            Category,
            lead_age) %>% 
    rename.(runtime_mins = runtimeMinutes,
            main_genre = mainGenre,
            num_genres = NumGenres,
            VOTES = numVotes,
            RATING = averageRating,
            EARNING = Earning,
            lead_category = Category,
            BUDGET = Budget,
            GROSS = WW_Gross) %>% 
    select.(tconst, VOTES, RATING, EARNING, GROSS, BUDGET, everything()) %>% 
    drop_na.()
  
  
  # 2.5. Save Final Table for Modeling ----
  saveRDS(movies_fin, paste0(loc.output, "movies_fin.RDS"))
  rm(name_basics, title_aka, prog, title_html, title_page, 
     title_rating, wiki_trilogies, title_prin, title_bg)
  gcQuiet()

# 3. Creating an Index using PCA ---------------------------------------------
  
  
  # 3.1. Create Weighted Y Variables ----
  movies_dependent <- movies_fin %>% 
    select.(VOTES, RATING, EARNING) %>%
    mutate.(VOTES = scales::rescale(VOTES, to = c(0, 100)),
            RATING = scales::rescale(RATING, to = c(0, 100)),
            EARNING = scales::rescale(EARNING, to = c(0, 100), )) %>% 
    
    # Apply Different Weights
    mutate.(Y1 = VOTES*0.50 + RATING*0.25 + EARNING*0.25,
            Y2 = VOTES*0.30 + RATING*0.20 + EARNING*0.50,
            Y3 = VOTES*0.33 + RATING*0.33 + EARNING*0.33,
            Y1_HIT = as.factor(fifelse(Y1 >= (IQR(Y1)*1.5)+quantile(Y1, 0.75), 1, 0)),
            Y2_HIT = as.factor(fifelse(Y2 >= (IQR(Y2)*1.5)+quantile(Y2, 0.75), 1, 0)),
            Y3_HIT = as.factor(fifelse(Y3 >= (IQR(Y3)*1.5)+quantile(Y3, 0.75), 1, 0))) %>% 
    
    select.(-VOTES, -RATING, -EARNING)
  
  
  # 3.2. Principal Components Analysis ----
  movies_pca <- movies_fin %>% 
    select.(VOTES, RATING, EARNING) %>%
    prcomp(scale. = T)
  
  # Get First Component from PCA
  movies_dependent$PCA <- scales::rescale(movies_pca$x[,1], to = c(0, 100))
  rm(movies_pca); gcQuiet()
  
  # Create Index
  movies_dependent <- movies_dependent %>% 
    rowwise() %>% 
    mutate(INDEX = mean(c(Y1, Y2, Y3, PCA))) %>% 
    mutate.(HIT = as.factor(fifelse(INDEX >= (IQR(INDEX)*1.5)+quantile(INDEX, 0.75), 1, 0)))
  
  
  # 3.3. Plot Distributions ----
  movies_dependent %>%
    select.(Y1, Y2, Y3, PCA, INDEX) %>% 
    pivot_longer.(names_to = "DEPENDENT", values_to = "SCORE") %>% 
    ggplot(aes(x = SCORE, y = DEPENDENT, fill = DEPENDENT)) +
    geom_density_ridges(aes(alpha = DEPENDENT), scale = 1,
                        jittered_points = TRUE,
                        position = position_points_jitter(width = 0.05, height = 0),
                        point_shape = '|', point_size = 1, point_alpha = 0.25) +
    # stat_density_ridges(quantile_lines = TRUE, quantiles = 0.5, alpha = 0.7) +
    geom_vline(xintercept = c((IQR(movies_dependent$PCA)*1.5)+quantile(movies_dependent$PCA, 0.75),
                              (IQR(movies_dependent$Y1)*1.5)+quantile(movies_dependent$Y1, 0.75),
                              (IQR(movies_dependent$Y2)*1.5)+quantile(movies_dependent$Y2, 0.75),
                              (IQR(movies_dependent$Y3)*1.5)+quantile(movies_dependent$Y3, 0.75)),
               color = c(viridis(5, begin = 0.1, end = 1)[2],
                         viridis(5, begin = 0.1, end = 1)[3],
                         viridis(5, begin = 0.1, end = 1)[4],
                         viridis(5, begin = 0.1, end = 1)[5]),
               linetype = 2) +
    geom_vline(xintercept = (IQR(movies_dependent$INDEX)*1.5)+quantile(movies_dependent$INDEX, 0.75),
               color = viridis(5, begin = 0.1, end = 1)[1],
               size = 1) +
    
    theme_light() +
    theme(text = element_text(size = 10),
          title = element_text(colour = "grey30", size = 10, face = "bold"),
          plot.subtitle = element_text(face = "italic"),
          axis.ticks = element_blank(), 
          axis.text.y = element_blank(),
          panel.border = element_rect(color = "grey20"), 
          panel.grid = element_line(color = "grey65", linetype = "dotted"),
          plot.margin = margin(0, 20, 0, 0),
          legend.position = "top") +
    scale_y_discrete(expand = c(0.025, 0)) +
    scale_x_continuous(expand = c(0, 0), 
                       breaks = c(0, 25, round((IQR(movies_dependent$INDEX)*1.5)+quantile(movies_dependent$INDEX, 0.75)), 50, 75, 100),
                       labels = c(0, 25, round((IQR(movies_dependent$INDEX)*1.5)+quantile(movies_dependent$INDEX, 0.75)), 50, 75, 100)) +
    scale_fill_manual(values = viridis(5, begin = 0.1, end = 1), name = "Dependent Var.") +
    scale_color_manual(values = viridis(5, begin = 0.1, end = 1), name = "Dependent Var.") +
    scale_alpha_manual(values = c(0.75, 0.2, 0.2, 0.2, 0.2), name = "Dependent Var.") + 
    labs(x = "Score",
         y = "Density",
         title = "Empirical Densities of Dependent Variables",
         subtitle = "Vertical lines are the Tukey Outlier Fences for each variable.")

    
# 4. Model Building ----------------------------------------------------------

  
  # Genre Levels
  all_genres <- unique(na.omit(title_basics$mainGenre))
  
  # Try Data
  movies_try <- bind_cols.(movies_fin, movies_dependent) %>%
    
    # Change Categorical Fields to Factors
    mutate.(num_genres = as.factor(num_genres),
            main_genre = as.factor(main_genre),
            lead_category = as.factor(lead_category),
            years_from_2020 = as.factor(years_from_2020))
  
  # Get Only Genres with Freq >= 2
  movies_try <- movies_try %>% 
    filter.(main_genre %fin% (
      movies_try %>% 
        count.(main_genre) %>% 
        filter.(N > 1) %>% 
        pull.(main_genre)
    ))
  
  
  # Split to Train and Test
  set.seed(as.numeric(Sys.Date()))

  movies_train <- slice_sample(movies_try, prop = 0.80)
  movies_test  <- filter.(movies_try, !tconst %fin% movies_train$tconst)

  # ids <- caret::createDataPartition(movies_try$main_genre, p = 0.80, list = F)
  # movies_train <- movies_try[ids,]
  # movies_test  <- movies_try[-ids,]
  
  
  # 4.1. Continuous Response Models ----------------------------------------------

  
  # 4.1.1. OLS Model ----
  
  features <- c("years_from_2020", "runtime_mins", "main_genre", "num_genres",
                "num_countries", "num_languages", "lead_category", "lead_age")
  features_numeric <- c("runtime_mins", "num_countries", "num_languages", "lead_age")
  features_categor <- c("years_from_2020", "main_genre", "lead_category", "num_genres")

  # Fit Full Model
  fit_ols_y1 <- lm(Y1 ~ lead_age + runtime_mins + num_countries + num_languages +
                      num_genres + main_genre + lead_category + years_from_2020, data = movies_train)
  
  fit_ols_y2 <- lm(Y2 ~ lead_age + runtime_mins + num_countries + num_languages +
                        num_genres + main_genre + lead_category + years_from_2020, data = movies_train)
  
  summary(fit_ols_y1) 
  summary(fit_ols_y2)
  
  # Fit Stepwise Model
  fit_ols_y1_step <- MASS::stepAIC(fit_ols_y1); summary(fit_ols_y1_step)
  fit_ols_y2_step <- MASS::stepAIC(fit_ols_y2); summary(fit_ols_y2_step)

  # Fit Final Model
  fit_ols_y1_fin <- lm(Y1 ~ runtime_mins + num_countries + num_languages + lead_category + years_from_2020, data = movies_train)
  fit_ols_y2_fin <- lm(Y2 ~ runtime_mins + num_countries + num_languages + lead_category + years_from_2020, data = movies_train)
  
  summary(fit_ols_y1_fin) 
  summary(fit_ols_y2_fin)
  
  # Get RMSE
  RMSE(predict(fit_ols_y1_fin, newdata = select.(movies_test, all_of(names(fit_ols_y1_fin$model[,-1])))), observed = movies_test$Y1)
  RMSE(predict(fit_ols_y2_fin, newdata = select.(movies_test, all_of(names(fit_ols_y2_fin$model[,-1])))), observed = movies_test$Y2)

  
  # 4.1.2. Polynomial Regression ----
  fit_poly_y1 <- lm(Y1 ~ lead_age + runtime_mins + poly(num_countries, 2) + num_languages +
                      lead_category + years_from_2020, data = movies_train)
  fit_poly_y2 <- lm(Y2 ~ poly(lead_age, 2) + runtime_mins + poly(num_countries, 2) + poly(num_languages, 2) +
                      main_genre + lead_category + years_from_2020, data = movies_train)
  
  summary(fit_poly_y1); plot(fit_poly_y1)
  summary(fit_poly_y2); plot(fit_poly_y2)
  
  
  # 4.1.3. GAM ----
  
  # Fit Full Model
  fit_gam_y1 <- gam(Y1 ~ lead_age + runtime_mins + s(num_countries, bs = "cr", k = 10) + s(num_languages, k = 10) +
                      num_genres + main_genre + lead_category + years_from_2020, data = movies_train, method = "REML")
  fit_gam_y2 <- gam(Y2 ~ lead_age + runtime_mins + s(num_countries, bs = "cr", k = 10) + s(num_languages, k = 10) +
                      num_genres + main_genre + lead_category + years_from_2020, data = movies_train, method = "REML")

  summary(fit_gam_y1) #; plot(fit_gam_y1)
  summary(fit_gam_y2) #; plot(fit_gam_y2)
  
  # Fit Final Model
  fit_gam_y1_fin <- gam(Y1 ~ lead_age + runtime_mins + s(num_countries, k = 10) + s(num_languages, k = 10) + 
                          lead_category + years_from_2020, data = movies_train)
  fit_gam_y2_fin <- gam(Y2 ~ lead_age + runtime_mins + s(num_countries, k = 10) + s(num_languages, k = 10) + 
                          lead_category + years_from_2020, data = movies_train)
  
  summary(fit_gam_y1_fin); gam.check(fit_gam_y1_fin) 
  summary(fit_gam_y2_fin); gam.check(fit_gam_y2_fin) 
  
  # Get RMSE
  RMSE(predict(fit_gam_y1_fin, newdata = select.(movies_test, all_of(names(fit_gam_y1_fin$model[,-1])))), observed = movies_test$Y1)
  RMSE(predict(fit_gam_y2_fin, newdata = select.(movies_test, all_of(names(fit_gam_y2_fin$model[,-1])))), observed = movies_test$Y2)
  
  
  # 4.2. Classification Approach -------------------------------------------------


  # 4.2.1. Logistic Regression ----
  
  # Fit Full Model
  fit_logi <- glm(HIT ~ lead_age + runtime_mins + num_countries + num_languages +
                    num_genres + main_genre + lead_category + years_from_2020, data = movies_train, family = "binomial")
  
  summary(fit_logi)
  
  # Fit Stepwise Model
  fit_logi_step <- MASS::stepAIC(fit_logi); summary(fit_logi_step)

  # Fit Final Model
  fit_logi_fin <- glm(HIT ~ runtime_mins + num_countries + num_languages + lead_category, data = movies_train, family = "binomial")
  summary(fit_logi_fin) 
  
  # Get Classification Metrics
  
  # Plot Sensitivity vs. Specificity Curve using Training Data
  sens <- data.frame(x = unlist(performance(prediction(predict(fit_logi_fin, type = "response"), 
                                                          movies_train$HIT), "sens")@x.values),
                        y = unlist(performance(prediction(predict(fit_logi_fin, type = "response"), 
                                                          movies_train$HIT), "sens")@y.values))
  spec <- data.frame(x = unlist(performance(prediction(predict(fit_logi_fin, type = "response"), 
                                                          movies_train$HIT), "spec")@x.values),
                        y = unlist(performance(prediction(predict(fit_logi_fin, type = "response"), 
                                                          movies_train$HIT), "spec")@y.values))
  sens %>% 
    ggplot(aes(x, y)) + 
    geom_line(size = 1) + 
    geom_line(data = spec, aes(x, y), size = 1, color = "royalblue") +
    scale_x_continuous(breaks = seq(0, 1, 0.05), limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(sec.axis = sec_axis(~., name = "Specificity"), limits = c(0, 1), expand = c(0, 0)) +
    labs(x = 'Cutoff', y = "Sensitivity",
         title = " Sensitivity and Specificity Curves at Different Cutoffs") +
    theme_light() +
    theme(text = element_text(size = 10),
          title = element_text(colour = "grey20", size = 10, face = "bold"),
          plot.subtitle = element_text(face = "italic"),
          axis.ticks = element_blank(), 
          axis.title.y.right = element_text(colour = "royalblue"),
          axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
          panel.grid = element_line(color = "grey90"),
          panel.grid.minor.y = element_blank(),
          panel.border = element_rect(color = "grey20"),
          legend.position = "none")

  
  Epi::ROC(stat =  movies_test$HIT, test = fit_logi_preds_response, plot = "ROC")
  # Epi::ROC(form = HIT ~ runtime_mins + num_countries + num_languages + lead_category, data = movies_train, plot = "ROC", MX = T)
  
  # Get Predicted Probabilities
  fit_logi_step_preds_response <- predict(fit_logi_step, newdata = select.(movies_test, all_of(names(fit_logi_step$model[,-1]))), type = "response")
  fit_logi_preds_response      <- predict(fit_logi_fin, newdata = select.(movies_test, all_of(names(fit_logi_fin$model[,-1]))), type = "response")
  
  # Get Predicted Class Labels
  ROCit::measureit(class = movies_test$HIT, score = fit_logi_step_preds_response,  measure = "PPV", negref = 0)
  ROCit::measureit(class = movies_test$HIT, score = fit_logi_preds_response,  measure = "PPV", negref = 0)
  fit_logi_step_preds_labels <- as.factor(fifelse(fit_logi_step_preds_response > 0.5, 1, 0))
  fit_logi_preds_labels      <- as.factor(fifelse(fit_logi_preds_response > 0.8, 1, 0))
  
  # Get Confusion Matrix
  confusionMatrix(data = fit_logi_step_preds_labels, reference = movies_test$HIT, positive = "1")
  confusionMatrix(data = fit_logi_preds_labels, reference = movies_test$HIT, positive = "1")
  
  # AUROC
  rocit_out <- ROCit::rocit(class = movies_test$HIT, score = fit_logi_preds_response, negref = 0)
  rocit_bin <- ROCit::rocit(class = movies_test$HIT, score = fit_logi_preds_response, negref = 0, method = "bin")
  rocit_non <- ROCit::rocit(class = movies_test$HIT, score = fit_logi_preds_response, negref = 0, method = "non")
  ModelMetrics::auc(movies_test$HIT, fit_logi_preds_labels)
  
  plot(rocit_out, col = c(1,"gray50"), 
       legend = FALSE, YIndex = FALSE)
  lines(rocit_bin$TPR~rocit_bin$FPR, 
        col = 2, lwd = 2)
  lines(rocit_non$TPR~rocit_non$FPR, 
        col = 4, lwd = 2)
  legend("bottomright", col = c(1,2,4),
         c("Empirical ROC", "Binormal ROC",
           "Non-parametric ROC"), lwd = 2)
  
  
# 5. Predicting HIT Movies > 2020 --------------------------------------------

  
  # Get New Movies
  title_basics_new <- fread(paste0(loc.imdb, "title-basics.tsv"), sep = "\t", 
                                            quote = "", na.strings = "\\N", encoding = "UTF-8",
                                            drop = c("originalTitle", "endYear")) %>%
    
    # Filter to Movies and startYear Until 2020 Only 
    filter.(titleType %fin% c("movie", "tvMovie") & startYear > 2020) %>% 
    
    # Separate Genres into Individual Columns
    separate.(genres, into = c("mainGenre", "subGenre1", "subGenre2"), sep = ",") %>% 
    
    # Change ID to int and get Other Variables
    mutate.(tconst_int = as.integer(str_sub(tconst, 3)),
            NumGenres = rowSums(!is.na(.[,c("mainGenre", "subGenre1", "subGenre2")])),
            FilmAgeYears = 2020 - startYear)
  
  
  
  