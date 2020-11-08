
    #---------------------------------------#
  #       Loading and Cleaning Datasets       #
    #---------------------------------------#
  
  # Author: David Brillantes (dsbrillantes@up.edu.ph)
  
  
  # << Set Options >>
  options(java.parameters = "-Xmx30g",
          scipen = 999999)
  
  
  # << Load Libraries >>
  suppressWarnings({
    suppressPackageStartupMessages({
      library(xml2)
      library(rvest)
      library(DBI)
      library(fastmatch)
      library(lubridate)
      library(data.table)
      library(dplyr)
      library(tidytable)
      library(stringr)
      library(stringi)
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

  # External Links
  link.iso3166 <- "https://salsa.debian.org/iso-codes-team/iso-codes/-/raw/main/data/iso_3166-1.json"
  link.top250  <- "https://www.imdb.com/chart/top-english-movies"
  link.trilogy <- "https://en.wikipedia.org/wiki/List_of_feature_film_series_with_three_entries"
  
  # << Utility Functions >>
  gcQuiet <- function(quiet = TRUE, ...) {
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- 
    # --- Quiet version of gc()
    # --- --- --- --- --- --- --- --- --- --- --- --- ---
    
    if (quiet) invisible(gc()) else gc(...)
  }
 

# 1. Dependencies ------------------------------------------------------------

  
  # Get ISO-3166-1 Codes for Countries
  country_codes <- jsonlite::fromJSON(link.iso3166, flatten = T) %>% 
    
    # Fix Names
    as.data.frame() %>% 
    rename_with.(function(colname) stri_replace_all_fixed(colname, "X3166.1.", "")) %>% 
    
    # Use Common Names and Select Columns
    mutate.(country_name = fifelse(!is.na(common_name), common_name, name)) %>% 
    select.(alpha_2, country_name) %>% 
    mutate_across.(everything(), function(col) str_trim(col))
  

# 2. Load IMDB Data ----------------------------------------------------------

  
  # For All Files:
  # - Uses UTF-8 Encoding
  # - NULLs are encoded as `\N`
  
  
  # 2.1 Get All Movie Titles Basic Info. ----
  # - Has 7M Titles
  # - Max of 3 genres
  # - titleTypes are:
  #     movie, short, tvEpisode, tvMiniSeries, tvMovie, 
  #     tvSeries, tvShort, tvSpecial, video, videoGame
  
  title_basics <- fread(paste0(loc.imdb, "title-basics.tsv"), sep = "\t", 
                        quote = "", na.strings = "\\N", encoding = "UTF-8",
                        drop = c("originalTitle", "endYear")) %>%
    
    # Filter to Movies and startYear Until 2020 Only 
    filter.(titleType %fin% c("movie", "tvMovie") & startYear < 2021) %>% 
    
    # Separate Genres into Individual Columns
    separate.(genres, into = c("mainGenre", "subGenre1", "subGenre2"), sep = ",") %>% 
    
    # Change ID to int and get Other Variables
    mutate.(tconst_int = as.integer(str_sub(tconst, 3)),
            NumGenres = rowSums(!is.na(.[,c("mainGenre", "subGenre1", "subGenre2")])),
            FilmAgeYears = 2020 - startYear)
  
  
  # 2.2. Get Title Ratings ----
  # - Average Rating is from 1.0 (worst) to 10.0 (best)
  
  title_rating <- fread(paste0(loc.imdb, "title-ratings.tsv"), sep = "\t",
                        quote = "", na.strings = "\\N", encoding = "UTF-8") %>% 
    
    # Filter to IDs in Title Basics
    filter.(tconst %fin% title_basics$tconst)
  
  
  # 2.3. Get Title Also Known As ----
  title_aka <- fread(paste0(loc.imdb, "title-akas.tsv"), sep = "\t",
                     quote = "", na.strings = "\\N", encoding = "UTF-8",
                     drop = c("ordering", "types", "attributes")) %>% 
    
    # Filter to IDs in Title Basics
    filter.(titleId %fin% title_basics$tconst) %>% 
    
    # Cleaning and Getting Country Names
    mutate.(region = fifelse(region %fin% c("XWG", "XWW"), NA_character_, region)) %>% 
    left_join.(country_codes, by = c("region" = "alpha_2")) %>% 
    select.(-region)
  
  rm(country_codes)
  
  
  # 2.4. Get Title Principal Movie Cast ----
  title_prin <- fread(paste0(loc.imdb, "title-principals.tsv"), sep = "\t",
                      quote = "", na.strings = "\\N", encoding = "UTF-8", 
                      drop = c("job", "characters")) %>% 
    
    # Filter to IDs in Title Basics
    filter.(tconst %fin% title_basics$tconst)
  
  
  # 2.5. Get Name Basic Info. ----
  name_basics <- fread(paste0(loc.imdb, "name-basics.tsv"), sep = "\t",
                       quote = "", na.strings = "\\N", encoding = "UTF-8") %>% 
    
    # Filter to IDs in Title Principal Cast
    filter.(nconst %fin% title_prin$nconst) %>% 
    
    # Get Age and Separate Professions
    mutate.(CastAgeYears = fifelse(is.na(deathYear), 2020 - birthYear, deathYear - birthYear)) %>% 
    separate.(primaryProfession, into = c("mainProfession", "subProfession1", "subProfession"), sep = ",")

    
# 3. Insert Cleaned Data into SQLite Database --------------------------------

  
  # Establish Connection to SQLite DB
  imdb_db   <- dbConnect(RSQLite::SQLite(), loc.imdb_db)
  imdb_tabs <- c("title_basics", "title_rating", "title_aka", "title_prin", "name_basics")
  
  # Write Tables into DB
  for (tab in imdb_tabs) {dbWriteTable(imdb_db, tab, as.data.frame(get(tab)), row.names = F)}
  
  # Check List of Tables
  dbListTables(imdb_db)  
  
  # List All Tables and Fields
  imdb_tabs <- lapply(dbListTables(imdb_db), function(tab) {data.frame(Table = tab, Field = dbListFields(imdb_db, tab))}) %>% rbindlist()
  
  # Delete Dataframes from Global Env
  rm(list = imdb_tabs); gcQuiet()
  
  
# 4. Webscraped Data ---------------------------------------------------------

  
  # 4.1. Scrape Top 250 Rated Movies ----
  imdb_top250 <- read_html(link.top250) %>% 
    
    # Get HTML Table
    html_nodes(".full-width") %>% 
    html_table() %>% 
    as.data.frame() %>% 
    
    # Cleaning
    separate.(Rank...Title, into = c("Rank", "Title", "ReleaseYear"), sep = "\\n") %>% 
    rename.(IMDB_Rating = IMDb.Rating) %>% 
    mutate.(Rank = as.numeric(Rank),
            IMDB_Rating = as.numeric(IMDB_Rating),
            ReleaseYear = as.numeric(str_extract(ReleaseYear, "\\d+")),
            
            # Manual Edits on Titles
            Title = case_when.(Rank == 22  ~ "Star Wars: Episode IV - A New Hope",
                               Rank == 26  ~ "Léon: The Professional",
                               Rank == 168 ~ "Monty Python's Life of Brian",
                               Rank == 169 ~ "The Legend of 1900",
                               Rank == 172 ~ "Sunrise",
                               Rank == 225 ~ "12 Monkeys",
                               TRUE ~ Title)) %>% 
    mutate_across.(where(is.character), str_trim) %>% 
    select(Rank, Title, ReleaseYear, IMDB_Rating) %>% 
    
    left_join.(
      dbGetQuery(imdb_db, "SELECT tconst, 
                                  primaryTitle,
                                  startYear
                             FROM title_basics 
                            WHERE titleType = 'movie'"), 
      by = c("Title" = "primaryTitle", "ReleaseYear" = "startYear"))
  
  
  # 4.2. Scrape Each Movie Page of Top 250 ----
  
  # Define Functions
  get_storyline <- function(html) {
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- 
    # --- Get Storyline and Plot Keywords
    # --- --- --- --- --- --- --- --- --- --- --- --- ---
    
    chk <- title_html %>% html_nodes(".canwrap.inline")
    
    if (length(chk) != 0) {
      res <- chk %>% 
        html_text(trim = T) %>% 
        data.frame(Plotline = str_squish(.[!stri_detect_regex(., "Keywords:|Genres:")]) %>% 
                     ifelse(length(.) == 0, NA, .), 
                   Keywords = str_extract_all(.[stri_detect_fixed(., "Keywords")], "\\|?\\n(.*)\\n", simplify = T) %>% 
                     stri_replace_all_regex("\\n|\\|", "") %>% 
                     paste0(collapse = ",") %>% 
                     ifelse(length(.) == 0, NA, .)) %>% 
        select.(Plotline, Keywords) %>% 
        .[1,]
    } else {
      res <- data.frame(Plotline = NA, Keywords = NA)
    }
    
    return(res)
  }
  
  get_release_date <- function(html) {
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- 
    # --- Get Release Date
    # --- --- --- --- --- --- --- --- --- --- --- --- ---
    
    chk <- title_html %>% html_nodes("a[title='See more release dates']") 
    
    if (length(chk) != 0) {
      res <- chk %>% 
        html_text(trim = T) %>% 
        stri_replace_all_regex("^(.*)\\(.*\\)$", "$1") %>% 
        ifelse(length(.) == 0, NA, .)
    } else {
      res <- NA
    }
    
    return(res)
  }
  
  get_budget_gross <- function(html) {
    
    # --- --- --- --- --- --- --- --- --- --- --- --- --- 
    # --- Get Budget and Gross
    # --- --- --- --- --- --- --- --- --- --- --- --- ---
    
    chk <- title_html %>% html_nodes(".txt-block") 
    
    if (length(chk) != 0) {
      res <- chk %>% 
        html_text(trim = T) %>% 
        data.frame(Budget = ifelse(length(.[stri_detect_fixed(., "Budget")]) == 0, NA, 
                                   paste0(stri_replace_all_regex(.[stri_detect_fixed(., "Budget")], "[[:alpha:][:punct:]]", ""), "")), 
                   WW_Gross = ifelse(length(.[stri_detect_fixed(., "Worldwide Gross")]) == 0, NA, 
                                     paste0(stri_replace_all_regex(.[stri_detect_fixed(., "Worldwide Gross")], "[[:alpha:][:punct:]]", ""), ""))) %>% 
        select.(Budget, WW_Gross) %>% 
        .[1,]
    } else {
      res <- NA
    }
    
    return(res)
    
  }
  
  # Main Loop
  link.title   <- "https://www.imdb.com/title/"
  title_tconst <- c(unique(na.omit(imdb_top250$tconst)))
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
                      bind_cols.(data.frame(tconst = title_tconst[i], 
                                            ReleaseDate = get_release_date(title_html)),
                                 get_storyline(title_html),
                                 get_budget_gross(title_html))
      )
  }  
  
  
  # 4.3. Final Top 250 Movies Data ----
  imdb_top250_fin <- imdb_top250 %>% 
    
    left_join.(
      
      title_page %>% 
        mutate_across.(everything(), str_trim) %>% 
        mutate.(Plotline = stri_replace_all_regex(Plotline, "Written by .*$", ""),
                ReleaseDate = dmy(ReleaseDate),
                Budget = as.numeric(str_extract(Budget, "\\d+")),
                WW_Gross = as.numeric(str_extract(WW_Gross, "\\d+"))), 
      
      by = "tconst") %>% 
    distinct.(Rank, .keep_all = T)
  
  # Insert to SQLite File
  dbWriteTable(imdb_db, "imdb_top250", as.data.frame(imdb_top250_fin), row.names = F)
  
  rm(imdb_top250, title_page, imdb_top250_fin, title_html, i, title_tconst, prog); gcQuiet()
  
  
  # 4.4. Movie Trilogies from Wikipedia ----
  trilogies <- read_html(link.trilogy) %>% 
    
    # Get HTML Nodes
    html_nodes(".div-col[style='-moz-column-width: 22em; -webkit-column-width: 22em; column-width: 22em;']") %>% 
    html_nodes("li") %>% 
    html_text(trim = T) %>% 
    .[stri_detect_regex(.,"\n")] %>% 
    
    # Extract Series and Entries
    as.data.frame() %>% 
    rename.(All = ".") %>% 
    mutate.(SeriesId = row_number.()) %>% 
    separate.(All, sep = "\n", into = c("Series", "Entry_1", "Entry_2", "Entry_3"))
  
  # Fix Fails
  trilogies_fails <- trilogies %>% 
    filter.(stri_detect_regex(Series, "\\[citation needed\\]|\\[1\\]|\\[2\\]")) %>% 
    # str_split_fixed(Series, "]", n = 2)
    mutate.(Entry_3 = Entry_2,
            Entry_2 = Entry_1) %>% 
    separate.(Series, sep = "]", into = c("Series_New", "Entry_1")) %>% 
    rename.(Series = Series_New)
    
  # Merge Back and Clean
  trilogies <- bind_rows.(trilogies[!trilogies$SeriesId %fin% trilogies_fails$SeriesId], 
                          trilogies_fails) %>% 
    mutate.(Series = stri_replace_all_regex(Series, "\\*+|\\(.\\)|\\[1|\\[2|\\[citation needed", "")) %>% 
    mutate_across.(everything(), str_squish) %>% 
    mutate_across.(everything(), str_trim)
  
  # Insert to SQLite File
  dbWriteTable(imdb_db, "wiki_trilogies", as.data.frame(trilogies), row.names = F)
  
  rm(trilogies, trilogies_fails); gcQuiet()
  
  
# 5. Kaggle Datasets ---------------------------------------------------------

  print("hi")