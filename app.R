library(shiny)
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)
library(rsconnect)
library(dplyr)

withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      hidden(
        img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    hidden(
      div(class = "btn-err",
          div(icon("exclamation-circle"),
              tags$b("Error: "),
              span(class = "btn-err-msg")
          )
      )
    )
  )
}

withBusyIndicatorServer <- function(buttonId, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })
  
  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                       time = 0.5))
    value
  }, error = function(err) { errorFunc(err, buttonId) })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}


ui = shinyUI(
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Movie Recommender"),
    dashboardSidebar(
      sidebarMenu(
        # Setting id makes input$tabs give the tabName of currently-selected tab
        id = "tabs",
        menuItem("By Genre", tabName = "genre", icon = icon("dashboard")),
        menuItem("By Rating", icon = icon("dashboard"), tabName = "rating")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = "genre",
          fluidRow(
            box(width = 12, title = "Step 1: Select Your Favorite Genre", status = "info", solidHeader = TRUE, collapsible = TRUE,
                div(class = "genreitems",
                    uiOutput('genres_dropdown')
                )
            )
          ),
          fluidRow(
            useShinyjs(),
            box(
              width = 12, status = "info", solidHeader = TRUE,
              title = "Step 2: Discover movies you might like",
              br(),
              withBusyIndicatorUI(
                actionButton("btnGenre", "Click here to get your recommendations", class = "btn-warning")
              ),
              br(),
              tableOutput("results_by_genre")
            )
          )
        ),
        tabItem(
          tabName = "rating",
          fluidRow(
            box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                div(class = "rateitems",
                    uiOutput('ratings_book_grid')
                )
            )
          ),
          fluidRow(
            useShinyjs(),
            box(
              width = 12, status = "info", solidHeader = TRUE,
              title = "Step 2: Discover movies you might like",
              br(),
              withBusyIndicatorUI(
                actionButton("btnRating", "Click here to get your recommendations", class = "btn-warning")
              ),
              br(),
              tableOutput("results")
            )
          )
        )
      )
    )
  )
) 

load("recIBCF.rda") 
load("genre_mat2.rda")
load("movieID.rda")

OPTIMAL_N = 10
CF_PARAMETERS = list(k=500, method="pearson", normalize="center")

ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# System 1

# extract year

get_system1_recommendations_MovieID = function(top_n_to_return, genre) {
  best_movies = get_top_N_movies_data_by_genre(top_n_to_return, genre)
  return (best_movies$MovieID)
}

get_top_N_movies_data_by_genre = function(top_n_to_return, genre) {
  full_movies_data_by_genre = get_full_movies_data_by_genre(genre);
  ret = full_movies_data_by_genre %>%
    mutate(final_rank = as.double(ave_ratings_rank + ave_ratings_timestamp_rank + num_ratings_rank)) %>%
    top_n(top_n_to_return, -((final_rank))) %>%
    select('MovieID', 'Title', 'final_rank', 'ave_ratings_rank', 'ave_ratings_timestamp_rank', 'num_ratings_rank') %>%
    arrange(desc(-(final_rank)))
  
  return(ret);
}

get_full_movies_data_by_genre = function(genre_name) {
  matching_movie_idxs_for_this_genre = genre_mat2[,genre_name] == 1
  movies_for_this_genre = movies[matching_movie_idxs_for_this_genre,]
  ratings_by_movie_data = ratings %>% 
    group_by(MovieID) %>% 
    summarize(
      num_ratings = n(), 
      ave_ratings = round(mean(Rating), dig=4),
      ave_ratings_timestamp = round(mean(Timestamp), dig=4),
    );
  ret = movies_for_this_genre %>%
    left_join(ratings_by_movie_data, by = 'MovieID') %>%
    replace(is.na(.), 0) %>% 
    mutate(ave_ratings_rank = dense_rank(desc(ave_ratings))) %>% 
    mutate(num_ratings_rank = dense_rank(desc(num_ratings))) %>% 
    mutate(ave_ratings_timestamp_rank = dense_rank(desc(ave_ratings_timestamp))) %>%
    arrange(desc(ave_ratings), desc(num_ratings), desc(ave_ratings_timestamp))
  return(ret);
}


# System 2
predict_CF = function(active_user) {
  tmp = matrix(data=NA, 1, length(movieIDs))
  colnames(tmp) = movieIDs
  tmp[1, active_user$MovieID] = active_user$Rating
  r.pred = predict(r.model, as(tmp, "realRatingMatrix"), OPTIMAL_N)
  return(as(r.pred, "list"))
}


# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

# Get unique movie genres for dropdown
unique_genres = c()
for (unsplit_genres in movies$Genres) {
  split_genres = strsplit(unsplit_genres[1], "|", fixed = TRUE)
  # Not sure why split_genres returns a list of list instead of a single list
  for (genresArr in split_genres) {
    for (eachGenre in genresArr) {
      if (!(eachGenre %in% unique_genres)) {
        unique_genres = append(unique_genres, eachGenre)
      } 
    }
  }
}

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))

server = shinyServer(function(input, output, session) {
  # show the books to be rated
  
  output$ratings_book_grid <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  outputOptions(output, "ratings_book_grid", suspendWhenHidden = FALSE)  
  
  # show genre dropdown
  output$genres_dropdown <- renderUI({
    selectInput("genreDropdown", "Genre:", as.list(unique_genres))
  })
  
  #Hide ratings container
  transition_to_loading_state <- function() {
    useShinyjs()
    jsCode <- "document.querySelector('[data-widget=collapse]').click();"
    runjs(jsCode)
  }
  
  df_genre <- eventReactive(input$btnGenre, {
    withBusyIndicatorServer("btnGenre", {
      transition_to_loading_state()
      value_list = reactiveValuesToList(input)
      selected_genre = value_list$genreDropdown
      top_genre_movies = get_top_N_movies_data_by_genre(OPTIMAL_N, selected_genre)
      user_results = (1:10)/10
      recom_genre_results <- data.table(Rank = 1:10, 
                                        MovieID = top_genre_movies$MovieID, 
                                        Title = top_genre_movies$Title, 
                                        Predicted_rating =  user_results)
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btnRating, {
    withBusyIndicatorServer("btnRating", { # showing the busy indicator
      transition_to_loading_state()
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      user_results = (1:10)/10
      user_predicted_ids = predict_CF(user_ratings) # 
      user_predicted_ids = lapply(user_predicted_ids, function(x) substring(x,2))
      user_predicted_ids = as.numeric(unlist(user_predicted_ids))
    }) 
    
  }) # clicked on button
  
  output$results_by_genre <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result = df_genre()
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        movie_idx = i * j
        movie_id = recom_result$MovieID[movie_idx]
        movie_title = recom_result$Title[movie_idx]
        rec_movie = movies[movies$MovieID == movie_id,]
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            div(style = "text-align:center", 
                a(img(src = rec_movie$image_url, height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movie_title)
            )
            
        )        
      }))) 
    })
  })
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_movie_ids <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        movie_idx = i * j
        movie_id = recom_movie_ids[movie_idx]
        rec_movie = movies[movies$MovieID == movie_id,]
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            div(style = "text-align:center", 
                a(img(src = rec_movie$image_url, height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(rec_movie$Title)
            )
            
        )         
      }))) 
    }) 
    
  }) 
  
}) 

# Run the application 
shinyApp(ui = ui, server = server)
