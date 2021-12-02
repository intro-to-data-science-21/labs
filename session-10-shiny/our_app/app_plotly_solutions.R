library(plotly)
library(ggplot2)
library(dplyr)
library(viridis)
library(hrbrthemes)
if (FALSE) {
  library(RSQLite)
  library(dbplyr)
}


# Setup -------------------------------------------------------------------


# Set up handles to database tables on app start
db <- src_sqlite("movies.db")
omdb <- tbl(db, "omdb")
tomatoes <- tbl(db, "tomatoes")

# Join tables, filtering out those with <10 reviews, and select specified columns
all_movies <- inner_join(omdb, tomatoes, by = "ID") %>%
  filter(Reviews >= 10) %>%
  select(ID, imdbID, Title, Year, Rating_m = Rating.x, Runtime, Genre, Released,
         Director, Writer, imdbRating, imdbVotes, Language, Country, Oscars,
         Rating = Rating.y, Meter, Reviews, Fresh, Rotten, userMeter, userRating, userReviews,
         BoxOffice, Production, Cast)

# Variables that can be put on the x and y axes
axis_vars <- c(
  "Tomato Meter" = "Meter",
  "Numeric Rating" = "Rating",
  "Number of reviews" = "Reviews",
  "Dollars at box office" = "BoxOffice",
  "Year" = "Year",
  "Length (minutes)" = "Runtime"
)


# Custom Function ---------------------------------------------------------

# For dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}


# Define UI ---------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Movie explorer"),
  fluidRow(
    column(3,
           wellPanel(
             h4("Filter"),
             sliderInput("reviews", "Minimum number of reviews on Rotten Tomatoes",
                         10, 300, 80, step = 10),
             sliderInput("year", "Year released", 1940, 2014, value = c(1970, 2014),
                         sep = ""),
             sliderInput("oscars", "Minimum number of Oscar wins (all categories)",
                         0, 4, 0, step = 1),
             sliderInput("boxoffice", "Dollars at Box Office (millions)",
                         0, 800, c(0, 800), step = 1),
             
             #### Answer to Exercise 1:
             selectInput("genre", "Genre (a movie can have multiple genres)",
                         c("All", "Action", "Adventure", "Animation", "Biography", "Comedy",
                           "Crime", "Documentary", "Drama", "Family", "Fantasy", "History",
                           "Horror", "Music", "Musical", "Mystery", "Romance", "Sci-Fi",
                           "Short", "Sport", "Thriller", "War", "Western")
             ),
             textInput("director", "Director name contains (e.g., Miyazaki)"),
             textInput("cast", "Cast names contains (e.g. Tom Hanks)")
           ),
           wellPanel(
             selectInput("xvar", "X-axis variable", axis_vars, selected = "Meter"),
             selectInput("yvar", "Y-axis variable", axis_vars, selected = "Reviews"),
             tags$small(paste0(
               "Note: The Tomato Meter is the proportion of positive reviews",
               " (as judged by the Rotten Tomatoes staff), and the Numeric rating is",
               " a normalized 1-10 score of those reviews which have star ratings",
               " (for example, 3 out of 4 stars)."
             ))
           )
    ),
    column(9,
           plotlyOutput("plot1"),
           wellPanel(
             span("Number of movies selected:",
                  textOutput("n_movies")
             )
           )
    )
  )
)


# Define Server -----------------------------------------------------------

server <- function(input, output, session) {
  
  # Filter the movies, returning a data frame
  movies <- reactive({
    # Due to dplyr issue #318, we need temp variables for input values
    imdb_ratings <- input$imdbRating
    reviews <- input$reviews
    oscars <- input$oscars
    minyear <- input$year[1]
    maxyear <- input$year[2]
    minboxoffice <- input$boxoffice[1] * 1e6
    maxboxoffice <- input$boxoffice[2] * 1e6
    
    # Apply filters
    m <- all_movies %>%
      filter(
        Reviews >= reviews,
        Oscars >= oscars,
        Year >= minyear,
        Year <= maxyear,
        BoxOffice >= minboxoffice,
        BoxOffice <= maxboxoffice
      ) %>%
      arrange(Oscars)
    
    #### Answer to Exercise 1 
    # Optional: filter by genre
    if (input$genre != "All") {
      genre <- paste0("%", input$genre, "%")
      m <- m %>% filter(Genre %like% genre)
    }

    # Optional: filter by director
    if (!is.null(input$director) && input$director != "") {
      director <- paste0("%", input$director, "%")
      m <- m %>% filter(Director %like% director)
    }
    # Optional: filter by cast member
    if (!is.null(input$cast) && input$cast != "") {
      cast <- paste0("%", input$cast, "%")
      m <- m %>% filter(Cast %like% cast)
    }
    
    
    m <- as.data.frame(m)
    
    # Add column which says whether the movie won any Oscars
    # Be a little careful in case we have a zero-row data frame
    m$has_oscar <- character(nrow(m))
    m$has_oscar[m$Oscars == 0] <- "No"
    m$has_oscar[m$Oscars >= 1] <- "Yes"
    m
  })
  
  
  
  # Function for generating tooltip text
  movie_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$ID)) return(NULL)
    
    # Pick out the movie with this ID
    all_movies <- isolate(movies())
    movie <- all_movies[all_movies$ID == x$ID, ]
    
    paste0("<b>", movie$Title, "</b><br>",
           movie$Year, "<br>",
           "$", format(movie$BoxOffice, big.mark = ",", scientific = FALSE)
    )
  }
  
  # A reactive expression with the plotly plot
  output$plot1 <- renderPlotly({
    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    
    # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
    # but since the inputs are strings, we need to do a little more work.
    xvar <- movies()[, input$xvar]
    yvar <- movies()[, input$yvar]
    
    p <- movies() %>% 
      ggplot(aes(
        x = xvar,
        y = yvar,
        fill = has_oscar,
        colour = has_oscar,
        ### Answer to Exercise 2:
        size = BoxOffice,
        text = paste0(
          "<b>", Title, "</b><br>",
          "Year: ", Year, "<br>",
          "Box Office: $", round(BoxOffice / 1000000, digits = 1), "m"
        )
      )) +
      geom_point(shape = 21) +
      ### Answer to Exercise 2:
      scale_size() +
      scale_fill_viridis(discrete=TRUE, option="C", name = "Won an Oscar", labels = c("Yes", "No"), alpha = 0.7) +
      scale_colour_viridis(discrete=TRUE, guide="none", option="C", alpha = 0.9) +
      theme_ipsum() +
      ylab(paste(yvar_name)) +
      xlab(paste(xvar_name)) +
      guides(fill=guide_legend(title="Won an Oscar"))
    
    ggplotly(p, tooltip = "text", height = 400)
  })
  
  output$n_movies <- renderText({ nrow(movies()) })
}



# Compiling the App -------------------------------------------------------

shinyApp(ui = ui, server = server)
