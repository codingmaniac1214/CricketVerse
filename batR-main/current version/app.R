#---- batR ----
# a tool to analyse batting statistics

#---- libraries ----
library(shiny)
library(bslib)
library(cricketdata)
library(tidyverse)
library(DT)
library(shinyjs)
library(plotly)
library(shiny.semantic)
library(sass)


#---- load data ----
mens_t20_data <- read_rds("data/mens_ball_by_ball_data.rds")
womens_t20_data <- read_rds("data/womens_ball_by_ball_data.rds")

# vector of tournaments for men and women
tournaments <- cricsheet_codes
mens_t20_tournaments <- tournaments |>
  select(code) |>
  filter(code %in% c("t20s", "it20s", "ipl", "apl", "bbl", "bpl", "cpl", "ctc", "ilt", "ipl", "ipt", "lpl", "msl", "ntb", "psl", "sat", "sma", "ssm")) |>
  pull(code)

womens_t20_tournaments <- tournaments |>
  select(code) |>
  filter(code %in% c("t20s", "blz", "cec", "frb", "wbb", "wcl", "wsl", "wtc")) |>
  pull(code)


#---- functions ----
# get ball by ball data of a player. Generates a list where each item is an innings
source("functions/find_bbb.R")

# two functions: one to filter out wides and put each ball in a column for an innings, one to apply that function to a list of innings and join the tibbles to create ball by ball data
source("functions/career_bbb.R")

# calculate mean runs scored and mean SR by ball faced
source("functions/career_mean_bbb.R")

# calculate mean runs scored and mean SR by ball faced broken down by tournament
source("functions/tournament_mean_bbb.R")

# create a tibble of all the innings a player has played
source("functions/innings_table.R")

# calculate balls per boundary
source("functions/balls_per_boundary.R")

# calculate dot ball %
source("functions/dot_ball_percentage.R")

# career summary table
source("functions/career_summary_table.R")

# spider plot
source("functions/spider_plot.R")

# metrics by tournament
source("functions/metrics_by_tournament.R")

# ball by ball data by phase of a player
source("functions/phase_bbb.R")

# metrics by phase
source("functions/metrics_by_phase.R")

# spider plot by phase
source("functions/spider_plot_by_phase.R")

# find initials
source("functions/find_initials.R")




#---- css ----
#my_font <- list("my-font" = font_google("Barlow"))
# css <- sass(
#   sass_file("www/styles.scss"),
#   output = "www/styles.css"
# )



#---- UI ----
ui <- semanticPage(
  title = "batR",
  
  # tags$head(
  #   tags$link(href = css, rel = "stylesheet", type = "text/css")
  # ),
  
  useShinyjs(),
  
  titlePanel("batR"),
  
  tabset(
    menu_class = "ui secondary pointing menu",
    tabs = 
      list(
        list(menu = "batR",
             content = 
               div(class = "ui basic segment",
                 div(class = "ui raised segment",
                   fluidRow(
                   column(12,
                          div(class = "ui top attached label",
                            h3("Player search")
                          ),
                          br(),
                          br(),
                          div(
                            fluidRow(
                              div(class = "ui grid",
                                  div(class = "two column row",
                                      div(class = "eight wide column",
                                          textInput(inputId = "player_selected",
                                                    label = "Search for a player",
                                                    placeholder = "E.g. MEK Hussey")),
                                      div(class = "eight wide column",
                                          multiple_radio(input_id = "male_or_female", 
                                                         label = "Male or female player?",
                                                         choices = c("male", "female"), 
                                                         selected = "male",
                                                         type = "radio",
                                                         position = "inline"))
                                  )
                              )
                            )
                          ),
                          helpText("The name of the player must be entered in the form of their full initials + surname. E.g. `CH Gayle`. You can search for a player of interest's name in this form on the Find Player page."),
                          br(),
                          br(),
                          div(actionButton(inputId = "find_data", 
                                           label = "Find data",
                                           class = "ui orange button"),
                                   hidden(div(id = "loading_spinner", 
                                              class = "ui active inline loader", 
                                              style = "display: inline-block")),
                                   style = "display:inline"),
                          br(),
                          uiOutput(outputId = "innings_warning"),
                          br()
                    )
                 )
                 ),
            
                 div(class = "ui raised segment",
                   fluidRow(
                   column(12,
                          div(class = "ui olive top attached label",
                              h3("Career statistics")
                          ),
                          br(),
                          h4("Career Summary"),
                          div(class = "ui primary placeholder segment",
                            div(plotOutput(outputId = "career_summary_plot"))
                          ),
                          tableOutput(outputId = "career_summary_table"),
                    )
                  ),
             
                   fluidRow(
                     column(12,
                            h4("Breakdown player statistics by:"),
                            tags$div(selectInput(inputId = "stats_breakdown_option",
                                                 label = "Options",
                                                 choices = c("None selected", "Tournament", "Phase"),
                                                 selected = "None selected",
                                                 multiple = F),
                                 hidden(tags$div(id = "loading_spinner_breakdown_option", 
                                                 class = "ui active inline loader",
                                                 style = "display: inline-block")),
                                 style = "display:inline"),
                            br(),
                            conditionalPanel(condition = "input.stats_breakdown_option == 'Tournament'",
                                             h5("Tournament"),
                                             plotOutput(outputId = "tournament_summary_plots"),
                                             tableOutput(outputId = "tournament_summary_table")
                                             ),
                            conditionalPanel(condition = "input.stats_breakdown_option == 'Phase'",
                                             h5("Phase"),
                                             plotOutput(outputId = "phase_summary_plots"),
                                             tableOutput(outputId = "phase_summary_table")
                                             )
                      )
                    )
                 ),
             
             div(class = "ui raised segment",
               fluidRow(
                 column(12,
                        div(class = "ui olive top attached label",
                            h3("Ball by ball analysis")
                        )
                 )
               ),
               br(),
               br(),
               div(class = "ui grid",
                 div(class = "two column row",
                     div(class = "six wide column",
                         div(h4("Career ball by ball analysis"),
                             div(class = "ui primary placeholder segment",
                                 dataTableOutput(outputId = "ball_by_ball_table")))),
                     div(class = "ten wide column",
                         div(h4("Career strike rate by ball"),
                             p("The horizontal line is the player's career strike rate and the black line is a model of how their typical innings progresses (steeper black line = greater acceleration). Where the two lines meet indicates how many balls it takes the player to reach their mean strike rate."),
                             div(class = "ui primary placeholder segment",
                                 plotOutput(outputId = "ball_by_ball_SR_plot"),
                                 textOutput(outputId = "balls_to_mean_SR"))))
                  )
               ),
               br(),
               fluidRow(
                     column(12,
                            h4("Strike rate by ball across tournaments"),
                            p("The horizontal line is the player's career strike rate and the other lines are models of how their typical innings progresses at each tournament."),
                            div(class = "ui primary placeholder segment",
                                plotlyOutput("tournament_ball_by_ball_SR_plot")
                            )
                      )
               )
            )
             ),
             id = "batR_tab"
        ),
        
        list(menu = "Find Player",
             content = 
               div(class = "ui raised segment",
                 h3("Find Player"),
                 sidebar_layout(
                   sidebar_panel(
                     p("On this page, you can search for the initials of a player whose stats you would like to analyse. You can type in the full name of the player or just their surname and the table opposite will give you their name in the form of initials + surname, which you will need to use batR."),
                     textInput(inputId = "player_to_find", 
                               label = "Search for a player's initials",
                               placeholder = "E.g. `Perry`, `E Perry` or `Ellyse Perry`"),
                     br(),
                     actionButton(inputId = "find_player",
                                  label = "Find player",
                                  class = "ui orange button"),
                     hidden(tags$div(id = "loading_spinner_2", 
                                     class = "ui active inline loader",
                                     style = "display: inline-block"))
                  ),
               
               main_panel(
                 h4("Search results"),
                 tableOutput(outputId = "players_found_table"),
                 uiOutput(outputId = "players_found_warning")
               )
             )
             ),
             id = "Find_Player_tab"
          ),
        
        list(menu = "About",
             content = 
               div(class = "ui raised segment",
                   h3("About"),
                   tags$hr(),
                   p("batR is a tool to analyse batting data from T20 matches."),
                   p("With big money being poured into T20 cricket around the world, using data to improve decision-making is becoming more and more important to acquire the right players and set them up for success. Similarly, for the armchair experts, there is more data than ever to form opinions grounded in reality rather than a handful of exceptions that stick in your mind. But not everyone has the technical background or resources to analyse all the data available. batR is for the key decision-makers and fans alike whether you're settling a debate with your mates, sitting at the auction table or picking your country's best 15 for the World Cup, batR is a tool for anyone to analyse batting statistics and compare players.
                     
                     To use batR, simply enter the name of a player in the form initials + surname. The app then computes summary statistics for the player as well as their ball by ball statistics. The data used are up to date as of 4th May 2023."),
                   br(),
                   h5("Statistics calculated"),
                   tags$hr(),
                   tags$ul(
                     tags$li("Ball per boundary: quite simply in T20 cricket, the team that hits the most boundaries tends to win."),
                     tags$li("Dot ball percentage: with only 120 balls to score from, it's critical that batters score from as many balls as possible, even if it's just 1 run. Losing sides often have high dot ball percentages."),
                     tags$li("Mean runs: we don't calculate the traditional batting average here. We calculate the mean number of runs scored per innings. The batting average we generally use in cricket is a measure of how many runs a batter scores per dismissal but when it's a new game, you don't get to carry over your score from the last game if you were not out so for many players the idea of how many runs they score per game is inflated."),
                     tags$li("Median runs: mean runs scored tells us an average of how many runs a player score per innings but it's not a perfect metric. The number of runs scored by players fluctuates so some averages are propped up by a handful of brilliant innings, which masks a lot of low scores. The median gives us a better idea of how consistent the batter is by telling us what 50% of the scores are greater (or less) than."),
                     tags$li("Mean SR: the usual career strike rate of player."),
                     tags$li("Median SR: just as with runs, strike rates can be inflated by a few extraordinary knocks. Knowing what the median is gives us a better idea of what a player's strike rate is when they get out. Batter's with low medians could be chewing up a lot of balls and scoring not many."),
                     tags$li("Mean balls faced: the number of balls faced a player faces per innings. If a batter tends to face few deliveries, they will ideally have high strike rates."),
                     tags$li("Median balls faced: another measure for the number of balls a player faces per innings."),
                     tags$li("Acceleration: how quickly a batter's strike rate increases as the innings goes on"),
                     tags$li("BASRA: stands for batting average and strike rate aggregate. As runs aren't the only currency in T20 cricket, BASRA helps us compare two players by taking into account strike rates too.")
                    ),
                   br(),
                   h5("Coming soon"),
                   tags$hr(),
                   tags$ul(
                     tags$li("Further ways to break down player stats"),
                     tags$li("Side-by-side player comparisons")
                    )
                ),
             id = "About_tab"
          )
        )
    )
)


#---- server ----
server <- function(input, output){
  #---- batR reactives ----
  #--- get ball by ball data for the player selected ---
  # when the find data button is clicked, give the user feedback
  observeEvent(input$find_data, {
    disable(id = "find_data")
    show(id = "loading_spinner")
    delay(5000, {
      enable(id = "find_data")
      hide(id = "loading_spinner")
      })
  })
  
  # get the data
  innings_list <- eventReactive(input$find_data, {
    find_bbb(player_name = input$player_selected, gender = input$male_or_female, mens_t20_data = mens_t20_data, womens_t20_data = womens_t20_data)
  })
  
  # render a warning when innings_list is length 0
  output$innings_warning <- renderUI({
    if(length(innings_list()) < 1){
      div(icon("exclamation-circle"), "Warning: please check that you have selected the correct gender for the player or check the spelling of their name", style = "color:red")
    } else{
      div("", style = "color:red")
    }
  })
  
  #--- create ball by ball data ---
  ball_by_ball_data <- reactive({
    if(length(innings_list()) > 0){
      career_bbb(innings_list())
      }
  })
  
  #--- number of innings played ---
  innings_n <- reactive({
    nrow(ball_by_ball_data())
  })
  
  #--- mean runs scored and mean SR by ball faced ---
  ball_by_ball_mean <- reactive({
    career_mean_bbb(ball_by_ball_data())
  })
  
  #--- linear model for strike rate by ball number ---
  model <- reactive({
    lm(ball_by_ball_mean()$`mean SR` ~ ball_by_ball_mean()$ball)
  })
  
  #--- mean runs scored and mean SR by ball faced at each tournament ---
  tournament_ball_by_ball_mean <- reactive({
    tournament_mean_bbb(ball_by_ball_data())
  })
  
  #--- create tibble of all innings played by the player ---
  player_innings <- reactive({
    innings_table(ball_by_ball_data())
  })
  
  #--- calculate mean runs scored ---
  mean_runs_scored <- reactive({
    round(mean(player_innings()$`total scored`), 2)
  })
  
  #--- calculate median runs scored ---
  median_runs_scored <- reactive({
    median(player_innings()$`total scored`)
  })
  
  #--- calculate mean SR ---
  mean_SR <- reactive({
    round(mean(player_innings()$SR), 2)
  })
  
  #--- calculate median SR ---
  median_SR <- reactive({
    median(player_innings()$SR)
  })
  
  #--- calculate mean balls faced ---
  mean_balls_faced <- reactive({
    round(mean(player_innings()$`balls faced`), 2)
  })
  
  #--- calculate median balls faced ---
  median_balls_faced <- reactive({
    median(player_innings()$`balls faced`)
  })
  
  #--- calculate ball per boundary ---
  boundary_rate <- reactive({
    balls_per_boundary(ball_by_ball_data())
  })
  
  #--- calculate dot ball percentage ---
  dbp <- reactive({
    dot_ball_percentage(ball_by_ball_data())
  })
  
  #--- calculate balls taken to reach mean SR ---
  balls_to_reach_mean_SR <- reactive({
    round((mean_SR()-model()$coefficients[1])/model()$coefficients[2], 1)
  })
  
  #--- career summary table ---
  player_summary_table <- reactive({
    career_summary_table(ball_by_ball_data(), player_innings(), model())
  })
  
  #--- breakdown option spinner ---
  observeEvent(input$stats_breakdown_option, {
    disable(id = "stats_breakdown_option")
    show(id = "loading_spinner_breakdown_option")
    delay(2500, {
      enable(id = "stats_breakdown_option")
      hide(id = "loading_spinner_breakdown_option")
    })
  })
  
  #--- tournament summary table ---
  list_metrics_by_tournament_summary <- reactive({
    ball_by_ball_by_tournament <- split(ball_by_ball_data(), ball_by_ball_data()$tournament)
    tournament_summary <- lapply(ball_by_ball_by_tournament, metrics_by_tournament)
  })
  
  metrics_by_tournament_summary_table <- reactive({
    y <- tibble()
    for (i in 1:length(list_metrics_by_tournament_summary())){
      x <- list_metrics_by_tournament_summary()[[i]] |>
        mutate(tournament = names(list_metrics_by_tournament_summary()[i]),
               .before = 1)
      y <- rbind(y, x)
    }
    return(y)
  })
  
  #--- ball by ball by phase ---
  player_bbb_by_phase <- reactive({
    phase_bbb(innings_list())
    })
  
  #--- phase summary table ---
  metrics_by_phase_summary_table <- reactive({
    metrics_by_phase_summary <- metrics_by_phase(player_bbb_by_phase())
  })
  
  #---- batR outputs ----
  #--- career summary plot ---
  output$career_summary_plot <- renderPlot({
    spider_plot(player_summary_table(), player_innings())
  })
  
  #--- career summary table ---
  output$career_summary_table <- renderTable({
    player_summary_table()
  })
  
  #--- tournament summary plot ---
  output$tournament_summary_plots <- renderPlot({
    par(mfrow = c(ceiling(length(list_metrics_by_tournament_summary())/3), 3))
    for (i in 1:length(list_metrics_by_tournament_summary())){
      spider_plot(list_metrics_by_tournament_summary()[[i]], player_innings(), title = names(list_metrics_by_tournament_summary()[i]))
    }
  })
  
  #--- tournament summary table ---
  output$tournament_summary_table <- renderTable({
    metrics_by_tournament_summary_table()
  })
  
  #--- phase summary plot ---
  output$phase_summary_plots <- renderPlot({
    spider_plot_by_phase(player_bbb_by_phase(), metrics_by_phase_summary_table())
  })
  
  #--- phase summary table ---
  output$phase_summary_table <- renderTable({
    metrics_by_phase_summary_table()
  })
  
  #--- ball by ball table ---
  output$ball_by_ball_table <- renderDataTable({
    ball_by_ball_mean()
  }, options = list(pageLength = 10), rownames = F)
  
  #--- ball by ball SR plot ---
  output$ball_by_ball_SR_plot <- renderPlot({
    ggplot(ball_by_ball_mean()) +
      geom_point(aes(ball, `mean SR`)) +
      geom_hline(yintercept = mean_SR(), linetype = "dashed", alpha = 0.5) +
      scale_x_continuous(n.breaks = nrow(ball_by_ball_mean())/5) +
      geom_abline(slope = model()$coefficients[2], intercept = model()$coefficients[1], alpha = 0.5) +
      theme(plot.background = element_rect(fill = "#FBFFF1"),
            panel.background = element_rect(fill = "#FBFFF1"),
            axis.text = element_text(colour = "black"),
            axis.line = element_line(colour = "black"),
            axis.ticks = element_line(colour = "black"))
  })
  
  #--- balls to reach mean SR ---
  output$balls_to_mean_SR <- renderText({
    paste("On average", input$player_selected, "takes", balls_to_reach_mean_SR(), "balls to reach their mean SR.")
  })
  
  #--- ball by ball SR split by tournament ---
  output$tournament_ball_by_ball_SR_plot <- renderPlotly({
    ggplotly(ggplot(tournament_ball_by_ball_mean()) +
      geom_smooth(aes(ball, `mean SR`, colour = tournament), method = "lm", se = F) +
      geom_hline(yintercept = mean_SR(), linetype = "dashed", alpha = 0.5) +
      scale_x_continuous(n.breaks = nrow(ball_by_ball_mean())/5) +
      geom_abline(slope = model()$coefficients[2], intercept = model()$coefficients[1], alpha = 0.5)+
      theme(plot.background = element_rect(fill = "#FBFFF1"),
            panel.background = element_rect(fill = "#FBFFF1"),
            axis.text = element_text(colour = "black"),
            axis.line = element_line(colour = "black"),
            axis.ticks = element_line(colour = "black"),
            legend.background = element_blank(),
            legend.position = "top"))
  })
  
  
  
  #---- Find Player reactives ----
  #--- Search for the initials of a player ---
  # when the button is clicked, give the user feedback
  observeEvent(input$find_player, {
    disable(id = "find_player")
    show(id = "loading_spinner_2")
    delay(3000, {
      enable(id = "find_player")
      hide(id = "loading_spinner_2")
    })
  })
  
  # search for players that match the search string
  players_found <- eventReactive(input$find_player, {
    find_initials(input$player_to_find)
  })
  
  #---- Find Player outputs ----
  # render a warning when players_found is length 0
  output$players_found_warning <- renderUI({
    if(length(players_found()) < 1){
      div(icon("exclamation-circle"), "Unfortunately, we cannot find a player with that name.", style = "color:red")
    } else{
      div("", style = "color:red")
    }
  })
  
  # render the table
  output$players_found_table <- renderTable({
    players_found()
  })
  
}


#---- app ----
shinyApp(ui, server)