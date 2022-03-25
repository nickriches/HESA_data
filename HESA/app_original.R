#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Read in data for table 1 on the following website
# https://www.hesa.ac.uk/data-and-analysis/finances/income



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Choose Uni"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30),
            
            selectizeInput(
                inputId = 'search',
                label = 'Search for unis',
                choices = my_autocomplete_list,
                selected = NULL,
                multiple = TRUE, # allow for multiple inputs
                options = list(create = FALSE) # if TRUE, allows newly created inputs
                
            )
        ),


        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("propnPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    df <- read_csv("https://www.hesa.ac.uk/data-and-analysis/finances/table-1.csv", skip = 12)
    
    df %>% 
        mutate(income_outgoing = case_when(
            Category == "Tuition fees and education contracts" ~ "inc",
            Category == "Funding body grants" ~ "inc",
            Category == "Research grants and contracts" ~ "inc",
            Category == "Other income" ~ "inc",
            Category == "Investment income" ~ "inc",
            Category == "Donations and endowments" ~ "inc",
            Category == "Staff costs" ~ "out",
            Category == "Restructuring costs" ~ "out",
            Category == "Other operating expenses" ~ "out",
            Category == "Depreciation and amortisation" ~ "out",
            Category == "Interest and other finance costs" ~ "out",
            TRUE ~ "no_category")) %>% 
        mutate(year_ending = case_when(
            `Academic year` == "2015/16" ~ 2016,
            `Academic year` == "2016/17" ~ 2017,
            `Academic year` == "2017/18" ~ 2018,
            `Academic year` == "2018/19" ~ 2019,
            `Academic year` == "2019/20" ~ 2020,
            `Academic year` == "2020/21" ~ 2021,
            TRUE ~ 0)) %>% 
        filter(income_outgoing != "no_category") %>% # removes categories not contributing to overall income / expenditure
        filter(`HE Provider` != "Year to date total",
               `HE Provider` != "Total") %>% # remove nuisance variables from HESA data
        mutate(value = 1000*as.numeric(`Value(£000s)`)) %>% # Convert 1000s into raw numbers
        group_by(`HE Provider`, Category, income_outgoing, year_ending) %>% 
        summarise(value = mean(value)) -> # This is necessary because observations are duplicated for each HE Provider by Category by year_ending combination. The columns responsible for this are columns 3 and 4 (Country of HE Provider / Region of HE Provider)
        df
    
    df %>%
        filter(income_outgoing == "out") %>%
        group_by(year_ending) %>% 
        summarise(total_outgoings_by_year_UK = sum(value, na.rm = TRUE)) %>% 
        left_join(df, by = c("year_ending")) %>% 
        filter(income_outgoing == "out") ->
        df
    
    df %>% 
        group_by(Category, year_ending, total_outgoings_by_year_UK) %>% 
        summarise(total_outgoings_by_year_and_category_UK = sum(value, na.rm = TRUE)) %>% 
        mutate(perc = (total_outgoings_by_year_and_category_UK/
                           total_outgoings_by_year_UK) *100) -> df.outgoings.perc
    
    df %>% 
        group_by(`HE Provider`, year_ending) %>% 
        summarise(total_outgoings_by_year_and_category_provider = sum(value, na.rm=TRUE)) %>% 
        right_join(df, by = ("HE Provider")) %>% 
        mutate(perc = (value/total_outgoings_by_year_and_category_provider)*100) ->
        df
    
    my_autocomplete_list <- unique(df$`HE Provider`)
    
    
    
    unis <- reactive({
     unis <- input$search
     return(unis)
    })

    output$propnPlot <- renderPlot({
        
        g <- ggplot(data = df.outgoings.perc %>%
                        filter(year_ending != 2021,
                               Category == "Staff costs"),
                    aes(x = year_ending, y = perc))
        
        g <- g + geom_line()
        g <- g + scale_y_continuous(breaks = seq(0, 100, by = 10))
        g <- g + xlab("Year ending")
        g <- g + ylab("Percentage of income spent on staff")
        
        g <- g + geom_hline(yintercept = c(40,50,60),
                            colour = "dark blue",
                            linetype = "dashed")
        
        g <- g + geom_line(data = df %>%
                                filter(year_ending != 2021) %>%
                                filter(`HE Provider` %in% unis()),
                            )
        
        g <- g + ggtitle("Proportion of income spent on staff")
        
        g
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
