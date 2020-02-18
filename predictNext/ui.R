#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
require(shiny)

require(data.table)

# Define UI for application that draws a histogram
shinyUI(navbarPage(
    strong("Data Science Capstone Project"),
    tabPanel(
        strong("Word Prediction"),
        
        fluidPage(
            # Application title
            titlePanel("Predict next word in a sentense"),
            
            # Sidebar with a slider input for number of bins
            sidebarLayout(
                sidebarPanel(
                    h5("Input the first part of phrase/sentence"),
                    textInput(
                        inputId = "Phrase",
                        placeholder = "Type a sentense here",
                        width = 300,
                        label = "Phrase:"
                    ),
                    sliderInput(
                        inputId = "TopN",
                        value = 10,
                        min = 1,
                        max = 100,
                        label = "Select Top N",
                        step = 1
                    ),
                    checkboxInput(
                        inputId = "Simple",
                        value = FALSE,
                        label = "Make Rough Estimate(Fast response)"
                    ),
                    checkboxInput(
                        inputId = "Discounting",
                        value = TRUE,
                        label = "Enable Discounting"
                    ),
                    submitButton("Predict next word"),
                    p("Note: wait 15 s for initial loading")
                ),
                
                # Show a plot of the generated distribution
                mainPanel(tableOutput("predictedWords"))
            )
        )
    ),
    tabPanel(
        strong("I N S T R U C T I O N S"),
        headerPanel("Instructions"),
        mainPanel(
            p(
                "1. Enter a sentence in the 'Phrase' input box on the 'Word Prediction' tab"
            ),
            p("2. Press the 'Predict next word' button"),
            p("3. The most probable words will appear on the right side"),
            hr(),
            h2("Limitations:"),
            tags$div(
                "This Shiny app is built on",
                strong("3-gram"),
                "predictive language model.",
                "It ignores common stop words (frequent words like 'the' in English laguage),",
                "all punctuation marks, white spaces (like line breaks or tabs),",
                "and words of one or two characters (like 'a' or 'to') in the",
                "input phrase and in results.",
                "It also ignores the grammer and does suggestions based purely on statistics."
            ),
            h4("Significant Word"),
            tags$div(
                "Significant word is defined as a word that is atleast 3 character long,",
                "that contains only alpha characters,",
                "and is not in the set of stopwords for English language.",
                "To be a significant word, the word should be part of the sentense.",
                "Any word coming before a fullstop (.) is considered to be ",
                "non-significant because it belongs to previous sentense."
            ),
            h2("Input fields and usage"),
            tags$br(),
            h3("Phrase:"),
            tags$div(
                "Phrase is the input field that gives context to the next word to be predicted.",
                "This is what the user has typed before looking for the next word",
                p(),
                tags$em("Default value: none"),
                "(Only a hint is provided. User must input a sentense/phrase into this field)",
                h4("Special Feature:"),
                "If user enters * at the end of last word, the system will recognize that ",
                "the last word is not complete and will suggest all the words that start with the",
                "few characters before the *.",
                p(),
                tags$em("Example:"),
                "If the sentense entered is: I want to g* ,",
                "then system will try to predict all the words that come after:",
                " I want to, and will suggest only the words that start with g.",
            ),
            tags$br(),
            h3("Select Top N:"),
            tags$div(
                "This slider inputs lets the user to choose the size of result set.",
                "User can select any number 1 to 100."
            ),
            p(),
            tags$em("Default value: 10"),
            tags$br(),
            h3("Make Rough Estimate(Fast response):"),
            tags$div(
                "Checking this box will cause the system default it's processing",
                "to rudimentory model of just predicting based on last two ",
                "significant words. In this case the system will try to suggest",
                "5 most frequent words after the combination of two significant words",
                "if it does not get any word suggestions, it will try to predict with",
                "last significant word. If that fails too, it will simply return",
                "the 5 most frequent words that is not part of stopwords set.",
                p(),
                tags$em("Default value: FALSE"),
                p(),
                tags$em("Note 1:"),
                "Rough estimate is considered to be the baseline model, against which",
                "other statistical models can be compared.",
                p(),
                tags$em("Note 2:"),
                "Please note that (Special Feature) and (Select Top N) will",
                "not work with this option."
                
            ),
            
            tags$br(),
            h3("Enable Discounting:"),
            tags$div(
                "If this box is checked, the statistical model will make use of",
                strong("Back-off Smoothing"),
                "for cases when input phrase is absent in training set.",
                "If this box is unchecked, system will only use a linear model",
                "based on probabilities of ", strong("1-gram"), ",",
                strong("2-gram"), ", or",strong("3-gram"),
                "models that were built on training set."
            ),
            p(),
            tags$em("Default value: TRUE"),
            p(),
            tags$div(
                tags$em("Note:"),
                "In most cases user may not see significant changes in the",
                "list of words suggested by checking or un-checking this box,",
                "but may observe slight changes in the probability in lower digits."
            ),
            hr()
        )
    )
))