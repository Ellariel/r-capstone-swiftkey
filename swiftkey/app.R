library(shiny)
library(stringi)
library(data.table)

remove_online_junk <- function(x){
    # replace emails and such but space
    x <- gsub("[^ ]{1,}@[^ ]{1,}"," ",x)
    x <- gsub(" @[^ ]{1,}"," ",x)
    # hashtags
    x <- gsub("#[^ ]{1,}"," ",x) 
    # websites and file systems
    x <- gsub("[^ ]{1,}://[^ ]{1,}"," ",x) 
    x}
remove_symbols <- function(x){
    # Edit out most non-alphabetical character
    # text must be lower case first
    x <- gsub("[`’‘]","'",x)
    x <- gsub("[^a-z']"," ",x)
    x <- gsub("'{2,}"," '",x)
    x <- gsub("' "," ",x)
    x <- gsub(" '"," ",x)
    x <- gsub("^'","",x)
    x <- gsub("'$","",x)
    x}
split_shortforms <- function(x){
    short_forms <- data.frame(
        "sub"=c("'d[^a-z]","'s[^a-z]"),
        "rep"=c(" 'd "," 's "))
    short_forms  <- rbind(short_forms, data.frame(
        "sub"=c("'ll[^a-z]","'re[^a-z]","'ve[^a-z]"),
        "rep"=c(" 'll "," 're "," 've ")))
    # add a space in front of short forms
    for(isf in seq(1,nrow(short_forms))){
        x <- gsub(short_forms[isf,"sub"],short_forms[isf,"rep"],x)}
    x}
by_f <-function (w) {as.character(f[w1==w[1] & w2==w[2] & w3==w[3],.(w4)][1,])}
by_t <-function (w) {as.character(t[w1==w[1] & w2==w[2],.(w3)][1,])}
by_b <-function (w) {as.character(b[w1==w[1],.(w2)][1,])}
by_u <-function (w) {as.character(u[sample(1:nrow(u), 1),1])}
back <- function (w) {switch(ifelse(identical(w, character(0)),1,length(w)+1),
           by_u(w), by_b(w), by_t(w), by_f(w))}
predict <- function (s = ""){
    if (s!=""){
        s <- tolower(s)
        s <- remove_online_junk(s)
        s <- split_shortforms(s)
        s <- remove_symbols(s)}
    w <- unlist(stri_extract_all_regex(s, "\\S+"))
    w <- tail(w,3)
    while (length(w)>=0){
        result <- back(w)
        if (is.na(result)){
            w <- tail(w,length(w)-1)
        } else {break}
    }
    result}

# Load data
load(file = "u.rda")
load(file = "b.rda")
load(file = "t.rda")
load(file = "f.rda")

ui <- fluidPage(
    titlePanel("Tiny SwiftKey ^_^''"),

    sidebarLayout(
        sidebarPanel(
            textInput("input", "Your text:", ""),
            verbatimTextOutput("value"),
            h6("Please check the manual: ",a(href="http://rpubs.com/Ellariel/swiftkey-pitch", "rpubs.com/ellariel/swiftkey-pitch")),
            h6("You can familiar with app.R in github: ",a(href="http://github.com/ellariel/r-capstone-swiftkey", "github.com/ellariel/r-capstone-swiftkey"))
        ),

        mainPanel(
            h4("Predicted word:"),
            h3(textOutput("output"))
        )
    )
)

server <- function(input, output) {
    #predict
    pred <- reactive({
        if (input$input != "")
         predict(input$input)
    })
    
    #print
    output$output <- renderText({
        pred()
    })
}

shinyApp(ui = ui, server = server)
