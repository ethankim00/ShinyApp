
library(shiny)

library(gsheet)
# Pull Data From Scraper and Google Sheets
hudsdata<-gsheet2tbl('https://docs.google.com/spreadsheets/d/1S3vFDul1PeB84Zd8G5ewe1ocZ6ezidQzAp5gn1eiHtQ/edit#gid=0')
hudsdata = data.frame(hudsdata)
# Add weekday column
hudsdata$Weekday = weekdays(as.Date(hudsdata$Date, '%m-%d-%Y'))

# Subset by Meal
breakfast = subset(hudsdata, hudsdata$Meal == "Breakfast")
lunch = subset(hudsdata, hudsdata$Meal == "Lunch")
dinner = subset(hudsdata, hudsdata$Meal == "Dinner")
#Get lists of foods by meals
breakfast.meals = unique(breakfast$Food)
lunch.meals = unique(lunch$Food)
dinner.meals = unique(dinner$Food)

# create dataframe for Breakfast
breakfast.byweekday= matrix(nrow = 7, ncol = length(breakfast.meals))
weekdays1 = c('Monday',"Tuesday", "Wednesday", "Thursday", "Friday",    "Saturday" , "Sunday" )
for (i in 1:length(breakfast.meals)){
  for (j in 1:length(weekdays1)){
    g = subset(breakfast, breakfast$Weekday == weekdays1[j])
    h = subset(g, g$Food == breakfast.meals[i] )
    breakfast.byweekday[j,i] = length(h$Food)
  }
}
df.breakfast = as.data.frame(breakfast.byweekday)
colnames(df.breakfast) <- breakfast.meals
rownames(df.breakfast) <- weekdays1
weekdays.shortened = c("Mon",    "Tue" ,  "Wed", "Thu" , "Fri" ,   "Sat"  ,"Sun")

# Create Data Frame for Lunch
lunch.meals = unique(lunch$Food)
lunch.byweekday= matrix(nrow = 7, ncol = length(lunch.meals))
for (k in 1:length(lunch.meals)){
  for (l in 1:length(weekdays1)){
    d = subset(lunch, lunch$Weekday == weekdays1[l])
    e = subset(d, d$Food == lunch.meals[k])
    lunch.byweekday[l,k] = length(e$Food)
  }
}
df.lunch = as.data.frame(lunch.byweekday)
colnames(df.lunch) <- lunch.meals
rownames(df.lunch) <- weekdays1

# Create Data Frame for Dinner
dinner.meals = unique(dinner$Food)
dinner.byweekday= matrix(nrow = 7, ncol = length(dinner.meals))
for (i in 1:length(dinner.meals)){
  for (j in 1:length(weekdays1)){
    g = subset(dinner, dinner$Weekday == weekdays1[j])
    h = subset(g, g$Food == dinner.meals[i] )
    dinner.byweekday[j,i] = length(h$Food)
  }
}
df.dinner = as.data.frame(dinner.byweekday)
colnames(df.dinner) <- dinner.meals
rownames(df.dinner) <- weekdays1



# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("HUDS Menu Tool"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Foods by Day of the Week"),
      
      
    
   
      selectInput("selected_meal", 
                  label = "Choose Meal",
                  choices = c("Breakfast", 
                              "Lunch",
                              "Dinner"),
                  selected = "Breakfast"),
      
      selectInput('Food', 'Choose Food', choices = NULL, selected = "Blueberries")
      
    ),
    
    mainPanel(
      #textOutput("selected_var"),
      textOutput("dddd"),
      plotOutput("plot")
      
    )
  )
)

server <- function(input, output, session) {
  

meals <- eventReactive(input$selected_meal, 
    switch(input$selected_meal,
           "Breakfast" = breakfast.meals,
            "Lunch" = lunch.meals,
           "Dinner" = dinner.meals
          )
                      )




observe({
updateSelectInput(session, 'Food', choices = meals(), selected = NULL)
})
  
meals_data.frame <- eventReactive(input$selected_meal, 
                                  switch(input$selected_meal,
                                         "Breakfast" = df.breakfast,
                                         "Lunch" = df.lunch,
                                         "Dinner" = df.dinner
                                  )
)



# Create Plot Object
observe({
output$plot <- renderPlot({
  barplot(meals_data.frame()[,which(colnames(meals_data.frame()) == input$Food)], names = weekdays.shortened, ylab = 
            'Freqency', xlab = input$Food, col = heat.colors(7, alpha = 0.5))
})
})


  
}

shinyApp(ui, server)