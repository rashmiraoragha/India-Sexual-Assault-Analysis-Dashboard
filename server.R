library('shiny')
library('shinydashboard')
library('ggplot2')
library('data.table')

# Set WD

# Read required files into R
crime01to12 <- read.csv('Incest_other_rape_victim.csv')

crime13 <- read.csv('Incest_other_rape_victim_2013.csv')

crimeVictimRelations <- read.csv('Offenders_relation_to_rape_victim.csv')

crimeDF <- rbind(crime01to12, crime13)
crimeDFBAK <- crimeDF

colnames(crimeDF) <- c(
  'state',
  'year',
  'crime_type',
  'cases_reported',
  'victim_under_10',
  'victim_10_to_14',
  'victim_14_to_18',
  'victim_18_to_30',
  'victim_30_to_50',
  'victim_over_50',
  'total_victims'
)

crimeDF <- crimeDF[crimeDF$crime_type != 'Total',]

crimeDF$display_state <- trimws(crimeDF$state)
crimeDF$state <- trimws(crimeDF$state)
crimeDF$year <- trimws(crimeDF$year)
crimeDF$state <- tolower(crimeDF$state)
crimeDF$state <- gsub(' ', '', crimeDF$state)
crimeDF <- crimeDF[!(crimeDF$state %like% 'total'),]

crimeDF$key = paste(crimeDF$state, crimeDF$year, sep="_")

colnames(crimeVictimRelations) <- c(
  'state',
  'year',
  'known_by_victim',
  'parents_or_close_family',
  'relatives',
  'neighbours',
  'other_known'
)

crimeVictimRelations$state <- trimws(crimeVictimRelations$state)
crimeVictimRelations$year <- trimws(crimeVictimRelations$year)
crimeVictimRelations$state <- tolower(crimeVictimRelations$state)
crimeVictimRelations$state <- gsub(' ', '', crimeVictimRelations$state)

crimeVictimRelations$key = paste(crimeVictimRelations$state, crimeVictimRelations$year, sep="_")

dropCols <- c('state', 'year')
crimeVictims <- crimeVictimRelations[, !(colnames(crimeVictimRelations) %in% dropCols)]

masterCrimeDF <- merge(crimeDF, crimeVictims, by = 'key', all.x = T)
masterCrimeDF$crime_type <- as.character(masterCrimeDF$crime_type)

uniqStates <- unique(masterCrimeDF$display_state)
uniqYears <- unique(masterCrimeDF$year)
uniqCrimes <- unique(masterCrimeDF$crime_type)

states <- c('All', uniqStates)
years <- c('All',uniqYears)
crimeTypes <- c('All', uniqCrimes)

# Metrics
# 1 - Year with Most Rapes
metric1 <- group_by(masterCrimeDF, year) %>% 
  summarize(total_rapes = sum(cases_reported)) %>%
  arrange(desc(total_rapes))

metric1Key <- 'Year with Most Rape Crimes'
metric1Val <- as.character(head(metric1, 1)[1,1])

# 2 - State with Most Rapes
metric2 <- group_by(masterCrimeDF, display_state, year) %>% 
  summarize(total_rapes = sum(cases_reported)) %>%
  arrange(desc(total_rapes))

metric2Key <- as.character(head(metric2, 1)[1,1])
metric2Val <- as.character(head(metric2, 1)[1,3])
metric2Val <- formatC(as.integer(metric2Val), format="d", big.mark=",")
metric2Key <- paste('State with Most Rape Crimes -', metric2Key)
metric2Val <- paste0(metric2Val, ' (in ', as.character(head(metric2, 1)[1,2]), ')')

# 3 - Most Common Rape Age Group

capitalize <- function(x) {
  s <- gsub('_', ' ', x)
  s <- strsplit(s, " ")[[1]]
  capWord <- paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
  return(capWord)
}

colnames(masterCrimeDF)
colNames <- c(
  'victim_under_10', 
  'victim_10_to_14',
  'victim_14_to_18',
  'victim_18_to_30',
  'victim_30_to_50',
  'victim_over_50'
  )

a = list()
for(col in colNames){
i=0
  i = i+1
  a[i] = sum(masterCrimeDF[col])
}
a <- unlist(a)
metric3DF <- data.frame(age = colNames, num_of_rapes = a)
metric3DF <- metric3DF %>% arrange(desc(num_of_rapes))

metric3Key <- as.character(head(metric3DF, 1)[1,1])
metric3Key <- capitalize(metric3Key)
metric3Val <- as.character(head(metric3DF, 1)[1,2])
metric3Val <- paste0(metric3Key, ' years')
metric3Key <- 'Most Common Rape Victim Group'

# 4 - Most number of Rapes in a Year
metric4Key <- 'Highest Number of Rape Cases in a Year'
metric4Val <- as.character(head(metric1, 1)[1,2])
metric4Val <- formatC(as.integer(metric4Val), format="d", big.mark=",")

# 5 - Most Common Rape Assailants
assailantCategories <- c(
  'known_by_victim', 
  'parents_or_close_family',
  'relatives',
  'neighbours',
  'other_known'
)

assailantCategoriesClean = list()
b = list()
i=0
for(col in assailantCategories){
  i = i+1
  b[i] = sum(masterCrimeDF[col], na.rm = TRUE)
  assailantCategoriesClean[i] <- capitalize(col)
}
b <- unlist(b)
assailantCategoriesClean <- unlist(assailantCategoriesClean)
metric5DF <- data.frame(assailants = assailantCategories, num_of_rapes = b)
metric5DF <- metric5DF %>% arrange(desc(num_of_rapes))

metric5Key <- as.character(head(metric5DF, 1)[1,1])
metric5Val <- as.character(head(metric5DF, 1)[1,2])
metric5Key <- capitalize(metric5Key)
metric5Val <- metric5Key
metric5Key <- 'Most Common Rape Assailant Group'

assailantDF <- data.frame(
  assailant_cat = assailantCategories,
  assailant_cat_clean = assailantCategoriesClean
  )

shinyServer(
  
  function(input, output, session) {
  
  output$progressBox1 <- renderValueBox({
    valueBox(
      metric1Val, 
      metric1Key, 
      icon = icon("calendar-alt"),
      color = "blue"
    )
  })
  
  output$progressBox2 <- renderValueBox({
    valueBox(
      metric2Val, 
      metric2Key, 
      icon = icon("map-marked-alt"),
      color = "blue"
    )
  })
  
  output$progressBox3 <- renderValueBox({
    valueBox(
      metric3Val, 
      metric3Key, 
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$progressBox4 <- renderValueBox({
    valueBox(
      metric4Val, 
      metric4Key, 
      icon = icon("exclamation-triangle"),
      color = "blue"
    )
  })
  
  output$progressBox5 <- renderValueBox({
    valueBox(
      metric5Val, 
      metric5Key, 
      icon = icon("address-book"),
      color = "blue"
    )
  })
  
  output$progressBox6 <- renderValueBox({
    valueBox(
      'DATA.GOV.IN', 
      '- Data Source', 
      icon = icon("city"),
      color = "blue"
    )
  })
  
  tmpDF <- reactive({
    
    if (is.null(input$uiStates)) return()
    if (is.null(input$uiYears)) return()
    if (is.null(input$uiCrimes)) return()
    
    tmpStates <- input$uiStates
    tmpYears <- input$uiYears
    tmpCrimes <- input$uiCrimes
    
    if (any(tmpStates %in% "All")) {
      tmpStates <- uniqStates
    }
    
    if (any(tmpYears %in% "All")) {
      tmpYears <- uniqYears
    } 
    
    if (any(tmpCrimes %in% "All")) {
      tmpCrimes <- uniqCrimes
    } 
    
    tmp <- masterCrimeDF
    tmp <- tmp[tmp$display_state %in% tmpStates,]
    tmp <- tmp[tmp$year %in% tmpYears,]
    tmp <- tmp[tmp$crime_type %in% tmpCrimes,]
    
    return(tmp)
  })
  
  output$barPlot <- renderPlot({
    ggplot(data = tmpDF(), aes(x = year, y = cases_reported, fill = crime_type)) +
      geom_bar(stat="identity")+theme_minimal()+
      theme(axis.text.x = element_text(angle = 90))
  })
  
  output$piePlot <- renderPlot({
    
    pieDF <- tmpDF()
    pieDF <- pieDF[,c('display_state', assailantCategories)]
    tempPieDF <- pieDF[!duplicated(pieDF),assailantCategories]
    assailantSums <- colSums(tempPieDF, na.rm = TRUE)
    piePlotDF <- data.frame(
      assailant_category = assailantCategoriesClean, 
      number_of_crimes = assailantSums,
      row.names = NULL
      )
    
    ggplot(data = piePlotDF, aes(x="", y=number_of_crimes, fill=assailant_category)) + geom_bar(stat="identity", width=1)+ coord_polar("y", start=0) + 
      geom_text(aes(label = number_of_crimes), position = position_stack(vjust = 0.5)) + theme_void()
    
  })
  
  }
)