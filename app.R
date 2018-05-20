

library(shiny)
library(shinythemes)
library(shinydashboard)
library(plotly)
library(sunburstR)
library(sqldf)

initial_data = as.data.frame(read_csv("./data/Karnataka1.csv",col_names = TRUE))

ui <- dashboardPage(
  
  dashboardHeader(title = "Whole-Part"), skin = "purple",
  
  dashboardSidebar(
    fileInput("file1", "Choose CSV File", multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    sidebarMenu(
    menuItem("Data", tabName = "Data", icon = icon("dashboard")),
    menuItem("River", tabName = "River", icon = icon("dashboard"))
    ),
    
  uiOutput("selections"), downloadButton("downloadData","Download filtered data")
   ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "River",
                uiOutput("radio"),
              uiOutput("dropdown")
              ,plotlyOutput("river")
              ,sunburstOutput("sunburst")
      ),
      tabItem(tabName = "Data", 
              DT::dataTableOutput("dtab")
      )
  )))


server <- function(input, output) {

  #observe file upload event and change dataset 'my_data' in reactive values
  observeEvent(input$file1,{
    inFile <- input$file1
  
   if (is.null(inFile))
     return(NULL)
  
   waitDatav$my_data = as.data.frame(read.csv(inFile$datapath),stringsAsFactors=FALSE)
   })

  waitDatav <-reactiveValues(my_data = NULL)
  
  #dynamically generated dropdowns based on non-numeric variables
  output$selections <- renderUI({
    my_data = waitDatav$my_data
    if(is.null(my_data))
      my_data=initial_data

    allName = names(my_data)
    nonnumerics= allName[!(allName %in% names(Filter(is.numeric, my_data)))]

    lapply(1:length(nonnumerics), function(i) {
      #unique function for one distinct value?? 

      selectInput(inputId = paste0("ind", i),label = nonnumerics[i],choices = unique(my_data[,nonnumerics[i]]) ,multiple = TRUE, selected = unique(my_data[,nonnumerics[i]]) )
    })
  })
  
  #Radio buttons for each numeric type. The metric to be used to summarize.
  output$radio <- renderUI({
    my_data = waitDatav$my_data
    if(is.null(my_data))
      my_data=initial_data
    
    #nums <- unlist(lapply(x, is.numeric)) 
    numerics= names(Filter(is.numeric, my_data))
    
    radioButtons("variable","Choose metric",choices = numerics)
    
  })
  
  #Selection of non numeric variable for the plots
  output$dropdown <- renderUI({
    my_data = waitDatav$my_data
    if(is.null(my_data))
      my_data=initial_data
    
    #nums <- unlist(lapply(x, is.numeric))
    allName = names(my_data)
    nonnumerics= allName[!(allName %in% names(Filter(is.numeric, my_data)))]
    
    selectInput("slevel","Select 2 or more categories",choices = nonnumerics,multiple = TRUE)
  })
  
  
  output$dtab <- DT::renderDataTable({
    my_data = waitDatav$my_data
    if(is.null(my_data))
      my_data=initial_data
    
    allName = names(my_data)
    nonnumerics= allName[!(allName %in% names(Filter(is.numeric, my_data)))]
    
    for(i in 1:length(nonnumerics))
    {
        #filter data based on dynamic dropdown list
        my_data = my_data[my_data[,nonnumerics[i]] %in% input[[paste0("ind", i)]],]
    }
    
    
    DT::datatable(my_data, filter = 'top',options = list(scrollX = TRUE))
  })
  
  output$downloadData <- downloadHandler(
    filename = 'whole_part.csv',
    content = function(file) {
      
      my_data = waitDatav$my_data
      if(is.null(my_data))
        my_data=initial_data
      
      allName = names(my_data)
      nonnumerics= allName[!(allName %in% names(Filter(is.numeric, my_data)))]
      
      for(i in 1:length(nonnumerics))
      {
        
        my_data = my_data[my_data[,nonnumerics[i]] %in% input[[paste0("ind", i)]],]
      } 
      
      write.csv(my_data,file)
    }
  )
  
  output$river <- renderPlotly({
    my_data = waitDatav$my_data
    if(is.null(my_data))
      my_data=initial_data
    
    allName = names(my_data)
    nonnumerics= allName[!(allName %in% names(Filter(is.numeric, my_data)))]
    
    for(i in 1:length(nonnumerics))
    {
      
      my_data = my_data[my_data[,nonnumerics[i]] %in% input[[paste0("ind", i)]],]
    }
    
    #check if more than 1 category selected
    if(length(input$slevel)>=2)
    {
      whilei = 1
      
      bubdata=my_data[0,]
      
      #rbind each dataset grouped by (i,i+1) & (i+1,i+2)...
      while(whilei<length(input$slevel))
      {
        #SUM is the aggregate function. This can be made dynamic. For sankey plots COUNT, SUM are ideal.
        querywhile = paste0("select ",input$slevel[whilei]," as Fromf, ",input$slevel[whilei+1]," as Tote, Sum(",input$variable,") as '",input$variable,"' from my_data group by ",input$slevel[whilei], ", ",input$slevel[whilei+1])
        
        bubdata = rbind( bubdata,sqldf(querywhile))
        
        whilei = whilei+1
      }
      #label for each node
      labb = unique(c(as.character(bubdata$Fromf),as.character(bubdata$Tote)))
      
      #mapping that index to source and target nodes to get source-->target mapping required for river
      #googlevis package allows to map just by names and no need for index. But googleVis currently does not have total when you hover on the node.
      bubdata$sor = match(bubdata$Fromf,labb)-1
      bubdata$tar = match(bubdata$Tote,labb)-1
      
      
      plot_ly(
        type = "sankey",
        orientation = "h",
        
        node = list(
          label = labb,
          #color = c("blue", "blue", "blue", "blue", "blue", "blue"),
          pad = 10,
          thickness = 10,
          line = list(
            color = "black", width =0.5
          )
        ),
        
        link = list(
          source = bubdata$sor,
          target = bubdata$tar,
          value =  bubdata[,colnames(bubdata) %in% input$variable]
        )
      ) %>% 
        layout(
          title = " ",
          font = list(
            size = 10
          )
        )
      
    }
    else
      plot_ly()
  


   })
  

    
    
  

   output$sunburst <- renderSunburst({
     my_data = waitDatav$my_data
     if(is.null(my_data))
       my_data=initial_data
     
     allName = names(my_data)
     nonnumerics= allName[!(allName %in% names(Filter(is.numeric, my_data)))]
     
     for(i in 1:length(nonnumerics))
     {
       
       my_data = my_data[my_data[,nonnumerics[i]] %in% input[[paste0("ind", i)]],]
     }
     
     
     if(length(input$slevel)>=2)
     {
       whilei = 1
       whilej = 2
       
       bubdata=my_data[0,]
       
       selectstr=NULL
       groupstr = NULL
       dname = NULL
       
       while(whilei<=length(input$slevel))
       {
         #Cannot just rbind like River plot, Sunburst summarizes all the levels.
         #appending strings to get the select part and group by part for sqldf query.
         dname = input$slevel[whilei]
         my_data[,c(dname)] = gsub("-","_",my_data[,c(dname)])
         my_data[,c(dname)] = gsub(" ","",my_data[,c(dname)])
         if(whilei == length(input$slevel))
         {
           selectstr = paste0(selectstr,input$slevel[whilei])
           groupstr = paste0(groupstr,input$slevel[whilei])
         }
         else
         {
           selectstr = paste0(selectstr,input$slevel[whilei],",")
           groupstr = paste0(groupstr,input$slevel[whilei],",")
         }
         whilei = whilei+1
       }
       sqry = paste0("select ",selectstr,", Sum(",input$variable,") as ",input$variable," from my_data group by ",groupstr)
       bubdata = sqldf(sqry)
       
       bubdata$col1 = bubdata[,c(input$slevel[1])]
       
       #sunburst function needs all the labels in one column/vector with categories separated by '-'
       while(whilej<=length(input$slevel))
       {
         dname1 = input$slevel[whilej]
         
         bubdata$col1 = paste0(bubdata$col1,"-",bubdata[,c(dname1)])
         whilej = whilej +1 
       }
       bubdata = bubdata[,c("col1",input$variable)]
       sunburst(bubdata)
       
     }

   })
}

shinyApp(ui = ui, server = server)
