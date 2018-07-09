

library(shiny)
library(shinythemes)
library(shinydashboard)
library(plotly)
library(sunburstR)
library(sqldf)
library(rsconnect)
library(readr)
library(googleVis)

initial_data = as.data.frame(read_csv("./data/Karnataka3.csv",col_names = TRUE))

ui <- dashboardPage(
  
  dashboardHeader(title = "Whole-Part"), skin = "purple",
  
  dashboardSidebar(
    fileInput("file1", "Choose CSV File. Format: category1, category2, metric1, <any>....", multiple = FALSE,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    sidebarMenu(
    
    menuItem("River", tabName = "River", icon = icon("dashboard")),
    menuItem("Data", tabName = "Data", icon = icon("dashboard"))
    ),
    
  uiOutput("selections"), downloadButton("downloadData","Download filtered data")
   ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "River",
                uiOutput("radio"),
              uiOutput("dropdown"), tabsetPanel(
                tabPanel("Sankey"
              ,plotlyOutput("river",height = 750)), 
              tabPanel("Sun burst",sunburstOutput("sunburst")),
              tabPanel("GoogleVis",htmlOutput("gviswidget"))
      )),
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
    
    alldataBind = my_data[1,]
    for(j in 1:length(alldataBind))
    {
      if(is.numeric(alldataBind[,j]))
      {
        alldataBind[,j]=0
      }
      else
      {
        alldataBind[,j]="All"
      }
        
    }
    
    
    my_data = rbind(my_data,alldataBind)

    lapply(1:length(nonnumerics), function(i) {
      #unique function for one distinct value?? not able to add "all" in choices hence a new dataset bind

      selectInput(inputId = paste0("ind", i),label = nonnumerics[i],choices = unique(my_data[,nonnumerics[i]]) ,multiple = TRUE, selected = "All")
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
    numerics= names(Filter(is.numeric, my_data))
    allName = names(my_data)
    nonnumerics= allName[!(allName %in% names(Filter(is.numeric, my_data)))]
    
    ss= allName[1:which(allName==numerics[1])]
    
    
    #selectInput("slevel","Select 2 or more categories (Ordered, river flow and sunburst layers)",choices = nonnumerics,multiple = TRUE,selected = c(nonnumerics[1],nonnumerics[2]))
    selectInput("slevel","Select 2 or more categories (Ordered, river flow and sunburst layers)",choices = nonnumerics,multiple = TRUE,selected = ss)
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
      if(!("All" %in% input[[paste0("ind", i)]]))
      {
        my_data = my_data[my_data[,nonnumerics[i]] %in% input[[paste0("ind", i)]],]
      }
        
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
        
         if(!("All" %in% input[[paste0("ind", i)]]))
         {
          my_data = my_data[my_data[,nonnumerics[i]] %in% input[[paste0("ind", i)]],]
         }
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
      
      if(!("All" %in% input[[paste0("ind", i)]]))
      {
        my_data = my_data[my_data[,nonnumerics[i]] %in% input[[paste0("ind", i)]],]
      }
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
        
        #removing rows with zero sum
        t = sqldf(querywhile)
        t=t[t[,3]>0,]
        
        bubdata = rbind( bubdata,t)
        
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
  

  output$gviswidget <- renderGvis({
    my_data = waitDatav$my_data
    if(is.null(my_data))
      my_data=initial_data
    
    allName = names(my_data)
    nonnumerics= allName[!(allName %in% names(Filter(is.numeric, my_data)))]
    
    for(i in 1:length(nonnumerics))
    {
      
      if(!("All" %in% input[[paste0("ind", i)]]))
      {
        my_data = my_data[my_data[,nonnumerics[i]] %in% input[[paste0("ind", i)]],]
      }
    }
    
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
      
      gvisSankey(bubdata, from="Fromf", 
                                 to="Tote", weight=input$variable,
                                 options=list(
                                   
                                   #sankey="{node: { colors: [ 'Red' ] }, link:{colorMode:'gradient', colors:['lightblue','Red','Green','Yellow','Orange','Blue','Grey']}}"
                                   sankey="{link:{colors:['Grey']}}"

                                 ))
      
    }
    
  })
    
  

   output$sunburst <- renderSunburst({
     my_data = waitDatav$my_data
     if(is.null(my_data))
       my_data=initial_data
     
     allName = names(my_data)
     nonnumerics= allName[!(allName %in% names(Filter(is.numeric, my_data)))]
     
     for(i in 1:length(nonnumerics))
     {
       
       if(!("All" %in% input[[paste0("ind", i)]]))
       {
         my_data = my_data[my_data[,nonnumerics[i]] %in% input[[paste0("ind", i)]],]
       }
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

