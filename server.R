library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  

  
  
    #infoboxes Dashboard1
  
  d1info1 =reactive({
    
    
    q1=paste(shQuote(input$d1client, type="cmd"), collapse=", ")
    q2 =paste(shQuote(input$d1store,type = "cmd"),collapse = ", ")
    query =paste0('{"client":{"$in":[',q1,']},"store":{"$in":[',q2,']}}')
    
    
    
    salesinfo$find(fields ='{"documentno":true,"_id":false,"DateOrdered":true}',query = query )
  })
  
  output$d1documentnumber = renderInfoBox({
    
    
    c=d1info1()
    
    c$month = months(as.Date(c$DateOrdered))
    c$weekday = weekdays(as.Date(c$DateOrdered))
    c = c%>%filter(month %in% input$d1month & weekday %in% input$d1weekday)%>%select(documentno)
    
    
    # c= ofc%>%filter(client %in% input$client & store %in% input$store & month %in% input$month & level %in% input$level)%>%
    #   select(orderid)
    
    infoBox("Total Transactions",value = h1(prettyNum(length(unique(c$documentno)),big.mark = ",")),icon = icon("caret-up"))
    
    
    
  })
  
  d1info2 =reactive({
    q1=paste(shQuote(input$d1client, type="cmd"), collapse=", ")
    q2 =paste(shQuote(input$d1store,type = "cmd"),collapse = ", ")
    query =paste0('{"client":{"$in":[',q1,']},"store":{"$in":[',q2,']}}')
    
    
    salesinfo$find(fields = '{"TotalNetAmount":true,"_id":false,"DateOrdered":true}',query = query)
  })
  
  
  output$d1netamount = renderInfoBox({
    
    c= d1info2()
    c$month = months(as.Date(c$DateOrdered))
    c$weekday = weekdays(as.Date(c$DateOrdered))
    c = c%>%filter(month %in% input$d1month & weekday %in% input$d1weekday)%>%select(TotalNetAmount)
    infoBox("Net Amount in millions ",value = h1(prettyNum( round(sum(c$TotalNetAmount)/1000000,2), big.mark = ","),"AED"),icon = icon("hand-holding-usd"))
    
    
    
  })
  
  d1info3 = reactive({
    
    
    q1=paste(shQuote(input$d1client, type="cmd"), collapse=", ")
    q2 =paste(shQuote(input$d1store,type = "cmd"),collapse = ", ")
    query =paste0('{"client":{"$in":[',q1,']},"store":{"$in":[',q2,']}}')
    
    
    salesinfo$find(fields = '{"customer":true,"_id":false,"DateOrdered":true}',query = query)
  })
  
  output$d1unique = renderInfoBox({
    
    c= d1info3()
    c$weekday = weekdays(as.Date(c$DateOrdered))
    c$month = months(as.Date(c$DateOrdered))
    c = c%>%filter(weekday %in%input$d1weekday & month %in% input$d1month)%>%select(customer)
    
    infoBox("Total Unique Customers ",value = h1(prettyNum(length(unique(c$customer))-1,big.mark = ",")))
    
    
    
  })#infoboxDash1 end
  
  
  
  
  
  
  #infobox Dash2 start
  
  
  
  
  d2info1 =reactive({
    
    
    
    salesinfo$find(fields ='{"documentno":true,"_id":false}' )
  })
  
  output$d2documentnumber = renderInfoBox({
    
    
    l=d2info1()
    
    # c= ofc%>%filter(client %in% input$client & store %in% input$store & month %in% input$month & level %in% input$level)%>%
    #   select(orderid)
    
    infoBox("Total Transactions",value = h1(prettyNum(length(unique(l$documentno)),big.mark = ",")),icon = icon("caret-up"))
    
    
    
  }) #end of info box 1
  
  
  
  
  
  d2info2 =reactive({
    
    salesinfo$find(fields = '{"TotalNetAmount":true,"_id":false}')
  })
  
  
  output$d2netamount = renderInfoBox({
    
    c= d2info2()
    infoBox("Net Amount in millions ",value = h1(prettyNum( round(sum(c$TotalNetAmount)/1000000,2), big.mark = ","),"AED"),icon = icon("hand-holding-usd"))
    
    
    
  })#end of info box2
  
  
  
  
  
  d2info3 = reactive({
    
    salesinfo$find(fields = '{"customer":true,"_id":false}')
  })
  
  output$d2unique = renderInfoBox({
    
    c= d2info3()
    
    infoBox("Total Unique Customers ",value = h1(prettyNum(length(unique(c$customer))-1,big.mark = ",")))
    
    
    
  })#end of info3
  
  
  
  
  
  d2client_input = reactive({
    
    
    salesinfo$find(fields = '{"client":true,"_id":false}')
  })
  output$d2client=renderUI({
    
    cl = d2client_input()
    
    pickerInput(inputId = "d2client",
                label = "Client",
                choices= as.character(unique(cl$client)),
                width = 150,
                multiple = T,
                selected = as.character(unique(cl$client))  )
    
  }) #end of picker client
  
  
  
  
  output$d2store = renderUI({
    c = ofc%>%filter(client %in% input$client )%>%
      select(store)
    pickerInput(inputId="d2store",
                label="Store",
                choices = levels(c$store),
                multiple = TRUE,
                width = 150,
                selected = levels(c$store))
              })#end of picker input store
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #input Functions Updation Dashboard1
  
  
  
  d1client_input = reactive({
    
    
    salesinfo$find(fields = '{"client":true,"_id":false}')
  })
  output$d1client=renderUI({
    
    cl = d1client_input()
    
    pickerInput(inputId = "d1client",
                label = "Client",
                choices= as.character(unique(cl$client)),
                width = 150,
                multiple = T,
                selected = as.character(unique(cl$client))  )
    
  })
  
  
  
  
  
  # output$d1legal= renderUI({
  #   pickerInput(inputId = "d1legale",
  #               label = "Legal Entity",
  #               choices =  as.character(unique(ofc[ofc$client %in%input$client,"legalentity"])),
  #               multiple = TRUE,
  #               selected = as.character(unique(ofc[ofc$client %in% input$client,"legalentity"])),
  #               width = 150
  #   ) })
  # 
  d1store_input = reactive({
    q1=paste(shQuote(input$d1client, type="cmd"), collapse=", ")
    
    query =paste0('{"client":{"$in":[',q1,']}}')
    
   values = salesinfo$find(fields = '{"store":true,"_id":false}',query = query)
    
  # }
  })
  
  
  output$d1store = renderUI({
    c = d1store_input()
    pickerInput(inputId="d1store",
                label="Store",
                choices = unique(c$store),
                multiple = TRUE,
                width = 150,
                selected = unique(c$store))
    
    
    
    
    
  })
  
  d1month_input = reactive({
    q1=paste(shQuote(input$d1client, type="cmd"), collapse=", ")
    q2=paste(shQuote(input$d1store, type="cmd"), collapse=", ")
    query =paste0('{"client":{"$in":[',q1,']},"store":{"$in":[',q2,']}}')
    
    
    salesinfo$find(fields = '{"DateOrdered":true}',query = query)
    
    
  })
  
  
  output$d1month = renderUI({
    
    mon = d1month_input()
    mon$DateOrdered = as.Date(mon$DateOrdered)
    mon$month = months(mon$DateOrdered)
    mon$month = factor(mon$month,levels = month.name)
    
    
    
    pickerInput(
      inputId = "d1month",
      label = "Month",
      choices = as.character(unique(mon$month)),
      multiple = T,
      selected = as.character(unique(mon$month)))
      
  })
  
  
  
  
  d1weekday_input = reactive({
    q1=paste(shQuote(input$d1client, type="cmd"), collapse=", ")
    q2=paste(shQuote(input$d1store, type="cmd"), collapse=", ")
    q3 = paste(shQuote(input$d1month,type = "cmd"),collapse = ", ")
    query =paste0('{"client":{"$in":[',q1,']},"store":{"$in":[',q2,']}}')
    
    
    salesinfo$find(fields = '{"DateOrdered":true}',query = query)
    
    
  })
  
  
  output$d1weekday = renderUI({
    
    
    week = d1weekday_input()
    week$DateOrdered = as.Date(week$DateOrdered)
    
    week$weekday = weekdays(week$DateOrdered)
    
    
    pickerInput("d1weekday",
                "WeekDay",
                choices = as.character(unique(week$weekday)),
                selected = as.character(unique(week$weekday)),
                multiple = TRUE,
                width = 150)
  })
  
  
  
  
  
  
  
  
  
  
  
  

  
#Graphs For Dashboard1  
  

  
    #graph1

  
  d1gr1 =reactive({
    
    
    q1=paste(shQuote(input$d1client, type="cmd"), collapse=", ")
    q2 =paste(shQuote(input$d1store,type = "cmd"),collapse = ", ")
    query =paste0('{"client":{"$in":[',q1,']},"store":{"$in":[',q2,']}}')
    
    
    
    salesinfo$find(fields ='{"TotalGrossAmount":true,"_id":false,"DateOrdered":true}',query = query )
  })
  
  output$d1graph1 = renderPlotly({
    
    g1= d1gr1()
    g1$weekday = weekdays(as.Date(g1$DateOrdered))
    g1$month = months(as.Date(g1$DateOrdered))
    g1 = g1%>%filter(weekday %in%input$d1weekday & month %in% input$d1month)%>%select(TotalGrossAmount,month)
    
    g1 = g1%>%group_by(month)%>% summarise(total = sum(TotalGrossAmount))
    g1$month = factor(g1$month,levels = month.name)
    plot_ly(g1,x = ~month,y=~total,type = "bar")%>%config(displayModeBar = F)
   
    
    
  })
  

  
  
  
  
  #Dash board 1 Graph2
  output$d1graph3 = renderPlotly({
    
    
    g3 = d1gr1()
    
    g3$weekday = weekdays(as.Date(g3$DateOrdered))
    g3$month = months(as.Date(g3$DateOrdered))
    g3 = g3%>%filter(weekday %in%input$d1weekday & month %in% input$d1month)%>%select(TotalGrossAmount,month)
    
    g3 = g3%>%group_by(month)%>% summarise(total = sum(TotalGrossAmount))
    g3$month = factor(g3$month,levels = month.name)
    
    
    plot_ly(g3, labels = ~month, values = ~total, type = 'pie', showlegend = FALSE) %>%
      layout(title = 'Total Sales By Month',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% config(displayModeBar = F)
    
    
  })#END
  
  
  
  #Dashboard1 Graph3
  
  output$d1graph4 = renderPlotly({
    
    g4 = d1gr1()
    
    g4$weekday = weekdays(as.Date(g4$DateOrdered))
    g4$month = months(as.Date(g4$DateOrdered))
    g4 = g4%>%filter(weekday %in%input$d1weekday & month %in% input$d1month)%>%select(TotalGrossAmount,month,weekday)
    
    g4 = g4%>%group_by(month,weekday)%>% summarise(total = sum(TotalGrossAmount))
    g4$month = factor(g4$month,levels = month.name)
    g4$weekday = factor(g4$weekday,levels = list("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
    
    
    plot_ly(g4, x = ~month, y = ~total,color = ~weekday, type = 'bar') %>%
      layout( barmode = 'group',showlegend = F)%>%config(displayModeBar = F)
    
    
  })#end
  
  
  
  
  

  
  
#Graphs for Dashboard 2
  
  
  #Graph 1 dashboard 2
  
  output$d2graph1 = renderPlotly({
    
    g1 = ofc%>% filter(client %in% input$d2client & store %in% input$d2store & month %in% input$d2month & level %in% input$d2level)%>%
      select(totalgrossamount,level)
    #pie
    g1 = g1%>%group_by(level)%>%summarise(amount = sum(totalgrossamount))
    
    
    plot_ly(g1, labels = ~level, values = ~amount, type = 'pie',key = g1$level,showlegend = F,source = "d2graph1") %>%
      layout(title = 'Spending by Levels of customers',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% config(displayModeBar = F)
  })#end of graph 1
  
  
  
  
  #graph 2 Dashboard 2
  
  
  
  
  
  output$d2graph3 = renderPlotly({
    
    
    new = ofc%>% filter(client %in% input$d2client & store %in% input$d2store & month %in% input$d2month )%>%
      select(level,pointsaccumulated)
    new =new %>% group_by(level)%>%summarise(Awarded_Cashback = sum(pointsaccumulated,na.rm = T))
    new$level = factor(new$level,levels = c("BioDynamic" , "Organic" , "Healthy_2" ,  "Healthy_1" , "no_loyality"))
    new = new[c(3,5,2,1),]
    
    plot_ly(new, labels = ~level, values = ~Awarded_Cashback, type = 'pie', showlegend = FALSE) %>%
      layout(title = 'Points accumulated by customer level',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),source = "d2graph3") %>% config(displayModeBar = F)
    
    
  })#END
  
  
  
  
  #graph 3 Dashboard 2
  
  
  output$d2graph4 = renderPlotly({
    
    graph4 = ofc%>% filter(client %in% input$d2client & store %in% input$d2store & month %in% input$d2month & level %in% input$d2level)%>%
      select(totalgrossamount,level,month)
    graph4 = graph4%>%group_by(month,level)%>%summarise(Total_sales = sum(totalgrossamount))
    graph4$month =factor(graph4$month,levels = month.name)
    graph4 = arrange(graph4,month)
    plot_ly(graph4, x = ~month,
            y = ~Total_sales,
            color = ~level ,
            type = 'bar',
            source = "graph4",
            text = ~paste('Month:', month,'<br>Level:',level,'<br>Sales:',
                          prettyNum(Total_sales,big.mark = ",",scientific =F)))    
    
  })#end of graph 4  Dashboard2
  
  
  
  
  
  #start of Modal Dialogs for Dashboard 2
  
  
  
  
  #Data  for the modal 
  d2tbl_reactive <- reactive({
    
    
    info <- event_data("plotly_click",source = "d2graph1")
    f  =info$key
    
    f= ofc%>%filter(level==f & client %in% input$d2client & store %in% input$d2store & month %in% input$d2month)%>%select(customer,store,dateordered,totalgrossamount,totalnetamount)
    f
  })
  
  output$d2modal_table <- renderDataTable({
    d2tbl_reactive()
  })
  
  
  
  d2myModal <- function(failed=FALSE){
    modalDialog(
      dataTableOutput('d2modal_table'),
      easyClose = TRUE,size = "l",fade = T
      
    )
  }
  
  observeEvent(event_data("plotly_click",source = "d2graph1"),{
    
    showModal(d2myModal())
    
  })
  
  
  
  output$click = renderPrint({
    d = event_data("plotly_click","graph4")
    d
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
})#end of server function






