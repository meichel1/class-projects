
ui <- fluidPage(
  theme= bs_theme(bootswatch = "slate"),
  titlePanel("Sentiment Exploration Across Star Wars Episodes IV-VI"),
  h2("Use the below to investigate the relationships between specific characters in the original Star Wars Trilogy."),
  checkboxGroupInput("film", label="Choose one or more films:", 
                choiceNames=c("Episode IV: A New Hope", "Episode V: The Empire Strikes Back", "Episode VI: Return of the Jedi"),
                choiceValues=c("iv","v","vi")),
  fluidRow(
    column(4,wellPanel(
      selectInput("character",
                  "Choose character 1:",
                  c("Han Solo"="HAN","Luke Skywalker"="LUKE","C-3PO"="THREEPIO","Leia Organa"="LEIA","Darth Vader"="VADER","Lando Calrissian"="LANDO","Emperor Palpatine"="EMPEROR",
                    "Red Leader"="RED LEADER","Biggs Darklighter"="BIGGS","Moff Tarkin"="TARKIN","Owen Lars"="OWEN","Jabba the Hutt"="JABBA","Stormtrooper"="TROOPER","Obi-Wan (Ben) Kenobi"="BEN",
                    "Admiral Ackbar"="ACKBAR","Gold Leader"="GOLD LEADER","Yoda"="YODA","Admiral Rieekan"="RIEEKAN"))
      )),
    column(4,wellPanel(  
      selectInput("character2",
                  "Choose character 2:",
                    c("Han Solo"="HAN","Luke Skywalker"="LUKE","C-3PO"="THREEPIO","Leia Organa"="LEIA","Darth Vader"="VADER","Lando Calrissian"="LANDO","Emperor Palpatine"="EMPEROR",
                    "Red Leader"="RED LEADER","Biggs Darklighter"="BIGGS","Moff Tarkin"="TARKIN","Owen Lars"="OWEN","Jabba the Hutt"="JABBA","Stormtrooper"="TROOPER","Obi-Wan (Ben) Kenobi"="BEN",
                    "Admiral Ackbar"="ACKBAR","Gold Leader"="GOLD LEADER","Yoda"="YODA","Admiral Rieekan"="RIEEKAN"))))),
    mainPanel(
      h3("General overall sentiment of characters when together"),
      p("This plot shows how characters interact together. This is colorcoded by the character's alignment:", 
      span("Rebel Alliance", style="color:#E5703D"),",", span("Galactic Empire", style="color:#BC1E22"), 
      ", or", span("Jabba the Hutt", style="color:#C8B64C"), 
      ". Jabba the Hutt is unaligned with either faction and aligned only to himself."),
      plotOutput("boxplot"),
      h3("Top words used by the character,", span("positive", style="color:#548C9C"), "or ",
         span("negative", style="color:#9A4946")),
      fluidRow(
        column(6,plotOutput("cloud")),
        column(6,plotOutput("cloud2"))), width="100%",
      h3("Most used types of words in the scenes in which the chosen characters both appear."),
      plotOutput("barplot"),
      h3("Overall sentiment - positive or negative - over the span of films."),
      plotOutput("line"))
  
  )


server <- function(input, output) {
  options(shiny.sanitize.errors=TRUE)
  
  output$boxplot <- renderPlot({ 
    
    full_sw_dial_scene%>%filter(film %in% input$film)%>% 
      filter(character %in% c(input$character,input$character2))%>%
      subset(duplicated(scene_num))%>%
      ggplot(aes(x=character, y=value, fill=alignment))+
      geom_boxplot()+
      scale_fill_manual(values=c("Alliance"="#E5703D","Hutt"="#C8B64C", "Empire"="#BC1E22"))+
      labs(x="",y="")+
      theme(legend.position = "none")
  })
  
  
  output$cloud<- renderPlot({
    par(mar = rep(0, 4))
    
    full_sw_dial%>%filter(film %in% input$film) %>%group_by(scene_num)%>%
      filter(character %in% c(input$character, input$character2))%>%
      subset(duplicated(scene_num))%>%filter(character == input$character)%>%
      inner_join(bing, "word")%>%
      count(word, sentiment, sort=TRUE) %>%
      acast(word ~ sentiment, value.var="n", fill=0) %>%
      comparison.cloud(colors=c("#9A4946", "#548C9C"), max.words=50, title.size=1,scale=c(5,.5), random.order=FALSE)
  })
  
  output$cloud2<-renderPlot({ 
    par(mar = rep(0, 4))
    
    full_sw_dial%>%filter(film %in% input$film) %>%group_by(scene_num)%>%
      filter(character %in% c(input$character, input$character2))%>%
      subset(duplicated(scene_num))%>%filter(character == input$character2)%>%
      inner_join(bing, "word")%>%
      count(word, sentiment, sort=TRUE) %>%
      acast(word ~ sentiment, value.var="n", fill=0) %>%
      comparison.cloud(colors=c("#9A4946", "#548C9C"), max.words=50, title.size=1,scale=c(5,.5), random.order=FALSE)
  
    })
  output$barplot<- renderPlot({
    
    full_sw_sent%>%filter(film%in%input$film)%>% 
      filter(character%in% c(input$character, input$character2))%>%
      subset(duplicated(scene_num))%>%
      count(word, sentiment, sort=TRUE)%>%
      ggplot(aes(x=reorder(sentiment, -n, sum), y=n)) + 
      geom_bar(stat="identity", aes(fill=sentiment), show.legend=FALSE) +
      labs(x="Sentiment", y="Frequency")+
      scale_fill_aaas()
  })
  output$line<- renderPlot({
    
    full_sw_dial_scene%>%filter(film%in%input$film)%>%
      
      filter(character %in% c(input$character, input$character2))%>%
      ggplot(aes(idx, mean, color=character))+
      geom_rect(data=df,aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=eps),
                alpha=0.2,inherit.aes=FALSE)+
      scale_fill_manual(values=c("#8CAECB","#9F0613", "#417D6C"))+
      geom_smooth(se=FALSE)+
      labs(x="Timeline", y="Sentiment")+
      theme(axis.text.x=element_blank(), axis.title.x = element_blank())
  })
}

shinyApp(ui = ui, server = server)

