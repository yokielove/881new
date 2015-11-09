library(shiny)
library(ggplot2)
library(reshape)
options(scipen = 10)

shinyServer(function(input, output){
  
  full <- read.csv("full.csv", stringsAsFactors = F)
  full <- data.frame(full)
  
  full$mean <- as.numeric(full$mean)
  full$median <- as.numeric(full$median)
  full$pub <- as.numeric(full$pub)
  full$priv <- as.numeric(full$priv)
  
  full[is.na(full)] <- 0
  
  full$cost <- full$pub + full$priv
  
  new <- full[,c(1,2,3,6)]
  colnames(new) <- c("Institutions Names", "Mean", "Median", "Cost")
  
  new$Cost[new$Cost==0] <- NA
  new <- na.omit(new)
  
  new <- new[order(new$Mean, decreasing = T),]
  new$obs <- c(1:nrow(new))
  rownames(new) <- NULL
  
  new_user <- reactive(head(new[,1:4], n = input$obs))
  
  output$view <- renderTable({
    new_table <- new_user()
    print(new_table)
    })
  
  output$fig <- renderPlot({
    p <- new_user()
    colnames(p) <- c("Institutions", "Mean", "Median", "Cost")
    pp <- melt(p, id="Institutions")
    ggplot(data=pp, aes(x=factor(Institutions), y=value, fill= variable)) + 
      geom_bar(stat="identity", position="dodge") + xlab("Institution Names") +
      scale_x_discrete(limits=p$Institutions) + theme(axis.text.x = element_text(angle = 90))
    })
})