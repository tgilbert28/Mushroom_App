library(shiny)
library(shinythemes)
library(rpart)
library(rpart.plot)
library(funModeling) 
library(Hmisc)
library (ISLR)
library(ggplot2)
library(ggcorrplot)
library(class)
library (MASS)
library (tree)
library (randomForest)
library (gbm)
library("factoextra")
library(rgl)

ui <- fluidPage(theme = shinytheme("flatly"),
                
    titlePanel(h1("Is Your Mushroom Edible or Poisonous?", align = "center")),
        sidebarPanel(
            tabsetPanel(
                tabPanel("K Nearest Neighbor",id = "tPanel",style = "overflow-y:scroll; max-height: 600px; position:relative;",h3("Enter Mushroom Details", align = "center"),column(12,
                        selectInput("cap.shape", h4("Cap Shape"), 
                                    choices = list("Bell" = 1, "Convex" = 6,
                                                "Flat" = 3,"Knobbed" = 4,"Sunken" = 5, "Conical" = 2), selected = 6),
                        selectInput("cap.surface", h4("Cap Surface"), 
                                    choices = list("Fibrous" = 1, "Grooves" = 2,
                                                "Scaly" = 4,"Smooth" = 3), selected = 3),
                        selectInput("cap.color", h4("Cap Color"), 
                                    choices = list("Brown" = 5, "Buff" = 1,
                                                "Cinnamon" = 2,"Gray" = 4,"Green" = 7,"Pink" = 6,"Purple" = 8,"Red" = 3,"White" = 9,"Yellow" = 10), selected = 2),
                        selectInput("bruises", h4("Bruises"), 
                                    choices = list("Bruises" = 2, "No Bruises" = 1
                                                ), selected = 2),
                        selectInput("odor", h4("Odor"), 
                                    choices = list("Almond" = 1, "Anise" = 4,
                                                "Creosote" = 2,"Fishy" = 1,"Foul" = 3,"Musty" = 5,"None" = 6,"Pungent" = 7,"Spicy" = 8), selected = 2),
                        selectInput("gill.attachment", h4("Gill Attachment"), 
                                    choices = list("Attached" = 1,
                                                "Free" = 2), selected = 1),
                        selectInput("gill.spacing", h4("Gill Spacing"), 
                                    choices = list("Close" = 1, "Crowded" = 2
                                             ), selected = 2),
                        selectInput("gill.size", h4("Gill Size"), 
                                    choices = list("Broad" = 1, "Narrow" = 2
                                                ), selected = 1),
                        selectInput("gill.color", h4("Gill Color"), 
                                    choices = list("Black" = 5, "Brown" = 6,
                                                "Buff" = 1,"Chocolate" = 4,"Gray" = 3,"Green" = 9,"Orange" = 7,"Pink" = 8,"Purple" = 10,"Red" = 2,"White" = 11,"Yellow" = 12), selected = 12),
                        selectInput("stalk.shape", h4("Stalk Shape"), 
                                    choices = list("Enlarging" = 1, "Tapering" = 2
                                                ), selected = 2),
                        selectInput("stalk.root", h4("Stalk Root"), 
                                    choices = list("Bulbous" = 2, "Club" = 3,
                                                "Equal" = 4,"Rooted" = 5,"Missing" = 1), selected = 4),
                        selectInput("stalk.surface.above.ring", h4("Stalk Surface Above Ring"), 
                                    choices = list("Fibrous" = 1, "Scaly" = 4,
                                                "Silky" = 2,"Smooth" = 3), selected = 3),
                        selectInput("stalk.surface.below.ring", h4("Stalk Surface Below Ring"), 
                                    choices = list("Fibrous" = 1, "Scaly" = 4,
                                                "Silky" = 2,"Smooth" = 3), selected = 3),
                        selectInput("stalk.color.above.ring", h4("Stalk Color Above Ring"), 
                                    choices = list("Brown" = 5, "Buff" = 1,
                                                "Cinnamon" = 2,"Gray" = 4,"Orange" = 6,"Pink" = 7,"Red" = 3,"White" = 8,"Yellow" = 9), selected = 8),
                        selectInput("stalk.color.below.ring", h4("Stalk Color Below Ring"), 
                                    choices = list("Brown" = 5, "Buff" = 1,
                                                "Cinnamon" = 2,"Gray" = 4,"Orange" = 6,"Pink" = 7,"Red" = 3,"White" = 8,"Yellow" = 9), selected = 8),
                        selectInput("veil.color", h4("Veil Color"), 
                                    choices = list("Brown" = 1, "Orange" = 2,
                                                "White" = 3,"Yellow" = 4), selected = 1),
                        selectInput("ring.number", h4("Ring Number"), 
                                    choices = list("None" = 1, "One" = 2,
                                                "Two" = 3), selected = 1),
                        selectInput("ring.type", h4("Ring Type"), 
                                    choices = list("Evanescent" = 1,
                                                "Flaring" = 3,"Large" = 2,"None" = 4,"Pendant" = 5), selected = 1),
                        selectInput("spore.print.color", h4("Spore Print Color"), 
                                    choices = list("Black" = 3, "Brown" = 4,
                                                "Buff" = 1,"Chocolate" = 2,"Green" = 6,"Orange" = 5,"Purple" = 7,"White" = 8,"Yellow" = 9), selected = 8),
                        selectInput("population", h4("Population"), 
                                    choices = list("Abundant" = 1, "Clustered" = 2,
                                                "Numerous" = 3,"Scattered" = 4,"Several" = 5,"Solitary" = 6), selected = 5),
                        selectInput("habitat", h4("Habitat"), 
                                    choices = list("Grasses" = 2, "Leaves" = 3,
                                                "Meadows" = 4,"Paths" = 5,"Urban" = 6,"Waste" = 7,"Woods" = 1), selected = 1))
                    
                
                    ),
                tabPanel("Decision Tree",id = "tPanel",style = "overflow-y:scroll; max-height: 600px; position:relative;",h3("Enter Mushroom Details", align = "center"),
                         
                         column(12,selectInput("gill_color2", h4("Gill Color"), 
                         choices = list("Black" = 5, "Brown" = 6,
                         "Buff" = 1,"Chocolate" = 4,"Gray" = 3,"Green" = 9,"Orange" = 7,"Pink" = 8,"Purple" = 10,"Red" = 2,"White" = 11,"Yellow" = 12), selected = 1)
                         , 
                         
                         conditionalPanel(
                             condition = "input.gill_color2 == 1 || input.gill_color2 == 2 || input.gill_color2 == 3 || input.gill_color2 == 4",
                             selectInput("population2", h4("Population"), 
                                         choices = list("Abundant" = 1, "Clustered" = 2,
                                         "Numerous" = 3,"Scattered" = 4,"Several" = 5,"Solitary" = 6), selected = 5)),
                             
                         conditionalPanel(
                                 condition = "((input.gill_color2 == 5||input.gill_color2 ==6||input.gill_color2 ==7||input.gill_color2 ==8||input.gill_color2 ==9||input.gill_color2 ==10||input.gill_color2 ==11||input.gill_color2 ==12) || (input.population2 == 1||input.population2 ==2||input.population2 ==3||input.population2 ==4))",
                                 selectInput("spore_print_color2", h4("Spore Print Color"), 
                                             choices = list("Black" = 3, "Brown" = 4,
                                             "Buff" = 1,"Chocolate" = 2,"Green" = 6,"Orange" = 5,"Purple" = 7,"White" = 8,"Yellow" = 9), selected = 1)),
                         
                         #selectInput("spore_print_color2", h4("Spore Print Color"), 
                         #choices = list("Black" = 3, "Brown" = 4,
                         #"Buff" = 1,"Chocolate" = 2,"Green" = 6,"Orange" = 5,"Purple" = 7,"White" = 8,"Yellow" = 9), selected = 8)
                         #,
                         
                         #selectInput("population2", h4("Population"), 
                         #choices = list("Abundant" = 1, "Clustered" = 2,
                         #"Numerous" = 3,"Scattered" = 4,"Several" = 5,"Solitary" = 6), selected = 5)
                         #,
                         
                         conditionalPanel(
                         condition = "((input.spore_print_color2 == 3 || input.spore_print_color2 == 4 || input.spore_print_color2 == 5 || input.spore_print_color2 == 6 || input.spore_print_color2 == 7 || input.spore_print_color2 == 8 || input.spore_print_color2 == 9) && (input.gill_color2 == 5||input.gill_color2 ==6||input.gill_color2 ==7||input.gill_color2 ==8||input.gill_color2 ==9||input.gill_color2 ==10||input.gill_color2 ==11||input.gill_color2 ==12))",
                         selectInput("gill_size2", h4("Gill Size"), 
                         choices = list("Broad" = 1, "Narrow" = 2), selected = 1))
                         ,
                         
                         conditionalPanel(
                         condition = "((input.spore_print_color2 == 1 || input.spore_print_color2 == 2 || input.spore_print_color2 == 3) && (input.gill_color2 == 5||input.gill_color2 ==6||input.gill_color2 ==7||input.gill_color2 ==8||input.gill_color2 ==9||input.gill_color2 ==10||input.gill_color2 ==11||input.gill_color2 ==12))",
                         selectInput("odor2", h4("Odor"), 
                         choices = list("Almond" = 1, "Anise" = 4,
                         "Creosote" = 2,"Fishy" = 1,"Foul" = 3,"Musty" = 5,"None" = 6,"Pungent" = 7,"Spicy" = 8), selected = 2))
                         ,
                         
                         conditionalPanel(
                         condition = "((input.gill_size2 == 2) && (input.gill_color2 == 5||input.gill_color2 ==6||input.gill_color2 ==7||input.gill_color2 ==8||input.gill_color2 ==9||input.gill_color2 ==10||input.gill_color2 ==11||input.gill_color2 ==12))",
                         selectInput("stalk_shape2", h4("Stalk Shape"), 
                         choices = list("Enlarging" = 1, "Tapering" = 2), selected = 2))
                         ,
                         
                         conditionalPanel(
                         condition = "((input.stalk_shape2 == 1) && (input.gill_color2 == 5||input.gill_color2 ==6||input.gill_color2 ==7||input.gill_color2 ==8||input.gill_color2 ==9||input.gill_color2 ==10||input.gill_color2 ==11||input.gill_color2 ==12))",
                         selectInput("habitat2", h4("Habitat"), 
                         choices = list("Grasses" = 2, "Leaves" = 3,
                         "Meadows" = 4,"Paths" = 5,"Urban" = 6,"Waste" = 7,"Woods" = 1), selected = 1))
                         ,
                         
                         conditionalPanel(
                         condition = "((input.habitat2 == 3 || input.habitat2 == 4 || input.habitat2 == 5 || input.habitat2 == 6 || input.habitat2 == 7) && (input.gill_color2 == 5||input.gill_color2 ==6||input.gill_color2 ==7||input.gill_color2 ==8||input.gill_color2 ==9||input.gill_color2 ==10||input.gill_color2 ==11||input.gill_color2 ==12))",
                         selectInput("bruises2", h4("Bruises"), 
                         choices = list("Bruises" = 2, "No Bruises" = 1), selected = 2)))
                         ,
                         )
                )),
    
                mainPanel(
                    tabsetPanel(
                        tabPanel("K Nearest Neighbor",htmlOutput("distPlot"),
                        rglwidgetOutput("plot",  width = 800, height = 600)
                    ),tabPanel("Decision Tree", plotOutput("treePlot"))))
                )

server <- function(input, output) {
    
    output$distPlot <- renderText({
        mush = read.csv("mushrooms.csv", header = T)
        i = 1:23
        mush[,i] = apply(mush[,i], 2, function(x) as.numeric(as.factor(x))) #making data numeric
        mush = mush[,-17]
        
        p = data.frame(cap.shape = as.numeric(input$cap.shape), cap.surface = as.numeric(input$cap.surface), cap.color = as.numeric(input$cap.color), bruises = as.numeric(input$bruises), odor = as.numeric(input$odor), gill.attachment = as.numeric(input$gill.attachment),
                       gill.spacing = as.numeric(input$gill.spacing), gill.size = as.numeric(input$gill.size), gill.color = as.numeric(input$gill.color), stalk.shape = as.numeric(input$stalk.shape), stalk.root = as.numeric(input$stalk.root), stalk.surface.above.ring = as.numeric(input$stalk.surface.above.ring),
                       stalk.surface.below.ring = as.numeric(input$stalk.surface.below.ring), stalk.color.above.ring = as.numeric(input$stalk.color.above.ring), stalk.color.below.ring = as.numeric(input$stalk.color.below.ring), veil.color = as.numeric(input$veil.color), ring.number = as.numeric(input$ring.number), ring.type = as.numeric(input$ring.type),
                       spore.print.color = as.numeric(input$spore.print.color), population = as.numeric(input$population), habitat = as.numeric(input$habitat))
        
        mush.knn = knn(train = mush[,2:22], test = p, cl = mush$class, k = 1)
        
        if (mush.knn[1] == 1){
            paste("<font size=\"8px\"><b>", "<font color=\"#008000\"><b>", "<div style = \"position:relative; left:300px; top:2px;\"><b>", "Edible")
        } else {
            paste("<font size=\"8px\"><b>", "<font color=\"#FF0000\"><b>", "<div style = \"position:relative; left:250px; top:2px;\"><b>", "POISONOUS")
        }
        
        
    })
    
    output$plot <- renderRglwidget({
        mush = read.csv("mushrooms.csv", header = T)
        i = 1:23
        mush[,i] = apply(mush[,i], 2, function(x) as.numeric(as.factor(x))) #making data numeric
        mush = mush[,-17]
        
        p = data.frame(class = as.numeric(3), cap.shape = as.numeric(input$cap.shape), cap.surface = as.numeric(input$cap.surface), cap.color = as.numeric(input$cap.color), bruises = as.numeric(input$bruises), odor = as.numeric(input$odor), gill.attachment = as.numeric(input$gill.attachment),
                       gill.spacing = as.numeric(input$gill.spacing), gill.size = as.numeric(input$gill.size), gill.color = as.numeric(input$gill.color), stalk.shape = as.numeric(input$stalk.shape), stalk.root = as.numeric(input$stalk.root), stalk.surface.above.ring = as.numeric(input$stalk.surface.above.ring),
                       stalk.surface.below.ring = as.numeric(input$stalk.surface.below.ring), stalk.color.above.ring = as.numeric(input$stalk.color.above.ring), stalk.color.below.ring = as.numeric(input$stalk.color.below.ring), veil.color = as.numeric(input$veil.color), ring.number = as.numeric(input$ring.number), ring.type = as.numeric(input$ring.type),
                       spore.print.color = as.numeric(input$spore.print.color), population = as.numeric(input$population), habitat = as.numeric(input$habitat))
        
        mush <- rbind(mush, p)
        
        mush.pr <- prcomp(mush, center = TRUE, scale = TRUE)
        rgl.open(useNULL=T)
        
        get_colors <- function(groups, group.col = palette()){
            groups <- as.factor(groups)
            ngrps <- length(levels(groups))
            if(ngrps > length(group.col)) 
                group.col <- rep(group.col, ngrps)
            color <- group.col[as.numeric(groups)]
            names(color) <- as.vector(groups)
            return(color)
        }
        
        cols <- get_colors(mush$class, c("#008000", "#FF0000","#000000"))
        
        plot3d(mush.pr$x[,1:3], col=cols, main = "3D Plot Of The First Three PCs", decorate = TRUE, size = 5)
        bg3d(color = "white")
        rglwidget()
    })
    
    output$treePlot <- renderPlot({
        mush = read.csv("mushrooms.csv", header = T)
        i = 1:23
        mush[,i] = apply(mush[,i], 2, function(x) as.numeric(as.factor(x))) #making data numeric
        mush = mush[,-17]
        mush2 = mush[,c(1,5,6,9,10,11,20,21,22)]
        
        for (i in c(1:8124)){
            if (mush2[i,1] == 1){
                mush2[i,1] = "Edible"
            } else {
                mush2[i,1] = "Poisonous"
            }
        }
        
        
        mush_rpart2 = rpart(as.factor(class)~., data = mush2, method = "class")
        #plotcp(mush_rpart2)
        min_cp = mush_rpart2$cptable[which.min(mush_rpart2$cptable[,"xerror"]),"CP"]
        #min_cp
        mush_rpart_prune2 = prune(mush_rpart2, cp = min_cp)
        #prp(mush_rpart_prune2)
        #rpart.plot(mush_rpart_prune2)
        
        p2 = data.frame(bruises = as.numeric(input$bruises2), odor = as.numeric(input$odor2),
                       gill.size = as.numeric(input$gill_size2), gill.color = as.numeric(input$gill_color2), stalk.shape = as.numeric(input$stalk_shape2),
                       spore.print.color = as.numeric(input$spore_print_color2), population = as.numeric(input$population2), habitat = as.numeric(input$habitat2))
        
        #p2
        
        predict_nodes <-
            function (object, newdata, na.action = na.pass) {
                where <-
                    if (missing(newdata)) 
                        object$where
                else {
                    if (is.null(attr(newdata, "terms"))) {
                        Terms <- delete.response(object$terms)
                        newdata <- model.frame(Terms, newdata, na.action = na.action, 
                                               xlev = attr(object, "xlevels"))
                        if (!is.null(cl <- attr(Terms, "dataClasses"))) 
                            .checkMFClasses(cl, newdata, TRUE)
                    }
                    rpart:::pred.rpart(object, rpart:::rpart.matrix(newdata))
                }
                as.integer(row.names(object$frame))[where]
            }
        #predict_nodes(mush_rpart_prune2, p2)
        #predict(mush_rpart_prune2, newdata = p2, type = "class")
        
        path.to.root <- function(node)
        {
            if(node == 1) # root?
                node
            else # recurse, %/% 2 gives the parent of node
                c(node, path.to.root(node %/% 2))
        }
        node = predict_nodes(mush_rpart_prune2, p2)
        nodes <- as.numeric(row.names(mush_rpart_prune2$frame))
        cols <- ifelse(nodes %in% path.to.root(node), "red", "gray")
        prp(mush_rpart_prune2, nn = F,
            col = cols, branch.col = cols, split.col = cols, nn.col = cols, varlen = 0, shadow.col = cols)
        
        
    })
}

shinyApp(ui = ui, server = server)
