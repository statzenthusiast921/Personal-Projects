# Name: Jon (me)
# Date: 06.28.2020
# Purpose: Create Chicago Face Database App 
#-------------------------------------------------#
#setwd("/Users/jonzimmerman/Desktop/Data Projects/Chicago Face Database/CFD")
library(readxl)
library(shinythemes)
library(shinyWidgets)
library(ggcorrplot)
library(RSQLite)
library(sqldf)
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(zoo)
library(dplyr)
library(htmltools)
library(formattable)
library(recipes)
library(car)
library(ggpmisc)
library(caret)
library(ERSA)
library(psy)
library(jtools)
library(broom.mixed)
library(TMB)
library(broom)
library(ggstance)


CFD=read_excel("CFD.xlsx",sheet=1)
CFD.f=CFD[,c(1,2,3,4,14:24,26:69)]
CFD.m=CFD.f[,c(4:59)]


CFD$Race=ifelse(CFD$Race=="A","Asian",
         ifelse(CFD$Race=="B","Black",
         ifelse(CFD$Race=="L","Latino",
         ifelse(CFD$Race=="W","White","NA"))))

CFD$Gender=ifelse(CFD$Gender=="M","Men",
           ifelse(CFD$Gender=="F","Women","NA"))



CFD.removecats=CFD[,4:69]
CFD.removecats2=CFD.removecats[,-22]
CFD.scaled=as.data.frame(scale(CFD.removecats2))

#Personality Correlation Matrix
CFD.m1=CFD.m[,c(4,2:3,6:7,9,12:16)]
#Physical Attributes Correlation Matrix
CFD.m2=CFD.m[,c(4,1,5,8,10,17:21,24,27:30,33,36:37,40,43:56)]
#Too Many - go with top 10 absolute value of strongest correlations
CFD.m2.2=CFD.m2[,c(1:2,4:5,8,11,18,24:25,28,33)]

AttributeChoices=c("Age","Afraid","Angry","AvgEyeHeight","BottomLipChin","ChinLength","Disgusted",
                   "Dominant","EyeShape","EyeSize","Feminine","fWHR","Happy","Masculine",
                   "NoseLength","Noseshape","Sad","Surprised","Threatening","Trustworthy","Unusual")


IndVarChoices=c("Age","Afraid","Angry","AvgEyeHeight","BottomLipChin","ChinLength","Disgusted","Dominant",
                "EyeShape","EyeSize","Feminine","fWHR","Happy","Masculine","NoseLength",
                "Noseshape","Sad","Surprised","Threatening","Trustworthy","Unusual")

RaceChoices=c("Asian","Black","Latino","White")
SexChoices=c("Women","Men")

CFD$Race = factor(CFD$Race, levels = c("Black","Asian","Latino","White"))
CFD$Gender = factor(CFD$Gender, levels = c("Women","Men"))

#For Principal Components
to_be_factored=CFD[,c(4,14:15,18:22,24,26:29,32,37,50,58,60:61,64,69)]



data1=as.matrix(CFD.m1)
data2=as.matrix(CFD.m2.2)

corr1 = cor(data1, use = "na.or.complete")
corr2 = cor(data2, use = "na.or.complete")




# Define UI for application
ui = fluidPage(
    navbarPage("Chicago Face Database Analysis",
        tabPanel("Welcome",
                 tabName = "welcome",
                 icon=icon("door-open"),
                 
          fluidPage(theme=shinytheme("cerulean"),
                    h1("Welcome to my Chicago Face Database Shiny Dashboard!"),
                    br(),
                    p(strong(tags$u("What is the Chicago Face Database?"))),
                    p("The",a("Chicago Face Database", href = "https://chicagofaces.org/default/"), 
                      "is a collection of high-resolution, standardized photographs of self-identifying 
                      male and female faces of varying ethnicities between the ages of 17 and 65.  
                      It was developed at the University of Chicago and intended for use in scientific 
                      research.  The data includes measures of",a("physical attributes", href='https://chicagofaces.org/default/cfd_public/cfdguide.pdf'),
                      "as well as average ratings of personality traits by independent judges based on viewings of the photographs.  
                      To maximize comparability across race and sex, the researchers presented the judges with images from multiple target categories.  
                      Further, to decrease bias in the ratings, the measures judges were asked to score were randomized."),  
                    br(),
                    p(strong(tags$u("What is the purpose of this analysis?"))),
                    p("I wanted to use this data to determine which subjective and objective factors are more strongly associated with higher perceived levels of attractiveness."),
                    br(),
                    p(strong(tags$u("How can I use this dashboard?"))),
                    p("You can click on any of the tabs above to see a different analysis of the data.")
                    ),
          fluidRow(tags$img(src='CFD-AM-210-035-N.jpg' ,width = "250px", height = "200px"),
                   tags$img(src='CFD-BF-010-003-HO.jpg',width = "250px", height = "200px"),
                   tags$img(src='CFD-WF-003-014-A.jpg' ,width = "250px", height = "200px"),
                   tags$img(src='CFD-WM-009-004-HO.jpg',width = "250px", height = "200px"))),

        
        
        tabPanel("Exploratory",
                 tabname="exploratory",
                 icon=icon("search"),
                 fluidPage(
                   fluidRow(column(width=6,
                       prettyRadioButtons(inputId = "rb", 
                                          label = "Make a selection:",
                                          c("Personality Correlation Matrix"="Personality Correlation Matrix",
                                            "Physicality Correlation Matrix"="Physicality Correlation Matrix"),
                                          animation = "pulse"),
                        fluidRow(plotOutput("CorrMatrix"))),
                                column(width=6,
                                              selectInput(inputId = "attributeselect",label="Make a selection:",choices=AttributeChoices,selected=AttributeChoices[1]),
                                       column(width=6,
                                              plotOutput("SexChart")),
                                       column(width=6,
                                              plotOutput("RaceChart")))))),
        
        tabPanel("Factor Analysis",
                 tabname="factors",
                 icon=icon("compress"),
                 
                 fluidPage(p(a("Factor Analysis",href="https://en.wikipedia.org/wiki/Factor_analysis"),"is an",a("unsupervised",href="https://en.wikipedia.org/wiki/Unsupervised_learning"),"statistical 
                             technique used to reduce the dimensionality of a large dataset by grouping independent variables through hidden relationships.  This procedure is largely exploratory, but 
                             can be applied in structured settings."),
                   fluidRow(

                 column(width=6,
        
                 sliderTextInput(inputId = "num", 
                                 label = "Number of Factors", 
                                 choices=c(10,9,8,7,6,5,4,3,2)),
                 p(strong("% Variance Explained by Each Factor")),
                 verbatimTextOutput("variance"),
                 p("This output displays the percentage of variance explained by each factor.
                 Ideally, one would select the number of factors where the cumulative variance 
                 is maximized and each additional factor contributes a neglible
                 percentage of proportional variance.  This output should be used in tandem
                 with the scree plot on the right.")),
                 
                 column(width=6,
                        plotOutput("scree"),
                        p("The ",a("scree plot",href="https://en.wikipedia.org/wiki/Scree_plot"),"is a visual tool to help determine how many factors to consider in the analysis.
                        Generally, one would choose the number of factors where the eigenvalues begin to flatten out."))),
                 p(strong("Matrix of Factor Loadings")),
                 p("This matrix displays the factor loadings for each factor in this analysis.  Factor loadings within a column with similar values indicate possible, hidden relationships.  For example, with 7 factors selected, Factor 1 appears to be associated with 'Eye' measurements."),
                   DTOutput("loadings"))),
                
                                    
              tabPanel("Regression",
                       tabname="regression",
                       icon=icon("chart-line"),
                       fluidPage(
                         fluidRow(column(width=6,
                                  selectInput(inputId = "indep", label = "Choose Independent Variable(s) to Include:", 
                                    multiple = TRUE, choices = as.list(IndVarChoices), selected = IndVarChoices[1:2])),
                                  column(width=6,
                                  p(strong("Visualizing Significance of Independent Variables")),
                                  p("The estimates for the independent variables are represented by the circles in the plot.  The lines reaching out from the circles are the 95% confidence intervals around the estimates.  Any estimate with lines that do not cross the vertical dashed line represent an independent variable with",a("statistical significance",href="https://en.wikipedia.org/wiki/Statistical_significance"),"
                                    in the regression model on the left."))),
                         tags$style("#RegOut {white-space: pre;height:30em; overflow:auto;}"),
                                  
                         fluidRow(column(width=6,
                                         verbatimTextOutput(outputId = "RegOut")),
                                  column(width=6,
                                         plotOutput("colorplot"))),
                         fluidRow(splitLayout(cellWidths = c("50%", "50%")),
                                  column(width=6,
                                         p(strong("Checking Normality")),
                                         p("A p-value greater than 0.05 indicates the null hypothesis of",a("normality",href="https://en.wikipedia.org/wiki/Normality_test"),"can not be rejected."),
                                         verbatimTextOutput(outputId = "Test1"),
                                         p(strong("Checking for Multicollinearity")),
                                         p("This table shows the Variance inflation factors (VIF) for each independent variable included in the model.  A VIF value >>> 1 indicates issues with", a("multicollinearity",href="https://en.wikipedia.org/wiki/Multicollinearity"),".  Generally, values under 10 are not concering."),
                                         verbatimTextOutput("Test3")),
                                  column(width=6,
                                        p(strong("Checking Model Diagnostics")),
                                        p("These plots show how well the linear model", a("assumptions",href="https://en.wikipedia.org/wiki/Linear_regression#Assumptions")," are met with the selected model.  Generally, one would not want to find any discernable pattern in these plots."),
                                         plotOutput("Test2"))))),
                                       
                         
                         
                         
                        
        
        tabPanel("Regression P2",
                 tabname="reg2",
                 icon=icon("chart-line"),
                 fluidPage(
                   fluidRow(column(width=6,
                                        selectInput(inputId = "attributeselect2",
                                                    label="Independent Variables",
                                                    choices=as.list(AttributeChoices),selected=AttributeChoices[1])),
                            column(width=6,
                                        p(strong("Note:"),"This tab is not intended to imply one race or sex is more 
                                          attractive than another.  These plots attempt to explain how the perception 
                                          of attractiveness depends on race and sex."))),
                    fluidRow(column(width = 6,
                                         selectInput(inputId = "raceselect",
                                                      label="Race",
                                                      choices=as.list(RaceChoices),selected = RaceChoices[1]),
                                          selectInput(inputId = "sexselect",
                                                        label="Sex",
                                                        choices=as.list(SexChoices),selected = SexChoices[1])),
                                         column(width=6,
                                                plotOutput("sex_race_plot"))))),
        tabPanel("Conclusion",
                 tabname="conclusion",
                 icon=icon("info"),
          fluidRow(column(width=9,
                        p(strong("Background")),
                        p("The purpose of this analysis was to determine which subjective 
                        and objective traits of the Chicago Face Database are more strongly associated with higher perceived 
                        levels of attractiveness. 43 objective physical measurements and 12 subjective personality ratings
                          were considered.  Only the top 10 physical and top 10 personality measures with the strongest 
                          correlations with Attractiveness were used in this analysis.")),
                   column(width=3,
                        tags$img(src='CFD-AF-230-193-N.jpg',width = "200px", height = "150px"))),
          fluidRow(
                   column(width=9,
                          p(strong("Analysis")),
                          p("To sufficiently describe the relationship between Attractiveness and both the physical and
                            personality measures, a linear regression model was constructed with",strong("Age, Dominant, 
                            Feminine, Sad, Surprised, Trustworthy, Unusual, Nose Shape, Eye Shape, Chin Length,"),"and", 
                            strong("Facial Width to Height Ratio (fWHR)"),"as independent variables in the model.
                            Most notably, Dominant (pval<0.000), Feminine (pval<0.000), and Trustworthy (pval<0.000) had
                            the strongest, positive relationships with Attractiveness, while Age (pval<0.000) had the 
                            strongest, negative relationship with Attractiveness in the model.")),
                   column(width=3,
                   tags$img(src='CFD-BM-002-002-F.jpg',width= "200px",height = "150px"))),
          fluidRow(column(width=9,
                          p(),
                          p(strong("Limitations")),
                          p("Limitations of this study include leaving out several physical and personality measures.
                            A future study can include all variables regardless of their respective correlations with 
                            Attractiveness.  Further, additional regression models can be utilized to better describe the 
                            behaviors of these variables; a Gamma regression model would be a sufficient alternative to the
                            linear regression model used in this analysis."),
                          p("While the researchers who conducted this analysis took steps to mitigate bias based on race and 
                            sex, it is impossible to completely eliminate it.  In the future, more judges can be asked to rate
                            the participants of the study.  Besides randomizing the sex and race of participants for a judge 
                            to rate, an equal number of judges from each race and sex can be asked to provide ratings for each 
                            participant in order to dilute the bias.  This may open the door to a new study to determine the 
                            bias in attractiveness ratings from judges who share race and/or sex with the participants.")),
                   column(width=3,
                          tags$img(src='CFD-WF-018-019-HC.jpg',width= "200px",height = "150px"))
                   
                          ))
    
          ))
                 
      



# Define server logic 
server <- function(input, output) {
  

#-------------------EXPLORATORY------------------#
  

  output$CorrMatrix=renderPlot({
      switch(input$rb,
            "Personality Correlation Matrix"=ggcorrplot(corr1,method="circle",type="upper",ggtheme = ggplot2::theme_gray,colors = c("#00BFC4", "white", "#F8766D")),
            "Physicality Correlation Matrix"=ggcorrplot(corr2,method="circle",type="upper",ggtheme = ggplot2::theme_grey,colors = c("#00BFC4", "white", "#F8766D")))
  })
  
  output$SexChart=renderPlot({
    ggplot(CFD, aes_string(x=input$attributeselect, y=CFD$Attractive))+geom_point(aes(col=Gender))+
      ylab("Attractive")
  })
  
  output$RaceChart=renderPlot({
    ggplot(CFD, aes_string(x=input$attributeselect, y=CFD$Attractive))+geom_point(aes(col=Race))+
      ylab("Attractive")
  })
  




#-------------------REGRESSION-------------------#
  

#Reactive portions of this tab
  recipe_formula = reactive(CFD.scaled %>%
                               recipe() %>%
                               update_role(Attractive,new_role = "outcome") %>%
                               update_role(!!!input$indep,new_role = "predictor") %>% 
                               formula())
  
  lm_reg = reactive(
    lm(recipe_formula(),data = CFD.scaled)
  )
  
  norm_test=reactive(
    shapiro.test(resid(lm_reg()))
  )
  
  diag_test=reactive(
    plot(lm_reg())
  )
  
  mc_vif_check=reactive(
    vif(lm_reg())
  )
  
  sig_plot=reactive(
    plot_summs(lm_reg(), scale = TRUE)
    
  )
  
  
  
#Output portions of this tab 
  output$RegOut = renderPrint({
    summary(lm_reg())

    })
  
  output$colorplot=renderPlot({
    sig_plot()
  })
  
  output$Test1 = renderPrint({
    norm_test()
    })
  
  output$Test2 = renderPlot({
    par(mfrow=c(2,2))
    diag_test()
  })
  
  output$Test3=renderPrint({
    mc_vif_check()
  })
  


#-------------------REGRESSION P2-------------------#
  
  filtered_regdata=reactive({
    filter=subset(CFD,Race==input$raceselect)
    filter2=subset(filter,Gender==input$sexselect)
    return(filter2)
  })
  
  
  
  

output$sex_race_plot=renderPlot({
  
  data=filtered_regdata()
  
  formula=data$Attractive~input$attributeselect2
  plot=ggplot(data,aes_string(input$attributeselect2,data$Attractive))+geom_point()
  plot=plot+geom_smooth(method=lm)
  plot=plot+stat_fit_glance(method = "lm",
                  label.y = "bottom",
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('r^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)
  plot=plot+ggtitle(paste0("Attractiveness by ",input$attributeselect2," for ",input$raceselect," ",input$sexselect))
  plot=plot+theme(plot.title = element_text(hjust = 0.5,face="bold"))
  plot=plot+xlab(input$attributeselect2)+ylab("Attractiveness")
  plot
  })





#--------------FACTOR ANALYSIS--------------#

testrun=reactive({
  fit = factanal(to_be_factored, input$num, rotation="varimax")
  loadings = round(fit$loadings[,1:input$num],5)
  return(loadings)
  
})

testrun2=reactive({
  fit = factanal(to_be_factored, input$num, rotation="varimax")
  corrs = fit$correlation
  return(corrs)
  
})

testrun3=reactive({
  fit = factanal(to_be_factored, input$num, rotation="varimax")
  return(fit)
})



output$loadings=renderDT({
  
  data=as.data.frame(testrun())
  
  dtable = datatable(data, rownames=TRUE, options = list(pageLength=20,lengthChange = FALSE, dom='t'))
  
  colRamp = colorRamp(c("white","lightgreen"))
  for(column in names(data)){
    x = na.omit(data[[column]])
    brks = quantile(x, probs = seq(.05, .95, .01))
    RGB = colRamp(c(0, (brks-min(x))/(max(x)-min(x))))
    clrs = apply(RGB, 1, function(rgb){
      sprintf("rgb(%s)", toString(round(rgb,0)))
    })
    dtable = dtable %>% 
      formatStyle(column, backgroundColor = styleInterval(brks, clrs))
  }
  
  dtable
  
  
})




output$scree=renderPlot({
 scree.plot(testrun2())
  abline(v=input$num,col="red")
})


output$variance=renderPrint({
  
PropVar = colSums(testrun3()$loading*testrun3()$loading)/dim(testrun3()$loading)[1]    
CumVar  = cumsum(colSums(testrun3()$loading*testrun3()$loading)/dim(testrun3()$loading)[1]) 
c=rbind(PropVar,CumVar)
c


})

}




# Run the application 
shinyApp(ui = ui, server = server)





