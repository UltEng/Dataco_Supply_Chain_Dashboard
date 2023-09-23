dashboardPage(title ="DataCo Smart Supply Chain Analysis Dashboard",
              skin = "black",
              
              # ------ HEADER -------
              dashboardHeader(title = span(tagList(icon = icon("chart-line"),
                                                   "Supply Chain Analysis"))),
              
              # ------ SIDE BAR -------
              dashboardSidebar(
                sidebarMenu(
                  menuItem(text = "Overview",
                           tabName = "page1",
                           icon = icon("magnifying-glass")),
                  menuItem(text = "Analysis",
                           icon = icon("chart-line"),
                           tabName = "page2",
                           badgeLabel = "new",
                           badgeColor = "green"),
                  menuItem(text = "Data Table",
                           tabName = "page3",
                           icon = icon("table-list"))
                )
              ),
              
              
              # ------ BODY -------
              dashboardBody(
                tabItems(
                  tabItem(tabName = "page1",
                          h2("Sales Report"),
                          br(),
                          fluidRow(
                            # A static infoBox
                            infoBox(title = "Total Order", 
                                    value = nrow(scm), 
                                    icon = icon("boxes-stacked"),
                                    color = "purple"),
                            infoBox(title = "Total Sales",
                                    value = sum(scm$sales), 
                                    icon = icon("dollar-sign"),
                                    color = "green"),
                            infoBox(title = "Total Country", 
                                    value = length(unique(scm$order_country)), 
                                    icon = icon("earth-americas"),
                                    color = "light-blue")
                          ),
                          # Row 2 (Pie Chart & Bar Chart)
                          fluidRow(
                            box(width = 6,
                                selectInput(inputId = "variable1", 
                                            label = "Choose What to show with Pie Chart", 
                                            choices = selectpiechart,
                                            selected = "customer_segment")),
                            box(width = 6,
                                selectInput(inputId = "variable2", 
                                            label = "Choose What to show with Bar Chart", 
                                            choices = selectbarchart,
                                            selected = "category_name"))
                          ),
                          fluidRow(  
                            box(width = 6,
                                echarts4rOutput("pie1",height = 600)),
                            box(width = 6,
                                echarts4rOutput("bar1",height = 600))
                          )

                          
                     ),
                  tabItem(tabName = "page2",
                          fluidRow(
                            infoBox(title = "Average Ship Duration", 
                                    value = round(mean(scm$total_shipping_days)), 
                                    icon = icon("boxes-stacked"),
                                    color = "purple"),
                            infoBox(title = "Average Sales",
                                    value = round(mean(scm$sales), 2), 
                                    icon = icon("dollar-sign"),
                                    color = "green"),
                            infoBox(title = "Total Late Delivery", 
                                    value = sum(scm$late_delivery), 
                                    icon = icon("triangle-exclamation"),
                                    color = "red")
                          ),
                          h3("Sales Analysis"),
                          fluidRow(
                            box(width = 12,
                                selectInput(inputId = "variable5", 
                                            label = "Choose how the trend is shown", 
                                            choices = selectlinechart,
                                            selected = "day"))
                          ),
                          fluidRow(
                            box(width = 12,
                                echarts4rOutput("line1",height = 500))
                          ),
                          fluidRow(
                            box(width = 6,
                                selectInput(inputId = "variable3", 
                                            label = "Choose What to show with Bar Chart", 
                                            choices = selectbarchart,
                                            selected = "category_name")),
                            box(width = 6,
                                selectInput(inputId = "variable4", 
                                            label = "Choose What to show with Violin Chart", 
                                            choices = selectpiechart,
                                            selected = "customer_segment")),
                          ),  
                          fluidRow(
                            box(width = 6,
                                echarts4rOutput("bar4",height = 500)),
                            box(width = 6,
                                plotlyOutput("violin1",height = 500))
                          ),
                          h3("Shipment Analysis"),
                          fluidRow(
                            box(width = 12,
                                echarts4rOutput("bar3",height = 500))
                          ),
                          fluidRow(
                            box(width = 6,
                                selectInput(inputId = "variable6", 
                                            label = "Choose What to show with pie Chart", 
                                            choices = selectbarchart2,
                                            selected = "shipping_mode")),
                            box(width = 6,
                                selectInput(inputId = "variable7", 
                                            label = "Choose What to show with Bar Chart", 
                                            choices = selectbarchart,
                                            selected = "category_name"))
                          ),
                          fluidRow(
                            box(width = 6,
                                echarts4rOutput("pie2",height = 600)),
                            box(width = 6,
                                echarts4rOutput("bar7",height = 600))
                          )
                          
                    ),
                  tabItem(tabName = "page3",
                          h2("Dataset DataCo Smart Supply Chain"),
                          fluidRow(
                            box(width = 12,
                                # title = "Dataset DataCo Smart Supply Chain",
                                dataTableOutput(outputId = "table_data")) 
                          )
                    )
                  
                )
              )
)
                          

