#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# fun fact! shiny doesn't work with pacman
#if(!require("pacman")) install.packages("pacman")

#p_load(DT,patchwork,latex2exp,RColorBrewer,shiny)
library(shiny)
library(tidyverse)
library(DT)
library(patchwork)
library(latex2exp)
library(RColorBrewer)
library(plotly)

# Define UI for application that draws a histogram
ui <- function(req){
    fluidPage(

    # Application title
    titlePanel("Locally Optimal and Maximin Designs for Cluster Randomized Trials"),

    # Sidebar with a slider input for number of bins
    #sidebarLayout(
        sidebarPanel(
            verticalLayout(
                fluidRow(
                    column(width=6,
                           # What type of analyses are we looking at? #
                           radioButtons(
                           "objectives", "Type of Objective:",
                           c("Single objective - HTE" = "SO_HTE",
                           "Single Objective - ATE" = "SO_ATE",
                           "Multiple Objective" = "MO")
                           )
                     ),
                    column(width=6,
                           # are we finding an LOD or an MMD? #
                           radioButtons(
                           "lod_mmd", "LOD or MMD?",
                           c("LOD" = "lod",
                             "MMD" = "mmd")
                           )
                    )
                    
                    
                ),#end fluidRow
                
                # budget inputs #
                fluidRow(
                    column(width=4,
                           numericInput(
                               "B", "Total budget:",
                               100000,
                               min=1, max=9999999999
                           )
                    ),
                    column(width=4,
                           numericInput(
                               "c", "Cost per cluster:",
                               500,
                               min=1, max=9999999999
                           )
                    ),
                    column(width=4,
                           numericInput(
                               "s", "Cost per participant:",
                               50,
                               min=1, max=9999999999
                           )
                    )
                ),
                
                # provide exact ICC inputs if LOD #
                conditionalPanel(
                    condition = "input.lod_mmd == 'lod'",
                    fluidRow(
                        column(width=6,
                               numericInput(
                                   "rho_yx", "o-ICC",
                                   0.005,
                                   min = 0, max = 1, step=0.0001
                               )
                        ),
                        column(width=6,
                               conditionalPanel(
                                   condition = "input.objectives != 'SO_ATE'",
                                   numericInput(
                                       "rho_x", "c-ICC",
                                       0.5,
                                       min = 0, max = 1, step=0.0001
                                   )
                               )#rho.x conditional
                        )
                    )
                    
                    
                ),#exact ICCs conditional
                
                # provide ICC ranges if MDD #
                conditionalPanel(
                    condition = "input.lod_mmd == 'mmd'",
                    fluidRow(
                        column(width=6,
                               numericInput(
                                   "rho_yx_min", "Min. o-ICC",
                                   0.005,
                                   min = 0, max = 1, step=0.0001
                               )
                        ),
                        column(width=6,
                               numericInput(
                                   "rho_yx_max", "Max. o-ICC",
                                   0.2,
                                   min = 0, max = 1, step=0.0001
                               )
                        )
                    ),
                    
                    
                    conditionalPanel(
                        condition = "input.objectives != 'SO_ATE'",
                        fluidRow(
                            column(width=6,
                                   numericInput(
                                       "rho_x_min", "Min. c-ICC",
                                       0.1,
                                       min = 0, max = 1, step=0.0001
                                   )
                            ),
                            column(width=6,
                                   numericInput(
                                       "rho_x_max", "Max. c-ICC",
                                       0.95,
                                       min = 0, max = 1, step=0.0001
                                   )
                            )
                        )
                        
                    )#rho.x ranges conditional
                ),#mmd icc ranges conditional

                # show m ranges if looking at MMD or MO-LOD #
                #conditionalPanel(
                    #condition = "input.lod_mmd == 'mmd' | (input.objectives == 'MO' & input.lod_mmd == 'lod')",
                    # number of clusters #
                    fluidRow(
                        column(width=6,
                               numericInput(
                                   "n_min", "Min. Number of Clusters (n)",
                                   6,
                                   min=2, max=99999
                               )
                        ),
                        column(width=6,
                               numericInput(
                                   "n_max", "Max. Number of Clusters (n)",
                                   100,
                                   min=2, max=99999
                               )
                        )
                    ),
                    
                    # cluster size #
                    fluidRow(
                        column(width=6,
                               numericInput(
                                   "m_min", "Min. Cluster Size (m)",
                                   2,
                                   min=2, max=99999
                               )
                        ),
                        column(width=6,
                               numericInput(
                                   "m_max", "Max. Cluster Size (m)",
                                   100,
                                   min=2, max=99999#(((1/input$n_min)*input$B-input$c)/input$s)
                               )
                        )
                    ),#end fluidRow
                    
                #),#m/n ranges conditional

                # prioirty weight #
                conditionalPanel(
                    condition = "input.objectives == 'MO'",
                    numericInput(
                        "lambda", "Priority weight:",
                        0.5,
                        min=0, max=1, step=0.001
                                 )
                )#priority weight conditional


            )#vertical layout,
            #submitButton("Apply Changes")

        ),#sidebar panel
    #),#sidebar layout

    # Show a plot of the generated distribution
    mainPanel(
        tabsetPanel(#mainPanel(
            tabPanel("Optimal Design",
                     
                     conditionalPanel(
                         condition = "input.lod_mmd == 'lod' & input.objectives != 'MO'",
                         span( textOutput("text_rslt"), style="font-size:20px; font-style:italic" )
                     ),
                     
                     conditionalPanel(
                         condition =  "(input.lod_mmd == 'lod' & input.objectives == 'MO') | input.lod_mmd == 'mmd'",
                         DT::dataTableOutput("table_rslt")
                     ),
                     
                     conditionalPanel(
                         condition = "input.lod_mmd == 'mmd'",
                         #dataTableOutput("table_rslt"),
                         plotOutput("plot_mmd")
                     )#plot conditional
                     
            ),#main results tab
            tabPanel("3D Plots",
                     conditionalPanel(
                         condition = "input.lod_mmd == 'mmd'",
                         column(width=6,
                         plotlyOutput("plot_mmd_3d_oICC",
                                      height="450px",width="100%")),
                         column(width=6,
                                plotlyOutput("plot_mmd_3d_cICC",
                                             height="450px",width="100%")),

                     )#3d plot tab
            )#,#3d plot tab
            #tabPanel("Power",
                     # conditionalPanel(
                     #     condition = "input.lod_mmd == 'mmd'",
                     # 
                     # )#plot conditional
             #        )#power tab
        )#tabset panel
    )#main panel
    
    )#fluid page
}

# Define server logic required to draw a histogram
server <- function(input, output) {

    #### FUNCTIONS ####

    poly_root <- function(x,y){
        sign(x) * abs(x)^(1/y)
    }
    
    m_opt_hte <- function(rho_yx, rho_x, B, c, s, min_n=6, quiet=TRUE,keep_invalid=FALSE){
        k <- c/s
        
        lower_limit <- rho_yx*(k+1)/(rho_yx*k+1)
        m_max <- ( (B/min_n) - c )/s
        
        # if we're going to get an invalid m #
        if(rho_x - lower_limit < 0+.Machine$double.eps){
            
            if(keep_invalid==FALSE){
                return(floor(m_max))
                
            }else{
                
                if(quiet==FALSE){
                    print("Rho_x outside of valid range")
                }
                return(NA)
            }
            
            
        } else{
            # where m will be valid #
            num_sqrt <- (1/rho_yx)*(1/k)*(1-rho_yx)*(rho_x-rho_yx)*( 1-(k+2)*rho_yx + (k+1)*rho_x*rho_yx )
            
            numerator <- (1-rho_yx)*(1-rho_x) + sqrt( num_sqrt )
            
            denominator <- (1/k)*(rho_x-rho_yx) - rho_yx*(1-rho_x)
            
            m <- floor( numerator/denominator )
            
            if(m <= m_max){
                return(m)  
            }else{
                return(floor(m_max))
            }
            
            
            
        }
        
    }
    
    m_opt_ote <- function(rho_yx,c,s){
        k <- c/s
        
        m <- sqrt( ( (k*(1-rho_yx))/rho_yx ) )
        
        return(floor(m))
    }

    n_opt <- function(m_opt, B, c, s){
        if(is.na(m_opt)){
            return(NA)
        }else{
            n_up <- ceiling( B/(c+s*m_opt) )
            n_down <- floor( B/(c+s*m_opt) )
            
            if( (n_up*(c+s*m_opt) > B) & n_down > 0){
                
                
                return(n_down)
                
            }else{
                
                return(n_up)
                
            }
        }
        
    }
    
    var_b4 <- function(m, rho_yx, rho_x, B, c, s){
        k <- c/s
        
        part1 <- s*(1-rho_yx)/B
        
        part2 <- (k+m)*(1+(m-1)*rho_yx)/( m*(1+(m-2)*rho_yx - (m-1)*rho_x*rho_yx) )
        
        part1*part2
    }
    
    var_b2 <- function(m,rho_yx, B,c,s){
        k <- c/s
        s2 <- s*(k+m)*(1+(m-1)*rho_yx)/(B*m)
        return(s2)
    }

    RE <- function(m, rho_yx, rho_x, B, c, s, min_n=6){
        # m is a vector of the span of the design space
        # everything else is a scalar
        # returns a vector of REs for specific (rho_yx, rho_x) combo over a range of m's
        #k <- c/s
        m_o <- m_opt_hte(rho_yx, rho_x, B,c,s, min_n)
        if(is.na(m_o)){
            #return(NA)
            var_LOD <- var_b4(max(m), rho_yx, rho_x, B, c, s)
            
            var_other <- mapply( var_b4, m, rho_yx, rho_x, B, c, s)
            
            var_LOD/var_other
        }else{
            var_LOD <- var_b4(m_o, rho_yx, rho_x, B, c, s)
            
            var_other <- mapply( var_b4, m, rho_yx, rho_x, B, c, s)
            
            var_LOD/var_other
        }
        
    }
    
    RE.ate <- function(m, rho_yx, B, c, s, min_n=6){
        # m is a vector of the span of the design space
        # everything else is a scalar
        # returns a vector of REs for specific rho_yx over a range of m's
        #k <- c/s
        m_o <- m_opt_ote(rho_yx, c,s)
        if(is.na(m_o)){
            #return(NA)
            var_LOD <- var_b2(max(m), rho_yx, B, c, s)
            
            var_other <- mapply( var_b4, m, rho_yx, rho_x, B, c, s)
            
            var_LOD/var_other
        }else{
            var_LOD <- var_b2(m_o, rho_yx, B, c, s)
            
            var_other <- mapply( var_b2, m, rho_yx, B, c, s)
            
            var_LOD/var_other
        }
        
    }

    power_hte <- function(rho_yx, rho_x, m, n, s_yx=1, s_x=1, s_w=sqrt(0.25), d, a=0.05){
        z <- qnorm(1-a/2)
        num_sqrt <- d^2*s_w^2*s_x^2*( 1 + (m-2)*rho_yx - (m-1)*rho_x*rho_yx )
        denom_sqrt <- s_yx^2*(1-rho_yx)*( 1 + (m-1)*rho_yx )
        
        inside <- sqrt(n*m*num_sqrt/denom_sqrt)-z
        
        pnorm(inside)
    }
    
    power_ote <- function(rho_yx, m, n, s_yx=1, s_w=sqrt(0.25), d, a=0.05){
        z <- qnorm(1-a/2)
        num_sqrt <- d^2*s_w^2
        denom_sqrt <- s_yx^2*(1-rho_yx)*( 1 + (m-1)*rho_yx )
        
        inside <- sqrt(n*m*num_sqrt/denom_sqrt)-z
        
        pnorm(inside)
    }
    
    opt_crit <- function(m,rho_yx,rho_x,lambda, B,c,s){
        
        
        
        m_ote <- m_opt_ote(rho_yx,c,s)
        m_hte <- m_opt_hte(rho_yx,rho_x, B,c,s)
        
        ote_part <- (lambda/var_b2(m_ote,rho_yx,B,c,s))*var_b2(m,rho_yx,B,c,s)
        hte_part <- ((1-lambda)/var_b4(m_hte,rho_yx,rho_x,B,c,s))*var_b4(m,rho_yx,rho_x,B,c,s)
        
        rslt <- ote_part + hte_part
        return(rslt) 
        
    }

    opt_crit.RE <- function(m,rho_yx,rho_x,lambda, B,c,s){
        
        
        
        m_ote <- m_opt_ote(rho_yx,c,s)
        m_hte <- m_opt_hte(rho_yx,rho_x, B,c,s)
        
        RE_ote <- var_b2(m_ote, rho_yx,B,c,s)/var_b2(m, rho_yx,B,c,s)
        RE_hte <- var_b4(m_hte, rho_yx, rho_x, B,c,s)/var_b4(m, rho_yx, rho_x, B,c,s)
        
        ote_part <- lambda*RE_ote
        hte_part <- (1-lambda)*RE_hte
        
        rslt <- ote_part + hte_part
        return(rslt) 
        
    }
    
    mo_lod <- function(rho_yx,rho_x,lambda,B,c,s, min_n=6,epsilon=1e-6,keep_invalid=FALSE){
        
        m_ote <- m_opt_ote(rho_yx,c,s)
        var_ote <- var_b2(m_ote,rho_yx, B,c,s)
        w2 <-lambda/var_ote
        
        m_hte <- m_opt_hte(rho_yx, rho_x, B, c, s)
        var_hte <- var_b4(m_hte, rho_yx, rho_x, B, c, s)
        w4 <- (1-lambda)/var_hte
        
        k <- c/s
        
        if(rho_x==1){
            #rho_x <- rho_x - epsilon
            
            root1 <- -sqrt( k*(1-rho_yx)/rho_yx )#*(w2*(1-rho_yx) + w4)/(rho_yx*(w2*(1-rho_yx) + w4)) )#sqrt( k*(w2*(1-rho_yx) + w4)/(rho_yx*(w2 + w4/(1-rho_yx))) )
            root2 <- sqrt( k*(1-rho_yx)/rho_yx )#*(w2*(1-rho_yx) + w4)/(rho_yx*(w2*(1-rho_yx) + w4)) )#-sqrt( k*(w2*(1-rho_yx) + w4)/(rho_yx*(w2 + w4/(1-rho_yx))) )
            
            return(c(NA,NA,root1,root2))
            
        }else{
            
            
            
            d1 <- rho_x*rho_yx*(3-rho_x)-2*rho_yx-rho_x+1
            d2 <- (rho_yx*(rho_x-2)+1)^2
            d3 <- rho_yx*(1-rho_yx)*k
            d4 <- rho_yx^3*(rho_x-1)^2
            
            b1 <- rho_yx*(rho_x-rho_yx)
            a1 <- rho_yx^2*(1-rho_x)
            a2 <- 2*rho_yx*(1-rho_yx)*(1-rho_x)
            a3 <- (1-2*rho_yx+rho_x*rho_yx)*(1-rho_yx)
            
            q4 <- w2*d4
            q3 <- 2*w2*rho_yx^2*d1
            q2 <- w2*rho_yx*(d2-d3*(rho_x-1)^2) + w4*(b1-k*a1)
            q1 <- -(2*w2*d3*d1 + w4*k*a2)
            q0 <- -(w2*k*(1-rho_yx)*d2 + w4*k*a3)
            
            p1 <- 2*q2^3-9*q3*q2*q1 + 27*q4*q1^2 + 27*q3^2*q0 - 72*q4*q2*q0
            p2 <- p1+sqrt(-4*(q2^2-3*q3*q1 + 12*q4*q0)^3 + p1^2)
            p3 <- (q2^2-3*q3*q1 + 12*q4*q0)/(3*q4*poly_root((p2/2),3)) + (poly_root((p2/2),3))/(3*q4)
            p4 <- sqrt(q3^2/(4*q4^2) - (2*q2)/(3*q4)+p3)
            p5 <- q3^2/(2*q4^2) - (4*q2)/(3*q4) - p3
            p6 <- (-(q3^3)/(q4^3) + (4*q3*q2)/(q4^2) - (8*q1)/(q4))/(4*p4)
            
            
            root1 <- -q3/(4*q4) - p4/2 - sqrt(p5-p6)/2
            root2 <- -q3/(4*q4) - p4/2 + sqrt(p5-p6)/2
            root3 <- -q3/(4*q4) + p4/2 - sqrt(p5+p6)/2
            root4 <- -q3/(4*q4) + p4/2 + sqrt(p5+p6)/2
            
            if( !(is.nan(root4)) ){
                return(c(root1,root2,root3,root4))
            }else{
                
                if(keep_invalid){
                    return(c(root1,root2,root3,root4))
                }else{ #if(rho_x !=1){
                    max_m <- ((1/min_n)*B-c)/s
                    return(c(root1,root2,root3,max_m))
                }
            }
            
        }
        
        
        
        
        
    }
    
    mo_lod.RE <- function(rho_yx, rho_x, lambda, B,c,s, min_n=6, keep_invalid=FALSE, quiet=TRUE){
        m_max <- floor(( (B/min_n) - c )/s)
        k <- c/s
        rslt <- rep(NA,2)
        
        if(quiet==TRUE){
            suppressWarnings({
                # LOD HTE m and variance #
                m_hte_o <- m_opt_hte(rho_yx, rho_x, B,c,s, min_n=min_n, keep_invalid=keep_invalid)
                var_hte_LOD <- var_b4(m_hte_o, rho_yx, rho_x, B, c, s)
                
                # LOD ATE m and variance #
                m_ate_o <- m_opt_ote(rho_yx,c,s)
                var_ate_LOD <- var_b2(m_ate_o, rho_yx, B, c, s)
                
                # OC weights using LOD variances #
                # m can be < 0 or NAN if lambda < 0_25 and rhos are close to each other (around 0_1 or 0_2)
                w4 <- (1-lambda)*var_hte_LOD
                w2 <- lambda*var_ate_LOD
                
                # OC #
                a1 <- rho_yx^2*(1-rho_x)
                a2 <- 2*rho_yx*(1-rho_yx)*(1-rho_x)
                a3 <- (1-2*rho_yx+rho_x*rho_yx)*(1-rho_yx)
                b1 <- rho_yx*(rho_x-rho_yx)
                
                a_q <- w4*(k*a1-b1)-w2*rho_yx
                b_q <- w4*k*a2
                c_q <- w2*k*(1-rho_yx)+w4*k*a3
                
                rslt[1] <- floor((-b_q+sqrt(b_q^2-4*a_q*c_q))/(2*a_q))
                rslt[2] <- floor((-b_q-sqrt(b_q^2-4*a_q*c_q))/(2*a_q))
                
                w2_limit <- w4*((k+1)*rho_yx - rho_x*(k*rho_yx+1))
                
                
                if(keep_invalid==FALSE){
                    if(w2 - w2_limit < 0+.Machine$double.eps){
                        return(m_max)
                    }else if(rslt[2] <= m_max){
                        return(rslt[2])
                    }else{
                        return(m_max)
                    }
                }else{
                    return(rslt[2])
                }
            })
        }else{
            # LOD HTE m and variance #
            m_hte_o <- m_opt_hte(rho_yx, rho_x, B,c,s, min_n=min_n, keep_invalid=keep_invalid)
            var_hte_LOD <- var_b4(m_hte_o, rho_yx, rho_x, B, c, s)
            
            # LOD ATE m and variance #
            m_ate_o <- m_opt_ote(rho_yx,c,s)
            var_ate_LOD <- var_b2(m_ate_o, rho_yx, B, c, s)
            
            # OC weights using LOD variances #
            # m can be < 0 or NAN if lambda < 0_25 and rhos are close to each other (around 0_1 or 0_2)
            w4 <- (1-lambda)*var_hte_LOD
            w2 <- lambda*var_ate_LOD
            
            # OC #
            a1 <- rho_yx^2*(1-rho_x)
            a2 <- 2*rho_yx*(1-rho_yx)*(1-rho_x)
            a3 <- (1-2*rho_yx+rho_x*rho_yx)*(1-rho_yx)
            b1 <- rho_yx*(rho_x-rho_yx)
            
            a_q <- w4*(k*a1-b1)-w2*rho_yx
            b_q <- w4*k*a2
            c_q <- w2*k*(1-rho_yx)+w4*k*a3
            
            rslt[1] <- floor((-b_q+sqrt(b_q^2-4*a_q*c_q))/(2*a_q))
            rslt[2] <- floor((-b_q-sqrt(b_q^2-4*a_q*c_q))/(2*a_q))
            
            w2_limit <- w4*((k+1)*rho_yx - rho_x*(k*rho_yx+1))
            
            
            if(keep_invalid==FALSE){
                if(w2 - w2_limit < 0+.Machine$double.eps){
                    return(m_max)
                }else if(rslt[2] <= m_max){
                    return(rslt[2])
                }else{
                    return(m_max)
                }
            }else{
                return(rslt[2])
            }
        }
        
        
        
        
        
        
    }

    #### OUTPUT ####
    # make things reactive #
    B_react <- reactive({input$B})
    c_react <- reactive({input$c})
    s_react <- reactive({input$s})
    lambda_react <- reactive({input$lambda})

    n_min_react <- reactive({input$n_min})
    n_max_react <- reactive({input$n_max})
    m_min_react <- reactive({input$m_min})
    m_max_react <- reactive({input$m_max})

    rho_x_react <- reactive({input$rho_x})
    rho_x_min_react <- reactive({input$rho_x_min})
    rho_x_max_react <- reactive({input$rho_x_max})
    rho_yx_react <- reactive({input$rho_yx})
    rho_yx_min_react <- reactive({input$rho_yx_min})
    rho_yx_max_react <- reactive({input$rho_yx_max})

    lod_mmd_react <- reactive({input$lod_mmd})
    objectives_react <- reactive({input$objectives})
    
    ## Render text first ##
    output$text_rslt <- renderText({
        if(lod_mmd_react()=="lod"){
            if(objectives_react() == "SO_HTE"){
                # SO-LOD HTE #
                m_lod <- m_opt_hte(rho_yx=rho_yx_react(), rho_x=rho_x_react(),
                                   B=B_react(), c=c_react(), s=s_react(),
                                   min_n=n_min_react())
                n_lod <- n_opt(m_opt=m_lod,
                               B=B_react(), c=c_react(), s=s_react())
                # lod_rslt <- matrix(c(m_lod, n_lod),ncol=2,nrow=1)
                # lod_rslt %>%
                #     as.data.frame() %>% 
                #     rename(m=V1,
                #            n=V2)
                
                paste("The LOD at o-ICC = ", rho_yx_react(), ", c-ICC = ", rho_x_react(), " with a total budget of $", B_react(), " and cost ratio of ", (c_react()/s_react()), " is m=", m_lod, ", n=", n_lod)
                
            }else if(objectives_react() == "SO_ATE"){
                # SO-LOD ATE #
                m_lod <- m_opt_ote(rho_yx=rho_yx_react(),
                                   c=c_react(),s=s_react())
                n_lod <- n_opt(m_opt=m_lod,
                               B=B_react(), c=c_react(), s=s_react())
                
                #lod_rslt <- matrix(c(m_lod, n_lod), ncol=2, nrow=1)
                
                # lod_rslt %>%
                #     as.data.frame() %>% 
                #     rename(m=V1,
                #            n=V2)
                
                paste("The LOD at o-ICC = ", rho_yx_react(), ", c-ICC = ", rho_x_react(), " with a total budget of $", B_react(), " and cost ratio of ", (c_react()/s_react()), " is m=", m_lod, ", n=", n_lod)
                
            }
        }
    })
    ## Then Render tables ##
    # LOD #
    output$table_rslt <- DT::renderDataTable({
        if(lod_mmd_react()=="lod"){
            if(objectives_react() == "MO"){
                # MO-LOD #
                #m <- seq(m_min_react(), m_max_react())
                
                mat <- cbind(lambda_react(), rho_yx_react(), rho_x_react(),
                               mo_lod.RE(rho_yx=rho_yx_react(),rho_x=rho_x_react(),
                                        lambda=lambda_react(),
                                        B=B_react(),c=c_react(),s=s_react(), min_n=n_min_react()),
                             opt_crit.RE(m=mo_lod.RE(rho_yx=rho_yx_react(),rho_x=rho_x_react(),
                                                   lambda=lambda_react(),
                                                   B=B_react(),c=c_react(),s=s_react(), min_n=n_min_react()),
                                         rho_yx=rho_yx_react(), rho_x=rho_x_react(),
                                         lambda=lambda_react(), B=B_react(), c=c_react(), s=s_react()))
                
                mat %>%
                    as.data.frame() %>%
                    rename(lambda=V1,
                           rho_yx=V2,
                           rho_x=V3,
                           mo_lod_RE=V4,
                           opt_crit_RE=V5) %>%
                    group_by(rho_yx,rho_x) %>%
                    #slice(which.min(opt_crit_search)) %>%
                    mutate(n = n_opt(m_opt=mo_lod_RE,
                                     B_react(), c_react(), s_react()),
                           opt_crit_RE = round(opt_crit_RE,3)) %>% 
                    dplyr::select(lambda, rho_yx, rho_x, opt_crit_RE, mo_lod_RE, n) %>% 
                    rename(`Optimality Criterion`=opt_crit_RE,
                           `o-ICC`=rho_yx,
                           `c-ICC`=rho_x,
                           `Priority Weight`=lambda,
                           m=mo_lod_RE)

            }#objectives ifelse



    }else{
        # MMD #
        if(objectives_react() == "SO_HTE"){
            ## SO-MMD HTE ##
            # STEP 1 #
            rho_yx <- c(rho_yx_min_react(), rho_yx_max_react())
            yx.n <- length(rho_yx)
            rho_x <- c(rho_x_min_react(), rho_x_max_react())
            x.n <- length(rho_x)
            rho <- cbind(rep(rho_yx, each=x.n), rep(rho_x, times=yx.n))

            m <- seq(m_min_react(), m_max_react())
            n <- seq(n_min_react(), n_max_react())

            k <- c_react()/s_react()

            # STEP 2 #
            for(i in 1:nrow(rho)){
                mat.t <- cbind(m, rho[i,1], rho[i,2],
                               RE(m, rho[i,1], rho[i,2], B_react(), c_react(), s_react()),
                               B_react(), c_react(), s_react())
                if(i == 1){
                    mat <- mat.t
                }else{
                    mat <- rbind(mat, mat.t)
                }

            }
            colnames(mat) <- c("m", "rho_yx", "rho_x", "RE","B","c","s")

            # STEP 3 #
            # choose the MMD #
            mat %>%
                as.data.frame() %>%
                drop_na() %>%
                dplyr::filter(RE != -Inf) %>%
                group_by(m) %>%
                slice(which.min(RE)) %>%
                ungroup() %>%
                group_by(rho_yx,rho_x) %>% 
                slice_max(RE) %>% 
                arrange(desc(RE)) %>%
                ungroup() %>% 
                dplyr::filter(abs(first(m) - m) < 2) %>% 
                mutate(n = n_opt(m_opt=m,
                                 B_react(), c_react(), s_react()),
                       RE = round(RE,3)) %>% 
                dplyr::select(rho_yx, rho_x, RE, m, n) %>% 
                rename(`o-ICC`=rho_yx,
                       `c-ICC`=rho_x)
            

        }else if(objectives_react() == "SO_ATE"){
            ## SO-MMD ATE ##
            # STEP 1 #
            rho_yx <- c(rho_yx_min_react(), rho_yx_max_react())
            # yx.n <- length(rho_yx)
            # rho_x <- c(rho_x_min_react(), rho_x_max_react())
            # x.n <- length(rho_x)
            # rho <- cbind(rep(rho_yx, each=x.n), rep(rho_x, times=yx.n))
            
            m <- seq(m_min_react(), m_max_react())
            n <- seq(n_min_react(), n_max_react())
            
            k <- c_react()/s_react()
            
            # STEP 2 #
            for(i in 1:length(rho_yx)){
                mat.t <- cbind(m, rho_yx[i],
                               RE.ate(m, rho_yx[i], B_react(), c_react(), s_react()),
                               B_react(), c_react(), s_react())
                if(i == 1){
                    mat <- mat.t
                }else{
                    mat <- rbind(mat, mat.t)
                }
                
            }
            colnames(mat) <- c("m", "rho_yx", "RE","B","c","s")
            
            # STEP 3 #
            # choose the MMD #
            mat %>%
                as.data.frame() %>%
                drop_na() %>%
                dplyr::filter(RE != -Inf) %>%
                group_by(m) %>%
                slice(which.min(RE)) %>%
                ungroup() %>%
                group_by(rho_yx) %>% 
                slice_max(RE) %>% 
                arrange(desc(RE)) %>%
                ungroup() %>% 
                dplyr::filter(abs(first(m) - m) < 2) %>% 
                mutate(n = n_opt(m_opt=m,
                                 B_react(), c_react(), s_react()),
                       RE = round(RE,3)) %>% 
                dplyr::select(rho_yx, RE, m, n) %>% 
                rename(`o-ICC`=rho_yx)


        }else{
            ## MO-MMD ##
            # STEP 1 #
            rho_yx <- c(rho_yx_min_react(), input$rho_yx_max)
            yx.n <- length(rho_yx)
            rho_x <- c(rho_x_min_react(), rho_x_max_react())
            x.n <- length(rho_x)
            rho <- cbind(rep(rho_yx, each=x.n), rep(rho_x, times=yx.n))

            k <- c_react()/s_react()

            m <- seq(m_min_react(),m_max_react())

            # STEP 2 #
            temp <- mat <- NULL
            for(i in seq(nrow(rho))){

                temp <-  cbind(m, lambda_react(), rho[i,1], rho[i,2],
                               opt_crit.RE(m, rho[i,1], rho[i,2], input$lambda, B_react(), c_react(), s_react()),
                               B_react(), c_react(), s_react())

                if(i==1){
                    mat <- temp
                }else{
                    mat <- rbind(mat, temp)
                }

            }

            colnames(mat) <- c("m", "lambda", "rho_yx", "rho_x", "opt_crit", "B", "c", "s")

            # STEP 3 #
            mat %>%
                as.data.frame() %>%
                drop_na() %>%
                group_by(m) %>%
                slice(which.min(opt_crit)) %>%
                ungroup() %>%
                group_by(rho_yx,rho_x) %>% 
                slice_max(opt_crit) %>% 
                arrange(desc(opt_crit)) %>%
                ungroup() %>% 
                dplyr::filter(abs(first(m) - m) < 2) %>% 
                mutate(n = n_opt(m,B,c,s),
                       opt_crit = round(opt_crit,3)) %>% 
                dplyr::select(lambda, rho_yx, rho_x, opt_crit, m, n) %>% 
                rename(`Optimality Criterion`=opt_crit,
                       `o-ICC`=rho_yx,
                       `c-ICC`=rho_x,
                       `Priority Weight`=lambda)


            }#objectives ifelse

        }#lod-mmd ifelse

    },
    # data table options #
    options = list(
        #scrollY = 250,
        #pageLength = 100,
        dom = "t",
        ordering = FALSE
    ),
    rownames=FALSE
    )

    # Now render plots for MMD #
    output$plot_mmd <- renderPlot({
        if(lod_mmd_react() == "mmd"){
        if(objectives_react() == "SO_HTE"){
            ## SO-MMD HTE ##
            # STEP 1 #
            rho_yx <- c(rho_yx_min_react(), rho_yx_max_react())
            yx.n <- length(rho_yx)
            rho_x <- c(rho_x_min_react(), rho_x_max_react())
            x.n <- length(rho_x)
            rho <- cbind(rep(rho_yx, each=x.n), rep(rho_x, times=yx.n))

            m <- seq(m_min_react(), m_max_react())
            n <- seq(n_min_react(), n_max_react())

            k <- c_react()/s_react()

            # STEP 2 #
            for(i in 1:nrow(rho)){
                mat.t <- cbind(m, rho[i,1], rho[i,2],
                               RE(m, rho[i,1], rho[i,2], B_react(), c_react(), s_react()),
                               B_react(), c_react(), s_react())
                if(i == 1){
                    mat <- mat.t
                }else{
                    mat <- rbind(mat, mat.t)
                }

            }
            colnames(mat) <- c("m", "rho_yx", "rho_x", "RE","B","c","s")

            # STEP 3 #
            legend_title <- latex2exp::TeX("($\\rho_{y|x}$, $\\rho_x$)")
            # visualize before actually choosing MMD #
            plot_mmd <- mat %>%
                as.data.frame() %>%
                mutate(rho=factor( paste("(",rho_yx,", ", rho_x, ")") )) %>%
                drop_na() %>%
                dplyr::filter(RE != -Inf) %>%
                ggplot(aes(x=m, y=RE))+
                geom_line(aes(color=rho, linetype=rho), size=1.2)+
                ylim(0,1)+
                xlim(0,max(m))+
                xlab("Cluster Size (m)")+
                labs(color=legend_title, linetype=legend_title)+
                ggtitle("HTE SO-MMD")+
                theme_minimal()+
                theme(axis.title = element_text(size=18),
                      text=element_text(size=18),
                      title=element_text(size=18))+
                scale_color_brewer(palette="Dark2")


            # choose the MMD #
            mmd_rslt <- mat %>%
                as.data.frame() %>%
                drop_na() %>%
                dplyr::filter(RE != -Inf) %>%
                group_by(m) %>%
                slice(which.min(RE)) %>%
                ungroup() %>%
                slice(which.max(RE))

                plot_mmd+
                geom_vline(xintercept=mmd_rslt$m, linetype="dashed", color="darkgrey", size=1.2)+
                geom_text(mapping = aes(x=mmd_rslt$m, y=0,
                                        label=paste0("MMD:\n m=", mmd_rslt$m, ", n=", n_opt(mmd_rslt$m,
                                                                                     B_react(), c_react(), s_react())),
                                        hjust = 1.2, vjust = -1), color="darkgrey", size=6)

        }else if(objectives_react() == "SO_ATE"){
            ## SO-MMD ATE ##
            # STEP 1 #
            rho_yx <- c(rho_yx_min_react(), rho_yx_max_react())
            # yx.n <- length(rho_yx)
            # rho_x <- c(rho_x_min_react(), rho_x_max_react())
            # x.n <- length(rho_x)
            # rho <- cbind(rep(rho_yx, each=x.n), rep(rho_x, times=yx.n))
            
            m <- seq(m_min_react(), m_max_react())
            n <- seq(n_min_react(), n_max_react())
            
            k <- c_react()/s_react()
            
            # STEP 2 #
            for(i in 1:length(rho_yx)){
                mat.t <- cbind(m, rho_yx[i],
                               RE.ate(m, rho_yx[i], B_react(), c_react(), s_react()),
                               B_react(), c_react(), s_react())
                if(i == 1){
                    mat <- mat.t
                }else{
                    mat <- rbind(mat, mat.t)
                }
                
            }
            colnames(mat) <- c("m", "rho_yx", "RE","B","c","s")
            
            # STEP 3 #
            legend_title_ATE <- latex2exp::TeX("$\\rho_{y|x}$")
            # visualize before actually choosing MMD #
            plot_mmd <- mat %>%
                as.data.frame() %>%
                mutate(rho_yx=factor( rho_yx )) %>%
                drop_na() %>%
                dplyr::filter(RE != -Inf) %>%
                ggplot(aes(x=m, y=RE))+
                geom_line(aes(color=rho_yx, linetype=rho_yx), size=1.2)+
                ylim(0,1)+
                xlim(0,max(m))+
                xlab("Cluster Size (m)")+
                labs(color=legend_title_ATE, linetype=legend_title_ATE)+
                ggtitle("ATE SO-MMD")+
                theme_minimal()+
                theme(axis.title = element_text(size=18),
                      text=element_text(size=18),
                      title=element_text(size=18))+
                scale_color_brewer(palette="Dark2")
            
            
            # choose the MMD #
            mmd_rslt <- mat %>%
                as.data.frame() %>%
                drop_na() %>%
                dplyr::filter(RE != -Inf) %>%
                group_by(m) %>%
                slice(which.min(RE)) %>%
                ungroup() %>%
                slice(which.max(RE))
            
            plot_mmd+
                geom_vline(xintercept=mmd_rslt$m, linetype="dashed", color="darkgrey", size=1.2)+
                geom_text(mapping = aes(x=mmd_rslt$m, y=0,
                                        label=paste0("MMD:\n m=", mmd_rslt$m, ", n=", n_opt(mmd_rslt$m,
                                                                                          B_react(), c_react(), s_react())),
                                        hjust = 1.2, vjust = -1), color="darkgrey", size=6)

        }else{
            ## MO-MMD ##
            # STEP 1 #
            rho_yx <- c(rho_yx_min_react(), input$rho_yx_max)
            yx.n <- length(rho_yx)
            rho_x <- c(rho_x_min_react(), rho_x_max_react())
            x.n <- length(rho_x)
            rho <- cbind(rep(rho_yx, each=x.n), rep(rho_x, times=yx.n))

            k <- c_react()/s_react()

            m <- seq(m_min_react(),m_max_react())

            # STEP 2 #
            temp <- mat <- NULL
            for(i in seq(nrow(rho))){

                temp <-  cbind(m, lambda_react(), rho[i,1], rho[i,2],
                               opt_crit.RE(m, rho[i,1], rho[i,2], input$lambda, B_react(), c_react(), s_react()),
                               B_react(), c_react(), s_react())

                if(i==1){
                    mat <- temp
                }else{
                    mat <- rbind(mat, temp)
                }

            }

            colnames(mat) <- c("m", "lambda", "rho_yx", "rho_x", "opt_crit", "B", "c", "s")

            # STEP 3 #
            legend_title_MO <- latex2exp::TeX("($\\rho_{y|x}, \\rho_x$)")
            # visualize before actually choosing MMD #
            plot_mmd <- mat %>%
                as.data.frame() %>%
                mutate(rho=factor( paste("(",rho_yx,", ", rho_x, ")") )) %>%
                drop_na() %>%
                ggplot(aes(x=m, y=opt_crit))+
                geom_line(aes(color=rho, linetype=rho), size=1.2)+
                xlim(0,max(m))+
                xlab("Cluster Size (m)")+
                ylab("Optimality Criterion")+
                labs(color=legend_title_MO, linetype=legend_title_MO)+
                ggtitle("MO-MMD")+
                theme_minimal()+
                theme(axis.title = element_text(size=18),
                      text=element_text(size=18),
                      title=element_text(size=18))+
                scale_color_brewer(palette="Dark2")
            
            mmd_rslt <- mat %>%
                as.data.frame() %>%
                drop_na() %>%
                group_by(m) %>%
                slice(which.min(opt_crit)) %>%
                ungroup() %>%
                slice(which.max(opt_crit))

            plot_mmd +
            geom_vline(xintercept=mmd_rslt$m, linetype="dashed", color="darkgrey", size=1.2)+
            geom_text(mapping = aes(x=mmd_rslt$m, y=0,
                                    label=paste0("MO MMD:\n m=", mmd_rslt$m, ", n=", n_opt(mmd_rslt$m, mmd_rslt$B, mmd_rslt$c, mmd_rslt$s)),
                                    hjust = 1.1, vjust = -0.15), color="darkgrey", size=6)
        }#objectives ifelse

        }#lod-mmd ifelse

    })
    
    # Now render 3d plots for MMD #
    output$plot_mmd_3d_oICC <- renderPlotly({
        if(lod_mmd_react() == "mmd"){
            if(objectives_react() == "SO_HTE"){
                ## SO-MMD HTE ##
                # STEP 1 #
                midpoint <- round((rho_yx_max_react() - rho_yx_min_react())/2,3)
                rho_yx <- c(rho_yx_min_react(), midpoint, rho_yx_max_react())
                yx.n <- length(rho_yx)
                rho_x <- seq(rho_x_min_react(), rho_x_max_react(),by=0.05)
                x.n <- length(rho_x)
                rho <- cbind(rep(rho_yx, each=x.n), rep(rho_x, times=yx.n))
                
                m <- seq(m_min_react(), m_max_react())
                n <- seq(n_min_react(), n_max_react())
                
                k <- c_react()/s_react()
                
                # STEP 2 #
                for(i in 1:nrow(rho)){
                    mat.t <- cbind(m, rho[i,1], rho[i,2],
                                   RE(m, rho[i,1], rho[i,2], B_react(), c_react(), s_react()),
                                   B_react(), c_react(), s_react())
                    if(i == 1){
                        mat <- mat.t
                    }else{
                        mat <- rbind(mat, mat.t)
                    }
                    
                }
                colnames(mat) <- c("m", "rho_yx", "rho_x", "RE","B","c","s")
                mat <- as.data.frame(mat)
                
                RE_min <- mat %>% 
                    dplyr::filter(rho_yx==rho_yx_min_react()) %>% 
                    dplyr::select(rho_x,m,RE) %>% 
                    pivot_wider(names_from=m,values_from=RE) %>% 
                    dplyr::select(!rho_x) %>% 
                    as.matrix()
                
                RE_mid <- mat %>%
                    dplyr::filter(rho_yx==midpoint) %>%
                    dplyr::select(rho_x,m,RE) %>%
                    pivot_wider(names_from=m,values_from=RE) %>%
                    dplyr::select(!rho_x) %>%
                    as.matrix()
                
                RE_max <- mat %>% 
                    dplyr::filter(rho_yx==rho_yx_max_react()) %>% 
                    dplyr::select(rho_x,m,RE) %>% 
                    pivot_wider(names_from=m,values_from=RE) %>% 
                    dplyr::select(!rho_x) %>% 
                    as.matrix()
                
                # STEP 3 #
                fig_3d <- mat %>% 
                    dplyr::filter(rho_yx==rho_yx_min_react()) %>% 
                    plot_ly() %>%
                    add_surface(y=rho_x, x=m, z=~RE_min, opacity=0.8,colorscale=list(c(0,1),c("#54278f","#cbc9e2")),#"Picnic",
                                contours = list(
                                    y = list(show = TRUE, start = rho_x_min_react(), end = rho_x_max_react(), size = 0.1, color = 'white'),
                                    x = list(show = TRUE, start = m_min_react(), end = m_max_react(), size = 20, color="white")),
                                #z=list(show=TRUE,start=0,end=1,size=0.1,color="white")),
                                colorbar=list(title=paste("o-ICC=", rho_yx_min_react()))) %>%
                    layout(scene = list(xaxis = list(title = "m",autorange="reversed"),
                                        yaxis = list(title = "c-ICC"),
                                        zaxis = list(title = "RE"),
                                        camera=list(
                                            eye=list(
                                                x=0.5,
                                                y=-2,
                                                z=0.7
                                            )
                                        )
                                        ))
                
                fig_3d %>%
                    add_surface(y=rho_x, x=m,z=~RE_mid, colorscale=list(c(0,1), c("#a63603","#fdbe85")), opacity=0.8,
                                contours = list(
                                    y = list(show = TRUE, start = rho_x_min_react(), end = rho_x_max_react(), size = 0.1, color = 'white'),
                                    x = list(show = TRUE, start = m_min_react(), end = m_max_react(), size = 20, color="white")),
                                colorbar=list(title=paste("o-ICC=", midpoint))) %>%
                    add_surface(y=rho_x, x=m,z=~RE_max, colorscale=list(c(0,1),c("#08519c","#bdd7e7")), opacity=0.8,
                                contours = list(
                                    y = list(show = TRUE, start = rho_x_min_react(), end = rho_x_max_react(), size = 0.1, color = 'white'),
                                    x = list(show = TRUE, start = m_min_react(), end = m_max_react(), size = 20, color="white")),
                                colorbar=list(title=paste("o-ICC=", rho_yx_max_react())))


            
                
            }else if(objectives_react() == "MO"){
                ## MO-MMD ##
                # STEP 1 #
                midpoint <- round((rho_yx_max_react() - rho_yx_min_react())/2,3)
                rho_yx <- c(rho_yx_min_react(), midpoint, rho_yx_max_react())
                yx.n <- length(rho_yx)
                rho_x <- seq(rho_x_min_react(), rho_x_max_react(),by=0.05)
                x.n <- length(rho_x)
                rho <- cbind(rep(rho_yx, each=x.n), rep(rho_x, times=yx.n))

                k <- c_react()/s_react()

                m <- seq(m_min_react(),m_max_react())

                # STEP 2 #
                temp <- mat <- NULL
                for(i in seq(nrow(rho))){

                    temp <-  cbind(m, lambda_react(), rho[i,1], rho[i,2],
                                   opt_crit.RE(m, rho[i,1], rho[i,2], input$lambda, B_react(), c_react(), s_react()),
                                   B_react(), c_react(), s_react())

                    if(i==1){
                        mat <- temp
                    }else{
                        mat <- rbind(mat, temp)
                    }

                }

                colnames(mat) <- c("m", "lambda", "rho_yx", "rho_x", "opt_crit", "B", "c", "s")
                mat <- as.data.frame(mat)
                
                opt_crit_min <- mat %>% 
                    dplyr::filter(rho_yx==rho_yx_min_react()) %>% 
                    dplyr::select(rho_x,m,opt_crit) %>% 
                    pivot_wider(names_from=m,values_from=opt_crit) %>% 
                    dplyr::select(!rho_x) %>% 
                    as.matrix()
                
                opt_crit_mid <- mat %>%
                    dplyr::filter(rho_yx==midpoint) %>%
                    dplyr::select(rho_x,m,opt_crit) %>%
                    pivot_wider(names_from=m,values_from=opt_crit) %>%
                    dplyr::select(!rho_x) %>%
                    as.matrix()
                
                opt_crit_max <- mat %>% 
                    dplyr::filter(rho_yx==rho_yx_max_react()) %>% 
                    dplyr::select(rho_x,m,opt_crit) %>% 
                    pivot_wider(names_from=m,values_from=opt_crit) %>% 
                    dplyr::select(!rho_x) %>% 
                    as.matrix()
                
                fig_3d <- mat %>% 
                    dplyr::filter(rho_yx==rho_yx_min_react()) %>% 
                    plot_ly() %>%
                    add_surface(y=rho_x, x=m, z=~opt_crit_min, opacity=0.8,colorscale=list(c(0,1),c("#54278f","#cbc9e2")),#"Picnic",
                                contours = list(
                                    y = list(show = TRUE, start = rho_x_min_react(), end = rho_x_max_react(), size = 0.1, color = 'white'),
                                    x = list(show = TRUE, start = m_min_react(), end = m_max_react(), size = 20, color="white")),
                                #z=list(show=TRUE,start=0,end=1,size=0.1,color="white")),
                                colorbar=list(title=paste("o-ICC=", rho_yx_min_react()))) %>%
                    layout(scene = list(xaxis = list(title = "m",autorange="reversed"),
                                        yaxis = list(title = "c-ICC"),
                                        zaxis = list(title = "Optimality Criterion"),
                                        camera=list(
                                            eye=list(
                                                x=0.5,
                                                y=-2,
                                                z=0.7
                                            )
                                        )
                    ))
                
                fig_3d %>%
                    add_surface(y=rho_x, x=m,z=~opt_crit_mid, colorscale=list(c(0,1), c("#a63603","#fdbe85")), opacity=0.8,
                                contours = list(
                                    y = list(show = TRUE, start = rho_x_min_react(), end = rho_x_max_react(), size = 0.1, color = 'white'),
                                    x = list(show = TRUE, start = m_min_react(), end = m_max_react(), size = 20, color="white")),
                                colorbar=list(title=paste("o-ICC=", midpoint))) %>%
                    add_surface(y=rho_x, x=m,z=~opt_crit_max, colorscale=list(c(0,1),c("#08519c","#bdd7e7")), opacity=0.8,
                                contours = list(
                                    y = list(show = TRUE, start = rho_x_min_react(), end = rho_x_max_react(), size = 0.1, color = 'white'),
                                    x = list(show = TRUE, start = m_min_react(), end = m_max_react(), size = 20, color="white")),
                                colorbar=list(title=paste("o-ICC=", rho_yx_max_react())))
            }#objectives ifelse
            
        }#lod-mmd ifelse
        
    })
    
    output$plot_mmd_3d_cICC <- renderPlotly({
        if(lod_mmd_react() == "mmd"){
            if(objectives_react() == "SO_HTE"){
                ## SO-MMD HTE ##
                # STEP 1 #
                midpoint <- round((rho_x_max_react() - rho_x_min_react())/2,3)
                rho_yx <- seq(rho_yx_min_react(), rho_yx_max_react(), by=0.01)
                yx.n <- length(rho_yx)
                rho_x <- c(rho_x_min_react(), midpoint, rho_x_max_react())
                x.n <- length(rho_x)
                rho <- cbind(rep(rho_yx, each=x.n), rep(rho_x, times=yx.n))
                
                m <- seq(m_min_react(), m_max_react())
                n <- seq(n_min_react(), n_max_react())
                
                k <- c_react()/s_react()
                
                # STEP 2 #
                for(i in 1:nrow(rho)){
                    mat.t <- cbind(m, rho[i,1], rho[i,2],
                                   RE(m, rho[i,1], rho[i,2], B_react(), c_react(), s_react()),
                                   B_react(), c_react(), s_react())
                    if(i == 1){
                        mat <- mat.t
                    }else{
                        mat <- rbind(mat, mat.t)
                    }
                    
                }
                colnames(mat) <- c("m", "rho_yx", "rho_x", "RE","B","c","s")
                mat <- as.data.frame(mat)
                
                RE_min <- mat %>% 
                    dplyr::filter(rho_x==rho_x_min_react()) %>% 
                    dplyr::select(rho_yx,m,RE) %>% 
                    pivot_wider(names_from=m,values_from=RE) %>% 
                    dplyr::select(!rho_yx) %>% 
                    as.matrix()
                
                RE_mid <- mat %>%
                    dplyr::filter(rho_x==midpoint) %>%
                    dplyr::select(rho_yx,m,RE) %>%
                    pivot_wider(names_from=m,values_from=RE) %>%
                    dplyr::select(!rho_yx) %>%
                    as.matrix()
                
                RE_max <- mat %>% 
                    dplyr::filter(rho_x==rho_x_max_react()) %>% 
                    dplyr::select(rho_yx,m,RE) %>% 
                    pivot_wider(names_from=m,values_from=RE) %>% 
                    dplyr::select(!rho_yx) %>% 
                    as.matrix()
                
                # STEP 3 #
                fig_3d <- mat %>% 
                    dplyr::filter(rho_x==rho_x_min_react()) %>% 
                    plot_ly() %>%
                    add_surface(y=rho_yx, x=m, z=~RE_min, opacity=0.8,colorscale=list(c(0,1),c("#54278f","#cbc9e2")),#"Picnic",
                                contours = list(
                                    y = list(show = TRUE, start = rho_yx_min_react(), end = rho_yx_max_react(), size = 0.1, color = 'white'),
                                    x = list(show = TRUE, start = m_min_react(), end = m_max_react(), size = 20, color="white")),
                                #z=list(show=TRUE,start=0,end=1,size=0.1,color="white")),
                                colorbar=list(title=paste("c-ICC=", rho_x_min_react()))) %>%
                    layout(scene = list(xaxis = list(title = "m",autorange="reversed"),
                                        yaxis = list(title = "o-ICC"),
                                        zaxis = list(title = "RE"),
                                        camera=list(
                                            eye=list(
                                                x=0.5,
                                                y=-2,
                                                z=0.7
                                            )
                                        )
                    ))
                
                fig_3d %>%
                    add_surface(y=rho_yx, x=m,z=~RE_mid, colorscale=list(c(0,1), c("#a63603","#fdbe85")), opacity=0.8,
                                contours = list(
                                    y = list(show = TRUE, start = rho_yx_min_react(), end = rho_yx_max_react(), size = 0.1, color = 'white'),
                                    x = list(show = TRUE, start = m_min_react(), end = m_max_react(), size = 20, color="white")),
                                colorbar=list(title=paste("c-ICC=", midpoint))) %>%
                    add_surface(y=rho_yx, x=m,z=~RE_max, colorscale=list(c(0,1),c("#08519c","#bdd7e7")), opacity=0.8,
                                contours = list(
                                    y = list(show = TRUE, start = rho_yx_min_react(), end = rho_yx_max_react(), size = 0.1, color = 'white'),
                                    x = list(show = TRUE, start = m_min_react(), end = m_max_react(), size = 20, color="white")),
                                colorbar=list(title=paste("c-ICC=", rho_x_max_react())))
                
                
                
                
            }else if(objectives_react() == "SO_ATE"){
                ## SO-MMD ATE ##
                # STEP 1 #
                rho_yx <- seq(rho_yx_min_react(), rho_yx_max_react(), by=0.01)
                # yx.n <- length(rho_yx)
                # rho_x <- c(rho_x_min_react(), rho_x_max_react())
                # x.n <- length(rho_x)
                # rho <- cbind(rep(rho_yx, each=x.n), rep(rho_x, times=yx.n))

                m <- seq(m_min_react(), m_max_react())
                n <- seq(n_min_react(), n_max_react())

                k <- c_react()/s_react()

                # STEP 2 #
                for(i in 1:length(rho_yx)){
                    mat.t <- cbind(m, rho_yx[i],
                                   RE.ate(m, rho_yx[i], B_react(), c_react(), s_react()),
                                   B_react(), c_react(), s_react())
                    if(i == 1){
                        mat <- mat.t
                    }else{
                        mat <- rbind(mat, mat.t)
                    }

                }
                colnames(mat) <- c("m", "rho_yx", "RE","B","c","s")
                mat <- as.data.frame(mat)
                
                RE_wide <- mat %>% 
                    dplyr::select(rho_yx,m,RE) %>% 
                    pivot_wider(names_from=m,values_from=RE) %>% 
                    dplyr::select(!rho_yx) %>% 
                    as.matrix()

                fig_3d <- mat %>% 
                    plot_ly() %>%
                    add_surface(y=rho_yx, x=m, z=~RE_wide, opacity=0.8,colorscale=list(c(0,1),c("#54278f","#cbc9e2")),#"Picnic",
                                contours = list(
                                    y = list(show = TRUE, start = rho_yx_min_react(), end = rho_yx_max_react(), size = 0.1, color = 'white'),
                                    x = list(show = TRUE, start = m_min_react(), end = m_max_react(), size = 20, color="white")),
                                colorbar=list(title="")) %>%
                    layout(scene = list(xaxis = list(title = "m",autorange="reversed"),
                                        yaxis = list(title = "o-ICC"),
                                        zaxis = list(title = "RE"),
                                        camera=list(
                                            eye=list(
                                                x=0.5,
                                                y=-2,
                                                z=0.7
                                            )
                                        )
                    ))
                 
             }else{
                 midpoint <- round((rho_x_max_react() - rho_x_min_react())/2,3)
                 rho_yx <- seq(rho_yx_min_react(), rho_yx_max_react(), by=0.01)
                 yx.n <- length(rho_yx)
                 rho_x <- c(rho_x_min_react(), midpoint, rho_x_max_react())
                 x.n <- length(rho_x)
                 rho <- cbind(rep(rho_yx, each=x.n), rep(rho_x, times=yx.n))
                 
                 k <- c_react()/s_react()
                 
                 m <- seq(m_min_react(),m_max_react())
                 
                 # STEP 2 #
                 temp <- mat <- NULL
                 for(i in seq(nrow(rho))){
                     
                     temp <-  cbind(m, lambda_react(), rho[i,1], rho[i,2],
                                    opt_crit.RE(m, rho[i,1], rho[i,2], input$lambda, B_react(), c_react(), s_react()),
                                    B_react(), c_react(), s_react())
                     
                     if(i==1){
                         mat <- temp
                     }else{
                         mat <- rbind(mat, temp)
                     }
                     
                 }
                 
                 colnames(mat) <- c("m", "lambda", "rho_yx", "rho_x", "opt_crit", "B", "c", "s")
                 mat <- as.data.frame(mat)
                 
                 opt_crit_min <- mat %>% 
                     dplyr::filter(rho_x==rho_x_min_react()) %>% 
                     dplyr::select(rho_yx,m,opt_crit) %>% 
                     pivot_wider(names_from=m,values_from=opt_crit) %>% 
                     dplyr::select(!rho_yx) %>% 
                     as.matrix()
                 
                 opt_crit_mid <- mat %>%
                     dplyr::filter(rho_x==midpoint) %>%
                     dplyr::select(rho_yx,m,opt_crit) %>%
                     pivot_wider(names_from=m,values_from=opt_crit) %>%
                     dplyr::select(!rho_yx) %>%
                     as.matrix()
                 
                 opt_crit_max <- mat %>% 
                     dplyr::filter(rho_x==rho_x_max_react()) %>% 
                     dplyr::select(rho_yx,m,opt_crit) %>% 
                     pivot_wider(names_from=m,values_from=opt_crit) %>% 
                     dplyr::select(!rho_yx) %>% 
                     as.matrix()
                 
                 fig_3d <- mat %>% 
                     dplyr::filter(rho_x==rho_x_min_react()) %>% 
                     plot_ly() %>%
                     add_surface(y=rho_yx, x=m, z=~opt_crit_min, opacity=0.8,colorscale=list(c(0,1),c("#54278f","#cbc9e2")),#"Picnic",
                                 contours = list(
                                     y = list(show = TRUE, start = rho_yx_min_react(), end = rho_yx_max_react(), size = 0.1, color = 'white'),
                                     x = list(show = TRUE, start = m_min_react(), end = m_max_react(), size = 20, color="white")),
                                 #z=list(show=TRUE,start=0,end=1,size=0.1,color="white")),
                                 colorbar=list(title=paste("c-ICC=", rho_yx_min_react()))) %>%
                     layout(scene = list(xaxis = list(title = "m",autorange="reversed"),
                                         yaxis = list(title = "o-ICC"),
                                         zaxis = list(title = "Optimality Criterion"),
                                         camera=list(
                                             eye=list(
                                                 x=0.5,
                                                 y=-2,
                                                 z=0.7
                                             )
                                         )
                     ))
                 
                 fig_3d %>%
                     add_surface(y=rho_yx, x=m,z=~opt_crit_mid, colorscale=list(c(0,1), c("#a63603","#fdbe85")), opacity=0.8,
                                 contours = list(
                                     y = list(show = TRUE, start = rho_yx_min_react(), end = rho_yx_max_react(), size = 0.1, color = 'white'),
                                     x = list(show = TRUE, start = m_min_react(), end = m_max_react(), size = 20, color="white")),
                                 colorbar=list(title=paste("c-ICC=", midpoint))) %>%
                     add_surface(y=rho_yx, x=m,z=~opt_crit_max, colorscale=list(c(0,1),c("#08519c","#bdd7e7")), opacity=0.8,
                                 contours = list(
                                     y = list(show = TRUE, start = rho_yx_min_react(), end = rho_yx_max_react(), size = 0.1, color = 'white'),
                                     x = list(show = TRUE, start = m_min_react(), end = m_max_react(), size = 20, color="white")),
                                 colorbar=list(title=paste("c-ICC=", rho_yx_max_react())))
             }#objectives ifelse
            
        }#lod-mmd ifelse
        
    })


    
}

# Run the application
shinyApp(ui = ui, server = server)
