#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  output$plot1 <- renderPlot({
    out <- runODEs(input$Bslider,input$Sslider,input$Gslider,input$Vslider,input$Dslider,input$Eslider,input$Lslider,input$modelselect)
    if (input$modelselect==1){
      plot(out$time, out$S, type = 'l', col = 'green',
           ylab = "Prop of popn in each compartment",
           xlab = "Time (days)", lwd = 3, ylim = c(0, 1))
      with(out, lines(time, I, col = 'red', lwd = 3))
      legend(x="right",legend=c("S","I"),col=c("green","red"),lwd=3)
    } else if (input$modelselect==2){
      plot(out$time, out$S, type = 'l', col = 'green',
           ylab = "Prop of popn in each compartment",
           xlab = "Time (days)", lwd = 3, ylim = c(0, 1))
      with(out, lines(time, I, col = 'red', lwd = 3))
      with(out, lines(time, R, col = 'grey', lwd = 3))
      legend(x="right",legend=c("S","I","R"),col=c("green","red","grey"),lwd=3)
    } else if (input$modelselect==3){
      plot(out$time, out$S, type = 'l', col = 'green',
           ylab = "Prop of popn in each compartment",
           xlab = "Time (days)", lwd = 3, ylim = c(0, 1))
      with(out, lines(time, E, col = 'orange', lwd = 3))
      with(out, lines(time, I, col = 'red', lwd = 3))
      with(out, lines(time, R, col = 'grey', lwd = 3))
      legend(x="right",legend=c("S","E","I","R"),col=c("green","orange","red","grey"),lwd=3)
    } else {
      plot(out$time, out$S, type = 'l', col = 'green',
           ylab = "Prop of popn in each compartment",
           xlab = "Time (days)", lwd = 3, ylim = c(0, 1))
      with(out, lines(time, I, col = 'red', lwd = 3))
      with(out, lines(time, R, col = 'grey', lwd = 3))
      with(out, lines(time, V, col = 'blue', lwd = 3))
      legend(x="right",legend=c("S","I","R","V"),col=c("green","red","grey","blue"),lwd=3)
    }
  }, height=600
  )

  output$plot2 <- renderPlot({
    x_true <- exp(2*seq(0, 6, 0.01))
    times <- seq(0, 6+input$Tstepslider, input$Tstepslider)
    x <- runEuler(input$Tstepslider)
    par(mar = c(4,4,0,1))
    plot(seq(0, 6, 0.01), x_true, type = 'l', col = 'blue',
         ylab = "x(t)",
         xlab = "t", lwd = 3, xaxs="i", yaxs="i", ylim=c(-1000,35000))
    lines(times, x, col = 'red', lwd = 3, lty=3)
    legend(x="topleft",legend=c("Analytical","Approximation"),col=c("blue","red"),lwd=3,lty=c(1,3))
  }, height=200, width=280
  )

  output$plot3 <- renderPlot({
    par(mar = c(4,4,1,1))
    NPOPS = strtoi(input$npops)
    if (NPOPS==1){
      popsize <- c(input$patchsize1)
    } else if (NPOPS==2){
      popsize <- c(input$patchsize1,input$patchsize2)
    } else if (NPOPS==3){
      popsize <- c(input$patchsize1,input$patchsize2,input$patchsize3)
    } else {
      popsize <- c(input$patchsize1,input$patchsize2,input$patchsize3,input$patchsize4)
    }
    initialI <- rep(0,NPOPS)
    if (strtoi(input$infectionlocation)<=NPOPS){
      initialI[strtoi(input$infectionlocation)] <- 10
    }

    if (input$popdistances==1){
      couplingrates <- matrix(input$interaction,nrow=NPOPS,ncol=NPOPS)
    } else if (input$popdistances==2){
      couplingrates <- matrix(input$interaction,nrow=NPOPS,ncol=NPOPS)
      for (i in 1:NPOPS){
        for (j in 1:NPOPS){
          if (i==1){
            loc1 <- input$patchloc1
          } else if (i==2){
            loc1 <- input$patchloc2
          } else if (i==3){
            loc1 <- input$patchloc3
          } else {
            loc1 <- input$patchloc4
          }
          if (j==1){
            loc2 <- input$patchloc1
          } else if (j==2){
            loc2 <- input$patchloc2
          } else if (j==3){
            loc2 <- input$patchloc3
          } else {
            loc2 <- input$patchloc4
          }
          if (abs(loc1-loc2)>0){
            distancefactor <- 1/abs(loc1-loc2)
            couplingrates[i,j] <- distancefactor*input$interaction
          } else{
            couplingrates[i,j] <- 1
          }
        }
      }
    }
    for (i in 1:NPOPS){
      couplingrates[i,i] <- 1
    }
    out <- runCoupling(0.4,1/5,popsize,50,NPOPS,initialI,couplingrates)
    if (input$plotPop==1){
      plot(out$time, out[,2], type = 'l', col = 'green',
           ylab = "Number in each compartment",
           xlab = "Time (days)", lwd = 3, lty=1, ylim=c(min(out[,2:(2+NPOPS-1)]),max(out[,2:(2+NPOPS-1)])))
      if (NPOPS>1){
        for (i in 1:(NPOPS-1)){
          lines(out$time, out[,(2+i)], col = 'green', lwd = 3, lty = (1+i))
        }
      }
      legend(x="right",legend=c(1:NPOPS),col=c("green"),lwd=3,lty=c(1:NPOPS))
    } else if (input$plotPop==2){
      plot(out$time, out[,NPOPS+2], type = 'l', col = 'red',
           ylab = "Number in each compartment",
           xlab = "Time (days)", lwd = 3, lty=1, ylim=c(min(out[,(NPOPS+2):(NPOPS+2+NPOPS-1)]),max(out[,(NPOPS+2):(NPOPS+2+NPOPS-1)])))
      if (NPOPS>1){
        for (i in 1:(NPOPS-1)){
          lines(out$time, out[,(NPOPS+2+i)], col = 'red', lwd = 3, lty = (1+i))
        }
      }
      legend(x="right",legend=c(1:NPOPS),col=c("red"),lwd=3,lty=c(1:NPOPS))
    } else if (input$plotPop==3){
      plot(out$time, out[,2*NPOPS+2], type = 'l', col = 'grey',
           ylab = "Number in each compartment",
           xlab = "Time (days)", lwd = 3, lty=1, ylim=c(min(out[,(2*NPOPS+2):((2*NPOPS+2)+NPOPS-1)]),max(out[,(2*NPOPS+2):((2*NPOPS+2)+NPOPS-1)])))
      if (NPOPS>1){
        for (i in 1:(NPOPS-1)){
          lines(out$time, out[,(2*NPOPS+2+i)], col = 'grey', lwd = 3, lty = (1+i))
        }
      }
      legend(x="right",legend=c(1:NPOPS),col=c("grey"),lwd=3,lty=c(1:NPOPS))
    } else if (input$plotPop==4){
      totalpop = rowSums(out[,seq(2,(2*NPOPS+2),NPOPS)])
      plot(out$time, totalpop, type = 'l', col = 'black',
           ylab = "Number in each compartment",
           xlab = "Time (days)", lwd = 3, lty=1, ylim=c(0,1000))
      if (NPOPS>1){
        for (i in 1:(NPOPS-1)){
          totalpop = rowSums(out[,seq(2+i,(2*NPOPS+2+i),NPOPS)])
          lines(out$time, totalpop, col = 'black', lwd = 3, lty = (1+i))
        }
      }
      legend(x="right",legend=c(1:NPOPS),col=c("black"),lwd=3,lty=c(1:NPOPS))}

  }, height=300
  )


  output$plot4 <- renderPlot({
    par(mar = c(4,4,1,1))
    NPOPS2 = strtoi(input$npops)
    if (NPOPS2==1){
      popsize <- c(input$patchsize1)
    } else if (NPOPS2==2){
      popsize <- c(input$patchsize1,input$patchsize2)
    } else if (NPOPS2==3){
      popsize <- c(input$patchsize1,input$patchsize2,input$patchsize3)
    } else {
      popsize <- c(input$patchsize1,input$patchsize2,input$patchsize3,input$patchsize4)
    }
    initialI <- rep(0,NPOPS2)
    if (strtoi(input$infectionlocation)<=NPOPS2){
      initialI[strtoi(input$infectionlocation)] <- 10
    }

    if (input$popdistances==1){
      migrationrates <- matrix(rep(input$interaction2/(NPOPS2-1),NPOPS2*NPOPS2),nrow=NPOPS2,ncol=NPOPS2)
    } else if (input$popdistances==2){
      migrationrates <- matrix(rep(input$interaction2/(NPOPS2-1),NPOPS2*NPOPS2),nrow=NPOPS2,ncol=NPOPS2)
      for (i in 1:NPOPS2){
        for (j in 1:NPOPS2){
          if (i==1){
            loc1 <- input$patchloc1
          } else if (i==2){
            loc1 <- input$patchloc2
          } else if (i==3){
            loc1 <- input$patchloc3
          } else {
            loc1 <- input$patchloc4
          }
          if (j==1){
            loc2 <- input$patchloc1
          } else if (j==2){
            loc2 <- input$patchloc2
          } else if (j==3){
            loc2 <- input$patchloc3
          } else {
            loc2 <- input$patchloc4
          }
          if (abs(loc1-loc2)>0){
            distancefactor <- 1/abs(loc1-loc2)
            migrationrates[i,j] <- distancefactor*input$interaction2
          } else{
            migrationrates[i,j] <- 1
          }
        }
      }
    }
    for (i in 1:NPOPS2){
      migrationrates[i,i] <- 0 #1-sum(migrationrates[-i,i])
    }
    out <- runMigration(0.4,1/5,popsize,50,NPOPS2,initialI,migrationrates)
    if (input$plotPop==1){
      plot(out$time, out[,2], type = 'l', col = 'green',
           ylab = "Number in each compartment",
           xlab = "Time (days)", lwd = 3, lty=1, ylim=c(min(out[,2:(2+NPOPS2-1)]),max(out[,2:(2+NPOPS2-1)])))
      if (NPOPS2>1){
        for (i in 1:(NPOPS2-1)){
          lines(out$time, out[,(2+i)], col = 'green', lwd = 3, lty = (1+i))
        }
      }
      legend(x="right",legend=c(1:NPOPS2),col=c("green"),lwd=3,lty=c(1:NPOPS2))
    } else if (input$plotPop==2){
      plot(out$time, out[,NPOPS2+2], type = 'l', col = 'red',
           ylab = "Number in each compartment",
           xlab = "Time (days)", lwd = 3, lty=1, ylim=c(min(out[,(NPOPS2+2):(NPOPS2+2+NPOPS2-1)]),max(out[,(NPOPS2+2):(NPOPS2+2+NPOPS2-1)])))
      if (NPOPS2>1){
        for (i in 1:(NPOPS2-1)){
          lines(out$time, out[,(NPOPS2+2+i)], col = 'red', lwd = 3, lty = (1+i))
        }
      }
      legend(x="right",legend=c(1:NPOPS2),col=c("red"),lwd=3,lty=c(1:NPOPS2))
    } else if (input$plotPop==3){
      plot(out$time, out[,2*NPOPS2+2], type = 'l', col = 'grey',
           ylab = "Number in each compartment",
           xlab = "Time (days)", lwd = 3, lty=1, ylim=c(min(out[,(2*NPOPS2+2):((2*NPOPS2+2)+NPOPS2-1)]),max(out[,(2*NPOPS2+2):((2*NPOPS2+2)+NPOPS2-1)])))
      if (NPOPS2>1){
        for (i in 1:(NPOPS2-1)){
          lines(out$time, out[,(2*NPOPS2+2+i)], col = 'grey', lwd = 3, lty = (1+i))
        }
      }
      legend(x="right",legend=c(1:NPOPS2),col=c("grey"),lwd=3,lty=c(1:NPOPS2))
    } else if (input$plotPop==4){
      totalpop = rowSums(out[,seq(2,(2*NPOPS2+2),NPOPS2)])
      plot(out$time, totalpop, type = 'l', col = 'black',
           ylab = "Number in each compartment",
           xlab = "Time (days)", lwd = 3, lty=1, ylim=c(0,1000))
      if (NPOPS2>1){
        for (i in 1:(NPOPS2-1)){
          totalpop = rowSums(out[,seq(2+i,(2*NPOPS2+2+i),NPOPS2)])
          lines(out$time, totalpop, col = 'black', lwd = 3, lty = (1+i))
        }
      }
      legend(x="right",legend=c(1:NPOPS2),col=c("black"),lwd=3,lty=c(1:NPOPS2))}
  }, height=300
  )

  output$plot5 <- renderPlot({
    par(mar = c(4,4,1,1))
    initIstoch <- 1
    out <- runObservationNoise(0.4,1/5,input$popsizeStoch,100,initIstoch,input$Pr,input$Pm)

    obstimes <- seq(0,100,input$ObsFreq)

    plot(out$time, out$I, type = 'l', col = 'black',
         ylab = "Number in each compartment",
         xlab = "Time (days)", lwd = 4, lty=1, ylim = c(0,max(c(out$noisyI,out$I))))
    for (i in 1:input$Nsims3){
      out <- runObservationNoise(0.4,1/5,input$popsizeStoch,100,initIstoch,input$Pr,input$Pm)
      lines(obstimes, out$noisyI[match(intersect(obstimes,out$time),out$time)], col = 'red', lwd = 3, lty = 2)
    }
    # par(fig = c(0.5,1, 0.5, 1), new = T)
    # hist(rnorm(100),main='',ylab='',xlab='')
  }, height=300
  )

  output$plot6 <- renderPlot({
    par(mar = c(4,4,1,1))
    initIstoch <- 1
    out_det <- runProcessNoise(0.4,1/5,input$popsizeStoch2,100,initIstoch,0,0)

    plot(out_det$time, out_det$I, type = 'l', col = 'black',
         ylab = "Number in each compartment",
         xlab = "Time (days)", lwd = 4, lty=1, ylim = c(0,input$popsizeStoch2))

    for (i in 1:input$Nsims){
      out <- runProcessNoise(0.4,1/5,input$popsizeStoch2,100,initIstoch,input$sigma_b,input$sigma_r)
      lines(out$time, out$I, col = 'red', lwd = 2, lty = 2)
    }

    lines(out_det$time, out_det$I, col = 'black', lwd = 4, lty = 1)

  }, height=300
  )

  # gillreact <- reactiveValues()
  #
  # observeEvent(c(input$gillrerun,input$popsizeStoch3), {
  #   gillreact$rerun <- input$popsizeStoch3
  # },ignoreNULL = FALSE)
  #
  output$plot7 <- renderPlot({
    par(mar = c(4,4,1,1))
    initIstoch <- 1
    out_det <- runProcessNoise(0.4,1/5,input$popsizeStoch3,100,initIstoch,0,0)

    plot(out_det$time, out_det$I, type = 'l', col = 'black',
         ylab = "Number in each compartment",
         xlab = "Time (days)", lwd = 4, lty=1, ylim = c(0,input$popsizeStoch3))

    for (i in 1:input$Nsims2){
      out <- runGillespie(0.4,1/5,input$popsizeStoch3,100,initIstoch)
      lines(out$time, out$I, col = 'red', lwd = 2, lty = 2)
    }
    lines(out_det$time, out_det$I, col = 'black', lwd = 4, lty = 1)

  }, height=300
  )

  output$Progress <- renderMenu({
    checkboxN <- 12
    completed <- input$checkbox1 + input$checkbox2 + input$checkbox3 + input$checkbox4 + input$checkbox5 + input$checkbox6 + input$checkbox7 + input$checkbox8 + input$checkbox9 + input$checkbox10 + input$checkbox11 + input$checkbox12
    dropdownMenu(type="tasks",badgeStatus = "success",
                 taskItem(value=round((completed/checkboxN)*100,digits=2),color="green",
                          "Completed"
                 ))
  })
}
