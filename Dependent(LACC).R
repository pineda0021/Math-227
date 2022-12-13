install.packages("devtools")
install.packages("roxygen2")
install_github("StirlingCodingClub/SCC_R_package")

# This is the function for Dependent Test Hypothesis
#' @export
Dependent.t.test <- function(data1,data2,alfa,alternative){
  if (alternative == "left.tail"){

    newdata=data1-data2
    newdata
    averagediff=mean(newdata)
    stdvdiff=sd(newdata)
    n=length(newdata)

    boxplot(newdata,col = 'blue',horizontal = TRUE,main='Boxplot for the Difference')

    # The test
    t=averagediff/(stdvdiff/sqrt(n))


    # Classical

    #Left-tail
    tleft=qt(alfa,n-1)
    tleft


    # P-value

    # Pv to the left
    pvalueL=pt(t,n-1)
    pvalueL

    Statistics <- c("The sample mean diff is :","The sample stdv.diff is :",
                    "The sample size is :","The test statistics is :",
                    "The critical value is :","The pvalue is :")
    Result <- c(averagediff,stdvdiff,n,t,tleft,pvalueL)

    my.table <- data.frame(Statistics,Result)

    cat("\n")
    cat("         Edward Pineda-Castro at LACC")
    cat("\n")
    print(my.table)

    if(pvalueL > alfa){
      print("Don't Reject the Ho")
    }

    else if (pvalueL < alfa)
      print("Reject the Ho")

  }

  if (alternative == "right.tail"){

    newdata=data1-data2
    averagediff=mean(newdata)
    stdvdiff=sd(newdata)
    n=length(newdata)

    # The test
    t=averagediff/(stdvdiff/sqrt(n))

    boxplot(newdata,col = 'red',horizontal = TRUE,main='Boxplot for the Difference')
    # Classical

    #Right-tail
    tright=qt(1-alfa,n-1)
    tright


    # P-value

    # Pv to the Right
    pvalueR=1-pt(t,n-1)
    pvalueR


    Statistics <- c("The sample mean diff is :","The sample stdv.diff is :",
                    "The sample size is :","The test statistics is :",
                    "The critical value is :","The pvalue is :")
    Result <- c(averagediff,stdvdiff,n,t,tright,pvalueR)

    my.table <- data.frame(Statistics,Result)

    cat("\n")
    cat("         Edward Pineda-Castro at LACC")
    cat("\n")
    print(my.table)

    if(pvalueR > alfa){
      print("Don't Reject the Ho")
    }

    else if (pvalueR < alfa)
      print("Reject the Ho")

  }

  if (alternative == "two.tail"){

    newdata=data1-data2
    averagediff=mean(newdata)
    stdvdiff=sd(newdata)
    n=length(newdata)

    # The test
    t=averagediff/(stdvdiff/sqrt(n))


    # Classical


    #Two-tail
    t.two.tail=c(qt(1-alfa/2,n-1))
    t.two.tail


    # P-value

    # Pv to the two.tail
    pvalueT=2*(1-pt(abs(t),n-1))
    pvalueT


    Statistics <- c("The sample mean diff is :","The sample stdv.diff is :",
                    "The sample size is :","The test statistics is :",
                    "The critical value is (-+) :","The pvalue is :")
    Result <- c(averagediff,stdvdiff,n,t,t.two.tail,pvalueT)

    my.table <- data.frame(Statistics,Result)

    cat("\n")
    cat("         Edward Pineda-Castro at LACC")
    cat("\n")
    print(my.table)

    if(pvalueT > alfa){
      print("Don't Reject the Ho")
    }

    else if (pvalueT < alfa)
      print("Reject the Ho")

  }
}

Father = c(70.3,67.1,70.9,66.8,72.8,70.4,71.8,70.1,69.9,70.8,70.2,70.4,72.4)
Son = c(74.1,69.2,66.9,69.2,68.9,70.2,70.4,69.3,75.8,72.3,69.2,68.6,73.9)

Dependent.t.test(Father,Son,.10,'left.tail')
Dependent.t.test(Son,Father,.10,'right.tail')


newdata=Father-Son
boxplot(newdata,col = 'blue',horizontal = TRUE,main='Boxplot for the Difference')


