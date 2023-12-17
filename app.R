library(shiny)
library(dplyr)
library(magrittr)
library(scales)
library(FinCal)

matrix_A <- function(mortgage_points, buying_costs_rate, mortgage_interest_rate, mortgage_term, tax_rate, insurance_rate) {
  matrix(
    c(
      mortgage_points - 1,  1 + buying_costs_rate,
      1,                    -pv(mortgage_interest_rate / 12, mortgage_term * 12, 0, tax_rate + insurance_rate, 0)
    ), nrow = 2, byrow = TRUE
  ) |> set_colnames(c("p_l", "p_h"))
}

find_max_affordable <- function(mortgage_points, buying_costs_rate, mortgage_interest_rate, mortgage_term, tax_rate, insurance_rate, cash_available, mortgage_fees, hoa_fees, max_pt) {
  
  A <- matrix_A(mortgage_points, buying_costs_rate, mortgage_interest_rate, mortgage_term, tax_rate, insurance_rate)

  b <- c(
    cash_available - mortgage_fees,
    pv(mortgage_interest_rate / 12, mortgage_term * 12, 0, hoa_fees - max_pt)
  )
  
  as_tibble(t(solve(A, b))) |> mutate(ltv = p_l / p_h)
}

find_solutions <- function(cash_available, max_pt, mortgage_fees, mortgage_interest_rate, mortgage_term, hoa_fees, mortgage_points, buying_costs_rate, tax_rate, insurance_rate) {
  cash_reserve <- seq(0, 0.8 * cash_available, 5000)
  pt_reserve <- seq(0, 0.8 * max_pt, 200)
  cases <- data.frame(expand.grid(pt_reserve = pt_reserve, cash_reserve = cash_reserve)) |>
    mutate(
      down = cash_available - mortgage_fees - cash_reserve,
      pt = pv(mortgage_interest_rate / 12, mortgage_term * 12, 0, hoa_fees - max_pt + pt_reserve, 0)
    )
  
  A <- matrix_A(mortgage_points, buying_costs_rate, mortgage_interest_rate, mortgage_term, tax_rate, insurance_rate)
  
  B <- t(as.matrix(cases[, c("down", "pt")]))
  
  cbind(cases[, c("pt_reserve", "cash_reserve")], t(solve(A, B))) |>
    mutate(
      p_h = ifelse(p_l < 0, NA, p_h),
      p_l = ifelse(p_l < 0, NA, p_l),
      down = p_h - p_l,
      ltv = p_l / p_h,
      tax = p_h * tax_rate,
      insurance = p_h * insurance_rate
   )
}

plot_contour <- function(x, y, z, xlab, ylab, main, ...) {
  options(repr.plot.width=10, repr.plot.height=8)
  contour(
    x=x, y=y, z=matrix(data=z, nrow=length(x), byrow=FALSE),
    axes=TRUE, frame.plot=FALSE, xlab=xlab, ylab=ylab, main=main,
    ...
  )
  axis(3, labels=FALSE); axis(4, labels=FALSE)
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "maxHousingExpenseFraction", label = "Maximum Housing Expense Fraction", value = .28, min = 0, max = 1, step = .01),
      numericInput(inputId = "maxDebtExpenseFraction", label = "Maximum Debt Expense Fraction", value = .36, min = 0, max = 1, step = .01),
      numericInput(inputId = "grossIncome", label = "Gross Income ($)", value = 220000, min = 0, step = 1),
      numericInput(inputId = "nonHousingMonthlyDebtService", label = "Non-Housing Monthly Debt Service ($)", value = 2000, min = 0, step = 1),
      numericInput(inputId = "currentMortgageBalance", label = "Current Mortgage Balance ($)", value = 250000, min = 0, step = 1),
      numericInput(inputId = "cashSavings", label = "Cash Savings ($)", value = 125000, min = 0, step = 1),
      numericInput(inputId = "currentHomeValue", label = "Current Home Value ($)", value = 400000, min = 0, step = 1),
      numericInput(inputId = "sellingCostsRate", label = "Home Selling Costs Rate (%)", value = 8.5, min = 0, max = 100, step = .01),
      numericInput(inputId = "buyingCostsRate", label = "Home Buying Costs Rate (%)", value = 2.5, min = 0, max = 100, step = .01),
      numericInput(inputId = "hoaFees", "HOA Association Fees", value = 0, min = 0, step = 1),
      numericInput(inputId = "propertyTaxRate", label = "Property Tax Rate (%)", value = 2, min = 0, max = 100, step = .01),
      numericInput(inputId = "insuranceRate", label = "Insurance Rate (%)", value = .1, min = 0, max = 100, step = .01),
      numericInput(inputId = "mortgageInterestRate", label = "Mortgage Interest Rate (%)", value = 3.75, min = 0, max = 100, step = .01),
      numericInput(inputId = "mortgageTerm", label = "Mortgage Term (Y)", value = 30, min = 0, max = 100, step = 1),
      numericInput(inputId = "mortgagePoints", label = "Mortgage Points", value = 0, min = 0, max = 5, step = .1),
      numericInput(inputId = "mortgageFees", label = "Mortgage Fees", value = 0, min = 0, step = 1)
    ),
    mainPanel(
      wellPanel(
        titlePanel("Assets"),
        textOutput("cashSavings"),
        textOutput("currentSaleProceeds"),
        textOutput("cashAvailable")
      ),
      wellPanel(
        titlePanel("Income"),
        textOutput("monthlyIncome")
      ),
      wellPanel(
        titlePanel("Non-Housing Debt Service"),
        textOutput("nonHousingDebt")
      ),
      wellPanel(
        titlePanel("Monthly Housing Costs Per Guidelines"),
        textOutput("maxPayment")
      ),
      wellPanel(
        titlePanel("Most Expensive Affordable House"),
        textOutput("housePrice")
      ),
      wellPanel(
        titlePanel("Purchase Details"),
        textOutput("downPayment"),
        textOutput("closingCosts"),
      ),
      wellPanel(
        titlePanel("Loan Details"),
        textOutput("loanPrincipal"),
        textOutput("loanToValue"),
        textOutput("loanPayment"
      ),
      wellPanel(
        titlePanel("Property Taxes and Insurance")
        textOutput("taxPayment"),
        textOutput("insurancePayment")
      ),
    
      plotOutput("housePricePlot"),
      plotOutput("loanPrincipalPlot"),
      plotOutput("ltvPlot"),
      plotOutput("propertyTaxPlot")
    )
  )
)
server <- function(input, output, session) {
  
  output$cashSavings <- renderText({
    paste0("cash savings ", dollar(input$cashSavings))
  })
  
  currentSaleProceeds <- reactive(input$currentHomeValue * (1 - input$sellingCostsRate / 100) - input$currentMortgageBalance)
  output$currentSaleProceeds <- renderText({
    paste0("sale proceeds ", dollar(currentSaleProceeds()))
  })
  
  cashAvailable <- reactive(currentSaleProceeds() + input$cashSavings)
  output$cashAvailable <- renderText({
    paste0("cash available ", dollar(cashAvailable()))
  })
  
  monthlyGrossIncome <- reactive(input$grossIncome / 12)
  maxPayment <- reactive(min(
    input$maxHousingExpenseFraction * monthlyGrossIncome(),
    input$maxDebtExpenseFraction * monthlyGrossIncome() - input$nonHousingMonthlyDebtService)
  )
  output$monthlyIncome <- renderText({
    paste0("monthly income ", dollar(monthlyGrossIncome()))
  })
  
  output$nonHousingDebt <- renderText({
    paste0("monthly payment ", dollar(input$nonHousingMonthlyDebtService))
  })
  
  output$maxPayment <- renderText({
    paste0("maximum monthly payment ", dollar(maxPayment()))
  })
  
  output$maxPayment <- renderText({
    
  })
  
  maxAffordable <- reactive(find_max_affordable(input$mortgagePoints / 100, input$buyingCostsRate / 100, input$mortgageInterestRate / 100,
                                                input$mortgageTerm, input$propertyTaxRate / 1200, input$insuranceRate / 1200,
                                                cashAvailable(), input$mortgageFees, input$hoaFees, maxPayment()))
  output$loanPayment <- renderText({
    paste0("monthly loan payment ", dollar(pmt(input$mortgageInterestRate / 1200, input$mortgageTerm * 12, -maxAffordable()[['p_l']], 0)))
  })
  
  output$taxPayment <- renderText({
    paste0("monthly property tax payment ", dollar(maxAffordable()[['p_h']] * input$propertyTaxRate / 1200))
  })
  
  output$insurancePayment <- renderText({
    paste0("monthly insurance payment ", dollar(maxAffordable()[['p_h']] * input$insuranceRate / 1200))
  })
  
  output$housePrice <- renderText({
   paste0("house price ", dollar(maxAffordable()[['p_h']]))
  })
  
  output$loanPrincipal <- renderText({
    paste0("loan principal ", dollar(maxAffordable()[['p_l']]))
  })
  
  output$downPayment <- renderText({
    paste0("down payment ", dollar(maxAffordable()[['p_h']] - maxAffordable()[['p_l']]))
  })

  output$closingCosts <- renderText({
    paste0("closing costs ", dollar(maxAffordable()[['p_h']] * input$buyingCostsRate / 100))
  })
  
  output$loanToValue <- renderText({
    paste0("loan-to-value ratio ", sprintf("%.0f%%", maxAffordable()[['ltv']] * 100))
  })
  
  solutions <- reactive(
    find_solutions(
      cashAvailable(),
      maxPayment(),
      input$mortgageFees / 100,
      input$mortgageInterestRate / 100,
      input$mortgageTerm,
      input$hoaFees,
      input$mortgagePoints / 100,
      input$buyingCostsRate / 100,
      input$propertyTaxRate / 1200,
      input$insuranceRate / 1200
    )
  )
  output$housePricePlot <- renderPlot({
    plot_contour(
      x = unique(sort(solutions()$pt_reserve)),
      y = unique(sort(solutions()$cash_reserve)) / 1000,
      z = solutions()$p_h / 1000,
      xlab = "monthly costs reserve ($)", ylab = "cash reserve ($k)", main = "house price ($k)"
    )
  }, res = 96)

  output$loanPrincipalPlot <- renderPlot({
    plot_contour(
      x = unique(sort(solutions()$pt_reserve)),
      y = unique(sort(solutions()$cash_reserve)) / 1000,
      z = solutions()$p_l / 1000,
      xlab = "monthly costs reserve ($)", ylab = "cash reserve ($k)", main = "loan principal ($k)"
    )
  }, res = 96)

  output$ltvPlot <- renderPlot({
    plot_contour(
      x = unique(sort(solutions()$pt_reserve)),
      y = unique(sort(solutions()$cash_reserve)) / 1000,
      z = solutions()$ltv * 100,
      xlab = "monthly costs reserve ($)", ylab = "cash reserve ($k)", main = "loan to value (%)"
    )
  }, res = 96)
  
  output$propertyTaxPlot <- renderPlot({
    plot_contour(
      x = unique(sort(solutions()$pt_reserve)),
      y = unique(sort(solutions()$cash_reserve)) / 1000,
      z = solutions()$tax,
      xlab = "monthly costs reserve ($)", ylab = "cash reserve ($k)", main = "property tax ($/mo)"
    )
  }, res = 96)
  
}
shinyApp(ui, server)