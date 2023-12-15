library(shiny)
library(dplyr)
library(scales)
library(FinCal)

find_max_affordable <- function(mortgage_points, buying_costs_rate, mortgage_interest_rate, mortgage_term, tax_rate, insurance_rate, cash_available, mortgage_fees, hoa_fees, max_pt) {
  
  A <- matrix(c(
      mortgage_points - 1,  1 + buying_costs_rate,
      1,                    -pv(mortgage_interest_rate / 12, mortgage_term * 12, 0, tax_rate + insurance_rate)
    ), nrow = 2, byrow = TRUE
  )
  colnames(A) <- c("p_l", "p_h")
  
  b <- c(
    cash_available - mortgage_fees,
    pv(mortgage_interest_rate / 12, mortgage_term * 12, 0, hoa_fees - max_pt)
  )
  
  as_tibble(t(solve(A, b))) |> mutate(ltv = p_l / p_h)
}

ui <- fluidPage(
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
  numericInput(inputId = "mortgageFees", label = "Mortgage Fees", value = 0, min = 0, step = 1),
  
  textOutput("currentSaleProceeds"),
  textOutput("cashAvailable"),
  textOutput("maxPayment"),
  textOutput("housePrice"),
  textOutput("loanPrincipal"),
  textOutput("loanToValue"),
  textOutput("downPayment"),
  textOutput("loanPayment"),
  textOutput("taxPayment"),
  textOutput("insurancePayment"),
)
server <- function(input, output, session) {
  
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
  output$maxPayment <- renderText({
    paste0("maximum monthly payment ", dollar(maxPayment()))
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

  output$loanToValue <- renderText({
    paste0("loan-to-value ratio ", sprintf("%.0f%%", maxAffordable()[['ltv']] * 100))
  })
}
shinyApp(ui, server)