library(atezo.data)

ASL <- asl(com.roche.cdt30019.go29436.re)
ATE <- ate(com.roche.cdt30019.go29436.re)

## Initialize Teal
x <- teal::init(
  data = list(ASL = ASL, ATE = ATE),
  modules = root_modules(
    tm_data_table(),
    tm_variable_browser(),
    tm_kmplot_ADAM( label = "KM PLOT" ) 
  )
)

## Initiate Shiny App
shinyApp(ui = x$ui, server = x$server)