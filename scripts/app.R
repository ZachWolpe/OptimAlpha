library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(readxl)


# ------- Import Data -------x


# ---- tickers ----x
tickers        <- read_excel('~/Desktop/OptimAlpha/Yahoo Ticker Symbols - September 2017.xlsx')
names(tickers) <- tickers[3,]
tickers        <- tickers[4:nrow(tickers),]
asset_list <- apply(tickers, 1, FUN = function(x) {list(label=x[[2]], value=x[[1]])})




app <- Dash$new()

app$layout(
  htmlDiv(
    list(
      htmlLabel('Dropdown'),
      dccDropdown(
        options = asset_list
        # options = list(list(label = "New York City", value = "NYC"),
        #                list(label = "Montreal", value = "MTL"),
        #                list(label = "San Francisco", value = "SF")),
        # value = 'MTL'
      ),
      
      htmlLabel('Multi-Select Dropdown'),
      dccDropdown(
        options = list(list(label = "New York City", value = "NYC"),
                       list(label = "Montreal", value = "MTL"),
                       list(label = "San Francisco", value = "SF")),
        value = list('MTL', 'SF'),
        multi=TRUE
      ),
      htmlLabel('Radio Items'),
      dccRadioItems(
        options = list(list(label = "New York City", value = "NYC"),
                       list(label = "Montreal", value = "MTL"),
                       list(label = "San Francisco", value = "SF")),
        value = 'MTL'
      ),
      
      htmlLabel('Checkboxes'),
      dccChecklist(
        options = list(list(label = "New York City", value = "NYC"),
                       list(label = "Montreal", value = "MTL"),
                       list(label = "San Francisco", value = "SF")),
        value = list('MTL', 'SF')
      ),
      
      htmlLabel('Text Input'),
      dccInput(value='MTL', type='text'),
      
      htmlLabel('Slider'),
      dccSlider(
        min = 0,
        max = 9,
        marks = c("", "Label 1", 2:5),
        value = 5
      )
      
    ), style = list('columnCount' = 2)
  )
)

app$run_server()
