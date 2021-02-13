library(plumber)

port <- Sys.getenv('PORT')

server <- plumb("MainFunction.R")



server$run(
	host = '0.0.0.0',
	port = as.numeric(port),
	swagger=TRUE
)
