# BDA400 Assignment 6 - Portfolio Visualization Dashboard
# Hanin Helfawi
# Run: install.packages(c('shiny','ggplot2','quantmod','TTR')) then shiny::runApp()
library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)

ui <- fluidPage(
  titlePanel('Stock Portfolio Technical Analysis Dashboard'),
  sidebarLayout(
    sidebarPanel(
      textInput('symbol','Yahoo Finance stock ticker','AAPL'),
      dateRangeInput('dates','Select date range',start=Sys.Date()-365,end=Sys.Date()),
      selectInput('frequency','Time frame',c('Daily','Weekly','Monthly')),
      selectInput('chart','Chart type',c('Line','Area','Candlestick')),
      checkboxGroupInput('indicators','Technical indicators',c('Moving Averages','RSI','MACD'),selected='Moving Averages'),
      numericInput('short','Short moving average',20,min=2,max=200),
      numericInput('long','Long moving average',50,min=3,max=300),
      checkboxInput('signals','Show crossover buy/sell annotations',TRUE),
      actionButton('refresh','Load / refresh data'),
      helpText('Signals are educational examples, not financial advice.')
    ),
    mainPanel(textOutput('status'),plotOutput('price',height='440px'),
              conditionalPanel("input.indicators.includes('RSI')",plotOutput('rsi',height='220px')),
              conditionalPanel("input.indicators.includes('MACD')",plotOutput('macd',height='240px')),
              tableOutput('latest'))
  )
)

server <- function(input,output,session){
  raw <- eventReactive(input$refresh, {
    sym <- toupper(trimws(input$symbol))
    validate(need(grepl('^[A-Z0-9.^=-]{1,20}$',sym),'Enter a valid ticker.'))
    tryCatch(getSymbols(sym,src='yahoo',from=as.Date(input$dates[1])-450,
                        to=as.Date(input$dates[2])+1,auto.assign=FALSE),
             error=function(e) structure(list(message=conditionMessage(e)),class='fetch_error'))
  },ignoreNULL=FALSE)
  series <- reactive({
    x <- raw()
    validate(need(!inherits(x,'fetch_error'),if(inherits(x,'fetch_error')) x$message else 'No data'))
    validate(need(NROW(x)>0,'No prices returned by Yahoo Finance.'))
    # Yahoo Finance can return missing OHLC values inside the series.
    # Remove incomplete price rows before aggregation and TTR calculations.
    prices <- cbind(Op(x), Hi(x), Lo(x), Cl(x))
    x <- x[stats::complete.cases(as.matrix(prices)), ]
    validate(need(NROW(x)>0,'No complete price observations available.'))
    if(input$frequency=='Weekly') x <- to.weekly(x,indexAt='endof',drop.time=TRUE)
    if(input$frequency=='Monthly') x <- to.monthly(x,indexAt='endof',drop.time=TRUE)
    # Compute indicators on the full history before restricting the date window.
    # Drop any incomplete aggregated bars too, avoiding non-leading NAs.
    x <- x[stats::complete.cases(as.matrix(cbind(Op(x), Hi(x), Lo(x), Cl(x)))), ]
    close <- as.numeric(Cl(x))
    s <- as.integer(input$short); l <- as.integer(input$long)
    validate(need(s<l,'Short MA must be smaller than long MA.'),
             need(length(close)>=l+2,'Not enough observations for the selected MA periods.'))
    ma_s <- as.numeric(SMA(close,n=s)); ma_l <- as.numeric(SMA(close,n=l))
    rsi <- as.numeric(RSI(close,n=14))
    macd <- MACD(close,nFast=12,nSlow=26,nSig=9,maType='EMA',percent=FALSE)
    m <- as.numeric(macd[,1]); sig <- as.numeric(macd[,2])
    prior_s <- c(NA,head(ma_s,-1)); prior_l <- c(NA,head(ma_l,-1))
    trade <- ifelse(!is.na(prior_s)&!is.na(prior_l)&ma_s>ma_l&prior_s<=prior_l,'Buy',
                    ifelse(!is.na(prior_s)&!is.na(prior_l)&ma_s<ma_l&prior_s>=prior_l,'Sell','Hold'))
    df <- data.frame(Date=as.Date(index(x)),Open=as.numeric(Op(x)),High=as.numeric(Hi(x)),
                     Low=as.numeric(Lo(x)),Close=close,ShortMA=ma_s,LongMA=ma_l,
                     RSI=rsi,MACD=m,MACDSignal=sig,Trade=trade)
    df <- df[df$Date>=as.Date(input$dates[1])&df$Date<=as.Date(input$dates[2]),]
    validate(need(nrow(df)>0,'No observations in selected date range.'))
    df
  })
  output$status <- renderText({d<-series();paste(input$symbol,'|',nrow(d),'observations |',input$frequency)})
  output$price <- renderPlot({
    d<-series()
    p<-ggplot(d,aes(x=Date))
    if(input$chart=='Line') p<-p+geom_line(aes(y=Close),colour='#2864a0',linewidth=.7)
    if(input$chart=='Area') p<-p+geom_area(aes(y=Close),fill='#8bb7db',alpha=.65)+geom_line(aes(y=Close),colour='#2864a0')
    if(input$chart=='Candlestick') {
      p<-p+geom_segment(aes(xend=Date,y=Low,yend=High),colour='gray35')+
        geom_segment(data=d[d$Close>=d$Open,],aes(xend=Date,y=Open,yend=Close),colour='#22875b',linewidth=2.7)+
        geom_segment(data=d[d$Close<d$Open,],aes(xend=Date,y=Open,yend=Close),colour='#cb4a49',linewidth=2.7)
    }
    if('Moving Averages'%in%input$indicators) p<-p+
      geom_line(aes(y=ShortMA),colour='#e09b26',linewidth=.7,na.rm=TRUE)+
      geom_line(aes(y=LongMA),colour='#8c54a3',linewidth=.7,na.rm=TRUE)
    if(input$signals){
      marks<-d[d$Trade!='Hold',]
      p<-p+geom_point(data=marks,aes(y=Close,colour=Trade),size=2.8)+
        geom_text(data=marks,aes(y=Close,label=Trade,colour=Trade),vjust=-1,size=3,check_overlap=TRUE,show.legend=FALSE)+
        scale_colour_manual(values=c(Buy='#13895c',Sell='#c33f4b'))
    }
    p+labs(title='Stock price and trading signals',y='Price (USD)',x=NULL)+theme_minimal()
  })
  output$rsi <- renderPlot({
    d<-series();ggplot(d,aes(Date,RSI))+geom_line(colour='#7154a4',na.rm=TRUE)+
      geom_hline(yintercept=c(30,70),linetype='dashed',colour='gray55')+
      coord_cartesian(ylim=c(0,100))+labs(title='Relative Strength Index (14)',x=NULL,y='RSI')+theme_minimal()
  })
  output$macd <- renderPlot({
    d<-series();ggplot(d,aes(Date))+geom_col(aes(y=MACD-MACDSignal),fill='gray75',na.rm=TRUE)+
      geom_line(aes(y=MACD),colour='#2864a0',na.rm=TRUE)+
      geom_line(aes(y=MACDSignal),colour='#e09b26',na.rm=TRUE)+
      labs(title='MACD (12, 26, 9)',x=NULL,y='MACD / signal / histogram')+theme_minimal()
  })
  output$latest <- renderTable({
    d<-tail(series(),10);d$Date<-format(d$Date,'%Y-%m-%d');d[,c('Date','Close','ShortMA','LongMA','Trade')]
  },digits=2)
}
shinyApp(ui,server)
