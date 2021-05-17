# There are a couple of useful helper funtions:
#   data() returns the data file
#   validateData() check whether a data file has been successfully uploaded



server <- function(input, output, session) {
  
  current <- reactiveVal(0)  # current number of treatments
  currentFilter <- reactiveVal(0)  # current number of filter
  activeFilter <- reactiveVal(0)  # current number of filters activated in the tab
  trigger <- reactiveValues()
  trigger$tt <- 0
  
  # uploading the file
  
  validateData <- function() {
    if(is.null(input$file1)) {
      showNotification("Please select a data file first!!")
    }
    validate(
      need(input$file1 != "", "  Notice: Please select a data file !!")
    )
  }
  
  # get the data set
  data <- reactive({
    df <- NULL
    validate(
          need(input$file1 != "", "  Notice: Please select a data file !!")
    )
    if(is.null(input$file1)) {
      return(NULL)
    } else {
      df <- read.csv(input$file1$datapath, header = TRUE)
    }
    
    return(df[rowSums(is.na(df)) == 0,])   # remove all rows with NA
  })
  
  # Check if file has been uploaded:
  # if yes, then upload button disappear
  output$fileUploaded <- reactive({
    return(is.null(data()))
  })
  
  numeric.colnames <- reactive({
    x <- dplyr::select_if(data(), is.numeric)
    colnames(x)
  })
  
  #observeEvent for graph selector TODO: Decide graph types and modify this sector
  observeEvent(c(input$graph,input$file1),{
    #initial page for graph selector
    if(input$graph=="Default"){
      #no other selector will show up
      output$loc<-renderUI({
      })
    }
    #parameter selectors for stacked graph
    else if(input$graph=="Bar" | input$graph == "Stacked(percentage)"){
      output$loc<-renderUI({
        selectInput("variables", "Choose segment variables:",
                    choices = numeric.colnames(), multiple = TRUE )
      })
    }
    #parameter selectors for linear graph
    else if(input$graph=="Density"){
      output$loc<-renderUI({
        selectInput("variables", "Choose variables:",
                    choices = numeric.colnames(), multiple = TRUE)
      })
    }
    else {
      output$loc<-renderUI({
        selectInput("variables", "Choose variables:",
                    choices = numeric.colnames(), multiple = TRUE)
      })
    }
  })#end of observeEvent for graph selector
  
  #Check if we need to set up groups of treatment
  observeEvent(input$set,{
    validateData()
    if(input$set==FALSE) {
      while(current()>0){
        removeTreat()
      }
      current(0)
    }
    
    output$treatvar<-renderUI({
      if(input$set==TRUE){
        selectInput("treatVar", "Choose dependent variable:",
                    choices = tail(colnames(data()),-1),multiple = FALSE)
      }
    })
    output$treat<-renderUI({
      if(input$set==TRUE){
        current(0)
        fluidRow(
          column(
            width = 4,
            actionButton("addTreat","Add", style='padding:5px; font-size:80%; width:60px')
          ),
          column(
            width = 4,
            actionButton("deleteTreat","Delete", style='padding:5px; font-size:80%; width:60px')
          )
          
        )
      }
    })
  })
  
  
  
  #Check if we need to set up groups of filters
  observeEvent(input$isFilter,{
    
    validateData()

    output$filterButton<-renderUI({
      if(input$isFilter==TRUE){
        fluidRow(
          column(
          width = 3,
          actionButton("addFilter","Add", style='padding:5px; font-size:80%; width:50px')
          ),
          column(
          width = 3,
          actionButton("deleteFilter","Delete", style='padding:5px; font-size:80%; width:50px')
          ),
          column(
            width = 3,
            actionButton("applyFilter","Apply", style='padding:5px; font-size:80%; width:50px;
                         background-color:white')
          )
          
        )

      }
    })
  })
  
  
  #addFilter button
  observeEvent(input$addFilter, {
    
    validateData()

    ### warning: need to select all variables first
    if(is.null(input$variables)) {
      showNotification("Please select graph type and variables first!!")
    }
    validate(
      need(!is.null(input$variables), "  Notice: Please select graph type and variables !!")
    )
    
    # update number of filters
    newValue <- currentFilter() + 1
    currentFilter(newValue)
    
    chs = setdiff(input$variables, allFilters())
    print(chs)
    
    insertUI(
      selector = "#filterButton",
      where = "beforeBegin",
      ui = selectInput(paste("filter",currentFilter(),sep=""), paste("Filter:",currentFilter()),
                       choices = chs,multiple = FALSE)
    )

  })
  
  

  
  #deleteFilter button
  observeEvent(input$deleteFilter, {
    
    
    slider = paste0('sliders', currentFilter())
    
    # close the box for filter
    # boxid <- paste0('sliderbox', input[[paste0("filter",as.character(currentFilter()))]])
    # print(boxid)
    # # js$collapse(boxid)
    # js$collapse(boxid)
    
    
    ui_todelete <- sprintf(".shiny-input-container:has(#%s)",slider )
    removeUI(
      selector = ui_todelete,
      multiple = TRUE,
      immediate = TRUE
    )
    
    # print(allFilters())
    ui_todelete <- sprintf(".shiny-input-container:has(#filter%s)",currentFilter())
    removeUI(
      selector = ui_todelete
    )
    
    newValue <- currentFilter() - 1
    if (newValue < 0) {
      newValue <- 0
    }
    currentFilter(newValue)
  })
  

  
  # applyFilter button
  observeEvent(input$applyFilter, {
    # check if filters exist. otherwise notice
    if(currentFilter()==0) {
      showNotification("Please add filter !!")
    }
    
    # remove all existing filters display(if any)
    removeUI(
      selector = "#boxdiv *", multiple = TRUE
    )
    
    i <- 0
    d <- data()
    
    for (f in allFilters()) {
      local({
      myf <- f
      print('here is:')
      print(f)
      
      minnum <- as.integer(min(unique(data()[myf])))
      maxnum <- as.integer(max(unique(data()[myf])))
      insertUI(
        selector = "#boxdiv",
        where = "afterBegin",
        ui = 
          # tags$div(id='boxdiv',
          box(
          id = paste('sliderbox', myf, sep=''),
          collapsible = TRUE,
          closable = TRUE,
          title = myf,
          plotOutput(
            outputId = paste('filterplot', myf, sep=''),
                   width = "100%",
            height = "150px",
          ),
          br(),
          br(),
          sliderInput(paste0("sliders", myf), label = h5(strong(" Set range")), min = minnum, 
                      max = maxnum, step=maxnum/100,
                      value = c(0, maxnum)),
          br()
        )
      )
      # ) #end div
      
      output[[paste('filterplot', myf, sep='')]] <- renderPlot(
        {
        densep <- d %>%
          ggplot( aes_string(x=myf)) +
          geom_density(fill="#66FF99", alpha=0.3) +
          # horizontal boxplots & density plots
            # vertical lines at Q1 / Q2 / Q3
          geom_histogram(aes(y=..density..), alpha=0.5, colour="black", fill="white",
                           position="identity", bins=40)+
          theme_minimal()
        
        boxp <- d %>%
          ggplot(aes_string(y = myf)) +
          geom_boxplot(fill = "lightblue", color = "black", 
                       outlier.colour = "red", outlier.shape = 4, width=10) + 
          coord_flip() +
          # xlab("") +
          theme_void() + theme(legend.position="none") 
        
        egg::ggarrange(boxp,densep, heights = c(1,6))
        },
        width = 250,
        height = 200
      )
      })
      
    }
    # print("update trigger")
    # trigger$tt <- trigger$tt+1
    print("update active filter")
    activeFilter(currentFilter())
    
    
  })
  
  # observeEvent(trigger$tt, {
  #   print("update active filter")
  #   activeFilter(currentFilter())
  # })
  # 
  
  # all the filters added, which shalled be listened for changes
  filtersToListen <- reactive({
    r = c()
    for (f in activeFilters()) {
      o = input[[paste0("sliders", f)]]
      r = append(r, o)
    }
    r
  })
  
  # listen all the filters: update all for any change
  observeEvent(filtersToListen(), {
    print("detect filter change")
    d <- filter.data()
    for (f in allFilters()) {
      local({
        myf <- f
        output[[paste('filterplot', myf, sep='')]] <- renderPlot(
          {
            densep <- d %>%
              ggplot( aes_string(x=myf)) +
              geom_density(fill="#66FF99", alpha=0.3) +
              # horizontal boxplots & density plots
              # vertical lines at Q1 / Q2 / Q3
              geom_histogram(aes(y=..density..), alpha=0.5, colour="black", fill="white",
                             position="identity", bins=50)+
              theme_minimal()
            
            boxp <- d %>%
              ggplot(aes_string(y = myf)) +
              geom_boxplot(fill = "lightblue", color = "black", 
                           outlier.colour = "red", outlier.shape = 4, width=10) + 
              coord_flip() +
              # xlab("") +
              theme_void() + theme(legend.position="none") 
            
            egg::ggarrange(boxp,densep, heights = c(1,6))
          },
          width = 250,
          height = 200
        )
      }) 
    }
  })
  
  observeEvent(input$table_picker,{
    output$table <- DT::renderDataTable({
      DT::datatable(output.table(), 
                    class="display",
                    editable = 'cell',
                    # caption = "Overall table",
                    filter = 'top',
                    # options = list(dom = 't')
      )
      
    })
    
  })
  
  output.table <- reactive({
    if (input$table_picker == "Original") {

        # d <- filter.data() %>%
        #  select(input$variables)
        d <- data()
        
      
    }
    else if (input$table_picker == "Filtered") {

        d <- filter.data()
        
      
    }
    else if (input$table_picker == "Selected Variables") {

        d <- data() %>%
          select(input$variables)
        
      
    }
    else {

        d <- filter.data() %>%
          select(input$variables)
      
    }
  })
  
  
  # Plot by choice
  observeEvent(input$plot_button,{
    # remove all existing plot display(if any)
    removeUI(
      selector = "#plotdiv *", multiple = TRUE
    )
    
    # create a new box for graph display
    insertUI(
      selector = "#plotdiv",
      where = "afterBegin",
      ui = 
        column(
          width = 12,
          box(
            collapsible = TRUE,
            closable = TRUE,
            width = 12,
            height = 8,
            title = "My Plot",
            # background = 'maroon',
            column(
              width = 12,
              plotOutput(
                outputId = "plot",
                height = "100%",
                width = "100%"
              )
            ),

            br()
          )
        )
        # tags$div(id='boxdiv',
        
    )
    
    if(input$graph=='Bar'){
      output$plot <- renderPlot({
        plotBar()
      }, 
      width=input$plot_width,
      height= input$plot_height,
      execOnResize = TRUE
      )
    }
    else if(input$graph=='Density' ){
      output$plot <- renderPlot({
        plotDensity()
      }, 
      width=input$plot_width,
      height= input$plot_height * 1.8,
      execOnResize = TRUE
      )
      
    }
    else if(input$graph=='Swarm' ){
      output$plot <- renderPlot({
        plotSwarm()
      }, 
      width=input$plot_width,
      height= input$plot_height * 1.3,
      execOnResize = TRUE
      )
      
    }
    
  })
  
  plotBar <- reactive({
    myplots <- lapply(input$variables, plot_bar)
    
    numrow <- 1
    if (length(input$variables)>5) {
      numrow <- 1 + as.integer(length(input$variables) / 5 -0.01)
    }
    numcol <- 5
    if (length(input$variables)<5) {
      numcol <- length(input$variables)
    }
    
    p <- ggpubr::ggarrange(plotlist=myplots, nrow=numrow, ncol= numcol,
                        common.legend = TRUE, legend = "right") 
    
    annotate_figure(p,
                    top =text_grob(input$title, 
                                   color = "red", face = "bold", size = 18),
                    bottom = text_grob(input$xlabel, color = "black",
                                       hjust = 1, x = 1, face = "italic", size = 12),
                    left = text_grob(input$ylabel, color = "blue", rot = 90)
                    ) 
    
 
  })
  
  # helper for plotBar
  plot_bar <- function(var) {
    # pallete <- allColors()
    d <- process.data.single(var)
    p <- ggplot(data=d, aes(x = group, y = mean, fill = as.factor(group))) +
      geom_bar(stat="identity", position='dodge', color="black") +

      # geom_text(aes(label=mean), vjust=1.6, color="white", size=3.5)+
      geom_errorbar(
        aes(ymin = mean-std, ymax=mean+std),
        position = "dodge", width = 0.5,
        colour="black"
      )+
      scale_fill_manual(name='treatment', values = allColors(), labels = get.treat.names())+
      labs(y=clean_name(var), x="") +
      theme(axis.title.x=element_blank(),
            legend.background = element_rect(fill = "white", size = 4, colour = "white"),
            panel.grid.major = element_line(colour = "white", size = 0.1),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(face="bold", color="black", 
                                       size=10, angle=45, hjust = 1)

            ) +
      scale_x_discrete(name ="", 
                       limits=get.treat.names())
      
    
    p
  }
  
  
  plotDensity <- reactive({
    myplots <- lapply(input$variables, plot_density)
    
    # number of rows: default 1 if less than 5 subplots
    numrow <- 1
    if (length(input$variables)>1) {
      numrow <- length(input$variables)
    }
    # number col: divide by 4
    numcol <- 1
    # if (length(input$variables)<4) {
    #   numcol <- length(input$variables)
    # }
    
    # stick plots together with ggpubr packge
    p <- ggpubr::ggarrange(plotlist=myplots, nrow=numrow, ncol= numcol,
                           common.legend = TRUE, legend = "right") 
    
    annotate_figure(p,
                    top =text_grob(input$title, 
                                   color = "red", face = "bold", size = 14),
                    bottom = text_grob(input$xlabel, color = "black",
                                       hjust = 1, x = 1, face = "italic", size = 10),
                    left = text_grob(input$ylabel, color = "blue", rot = 90)
    ) 
    
    
  })
  
  # helper for plotDensity
  plot_density <- function(var) {
    # pallete <- allColors()
    d <- process.data()
    groupvar <- 'group'
    params<-append(c(var),c("group"),after=0)
    densep <- d %>%
      select(params) %>%
      ggplot( aes_string(x=var)) +
      geom_density(aes(group = as.factor(group), fill=as.factor(group)),alpha=0.2) +
      # horizontal boxplots & density plots
      # vertical lines at Q1 / Q2 / Q3
      # geom_histogram(aes(y=..density..), alpha=0.5, colour="black", fill="white",
      #                position="identity", bins=50)+
      scale_fill_manual(name = "treatment", values = allColors(), labels = get.treat.names())+
      # geom_vline(data=mu, aes(xintercept=group.mean, color=group),
      #            linetype="dashed") +
      labs(y="density", x=clean_name(var)) +
      theme(axis.text.x = element_text(color="black", 
                                       size=8, angle=0)

      )
      
    densep
  }
  
  
  plotSwarm <- reactive({
    myplots <- lapply(input$variables, plot_swarm)
    
    numrow <- 1
    if (length(input$variables)>3) {
      numrow <- 1 + as.integer(length(input$variables) / 3 - 0.01)
    }
    numcol <- 3
    if (length(input$variables)<3) {
      numcol <- length(input$variables)
    }
    
    p <- ggpubr::ggarrange(plotlist=myplots, nrow=numrow, ncol= numcol,
                           common.legend = TRUE, legend = "right") 
    
    annotate_figure(p,
                    top =text_grob(input$title, 
                                   color = "red", face = "bold", size = 14),
                    bottom = text_grob(input$xlabel, color = "black",
                                       hjust = 1, x = 1, face = "italic", size = 10),
                    left = text_grob(input$ylabel, color = "blue", rot = 90)
    ) 
    
    
  })
  
  # helper for plotDot
  plot_swarm<- function(var) {
    # pallete <- allColors()
    d <- process.data()

    p <- d %>% 
      ggplot(aes_string(x = 'group', y = var)) +
      geom_boxplot() +
      geom_jitter(width=0.15)+
      labs(y=clean_name(var), x="") +
      theme(axis.text.x = element_text(face="bold", color="black", 
                                       size=10, angle=45, hjust = 1),
            axis.line = element_line(colour = "black", 
                                     size = 0.9, linetype = "solid"),
            axis.ticks = element_line(size = 1) 
            )+
      scale_x_discrete(name ="", 
                       limits=get.treat.names())
    p
  }
  
  
  
  
  ############ Reactive helper functions #####################
  
  clean_name <- function(s) {
    r <- str_replace_all(s, '\\.', ' ')
    r
  }
  
  # get the data based on the filters
  filter.data <- reactive({
    print("filter data")
    d <- data()
    fs <-  activeFilters()
    # if no filters, then return data()
    if (length(fs)<=0) {
      print("active filters shouldn't be zero!!!!")
    } 
    else {
      for (f in fs) {
        print(f)
        # local({
          bucket = input[[paste0("sliders", f)]]
          print("bucket....:")
          print(bucket)
          if (is.null(bucket)){
            print("bucket is null ..")
            minnum <- as.integer(min(unique(d[f])))
            maxnum <- as.integer(max(unique(d[f])))
            bucket = c(minnum, maxnum)
          }
          d <- d %>%
            filter(d[[f]] >= bucket[1], d[[f]]<=bucket[2])
         # }) # end local
      }
    }
    
    d
  })
  
  
  # get all filter selections
  allFilters <- reactive({
    vs <- c()
    for (i in 1:currentFilter()){
      a = input[[paste0("filter",as.character(i))]]
      vs <- append(vs, a)
    }
    unique(vs) # remove duplicates
  })
  
  #get all Colors
  allColors <- reactive({
    vs <- c()
    for (i in 1:current()) {
      print(input[[paste0("color",as.character(i))]])
      a  <- input[[paste0("color",as.character(i))]]
      vs <- append(vs, a)
    }
    if (current()<=0) {
      vs <- c("#66ccff")
    }
    vs
  })
  
  # get active filter selections (before clicking apply)
  activeFilters <- reactive({
    vs <- c()
    for (i in 1:activeFilter()){
      a = input[[paste0("filter",as.character(i))]]
      vs <- append(vs, a)
    }
    print("active filters: ---")
    print(unique(vs))
    unique(vs) # remove duplicates
  })
  
  
  # 3. return names of all treatments as list of vectors
  all_treat <- reactive({
    result <- list()
    if (current() > 0){
      for (i in seq(current())) {
        treat<-c(input[[paste0("group",as.character(i))]])
        result[[i]] <- treat
      }
    }
    else {
      # return all treatment variables as a whole 
      # result[[1]] = c(unique(data()[input$treatVar]))
      result <- list()
    }
    result
  })
  
 
  ####### Playing with tables #############
  # 1.take sub-table by selected 'Well Name' and Params(cols). Add mean and std error for each columns var.
  # In current version, we're using #6, which returns a enlongated(see function 'gather') version of data frame
  # msg me if any confusion (Ricky)
  sub.table <- function(df,names, params){
    #inFile <- input$file1
    #if (is.null(inFile))
    #  print("There's no input file")
    #  return(NULL)
    df1 <- df %>%
      # filter(input$treatVar %in% names) %>%
      select(params) %>%
      # group_by(Time.Point) %>%
      summarise_all(list(~mean(.),~std.error(.)))
    
    df1
  }
  
  # 1.5 call filter_data and append their group numbers
  process.data <- reactive({
     
    df <- filter.data()
    treat.names = all_treat()
    d = NULL
      
    if(length(treat.names) == 0) {
      d <- df %>%
        mutate(group = 1)
    }
    else {
      i<-1
      for (treat in treat.names){
        d1<- df %>%
          filter(df[[input$treatVar]] %in% unlist(treat)) %>%
          mutate(group = as.integer(i), .before=input$treatVar)
        d <- bind_rows(d,d1)
        i<-i+1
      }
    }
    d
  })
  
  # calculate the stats for each group
  process.data.stats <- reactive({
    
    limit.decimal <- function(x) {
      round(x, 3)
    }
    
    df <- process.data()
    params1<-append(input$variables,c("group"),after=0)
    d <- df %>%
      select(params1) %>%
      group_by(group) %>%
      summarise_all(list(~mean(.), ~std.error(.))) %>%
      dplyr::mutate_if(is.numeric,limit.decimal)
    d
  })

  # process data based on single var and summarize the stats
  # the output is one row for each group
  process.data.single <- function(var) {
    limit.decimal <- function(x) {
      round(x, 3)
    }
    
    df <- process.data()
    params1<-append(c(var),c("group"),after=0)
    d <- df %>%
      select(params1) %>%
      group_by(group) %>%
      summarise_at(c(var), list( mean=mean, std=std.error)) %>%
      dplyr::mutate_if(is.numeric,limit.decimal)
    d
  }
  
  # not used
  process.data.color <- function(df) {
    d <- df %>%
      mutate(color = allColors()[group])
    d
  }
  
  observeEvent(current(), {
    output$treatCheckbox <- renderUI(
      checkboxGroupButtons(
        inputId = "stats_picker",
        label = "Select treatments to run",
        choices = c(1:max(current(),1)),
        status = "primary",
        checkIcon = list(
          yes = icon("ok", 
                     lib = "glyphicon"),
          no = icon("remove",
                    lib = "glyphicon"))
      )
      
    )
    
  })
  
  # add and delete for treatments
  #add button
  observeEvent(input$addTreat, {
    newValue <- current() + 1
    current(newValue)
    selectVals <- lapply(data()[input$treatVar], as.character)
    insertUI(
      selector = "#treatdiv",
      where = "beforeBegin",
      ui = 
        # pickerInput(
        #   inputId = paste("group",current(),sep=""),
        #   label = paste("Treatment:",current()),
        #   choices =unique(selectVals),
        #   options = list(
        #     `actions-box` = TRUE,
        #     size = 10
        #     # `selected-text-format` = "static"
        #   ),
        #   multiple = TRUE
        # )
        selectInput(paste("group",current(),sep=""), paste("Treatment:",current()),
                       c(unique(data()[input$treatVar])),multiple = TRUE)
    )
  })
  
  removeTreat <- function() {
    ui_todelete <- sprintf(".shiny-input-container:has(#group%s)",current())
    removeUI(
      selector = ui_todelete
    )
    newValue <- current() - 1
    if (newValue < 0) {
      newValue <- 0
    }
    current(newValue)
  }
  
  #delete button
  observeEvent(input$deleteTreat, {
    removeTreat()
  })
  
  # if the dependent variable change, then zero all the treatments
  observeEvent(input$treatVar, {
    while(current()>0){
      removeTreat()
    }
    current(0)
    
  })
  
  
  
  
  
  ############## color picker #############
  observeEvent(current(), {
    
    removeUI(
      selector = "#colordiv *", multiple = TRUE
    )
    
    # curr = isolate(current()) 
    # while (curr > 0) {
    #   picker = paste0("color", curr)
    #   ui_todelete <- sprintf(".shiny-input-container:has(#%s)",picker )
    #   removeUI(
    #     selector = ui_todelete,
    #     multiple = TRUE,
    #     immediate = TRUE
    #   )  
    #   curr <- curr -1 
    # }
    # 
    
    color_list = c("#1b9e77", "#7570b3", "#d95f02", "#ffffbf", "#0033ee",
                   "#ee5577", "#bdbdbd", "#c51b8a")
    insertUI(
      selector = "#colordiv",
      where = "afterBegin",
      ui = 
        
        box(
        title = "Colors",
        collapsible = TRUE,
        closable = FALSE,
        id = "colorbox",
        width = 12,
        background = 'navy',
        tags$div(
          id= "colorboxdiv"
        ),
        br()
      )
    )
    
    
    local({
      if (current() >= 1) {
        for( i in 1:current()) {
          curr = i
          insertUI(
            selector = "#colorboxdiv",
            where = "beforeBegin",
            ui = colorPickr(
              inputId = paste0('color',curr),
              label = paste0("Treatment: ", curr),
              update = "change",
              selected = color_list[i%%7+1],
              width = 150,
              interaction = list(
                clear = FALSE,
                save = FALSE
              ),
              position = "right-start"
            )
          )
        }
      }
      
    })

    
  })
  
  # called from get.treat.names
  treat.names <- reactive({
    r = c()
    for ( s in str_split(input$treatNames, ";") ){
      s <- trimws(s)
      r <- append(r, s)
      
    }
    r[r!=""]
  })
  
  # called from get.treat.names
  default.treat.names <- reactive({
    r = c()
    ml <- max(current(), 1)
    for (i in 1:ml) {
      r <- append(r, paste0("Treatment ", as.character(i)))
    }
    r
  })
  
  # use this:
  # if number doesn match then use defult
  # Otherwise use user input
  get.treat.names <-reactive({
    treatNames <- default.treat.names()
    if (length(treat.names())==max(current(),1)){
      treatNames <- treat.names()
    }
    treatNames
  })
  
  
  table.stats <- reactive({
    limit.decimal <- function(x) {
      round(x, 3)
    }
    
    params <- input$variables
    df1 <- filter.data() %>%
      # filter(input$treatVar %in% names) %>%
      select(params) %>%
      # group_by(Time.Point) %>%
      summarise_all(list(~mean(.), ~median(.), ~std.error(.), ~min(.), ~max(.)), na.rm= TRUE) %>%
      tidyr::pivot_longer(cols = everything(),
                          names_sep = "_",
                          names_to  = c("Variable", ".value")) %>%
      dplyr::mutate_if(is.numeric,limit.decimal)
    
    
    df1
  })
  
  # For single group
  table.stats.single <- function(n){
    limit.decimal <- function(x) {
      round(x, 3)
    }
    
    
    params<-input$variables
    df1 <- process.data() %>%
      # filter(input$treatVar %in% names) %>%
      filter(group == n) %>%
      select(params) %>%
      summarise_all(list(~mean(.), ~median(.), ~std.error(.), ~min(.), ~max(.)), na.rm= TRUE) %>%
      tidyr::pivot_longer(cols = everything(),
                          names_sep = "_",
                          names_to  = c("Variable", ".value")) %>%
      dplyr::mutate_if(is.numeric,limit.decimal)
    
    
    df1
  }
  
  
  
  ################## all the save buttons ##################
  
  fname <- reactive({
    f<-input$file_name
    if(input$file_name=="default"){
      f<-download.name()
    }
    f
  })
  
  #generate default download file name
  download.name <- reactive({
    treats <- all_treat()
    name <-""
    for (i in 1:length(treats)){
      a <- unlist(treats[i])
      for (j in 1:length(a)) {
        if (j!=1){
          name <- paste(name, a[j],sep='-')
        }else{
          name <- paste(name, a[j], sep='')
        }
      }
      name<-paste(name,'#',sep="")
    }
    substr(name,1, nchar(name)-1)
  })
  
  output$save_t <- downloadHandler(
    filename = function(){
      paste0(fname(), '.csv')
      }, 
    content = function(fname){
      write.csv(output.table(), fname, row.names = FALSE)
    }
  )
  
  
  output$downloadPlot <- downloadHandler(
    
    filename = function() { paste(fname(), input$fileFormat, sep='.') },
    content = function(file) {
      if(input$fileFormat == 'png'){
        png(file,
            width=input$plot_width,
            height=input$plot_height+(current()-1)*(input$plot_height-input$plot_height/2)
        )
      }
      else if(input$fileFormat == 'jpeg'){
        jpeg(file,
             width=input$plot_width,
             height=input$plot_height+(current()-1)*(input$plot_height-input$plot_height/2)
        )
      }
      else{
        # pointsize to be approximately in points
        # default scale is inch
        pdf(file,
            width=input$plot_width/72,
            height=(input$plot_height+0.7*(current()-1)*(input$plot_height-input$plot_height/2))/72,
            pointsize=12
        )
      }
      g<-NA
      if (input$graph=="Bar"){
        g<-plotBar()
      } else if(input$graph=="Density"){
        g<-plotDensity()
      } else {
        g<-plotSwarm()
      }

      print(g)
      dev.off()
    }
  )
  
  
  observeEvent(input$variables,{
    output$stats <- DT::renderDataTable({
      DT::datatable(table.stats(), 
                    class="row-border",
                    editable = 'cell',
                    # caption = "Overall table",
                    # filter = 'top',
                    options = list(dom = 't')
      )
      
    })
  })
  
  observeEvent(input$stats_picker, {
    treatSelected <- input$stats_picker
    groupSelected <- c()
    
    if (length(all_treat()) > 0) {
      for ( i in treatSelected) {
        groupSelected <- append(groupSelected, all_treat()[[as.integer(i)]] )
      }
      groupSelected <- unique(groupSelected)
    }
    
    
    limit.decimal <- function(x) {
      round(x, 3)
    }
    
    params <- input$variables
    df <- process.data()
    df1 <- NA
    
    print(groupSelected)
    
    if (length(all_treat())==0) {
      df1 <- NA
    }
    else {
      df1 <- df %>%
        filter(df[[input$treatVar]] %in% groupSelected) %>%
        select(params) %>%
        summarise_all(list(~mean(.), ~median(.), ~std.error(.), ~min(.), ~max(.)), na.rm= TRUE) %>%
        tidyr::pivot_longer(cols = everything(),
                            names_sep = "_",
                            names_to  = c("Variable", ".value")) %>%
        dplyr::mutate_if(is.numeric,limit.decimal)
      
      output$groupStats <- DT::renderDataTable({
        DT::datatable(df1, 
                      class="row-border",
                      editable = 'cell',
                      # caption = "Overall table",
                      # filter = 'top',
                      options = list(dom = 't')
        )
        
      })
    }
  })
  
  ###### Button for testing ## comment out this before launching #####
  observeEvent(input$test, {

      
      
      
      
      
      
      
      
    # 
    
      # for(i in 1:max(current(),1)) {
      #   local({
      #     insertUI(
      #       selector = "#statshere",
      #       where = "afterBegin",
      #       ui = 
      #         DT::dataTableOutput(paste0("stat", as.character(i)))
      #       
      #     )
      #     
      #     output[[paste0("stat", as.character(i))]] <- DT::renderDataTable({
      #       table.stats.single(i)
      #     })
      #   })
      #   # insertUI(
      #   #   selector = "#statshere",
      #   #   where = "beforeBegin",
      #   #   ui = HTML("<h4> Next group </h4>")
      #   #   
      #   # )
      #   
      # }
      
      
    # })
    
    # 
    # insertUI(
    #   selector = "#tablehere",
    #   where = "afterBegin",
    #   ui = 
    #     # tags$div(id='boxdiv',
    #     box(
    #       id = 'tt0',
    #       collapsible = TRUE,
    #       closable = TRUE,
    #       title = "test",
    #       DT::dataTableOutput(paste0("table", 1))
    #   )
    # )
    # 
    # output$table1 <- DT::renderDataTable({
    #   print('render')
    #   filter.data()
    # })
    # output$table <- DT::renderDataTable({

      # process.data.stats()
      # process.data()
      # filter.data()
      # table.stats.single(1)
      # process.data() %>%
      #   filter(group == 1)
    # })

  })
  

}