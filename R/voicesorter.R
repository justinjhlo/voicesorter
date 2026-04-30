#' Generate Shiny app for audio clustering experiment
#' 
#' This function generates a Shiny app with draggable audio stimuli for users to
#' perform free classification.
#' 
#' @param exp_id Experimenter-defined identifier of the experiment
#' @param sounds A string vector of filenames for the audio stimuli to be used
#'     in the experiment. Draggables will be created in the specified order.
#'     Files must all be in dir and paths should be provided as relative to dir.
#' @param dir Path specifying the folder containing sounds
#' @param labels A string vector of labels to place on draggables (in the
#'     specified order). Must be the same length as sounds.
#' @param colors to do.
#' @param n_group If specified, the number of clusters that will be fixed.
#' @param title Experiment title to show at the top of the app.
#' @param instructions to do.
#' @param uid_format to do.
#' 
#' @import shiny
#' 
#' @export
voicesorter <- function(exp_id, sounds, dir, labels, colors = NULL, n_group = 0,
                        title = "Voice sorter",
                        instructions = "Drag the circles into the box and group them by the voice you hear",
                        uid_format = NULL){
  stopifnot("sound file not found" = file.exists(file.path(dir, sounds)))
  stopifnot("number of sound files not equal to number of labels" = length(sounds) == length(labels))
  stopifnot("n_group must be a positive integer greater than 1" = n_group == 0 | (is.integer(n_group) & n_group > 1))
  
  addResourcePath("media", dir)
  sounds <- paste0("media/", sounds)
  n_stim <- length(sounds)

  if(is.null(colors)){
    if(n_stim >= 24) colors <- c(khroma::color("discreterainbow")(23), "#777777")
    else colors <- c(khroma::color("discreterainbow")(n_stim - 1), "#777777")
  }
  cluster_class <- paste0("border: 2px solid ", colors)
  names(cluster_class) <- paste0(".stim-c", 1:length(colors))
  
  insert_buttons <- function(){
    for(i in 1:n_stim){
      insertUI(
        selector = "#stimbar",
        where = "beforeEnd",
        ui = tags$div(class = "stim", id = sprintf("stim%03d", i), labels[i])
      )
    }
    
    shinyjqui::jqui_draggable(paste0(sprintf("#stim%03d", 1:n_stim), collapse = ","), options = list(revert = "invalid"))
  }
  
  remove_buttons <- function(){
    for(i in 1:n_stim) removeUI(".stim")
  }
  
  click_to_play <- function(id, sound, t = 1500){
    shinyjs::onclick(id, function(e){
      if(e$shiftKey){
        shinyjs::addClass(id, "stim-playing")
        howler::playSound(sound)
        shinyjs::runjs(paste0("Shiny.setInputValue('curr_playing', '", id, "___", sound, "', {priority: 'event'});"))
        shinyjs::delay(t, shinyjs::removeClass(id, "stim-playing"))
      }
    }, add = TRUE)
  }
  
  click_to_play_all <- function(t = 1500){
    lapply(1:n_stim, function(x) click_to_play(sprintf("stim%03d", x), sounds[x], t))
  }
  
  ui <- fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(c(
      list(
        ".stim" = c("display: inline-block", "height: 50px", "width: 50px", "line-height: 50px",
                    "border-radius: 50%", "-moz-border-radius: 50%", "-webkit-border-radius: 50%",
                    "border: 1px solid #7f7f7f", "text-align: center", "margin: 5px",
                    "background-color: rgba(127,127,127,0.2)", "z-index: 2"),
        ".stim-playing" = "background-color: rgba(127,127,127,0.5)",
        ".stim-sink" = c("position: relative", "top: 10px", "left: 0px",
                         "height: 600px", "width: 95%", "border: 3px solid black")
      ),
      as.list(cluster_class)
    )),
    titlePanel(title),
    fluidRow(
      column(3, id = "stimbar",
             p(strong("Shift + click"), "to play"),
             p(instructions)
      ),
      column(9,
             fluidRow(
               column(8,
                      p(textOutput("msg_display"), style = "color:red;"),
                      style = "height:60px;"
               ),
               column(4,
                      actionButton("reset", "Reset", icon = icon("arrows-rotate")),
                      actionButton("validate", "Check", icon = icon("check")),
                      shinyjs::hidden(downloadButton("submit", "Submit", icon = icon("arrow-right-to-bracket"))),
                      # shinyjs::disabled(downloadButton("submit", "Submit", icon = icon("arrow-right-to-bracket"))), # does not actually disable due to Shiny bug
                      # shinyjs::disabled(actionButton("submit", "Submit", icon = icon("arrow-right-to-bracket")))
                      style = "height:60px;"
               )
             ),
             fluidRow(
               tags$div(class = "stim-sink", id = "sink")
             )
      )
    )
  )
  
  server <- function(input, output, session) {
    # define reactives
    rvals <- reactiveValues(orig_coords = data.frame(top = numeric(1), left = numeric(1)),
                            positions = data.frame(top = numeric(1), left = numeric(1)),
                            all_dropped = FALSE,
                            userid = "",
                            validation_msg = "",
                            play_log = c(),
                            submit_flag = FALSE)
    
    # add draggables and droppable
    insert_buttons()
    shinyjqui::jqui_droppable("#sink", options = list(accept = ".stim"))
    
    # demand user code
    showModal({
      modalDialog(
        textInput("userid", "Enter user code:", placeholder = "abc1234"),
        footer = actionButton("submitid", "Submit"),
        size = "s"
      )
    })
    
    observeEvent(input$submitid,{
      if(input$userid != ""){
        rvals$userid <- input$userid
        removeModal()
      }
    })
    
    # link stim to sounds
    click_to_play_all()
    
    observeEvent(input$curr_playing,{
      rvals$play_log <- append(rvals$play_log, input$curr_playing)
    })

    # remove border styling if any stim is dragged after clustering
    observeEvent(sapply(1:n_stim, function(x) input[[sprintf("stim%03d_position", x)]]), {
      rvals$validation_msg <- ""
      if(ncol(rvals$positions) >= 4){
        for(i in 1:n_stim) shinyjs::removeClass(sprintf("stim%03d", i), paste0("stim-c", rvals$positions[i,4]))
        rvals$positions[,4] <- NULL # remove cluster information
        shinyjs::disable("submit")
      }
    })
    
    observeEvent(input$reset, {
      # reconstruct stims
      remove_buttons()
      insert_buttons()
      click_to_play_all()
    })
    
    observeEvent(input$validate, {
      # update stim coordinates
      rvals$positions <- do.call(rbind.data.frame, lapply(1:n_stim, function(x) input[[sprintf("stim%03d_position", x)]]))
      rvals$positions <- cbind(id = sprintf("stim%03d", 1:n_stim), rvals$positions)
      
      # check presence of all stims
      rvals$all_dropped <- length(input$sink_dropped) == n_stim
      
      if(rvals$all_dropped){
        hc <- hclust(dist(rvals$positions[,2:3]))
        if(n_group > 0) optimal_k <- n_group
        else optimal_k <- which.max(sapply(1:(n_stim-1), function(j) fpc::calinhara(rvals$positions[,2:3], cutree(hc, k = j))))

        # add cluster information
        rvals$positions <- cbind(rvals$positions, cluster = cutree(hc, k = optimal_k))

        # color border by cluster
        for(i in 1:n_stim) shinyjs::addClass(sprintf("stim%03d", i), paste0("stim-c", rvals$positions[i,4]))
        
        rvals$validation_msg <- paste("You have made", optimal_k, "groups.")
        
        # activate submit button
        shinyjs::show("submit")
        # shinyjs::enable("submit")
      } else {
        rvals$validation_msg <- "Please make sure all the voices are in the box."
      }
    })
  
    output$msg_display <- renderText(rvals$validation_msg)

    output$submit <- downloadHandler(
      filename = function(){
        paste0(exp_id, "_", rvals$userid, "_", Sys.Date(), ".rds")
      },
      content = function(file){
        out_content <- cbind(userid = rep(rvals$userid, n_stim), id = rvals$positions$id, sounds, labels, rvals$positions[, -1])
        saveRDS(list(play_log = rvals$play_log, clusters = out_content), file)
        
        # prompt exit
        rvals$submit_flag <- TRUE
        if(rvals$submit_flag){
          showModal({
            modalDialog(
              p("Thank you for completing the experiment. Once download is complete, click the button below to exit."),
              footer = actionButton("exitapp", "Exit"),
              size = "s"
            )
          })
        }
      }
    )
    
    # Quit app
    observeEvent(input$exitapp,{
      stopApp()
    })
    
  }
  
  shinyApp(ui = ui, server = server)
}

