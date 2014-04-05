library(shinyAce)
library(shiny)

textInput2 <- function(inputId, label, value = ""){
      tagList(tags$label(label, `for` = inputId, style="display:inline; font-weight:bold; font-size:16px; padding:0 5px"),
              tags$input(id = inputId, type = "text", value = value))
}

shinyUI(
  fluidPage(
    singleton(tags$head(tags$link(rel='stylesheet', type='text/css', href='style.css'))),
    fluidRow(
      column(2, h2("knit2wp")),
      column(8, 
             uiOutput("loginDlg")
      ),
      column(2,
             uiOutput("logoutDlg")
      )
    ),
    fluidRow(
      column(2,
             wellPanel(
               numericInput("postId", "Post ID", "0"),
               actionButton("deleteBtn","Delete"),
               actionButton("editBtn", "Edit"),
               br(),
               h4("Publish options"),
               checkboxInput("shortcodeCBox", "shortcode", value = TRUE),
               actionButton("publishBtn", "Publish"),
               uiOutput("publishMsg")
             )),
      column(10, 
             tabsetPanel(id="pepTabSetPanel", ## Posts-Editor-Preview Panel
                         tabPanel("PostsList", 
                                  dataTableOutput('postsList')),
                         tabPanel("Editor",
                                  actionButton("newFileBtn", "New file"),
                                  actionButton("saveFileBtn", "Save file"),
                                  actionButton("loadFileBtn", "Load file"),
                                  uiOutput("saveFileMsg"),
                                  uiOutput("loadFileDlg"),
                                  textInput2("title", "Title ", ""),
                                  aceEditor("editor", mode="markdown", value=" ")),
                         tabPanel("Preview", htmlOutput("knitDoc"))
             ))
    )
  )
)