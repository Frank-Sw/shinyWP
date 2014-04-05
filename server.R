library(shinyAce)
library(shiny)
library(knitr)
if(!require(RWordPress))
  stop("Please install the RWordPress package first.")
## User login information
userInfo = reactiveValues(LoggedIn = FALSE, URL = '', userName = '', passWord = '')
uiControl = reactiveValues(showFileUploadDlg=FALSE, saveFileMsg="", publishMsg="")
wpPosts = reactiveValues(posts=NULL)

## Get all posts
getAllPosts <- function(){
  print("getAllPosts function invoked")
  .login = userInfo$passWord
  names(.login) = userInfo$userName
  .server = userInfo$URL
  posts = try(getPosts(login=.login, .server=.server), silent = TRUE)
  if(inherits(posts, "try-error")) return(NULL)
  options(WordpressLogin = .login,
          WordpressURL = .server)
  return(posts)
}

## Judge whether the user login information is correct
loginSucceed <- function(){
  print("loginSucceed function invoked")
  wpPosts$posts <<- getAllPosts()
  if(is.null(wpPosts$posts)) return(FALSE)
  return(TRUE)
}


shinyServer(function(input, output, session){
  
  ## Login Dialog UI
  output$loginDlg <- renderUI({
    print("output$loginDlg is invoked")
    if(!userInfo$LoggedIn){
      print("output$login invoked")
      fluidRow(
        column(12,
               fluidRow(
                 column(3, offset = 2, textInput("URL", "Blog URL:")),
                 column(3, textInput("userName", "User Name:")),
                 column(3, textInput("passWord", "Password:")),
                 column(1, actionButton("loginBtn", "Login"))
               ),
               fluidRow(
                 column(12, offset = 2, htmlOutput("loginMessage"))
               )
        )
      )
    } else {
      div(id="loginIcon", userInfo$userName)
    }
  })
  
  output$logoutDlg <- renderUI({
    print("output$logoutDlg is invoked")
    if(userInfo$LoggedIn){
      fluidRow(
        column(6, actionButton("newPostBtn", "New Post")),
        column(6, actionButton("logoutBtn", "Sign Out"))
        )
    }
  })
  
  output$loginMessage <- renderUI({
    print("output$loginMessage invoked")
    if(!userInfo$LoggedIn){
      if(input$loginBtn > 0){
        userInfo$URL = isolate(input$URL)
        userInfo$userName = isolate(input$userName)
        userInfo$passWord = isolate(input$passWord)
        if(loginSucceed()){
          userInfo$LoggedIn = TRUE
          ""
        } else{
          "Incorrect Blog address or password, please input again"
        }
      }
    }
  })
  
  output$postsList <- renderDataTable({
    if(userInfo$LoggedIn){
      print("output$postsList is invoked")
      wpPosts$posts
    }
  })

  output$knitDoc <- renderUI({
    print("output$knitDoc is invoked")
    return(HTML(knit2html(text = input$editor, fragment.only = TRUE, quiet = TRUE)))
  })
  
  output$loadFileDlg <- renderUI({
    if(uiControl$showFileUploadDlg){
      div(class="modal-dialog", fileInput('rmdFile', 'Please choose Rmd file'),
          actionButton("loadFileDlgCloseBtn", "Close"))
    }
  })
  
  output$saveFileMsg <- renderUI({
    div(class="status-message", p(uiControl$saveFileMsg))
  })
  
  output$publishMsg <- renderUI({
    div(class="status-message", p(uiControl$publishMsg))
  })
  
  ## Observe the New Post button click event
  observe({
    if(!is.null(input$newPostBtn) && input$newPostBtn > 0){
      updateTabsetPanel(session, "pepTabSetPanel", selected = "Editor")
      updateAceEditor(session, "editor", value=" ")
    }
  })
  ## Observe the Sign Out button click event
  observe({
    if(!is.null(input$logoutBtn) && input$logoutBtn > 0){
      print("input$logoutBtn is invoked")
      userInfo$LoggedIn = FALSE
      updateTextInput(session, "URL", value=userInfo$URL)
      updateTextInput(session, "userName", value=userInfo$userName)
      updateTextInput(session, "passWord", value=userInfo$passWord)
    }
  })
  
  ## Observe the New file button click event
  observe({
    if(!is.null(input$newFileBtn) && input$newFileBtn > 0){
      print("input$newFileBtn is invoked")
      updateAceEditor(session, "editor", value=" ")
    }
  })
  
  ## Observe the Save file button click event
  observe({
    if(!is.null(input$saveFileBtn) && input$saveFileBtn > 0){
      print("input$saveFileBtn is invoked")
      title = isolate(input$title)
      if(title == ""){
        uiControl$saveFileMsg = "Please input the title."
      } else
      {
        title = paste0(gsub(" ", "-", title), ".Rmd")
        fPath = paste0(getwd(), "/", title)
        writeLines(isolate(input$editor), con = fPath)
        uiControl$saveFileMsg = paste0("The file has been succeeded saved to: ", fPath)
      }
    }
  })
  
  ## Observe the Load file button click event
  observe({
    if(!is.null(input$loadFileBtn) && input$loadFileBtn > 0){
      uiControl$showFileUploadDlg = TRUE
    }
  })
  
  ## Observe the Delete button click event
  observe({
    if(!is.null(input$deleteBtn) && input$deleteBtn > 0){
      postid = isolate(input$postId)
      status = try(deletePost(postid), silent = TRUE)
      if(inherits(status, "try-error")){
        uiControl$publishMsg = paste0("Failed to delete the post with postoid", postid) 
      } else{
        wpPosts$posts = getAllPosts()
        uiControl$publishMsg = paste0("Post with postid", postid, " has been deleted successfully!")
      }
    }
  })
  ## Observe the Publish button click event
  observe({
    if(!is.null(input$publishBtn) && input$publishBtn > 0){
      print("input$publishBtn invoked")
      content = knit2html(text = isolate(input$editor), fragment.only = TRUE, quiet = TRUE)
      if(isolate(input$shortcodeCBox)){
        content = gsub("<pre><code class=\"([[:alpha:]]+)\">", 
                       "[sourcecode language=\"\\1\"]", content)
        content = gsub("<pre><code( class=\"no-highlight\"|)>", 
                       "[sourcecode]", content)
        content = gsub("</code></pre>", "[/sourcecode]", content)
      }
      title = isolate(input$title)
      status = try(newPost(list(description = content, title = title)), silent = TRUE)
      if(inherits(status, "try-error")){
        uiControl$publishMsg = "Failed to publish this post to wordpress!"
      } else{
        wpPosts$posts = getAllPosts()
        uiControl$publishMsg = paste0('"', title, '"', ' has been published to wordpress successfully!')
      }
    }
  })
  
  ## Observe the load file dialog tutton click event
  observe({
    if(!is.null(input$loadFileDlgCloseBtn) && input$loadFileDlgCloseBtn > 0){
      uiControl$showFileUploadDlg = FALSE
    }
  })
  
  ## Observe the file upload event and update the editor content
  observe({
    inFile = input$rmdFile
    if (is.null(inFile))
      return(NULL)
    tmpFile = readLines(inFile$datapath)
    updateAceEditor(session, "editor", value=paste(tmpFile, collapse="\n"))
    Sys.sleep(0.5) ## To see the loading animation:D
    uiControl$showFileUploadDlg = FALSE
  })
})