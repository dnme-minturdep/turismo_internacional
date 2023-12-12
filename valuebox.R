valueBox <- function(value, title = "", subtitle, icon, color) {
  
  div(class = "col-lg-3 col-md-6", style = "width: 100%;",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color),
              div(class = ("col-xs-12 text-right"),
                  if(!is.null(title))div(title, style = ("font-size: 20px; font-weight: bold;"))
              ),
              div(class = "row",
                  div(class = "col-xs-3",
                      icon(icon)
                  ),
                  div(class = ("col-xs-9 text-right"),
                      div(style = ("font-size: 40px; font-weight: bold;"),
                          value
                      ),
                      div(subtitle)
                  )
              )
          )
      )
  )
}
