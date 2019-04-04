###
# Dashboard using 'shinydashboard' library 
###
# library(shinydashboard)
# ui <- tagList(
#   dashboardPage(
#     dashboardHeader(),
#     dashboardSidebar(
#       sidebarMenu(
#         sidebarUserPanel(
#           "Ambika Tyres",
#           "Admin"
#         ),
#         menuItem("Dashboard", tabName = "dashboard"),
#         menuItem("Purchase",
#                  menuSubItem("Add new entry", tabName = "newEntPur"),
#                  menuSubItem("Edit an entry", tabName = "editEntPur"),
#                  menuSubItem("Catalogue", tabName = "cataloguePur")),
#         menuItem("Sales",
#                  menuSubItem("Add new entry", tabName = "newEnt"),
#                  menuSubItem("Edit an entry", tabName = "editEnt"),
#                  menuSubItem("Catalogue", tabName = "catalogue")),
#         menuItem("Prediction",
#                  menuSubItem("Requirement Forecasting", tabName = "reqFor"),
#                  menuSubItem("Sales prediction", tabName = "salesPred"),
#                  menuSubItem("Monthly Prediction", tabName = "monPred"))
#         )
#       ),
#     dashboardBody(
#       tabItems(
#         tabItem(tabName = "dashboard",
#                 h3("Dashboard"),
#                 hr()),
#         tabItem(tabName = "newEntPur",
#                 addNew()),
#         tabItem(tabName = "editEntPur",
#                 editItem()),
#         tabItem(tabName = "cataloguePur",
#                 showAll()),
#         tabItem(tabName = "newEnt",
#                 h3("New sales entry"),
#                 hr()),
#         tabItem(tabName = "editEnt",
#                 h3("Edit sales entry"),
#                 hr()),
#         tabItem(tabName = "catalogue",
#                 h3("Catalogue"),
#                 hr())
#       )
#     )
#   )
# )
###################################################################