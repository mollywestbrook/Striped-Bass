ui <- fluidPage(
  theme = bs_theme(preset = "sandstone"),
  
  
  titlePanel(
    title = "Coastal Bays Water Quality Covariance Widget"
  ),
  
  navset_card_tab(height = "600px",
                  
                  nav_panel("Covariance Plot",
                            "Plot different water quality parameters against each other.",
                            sidebarLayout(
                              sidebarPanel(
                                #select bay body
                                selectInput(
                                  inputId = "select_bay",
                                  label = "Select Bay",
                                  choices = c(
                                    "All",
                                    "Assawoman Bay",
                                    "Chincoteague Bay",
                                    "Isle of Wight Bay",
                                    "Newport Bay",
                                    "St. Martin River"
                                  ),
                                  selected = "All",
                                  multiple = T
                                ),
                                
                                #select x axis points
                                selectInput(
                                  inputId = "select_x_axis",
                                  label = "Select X-Axis Parameter",
                                  choices = c(
                                    "Chlorophyll" = "CHLA_UG/L",
                                    "Conductivity" = "COND_UMHOS/CM",
                                    "Dissolved Oxygen" = "DO_MG/L",
                                    "Dissolved Organic Carbon" = "DOC_MG/L",
                                    "Ammonia" = "NH4_MG/L",
                                    "Nitrate + Nitrite" = "NO23_MG/L",
                                    "Particulate Carbon" = "PC_MG/L",
                                    "pH" = "PH_SU",
                                    "Pheophytin" = "PHEO_UG/L",
                                    "Particulate Nitrogen" = "PN_MG/L",
                                    "Phosphorus" = "PO4_MG/L",
                                    "Particulate Phosphorus" = "PP_MG/L",
                                    "Salinity in the Field" = "	SALIN_FLD_PPT",
                                    "Salinity" = "SALINITY_PPT",
                                    "Secchi Depth" = "SECCHI_M",
                                    "Total Dissolved Nitrogen" = "TDN_MG/L",
                                    "Total Dissolved Phosphorus" = "TDP_MG/L",
                                    "Total Suspended Solids" = "TSS_MG/L",
                                    "Water Temperature" = "WTEMP_DEG C"),
                                  selected = "Salinity",
                                  multiple = F
                                ),
                                
                                #select y axis points
                                selectInput(
                                  inputId = "select_y_axis",
                                  label = "Select Y-Axis Parameter",
                                  choices = c(
                                    "Chlorophyll" = "CHLA_UG/L",
                                    "Conductivity" = "COND_UMHOS/CM",
                                    "Dissolved Oxygen" = "DO_MG/L",
                                    "Dissolved Organic Carbon" = "DOC_MG/L",
                                    "Ammonia" = "NH4_MG/L",
                                    "Nitrate + Nitrite" = "NO23_MG/L",
                                    "Particulate Carbon" = "PC_MG/L",
                                    "pH" = "PH_SU",
                                    "Pheophytin" = "PHEO_UG/L",
                                    "Particulate Nitrogen" = "PN_MG/L",
                                    "Phosphorus" = "PO4_MG/L",
                                    "Particulate Phosphorus" = "PP_MG/L",
                                    "Salinity in the Field" = "	SALIN_FLD_PPT",
                                    "Salinity" = "SALINITY_PPT",
                                    "Secchi Depth" = "SECCHI_M",
                                    "Total Dissolved Nitrogen" = "TDN_MG/L",
                                    "Total Dissolved Phosphorus" = "TDP_MG/L",
                                    "Total Suspended Solids" = "TSS_MG/L",
                                    "Water Temperature" = "WTEMP_DEG C"),
                                  selected = "Conductivity",
                                  multiple = F
                                )
                              ),
                              mainPanel(
                                plotOutput("wqplot"),
                              )
                            )
                  ),
                  nav_panel("Station Map",
                            "Plot Stations sampled per bay.",
                            mainPanel(
                              leafletOutput("StationMap"), width = "100%"
                            )
                  )
  )
)