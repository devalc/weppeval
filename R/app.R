#' WEPPeval shiny application to be run locally
#'
#'
#' @export





run_WEPPeval_application <- function() {
options(shiny.maxRequestSize = 200 * 1024 ^ 2)
ui <-     shiny::navbarPage(
  title = "WEPPeval",

  windowTitle = "WEPPeval",


  theme = shinythemes::shinytheme("united"),
  # use_sever(),

  shiny::sidebarLayout(

    shiny::sidebarPanel(
      width = 3,

      shinyWidgets::awesomeRadio(
        inputId = "DefFileInput",
        label = "WEPP Run Import Options:",
        choices = c(
          # "Example dataset" = "example_dataset",
          # "Local run with wepppy-win-bootstrap" = "wepppy_win_bootstrap",
          "From WEPPCloud" = "from_WEPPCloud"
        ),
        selected = "from_WEPPCloud",
        status = 'default'
      ),



      shiny::conditionalPanel("input.navbar != 'grph_tab_val'",

      shiny::uiOutput("FileInput"),

      shinyWidgets::airDatepickerInput(
        "sim_start_dt",
        "Simulation start date:",
        value = "1990-01-02",
        # format = "yyyy-mm-dd"
      ),

      shinyWidgets::airDatepickerInput(
        "sim_end_dt",
        "Simulation end date:",
        value = "2020-01-01",
        # format = "yyyy-mm-dd"
      )),

      shiny::conditionalPanel("input.navbar == 'Daily_TS' || (input.navbar == 'WaterYear_TS')",

                              shinyWidgets::pickerInput(
          inputId = "var",
          label = "Select variable: ",
          choices = c(
            "Streamflow(mm)",
            "Sediments(tonnes/day)",
            "SWE(mm)",
            "TP(kg/day)",
            "PP(kg/day)",
            "SRP(kg/day)"
          ),
          options = list(style = "btn-danger")
        )
      ),
      #
      shiny::conditionalPanel("input.navbar == 'grph_tab_val'",

                       shiny::fileInput("graph_file", "Upload grph_*.txt file"),

                       shinyWidgets::pickerInput(
                         inputId = "grph_var",
                         label = "Select variable for time-series and scatter plots: ",
                         choices = grph_file_header_names[2:104],
                         selected = "Soil_water_in_layer_1_in",
                         options = list(style = "btn-danger")
                       ),

                       shinyWidgets::pickerInput(
                         inputId = "grph_var1",
                         label = "Select secondary variable to display as time-series: ",
                         choices = grph_file_header_names[2:104],
                         selected = "Precipitaiton_in",
                         options = list(style = "btn-danger")
                       )),

      # shinyWidgets::actionBttn(
      #     inputId = "gobttn",
      #     label = "Go!",
      #     color = "success",
      #     icon = icon("thumbs-up")
      #   )


     ),


    shiny::mainPanel(

      shiny::tabsetPanel(
        id = "navbar",
        type = "pills",
        shiny::tabPanel(
          "WaterBalance",
          shiny::column(
            6,
            shinycustomloader::withLoader(
              echarts4r::echarts4rOutput("totwatsd_wb_plt"),
              loader = "loader6"
            )
          ),
          shiny::column(6, shinycustomloader::withLoader(echarts4r::echarts4rOutput("plot_perc",width = "100%"),
                               loader = "loader6")),
          shiny::column(6, shinycustomloader::withLoader(DT::dataTableOutput("totwatsd_wb_tab"),
                               loader = "loader6")),
          shiny::column(6, shinycustomloader::withLoader(leaflet::leafletOutput("wbalHills"),
                               loader = "loader6"))
        ),
        shiny::tabPanel(
          "Daily_TS",
          shiny::column(
            12,
            shinycustomloader::withLoader(
              echarts4r::echarts4rOutput("plot_d"),
              loader = "loader6",proxy.height = "300px"
            )
          ),
          shiny::column(12,  DT::dataTableOutput("stats_d"))
        ),
        shiny::tabPanel(
          "WaterYear_TS",
          shiny::column(12,
                 shinycustomloader::withLoader(echarts4r::echarts4rOutput("plot_wy"),
                            loader = "loader6")        ),
          DT::dataTableOutput("stats_wy")
        ),
        shiny::tabPanel(
          "Grph_file_Visuals",
          id = "grph_tab",
          value = "grph_tab_val",
          shiny::fluidRow(
            shiny::column(
              12,
              shinycustomloader::withLoader(
                plotly::plotlyOutput("var_ts"),
                loader = "loader6"
              )
            )

          ),
          shiny::fluidRow(

            shiny::column(4, shinycustomloader::withLoader(plotly::plotlyOutput("var_scat1"),
                                 loader = "loader6")),
            shiny::column(
              4,
              shinycustomloader::withLoader(
                plotly::plotlyOutput("var_scat2"),
                loader = "loader6"
              )
            ),
            shiny::column(
              4,
              shinycustomloader::withLoader(
                plotly::plotlyOutput("var_scat3"),
                loader = "loader6"
              )
            )
          ),
          shiny::HTML("<br/><br/><br/>")
        )
        # tabPanel("WatBal", verbatimTextOutput("WatBal")),
        # tabPanel("Table_debug", tableOutput("tab1")),
        # tabPanel("Txt_debug", verbatimTextOutput("txto"))

      )


    )))


## --------------------------------------------------------------------------------------##
# Define Server
## --------------------------------------------------------------------------------------##


server <- function(input, output, session) {
  sever::sever()

  output$FileInput <- shiny::renderUI({
    if (input$DefFileInput == 'from_WEPPCloud') {
      shiny::textInput("prj_name", "Project URL")
    } else
      if (input$DefFileInput == 'wepppy_win_bootstrap') {
        shiny::fileInput(
          "uploadedfile",
          label = "Uplaod totalwatsed.txt | chanwb.out | ebe_pw0.txt | loss_pw0.txt | grph_*.txt",
          multiple = T,
          placeholder = "No file selected",
          accept = ".csv, .txt, .out"
        )
      }else{}
  })


  # observeEvent(input$gobttn, {


  wshed_area_m2 <- shiny::reactive({
    if (input$DefFileInput == 'wepppy_win_bootstrap') {
      shiny::req(input$uploadedfile)
      if(str_detect(input$uploadedfile[[3, "name"]], "grph", negate = FALSE) == FALSE){
      get_WatershedArea_m2(input$uploadedfile[[3, "datapath"]])}else
        if(str_detect(input$uploadedfile[[3, "name"]], "grph", negate = FALSE) == TRUE){
          get_WatershedArea_m2(input$uploadedfile[[4, "datapath"]])
        }
    } else
      if (input$DefFileInput == 'from_WEPPCloud') {
        shiny::req(input$prj_name)
        get_WatershedArea_m2(paste0(
          input$prj_name,
          "browse/wepp/output/",
          "loss_pw0.txt"
        ))
      }else
        if(input$DefFileInput == 'example_dataset'){
          get_WatershedArea_m2("data/loss_pw0.txt")
        }
  })

  totalwatsed <- shiny::reactive({
    if (input$DefFileInput == "from_WEPPCloud") {
      shiny::req(input$prj_name)
      shiny::req(input$sim_start_dt)
      shiny::req(input$sim_end_dt)
      df <- data.table::fread(paste0(
        input$prj_name,"resources/wepp/totalwatsed.csv"))

      df <- janitor::clean_names(df)%>%dplyr::rename("WY" = "water_year")%>%
        dplyr::mutate(Date = lubridate::make_date(year, mo, da))


    } else{
      if (input$DefFileInput == "wepppy_win_bootstrap") {
        shiny::req(input$uploadedfile)
        shiny::req(input$sim_start_dt)
        shiny::req(input$sim_end_dt)
        if(stringr::str_detect(input$uploadedfile[[3, "name"]], "grph", negate = FALSE) == FALSE){
        df <- data.table::fread(input$uploadedfile[[4, "datapath"]])
        df <- janitor::clean_names(df)%>%dplyr::rename("WY" = "water_year")%>%
          dplyr::mutate(Date = lubridate::make_date(year, mo, da))

        }else
            if(str_detect(input$uploadedfile[[3, "name"]], "grph", negate = FALSE) == TRUE){
              df <-data.table::fread(input$uploadedfile[[5, "datapath"]])
              df <- janitor::clean_names(df)%>%dplyr::rename("WY" = "water_year")%>%
                dplyr::mutate(Date = lubridate::make_date(year, mo, da))  }

      }
    }
  })

  totalwatsed_WY_mean <- shiny::reactive({
    shiny::req(totalwatsed())
      wymeandf <- totalwatsed() %>%
          dplyr::select(-julian,-year,-Date,-mo,-da) %>%
          dplyr::group_by(WY) %>%
          dplyr::summarise_all(mean)

  })

  ebe <- shiny::reactive({
      if (input$DefFileInput == "from_WEPPCloud") {
        shiny::req(input$prj_name)
        shiny::req(input$sim_start_dt)
        shiny::req(input$sim_end_dt)
          edf <-
              process_ebe(
                  paste0(
                      input$prj_name,
                      "browse/wepp/output/",
                      "ebe_pw0.txt"
                  ),
                  input$sim_start_dt,
                  input$sim_end_dt
              )
      } else
          if (input$DefFileInput == "wepppy_win_bootstrap") {
            shiny::req(input$uploadedfile)
            shiny::req(input$sim_start_dt)
            shiny::req(input$sim_end_dt)
              edf <-
                  process_ebe(input$uploadedfile[[2, "datapath"]],
                              input$sim_start_dt,
                              input$sim_end_dt)
          }
  })


  ebe_WY_mean <- shiny::reactive({
    shiny::req(ebe())
      wymeandf <- ebe() %>%
          dplyr::select(-Date,-Day_ebe,-Month_ebe,-Year_ebe) %>%
          dplyr::group_by(WY) %>%
          dplyr::summarise_all(mean)
  })


  chanwb <- shiny::reactive({
      if (input$DefFileInput == "from_WEPPCloud") {
        shiny::req(input$prj_name)
          cdf <-
              process_chanwb(
                  paste0(
                      input$prj_name,
                      "browse/wepp/output/",
                      "chanwb.out"
                  ),
                  wshed_area_m2()
              )
      } else
          if (input$DefFileInput == "wepppy_win_bootstrap") {
              {
                shiny::req(input$uploadedfile)
                  cdf <-
                      process_chanwb(input$uploadedfile[[1, "datapath"]], wshed_area_m2())
              }

          }
  })

  chanwb_WY_mean <- shiny::reactive({
    shiny::req(chanwb())
      wymeandf <- chanwb() %>%
          dplyr::select(-Year_chan,
                        -Day_chan,
                        -Chan_ID_chan,
                        -Elmt_ID_chan,
                        -Date) %>%
          dplyr::group_by(WY) %>%
          dplyr::summarise_all(mean)
  })


  merged_daily <- shiny::reactive({
    shiny::req(input$prj_name)
      daily_df <-
          merge_daily_Sim_obs_Vars(totalwatsed(), chanwb(), ebe())
  })


  merged_WY <- shiny::reactive({
    shiny::req(totalwatsed_WY_mean())
    shiny::req(chanwb_WY_mean())
    shiny::req(ebe_WY_mean())
      # req(obs_data_WY_mean())
      wy <-
          dplyr::left_join(
              as.data.frame(totalwatsed_WY_mean()),
              as.data.frame(chanwb_WY_mean()),
              by = c("WY")
          ) %>%
          dplyr::left_join(as.data.frame(ebe_WY_mean()), by = c("WY")) %>%
          dplyr::mutate(
              PercentRunoff = runoff_mm / Q_outlet_mm * 100,
              PercentLateral = lateral_flow_mm / Q_outlet_mm *
                  100,
              PercentBaseflow = baseflow_mm / Q_outlet_mm *
                  100
          )

  })

  perc_flow <- shiny::reactive({
    shiny::req(merged_WY())
      percdf <- merged_WY() %>%
          dplyr::select(WY, PercentRunoff, PercentLateral, PercentBaseflow)

      percdf <- percdf %>% dplyr::mutate(WY = as.Date(paste0(WY,"-12-31")))
  })

  output$plot_perc <- echarts4r::renderEcharts4r({
      perc_flow() %>%
      echarts4r::e_charts(x = WY) %>%
      echarts4r::e_area(PercentBaseflow) %>%
      echarts4r::e_area(PercentLateral) %>%
      echarts4r::e_area(PercentRunoff) %>%
      echarts4r::e_tooltip()

  })


  output$plot_d <- echarts4r::renderEcharts4r(if (input$var == "Streamflow(mm)") {
      merged_daily() %>%
      echarts4r::e_charts(Date) %>%
      echarts4r::e_line(Q_outlet_mm, name = "Simulated") %>%
      echarts4r::e_tooltip() %>%
      echarts4r::e_datazoom(x_index = 0, type = "slider") %>%
      echarts4r::e_datazoom(y_index = 0, type = "slider")
  } else
      if (input$var == "SWE(mm)") {
          merged_daily() %>%
          echarts4r::e_charts(Date)%>%
          echarts4r::e_line(swe_mm, name = "Simulated") %>%
          echarts4r::e_tooltip() %>%
          echarts4r::e_datazoom(x_index = 0, type = "slider") %>%
          echarts4r::e_datazoom(y_index = 0, type = "slider")
      } else
          if (input$var == "Sediments(tonnes/day)") {
              merged_daily() %>%
              echarts4r::e_charts(Date) %>%
              echarts4r::e_line(Sediment_tonnes_ebe, name = "Simulated") %>%
              echarts4r::e_tooltip() %>%
              echarts4r::e_datazoom(x_index = 0, type = "slider") %>%
              echarts4r::e_datazoom(y_index = 0, type = "slider")
          } else
              if (input$var == "TP(kg/day)") {
                  merged_daily() %>%
                  echarts4r::e_charts(Date) %>%
                  echarts4r::e_line(total_p_kg, name = "Simulated") %>%
                  echarts4r::e_tooltip() %>%
                  echarts4r::e_datazoom(x_index = 0, type = "slider") %>%
                  echarts4r::e_datazoom(y_index = 0, type = "slider")
              } else
                  if (input$var == "PP(kg/day)") {
                      merged_daily() %>%
                      echarts4r::e_charts(Date) %>%
                      echarts4r::e_line(particulate_p_kg, name = "Simulated") %>%
                      echarts4r::e_tooltip() %>%
                      echarts4r::e_datazoom(x_index = 0, type = "slider") %>%
                      echarts4r::e_datazoom(y_index = 0, type = "slider")
                  } else
                      if (input$var == "SRP(kg/day)") {
                          merged_daily() %>%
                          echarts4r::e_charts(Date) %>%
                          echarts4r::e_line(soluble_reactive_p_kg, name = "Simulated") %>%
                          echarts4r::e_tooltip() %>%
                          echarts4r::e_datazoom(x_index = 0, type = "slider") %>%
                          echarts4r::e_datazoom(y_index = 0, type = "slider")
                      } else
                      {
                          return()
                      })


  output$plot_wy <- echarts4r::renderEcharts4r(


      if (input$var == "Streamflow(mm)") {

         merged_WY() %>%
              dplyr::select(WY, Q_outlet_mm ) %>% dplyr::mutate(WY = as.Date(paste0(WY,"-12-31")))%>%
              dplyr::filter(complete.cases(.)) %>%
          echarts4r::e_charts(WY) %>%
          echarts4r::e_line(Q_outlet_mm, name = "Simulated") %>%
          echarts4r::e_tooltip() %>%
          echarts4r::e_datazoom(x_index = 0, type = "slider") %>%
          echarts4r::e_datazoom(y_index = 0, type = "slider")
  } else
      if (input$var == "SWE(mm)") {
        merged_WY() %>%
          dplyr::select(WY, swe_mm ) %>% dplyr::mutate(WY = as.Date(paste0(WY,"-12-31")))%>%
              dplyr::filter(complete.cases(.)) %>%
          echarts4r::e_charts(WY) %>%
          echarts4r::e_line(swe_mm, name = "Simulated") %>%
          echarts4r::e_tooltip() %>%
          echarts4r::e_datazoom(x_index = 0, type = "slider") %>%
          echarts4r::e_datazoom(y_index = 0, type = "slider")
      } else
          if (input$var == "Sediments(tonnes/day)") {
              merged_WY() %>%
                  dplyr::select(WY, Sediment_tonnes_ebe ) %>% dplyr::mutate(WY = as.Date(paste0(WY,"-12-31")))%>%
                  dplyr::filter(complete.cases(.)) %>%
              echarts4r::e_charts(WY) %>%
              echarts4r::e_line(Sediment_tonnes_ebe, name = "Simulated") %>%
              echarts4r::e_tooltip() %>%
              echarts4r::e_datazoom(x_index = 0, type = "slider") %>%
              echarts4r::e_datazoom(y_index = 0, type = "slider")
          } else
              if (input$var == "TP(kg/day)") {


                 merged_WY() %>%
                      dplyr::select(WY, total_p_kg ) %>% dplyr::mutate(WY = as.Date(paste0(WY,"-12-31")))%>%
                      dplyr::filter(complete.cases(.)) %>%
                  echarts4r::e_charts(WY) %>%
                  echarts4r::e_line(total_p_kg, name = "Simulated") %>%
                  echarts4r::e_tooltip() %>%
                  echarts4r::e_datazoom(x_index = 0, type = "slider") %>%
                  echarts4r::e_datazoom(y_index = 0, type = "slider")
              } else
                  if (input$var == "PP(kg/day)") {
                      merged_WY() %>%
                         dplyr::select(WY,  particulate_p_kg ) %>%
                      dplyr::mutate(WY = as.Date(paste0(WY,"-12-31")))%>%
                          dplyr::filter(complete.cases(.)) %>%
                      echarts4r::e_charts(WY) %>%
                      echarts4r::e_line(particulate_p_kg, name = "Simulated") %>%
                      echarts4r::e_tooltip() %>%
                      echarts4r::e_datazoom(x_index = 0, type = "slider") %>%
                      echarts4r::e_datazoom(y_index = 0, type = "slider")
                  } else
                      if (input$var == "SRP(kg/day)") {
                          merged_WY() %>%
                              dplyr::select(WY,  soluble_reactive_p_kg ) %>%
                          dplyr::mutate(WY = as.Date(paste0(WY,"-12-31")))%>%
                              dplyr::filter(complete.cases(.)) %>%
                          echarts4r::e_charts(WY) %>%
                          echarts4r::e_line(soluble_reactive_p_kg, name = "Simulated") %>%
                          echarts4r::e_tooltip() %>%
                          echarts4r::e_datazoom(x_index = 0, type = "slider") %>%
                          echarts4r::e_datazoom(y_index = 0, type = "slider")
                      } else
                      {
                          return()
                      })


  totalwatsed_wbal1 <- shiny::reactive({
    shiny::req(totalwatsed())
      totalwatsed_file <-
          totalwatsed() %>% dplyr::select(
              "WY",
              "Date",
              "year",
              "mo",
              "da",
              "precipitation_mm",
              "rain_melt_mm",
              "transpiration_mm",
              "evaporation_mm",
              "percolation_mm",
              "runoff_mm",
              "lateral_flow_mm"
          )

      if (lubridate::month(input$sim_start_dt) != 10 &
          lubridate::month(input$sim_start_dt) < 10) {
          totalwatsed_file %>% dplyr::filter(WY >= lubridate::year(input$sim_start_dt))
      } else
          if (lubridate::month(input$sim_start_dt) != 10 &
              lubridate::month(input$sim_start_dt) > 10) {
              totalwatsed_file %>% dplyr::filter(WY >= lubridate::year(input$sim_start_dt) +
                                                     1)
          } else
              if (lubridate::month(input$sim_start_dt) == 10) {
                  totalwatsed_file %>% dplyr::filter(WY >= lubridate::year(input$sim_start_dt) +
                                                         1)

              }
  })



  totalwatsed_wbal2 <- shiny::reactive({
      shiny::req(totalwatsed_wbal1())

      dfwb <- if (lubridate::month(input$sim_end_dt) <= 9) {
          totalwatsed_wbal1() %>% dplyr::filter(WY <= lubridate::year(input$sim_end_dt))
      } else
          if (lubridate::month(input$sim_end_dt) > 9) {
              totalwatsed_wbal1() %>%
              dplyr::filter(WY <= lubridate::year(input$sim_end_dt) +
                                                        1)
          }

      dfwb %>% dplyr::select(-Date,-year,-mo,-da)
  })


  totalwatsed_wbal <- shiny::reactive({
      shiny::req(totalwatsed_wbal2())

      totalwatsed_wbal2() %>% dplyr::select(-WY) %>%
          dplyr::summarise_all(.funs = sum) %>%
          dplyr::mutate(
              precipitation_mm = precipitation_mm / as.numeric(length(unique(
                  totalwatsed_wbal2()$WY
              ))),
              rain_melt_mm = rain_melt_mm / as.numeric(length(unique(
                  totalwatsed_wbal2()$WY
              ))),
              transpiration_mm = transpiration_mm / as.numeric(length(unique(
                  totalwatsed_wbal2()$WY
              ))),
              evaporation_mm = evaporation_mm / as.numeric(length(unique(
                  totalwatsed_wbal2()$WY
              ))),
              percolation_mm = percolation_mm / as.numeric(length(unique(
                  totalwatsed_wbal2()$WY
              ))),
              runoff_mm = runoff_mm / as.numeric(length(unique(
                  totalwatsed_wbal2()$WY
              ))),
              lateral_flow_mm = lateral_flow_mm / as.numeric(length(unique(
                  totalwatsed_wbal2()$WY
              )))
          ) %>%
          dplyr::mutate(
              rain_melt_mm = rain_melt_mm / precipitation_mm * 100,
              transpiration_mm = transpiration_mm / precipitation_mm *
                  100,
              evaporation_mm = evaporation_mm / precipitation_mm *
                  100,
              percolation_mm = percolation_mm / precipitation_mm *
                  100,
              runoff_mm = runoff_mm / precipitation_mm * 100,
              lateral_flow_mm = lateral_flow_mm / precipitation_mm *
                  100,
              WbalErr_mm = rain_melt_mm - (
                  transpiration_mm + evaporation_mm + percolation_mm + runoff_mm + lateral_flow_mm
              )
          ) %>%
          dplyr::rename(
              "precipitation_mm" = "precipitation_mm",
              "rain+melt(%)" = "rain_melt_mm",
              "transpiration(%)" = "transpiration_mm",
              "evaporation(%)" = "evaporation_mm",
              "percolation(%)" = "percolation_mm",
              "runoff(%)" = "runoff_mm",
              "lateral_flow(%)" = "lateral_flow_mm",
              "WbalErr(%)" = "WbalErr_mm"
          ) %>%
          tidyr::gather(key = "variable") %>% dplyr::mutate(dplyr::across(where(is.numeric), round, 2))

  })

  output$totwatsd_wb_tab <- DT::renderDataTable({
      totalwatsed_wbal()


  })

  output$totwatsd_wb_plt <- echarts4r::renderEcharts4r({
        shiny::req(totalwatsed_wbal())
        df <-
            totalwatsed_wbal() %>% dplyr::filter(variable != "precipitation_mm")  %>%
            echarts4r::e_charts(variable)  %>%
            echarts4r::e_pie(
                value,
                roseType = "radius",
                radius = c("55%", "70%"),
                legend = FALSE,
                hoverAnimation = TRUE
            )%>%
          echarts4r::e_tooltip(
                # formatter = e_tooltip_item_formatter("percent")
            )
    })


  shp <- shiny::reactive({
      shiny::req(input$prj_name)
      get_geometry(input$prj_name)
  })


  watfiles<- shiny::reactive({
    shiny::req(input$prj_name)
    gethillwatfiles(input$prj_name)
  })

  Hwatbal <- shiny::reactive({
    shiny::req(watfiles())
    # future::plan(multisession, workers = 6)
    purrr::map_df(watfiles(), calc_watbal)%>%
      dplyr::bind_rows() %>%    # make larger sample data
      dplyr::mutate_if(is.list, purrr::simplify_all) %>%    # flatten each list element internally
      tidyr::unnest(cols = c("wb", "WeppID"))
  })

  Hwatbal_spdf <- shiny::reactive({
    shiny::req(shp())
    shiny::req(Hwatbal())
    sp::merge(shp(),Hwatbal())

  })

  output$wbalHills <- leaflet::renderLeaflet({
    shiny::req(Hwatbal_spdf())

      pal <- leaflet::colorNumeric("viridis", domain = Hwatbal_spdf()$wb)

      leaflet::leaflet(Hwatbal_spdf()) %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) %>%
                      leaflet::addPolygons(fillColor = ~pal(wb),
                                           weight = 2,
                                           opacity = 1,
                                           color = "white",
                                           dashArray = "3",
                                           fillOpacity = 0.7,
                                           popup = ~paste("Hillslope ID:", Hwatbal_spdf()$WeppID,
                                                          "<br>","WaterBalance Error:", Hwatbal_spdf()$wb),
                                           label = ~WeppID,
                                           highlightOptions = leaflet::highlightOptions(
                                             weight = 5,
                                             color = "#666",
                                             dashArray = "",
                                             fillOpacity = 0.7,
                                             bringToFront = TRUE))%>% leaflet::addLegend(pal = pal,
                                                                         values = ~wb)
  })



  # https://wepp.cloud/weppcloud/runs/lt_202012_26_Bliss_Creek_CurCond/lt-wepp_bd16b69-snow/
  output$tab1 <- shiny::renderTable({# head(chanwb())
    head(grphdf())
  })



  grphdf <- shiny::reactive({
    shiny::req(input$graph_file)
    df <-
      data.table::fread(input$graph_file[[1, "datapath"]], skip = 121,
                        col.names = grph_file_header_names)


  })

  output$var_ts <- plotly::renderPlotly({

    shiny::req(grphdf())

    ay <- list(
      tickfont = list(color = "orange"),
      overlaying = "y",
      side = "right"
      # title = input$grph_var1
    )

    fig <- plotly::plot_ly(grphdf(), x = ~Days_In_Simulation, y = ~get(input$grph_var),
                   type = 'scatter', mode = 'lines', name = input$grph_var)%>%
      plotly::add_lines(x = ~Days_In_Simulation, y = ~get(input$grph_var1),
                name = input$grph_var1, yaxis = "y2")


    fig <- fig %>% plotly::layout(
      yaxis2 = ay,
      yaxis = list(title = ""),
      legend = list(orientation = 'h',
                    xanchor = "center",  # use center of legend as anchor
                    x = 0.5,
                    y = 1.0),
      margin =  list(
        l = 50,
        r = 50,
        pad = 4
      )

    )

  })



  output$var_scat1 <- plotly::renderPlotly({

    shiny::req(grphdf())

    fig1 <- plotly::plot_ly(data = grphdf(), x = ~Suction_across_wetting_front_in,
     y = ~get(input$grph_var),type = "scatter",mode = "markers",
                    marker = list(size = 10,
                                  color = '#fcca46', opacity=0.7,
                                  line = list(color = 'rgba(152, 0, 0, .8)',
                                              width = 2)))


    fig1<-fig1 %>% plotly::layout(yaxis = list(title = ''))
    fig1

  })


  output$var_scat2 <- plotly::renderPlotly({

    shiny::req(grphdf())

    fig2 <- plotly::plot_ly(data = grphdf(), x = ~Effective_hydraulic_conductivity_in_hr,
    y = ~get(input$grph_var),type = "scatter", mode = "markers",
                    marker = list(size = 10,
                                  color = '#f19c79',opacity=0.7,
                                  line = list(color = 'rgba(152, 0, 0, .8)',
                                              width = 2)))

    fig2<-fig2 %>% plotly::layout(yaxis = list(title = ''))
    fig2


  })

  output$var_scat3 <- plotly::renderPlotly({

    shiny::req(grphdf())

    fig3 <- plotly::plot_ly(data = grphdf(), x = ~Precipitation_in,
     y = ~get(input$grph_var),type = "scatter", mode = "markers",
                    marker = list(size = 10,
                                  color = '#06d6a0',opacity=0.7,
                                  line = list(color = 'rgba(152, 0, 0, .8)',
                                              width = 2)))

    fig3<-fig3 %>% plotly::layout(yaxis = list(title = ''))
    fig3


  })


  # })
  # output$txto <- renderText({
  #   if(str_detect(input$uploadedfile[[3, "name"]], "grph", negate = FALSE) == FALSE){
  #     print("Did you upload grph_*.txt file?")
  #   }
  #
  # })

}

## --------------------------------------------------------------------------------------##
# Run the application
## --------------------------------------------------------------------------------------##
shiny::shinyApp(ui = ui, server = server)

}
