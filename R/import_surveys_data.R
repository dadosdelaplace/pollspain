


import_raw_survey_wiki <- function(date_elec, link, id_wiki_table = 1) {

    # Import html from link
    html_link <- tryCatch(read_html(link), error = function(x) { "error" })

    # Check error
    if (length(html_link) == 1 & is.character(html_link)) {

      return(html_link)

    } else { #

      # Raw wiki table
      raw_survey <-
        html_table(html_nodes(html_link, ".wikitable")[[id_wiki_table]],
                   header = TRUE)

      # Preprocess links and img
      suppressWarnings({
      wiki_links_img <-
        html_nodes(html_link, "a") |>
        map_chr(function(x) { str_to_upper(as.character(x)) }) |>
        as_tibble() |>
        # Parse party names
        mutate(value = str_remove_all(value, "'|\\."),
               value = str_replace_all(value, "–", "-"),
               value = stringi::stri_trans_general(value, "Latin-ASCII"),
               value =
                 str_replace_all(value, "CONFEDERATION OF THE GREENS",
                                 "LV"),
               value =
                 str_replace_all(value, "UNIDOS PODEMOS|UNIDAS PODEMOS", "UP"),
               value = str_replace_all(value, "IU-UPEC", "IU"),
               value = str_replace_all(value, "ERC-CATSI", "ERC"),
               value = str_replace_all(value, "PSA-PA", "PA"),
               value =
                 str_replace_all(value,
                                 "GALICIAN COALITION|COALICION GALLEGA", "CG"),
               value =
                 str_replace_all(value, "AP-PDP-PAR|AP-PDP-PDL|AP-PDP-PL|AP-PL-UPN|AP-PDL|AP-PL", "AP"),
               value = str_replace_all(value, "EHBILDU|EH BILDU", "EH-BILDU"),
               value = str_replace_all(value, "VERDES|LOS VERDES", "LV"),
               value =
                 str_replace_all(value,
                                 "RUIZ-MATEOS GROUP|AGRUPACION RUIZ-MATEOS|RUIZ-MATEOS",
                                 "ARM"),
               value =
                 str_replace_all(value, "BASQUE NATIONALIST PARTY", "EAJ-PNV"),
               value =
                 str_replace_all(value, "GALICIAN NATIONALIST BLOC", "BNG"),
               value =
                 str_replace_all(value, "ANDALUSIAN PROGRESS PARTY", "PAP"),
               value = str_replace_all(value, "EUSKADIKO EZKERRA", "EE"),
               value = str_replace_all(value, "IU-LV", "IU"),
               value =
                 str_replace_all(value, "INITIATIVE FOR CATALONIA", "ICV"),
               value = str_replace_all(value, "ERC-SOBIRANISTES", "ERC"),
               value = str_replace_all(value, "CCA", "CC"),
               value =
                 str_replace_all(value,
                                 "REGIONALIST PARTY OF CANTABRIA", "PRC"),
               value = str_replace_all(value, "MAS PAIS", "MP"),
               value = str_replace_all(value, "NA\\+", "NA-SUMA"),
               value = str_replace_all(value, "CC-NCA|NC-CCA-PNC", "CC-NC"),
               value = str_replace_all(value, "JXCAT", "JXCAT-JUNTS"),
               value = str_replace_all(value, "TERUEL EXISTE", "TE"),
               value = str_remove_all(value, "SVG |ISOTIPO |LOGO "),
               value = str_remove_all(value, "SVG|ISOTIPO|LOGO"),
               value = str_trim(value, side = "both"))

      })

      # HB --> EH
      if ((wiki_links_img |>
           filter(str_detect(value, "EUSKAL_HERRITARROK")) |> nrow()) > 0) {

        wiki_links_img <-
          wiki_links_img |>
          mutate(value =
                   str_replace_all(value, "HERRI_BATASUNA",
                                   "EUSKAL_HERRITARROK"),
                 value =
                   ifelse(str_detect(value, "EUSKAL_HERRITARROK"),
                          str_replace_all(value, "HB", "EH"), value))

      }

      # Match wiki table columns with valid party names
      col_names <-
        wiki_links_img |>
        mutate(value = str_extract(value, paste(paste0('TITLE="',
                                     historical_parties |>
                                       pull(abbrev_candidacies), '"'),
                                      collapse = "|")),
               value = str_remove_all(value, 'TITLE="|"'),
               value = str_replace_all(value, "DIL|CDC", "DIL-CDC")) |>
        drop_na() |> distinct() |>
        slice(1:(ncol(raw_survey) - 5)) |>
        pull(value)

      # Rename raw_survey
      names(raw_survey) <-
        c("pollster", "field_date", "size", "turnout", col_names, "lead")

      suppressWarnings({
      survey <-
        raw_survey |>
        # Pollster to upper
        mutate(pollster = str_to_upper(pollster)) |>
        # Remove elections results
        filter(!str_detect(pollster,
                           "GENERAL ELECTION|GENERAL ELECTIONS|
                           LOCAL ELECTIONS|LOCAL ELECTION|
                           EP ELECTION|EP ELECTIONS|EP ELECTION")) |>
        # Remove NULL rows
        filter(!if_all(-c(pollster:turnout, lead),
                       function(x) {
                         x == ""
                         })) |>
        # Remove ? rows
        filter(!if_all(-c(pollster:turnout, lead),
                       function(x) {
                         str_detect(x, "\\?|–|-|\\[[:alnum:]\\]")
                         })) |>
        # Parsing numbers
        mutate(across(size:last_col(), parse_number),
               across(-c(pollster:turnout, lead),
                      function(x) {
                        char <-
                          ifelse(is.na(x) |
                                   str_detect(x, "\\?|\\[[:alnum:]\\]"), NA,
                                 str_split(as.character(x), pattern = "\\."))
                        output <-
                          char |>
                          map_dbl(function(x) {
                            if(is.na(x[1])) { NA }
                            else {
                            parse_number(x[1]) +
                              parse_number(
                                glue("0.{str_sub(x[2], start = 1, end = 1)}"))
                            }})
                        return(output) }),
               across(turnout:last_col(),
                      function(x) { ifelse(x < 0 | x > 100, NA, x) })) |>
        # Include date elections
        mutate(date_elec = date_elec, .before = everything()) |>
        # Relocate columns
        relocate(lead, .after = turnout) |>
        # Remove duplicates
        distinct(.keep_all = TRUE)
      })

      # Output
      return(survey)

    }
  }

get_raw_surveys_wiki <-
  function(from = 1982, to = 2019, type_elec = "congress",
           type_survey = "national",
           bd_links = historical_survey_links_wikipedia) {

    if (to < from) {

      to <- from
      warning("Parameter 'to' has been fixed equal to 'from'
              since it cannot be before 'from'")
    }
    if (from < 1982) {

      from <- 1982
      warning("Elections will be downloaded from surveys of elections of 1982")

    }

    links <-
      bd_links |>
      filter(type_elec == type_elec & type_survey == type_survey &
               between(year, from, to))

    if ((links |> nrow()) == 0) {

      stop("No surveys are available for the required settings")

    }

    filter_links <-
      links |>
      filter(!str_detect(links, "2015") & !str_detect(links, "2019") &
               !str_detect(links, "next_Spanish_general_election_"))

    raw_surveys <-
      map2(.x = filter_links$date_elec, .y = filter_links$links,
           function(x, y) { import_raw_survey_wiki(date_elec = x, link = y)}) |>
      discard(function(x) { length(x) == 1}) |>
      list_rbind() |>
      mutate(date_elec = date_elec)

    if (between(2015, from, to)) {

      filter_links <-
        links |>
        filter(str_detect(links, "2015"))

      raw_1 <-
        map2(.x = filter_links$date_elec, .y = filter_links$links,
             function(x, y) { import_raw_survey_wiki(date_elec = x, link = y)}) |>
        discard(function(x) { length(x) == 1}) |>
        list_rbind() |>
        mutate(date_elec = date_elec, field_date = glue("{field_date} 2015"))

      raw_2 <-
        map2(.x = filter_links$date_elec, .y = filter_links$links,
             function(x, y) { import_raw_survey_wiki(date_elec = x, link = y)}) |>
        discard(function(x) { length(x) == 1}) |>
        list_rbind() |>
        mutate(date_elec = date_elec, field_date = glue("{field_date} 2014"))

      raw_3 <-
        map2(.x = filter_links$date_elec, .y = filter_links$links,
             function(x, y) { import_raw_survey_wiki(date_elec = x, link = y)}) |>
        discard(function(x) { length(x) == 1}) |>
        list_rbind() |>
        mutate(date_elec = date_elec, field_date = glue("{field_date} 2013"))

      raw_4 <-
        map2(.x = filter_links$date_elec, .y = filter_links$links,
             function(x, y) { import_raw_survey_wiki(date_elec = x, link = y)}) |>
        discard(function(x) { length(x) == 1}) |>
        list_rbind() |>
        mutate(date_elec = date_elec, field_date = glue("{field_date} 2012"))

      raw_surveys <-
        raw_surveys |>
        bind_rows(raw_1, raw_2, raw_3, raw_4)

    }

    if (between(2019, from, to) &
        (links |>
         filter(str_detect(links, "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2019_Spanish")) |>
         nrow() == 1)) {

      filter_links <-
        links |>
        filter(str_detect(links, "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2019_Spanish"))

      raw_1 <-
         map2(.x = filter_links$date_elec, .y = filter_links$links,
             function(x, y) { import_raw_survey_wiki(date_elec = x, link = y)}) |>
        discard(function(x) { length(x) == 1}) |>
        list_rbind() |>
        mutate(date_elec = date_elec, field_date = glue("{field_date} 2019"))

      raw_2 <-
        map2(.x = filter_links$date_elec, .y = filter_links$links,
             function(x, y) { import_raw_survey_wiki(date_elec = x, link = y)}) |>
        discard(function(x) { length(x) == 1}) |>
        list_rbind() |>
        mutate(date_elec = date_elec, field_date = glue("{field_date} 2018"))

      raw_3 <-
        map2(.x = filter_links$date_elec, .y = filter_links$links,
             function(x, y) { import_raw_survey_wiki(date_elec = x, link = y)}) |>
        discard(function(x) { length(x) == 1}) |>
        list_rbind() |>
        mutate(date_elec = date_elec, field_date = glue("{field_date} 2017"))

      raw_4 <-
        map2(.x = filter_links$date_elec, .y = filter_links$links,
             function(x, y) { import_raw_survey_wiki(date_elec = x, link = y)}) |>
        discard(function(x) { length(x) == 1}) |>
        list_rbind() |>
        mutate(date_elec = date_elec, field_date = glue("{field_date} 2016"))

      raw_surveys <-
        raw_surveys |>
        bind_rows(raw_1, raw_2, raw_3, raw_4)

    }

    if (between(2019, from, to) &
        (links |>
         filter(str_detect(links, "https://en.wikipedia.org/wiki/Opinion_polling_for_the_November_2019_Spanish_general_election")) |>
         nrow() == 1)) {

      filter_links <-
        links |>
        filter(str_detect(links, "https://en.wikipedia.org/wiki/Opinion_polling_for_the_November_2019_Spanish_general_election"))

      raw_1 <-
        map2(.x = filter_links$date_elec, .y = filter_links$links,
             function(x, y) { import_raw_survey_wiki(date_elec = x, link = y)}) |>
        discard(function(x) { length(x) == 1}) |>
        list_rbind() |>
        mutate(date_elec = date_elec, field_date = glue("{field_date} 2019"))

      raw_surveys <-
        raw_surveys |> bind_rows(raw_1)


    }

    if (between(2023, from, to) &
        (links |>
         filter(str_detect(links, "next_Spanish_general_election_")) |>
         nrow()) == 1) {

      filter_links <-
        links |>
        filter(str_detect(links, "next_Spanish_general_election_"))

      raw_1 <-
        map2(.x = filter_links$date_elec, .y = filter_links$links,
             function(x, y) { import_raw_survey_wiki(date_elec = x, link = y)}) |>
        discard(function(x) { length(x) == 1}) |>
        list_rbind() |>
        mutate(date_elec = date_elec, field_date = glue("{field_date} 2021"))

      raw_2 <-
        map2(.x = filter_links$date_elec, .y = filter_links$links,
             function(x, y) { import_raw_survey_wiki(date_elec = x, link = y)}) |>
        discard(function(x) { length(x) == 1}) |>
        list_rbind() |>
        mutate(date_elec = date_elec, field_date = glue("{field_date} 2020"))

      raw_3 <-
        map2(.x = filter_links$date_elec, .y = filter_links$links,
             function(x, y) { import_raw_survey_wiki(date_elec = x, link = y)}) |>
        discard(function(x) { length(x) == 1}) |>
        list_rbind() |>
        mutate(date_elec = date_elec, field_date = glue("{field_date} 2019"))

      raw_surveys <-
        raw_surveys |>
        bind_rows(raw_1, raw_2, raw_3)

    }

    # Remove duplicates
    raw_surveys <-
      raw_surveys |>
      distinct(.keep_all = TRUE) |>
      mutate(id_elec =
               glue("{type_to_code_election(type_elec)}-{date_elec}"),
             .before = everything())

    return(raw_surveys)
  }

# historical_surveys <- preproc_raw_surveys(historical_raw_surveys)
# historical_surveys |> filter(is.na(pollster)) |> nrow()
# historical_surveys |> filter(is.na(field_date_from)) |> nrow()
# historical_surveys |> filter(is.na(field_date_to)) |> nrow()
preproc_raw_surveys <-
  function(historical_raw_surveys = historical_raw_surveys) {

    # Preprocess field date
    suppressWarnings({
    historical_surveys <-
      historical_raw_surveys |>
      # Two columns for field date: from and to field date
      separate(col = "field_date", into = c("field_date_from", "field_date_to"),
               sep = "–", remove = FALSE) |>
      # end date: if NA --> the same as from --> convert to date
      mutate(field_date_to =
               if_else(is.na(field_date_to), field_date_from, field_date_to),
             field_date_to = dmy(field_date_to)) |>
      # start date: get just day month (if parse_number("22 Oct 1989") --> 22)
      # we assign the same month and year that end date
      # if month is not available (30 days) --> NA --> month - 1
      mutate(aux =
               dmy(glue("{parse_number(field_date_from)}/{month(field_date_to)}/{year(field_date_to)}")),
             field_date_from =
               if_else(is.na(aux),
                       dmy(glue("{parse_number(field_date_from)}/{month(field_date_to) - 1}/{year(field_date_to)}")),
                       aux)) |>
      # If from > to (between months or years) --> minus 1 month
      mutate(field_date_from =
               if_else(field_date_from <= field_date_to, field_date_from,
                      field_date_from - months(1))) |>
      # Exit poll: same day of elections and just one day of field work
      mutate(n_days_surv = as.numeric(field_date_to - field_date_from) + 1,
             days_to_elec = as.numeric(date_elec - field_date_to),
             exit_poll = n_days_surv == 1 & days_to_elec == 0) |>
      # Remove columnas
      select(-aux, -field_date) |>
      # Relocate columns
      relocate(c(n_days_surv, days_to_elec, exit_poll), .after = field_date_to)

    # Preprocess pollster
    historical_surveys <-
      historical_surveys |>
      # Remove cites [1]
      mutate(pollster = str_to_upper(pollster),
             pollster =
               str_remove_all(pollster,
                              pattern = "\\[[:alnum:]\\]|\\[[:alnum:][:alnum:]\\]|\\[[:alnum:][:alnum:][:alnum:]\\]|\\[[:alnum:][:alnum:][:alnum:][:alnum:]\\]")) |>
      # Two columns for pollster: pollster and media
      separate(col = "pollster", into = c("pollster", "media"),
               sep = "/") |>
      # Preprocess media
      mutate(media =
               case_when(str_detect(media, "DIARIO16|DIARIO16") ~ "DIARIO 16",
                         str_detect(media, "EL PERIÒDIC") ~
                           "EL PERIÓDIC ANDORRA",
                         str_detect(media, "TELECINCO|TELE 5|TELE CINCO") ~
                           "TELECINCO",
                         str_detect(media, "LASEXTA|LA SEXTA") ~ "LA SEXTA",
                         TRUE ~ media)) |>
      # Preprocess pollster
      mutate(pollster =
               case_when(str_detect(pollster, "CIS") ~ "CIS",
                         str_detect(pollster, "40 DB|40DB|40|DB") ~ "40DB",
                         str_detect(pollster, "EMOPÚBLICA|EMOPUBLICA") ~
                           "EMOPÚBLICA",
                         str_detect(pollster,
                                    "DEMOSCOPIA Y SERVICIOS|
                                    DEMOSCOPIA SERVICIOS") ~
                           "DEMOSCOPIA Y SERVICIOS",
                         str_detect(pollster, "IPSOS") ~ "IPSOS",
                         str_detect(pollster, "GALLUP") ~ "GALLUP",
                         str_detect(pollster,
                                    "SIGMADOS|SIGMA DOS|SIGMA2|SIGMA 2") ~
                           "SIGMA DOS",
                         str_detect(pollster,
                                    "ÁGORA INTEGRAL|AGORA INTEGRAL") ~
                           "ÁGORA INTEGRAL",
                         str_detect(pollster, "GAD|GAD 3") ~ "GAD3",
                         str_detect(pollster,
                                    "GRUPO 16|GRUPO16|ALEF") ~ "GRUPO 16",
                         str_detect(pollster, "VOX PÚBLICA|VOX PUBLICA") ~
                           "VOX PÚBLICA",
                         str_detect(pollster,
                                    "EL CORREO|ELCORREO|ÁBACO|ABACO") ~
                           "ÁBACO-EL CORREO",
                         str_detect(pollster, "SOCIOMÉTRICA|SOCIOMETRICA") ~
                           "SOCIOMÉTRICA",
                         str_detect(pollster,
                                    "DATA10|DATA 10") ~ "DATA 10",
                         TRUE ~ pollster)) |>
      filter(!str_detect(pollster, "INTERIOR MINISTRY"))
    })

    # Create id for surveys
    historical_surveys <-
      historical_surveys |>
      # Create id by unique pollster
      mutate(id_pollster = glue("pollster-{cur_group_id()}"),
             .before = everything(), .by = pollster) |>
      mutate(id_survey =
               glue("{type_to_code_election(type_elec)}-{type_survey}-{id_pollster}-{field_date_from}-{field_date_to}"),
             .before = everything()) |>
      distinct(id_survey, .keep_all = TRUE)

    # Output
    return(historical_surveys)
  }

get_surveys <-
  function(date_from = NULL, date_to = NULL, select_date_elec = NULL,
           type_elec = "congress", type_survey = "national",
           select_pollster = "all", select_media = "all",
           select_parties = "all",
           min_field_days = NULL, max_field_days = NULL, min_size = NULL,
           min_days_to_elec = NULL, max_days_to_elec = NULL,
           include_media = TRUE, exclude_NA_size = TRUE,
           exclude_exit_poll = TRUE, exclude_parties_poll = TRUE,
           long_format = TRUE, longer_var_init = "id_survey",
           longer_var_end = "lead",
           date_formats = c("%d-%m-%Y", "%d/%m/%Y", "%d.%m.%Y",
                            "%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d"),
           n_current_surveys = 2,
           link_current_survey =
             "https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_Spanish_general_election") {

    # year
    if (is.numeric(date_from)) {
      if (between(date_from, 1979, 2100)) {

        date_from <- as_date(glue("{date_from}/01/01"))

      } else {

        stop("If date_from/to are numeric, it should be in year format yyyy between 1979 and 2100")

      }

    }
    if (is.numeric(date_to)) {
      if (between(date_to, 1979, 2100)) {

        date_to <- max(date_from, as_date(glue("{date_to}/12/31")))

      } else {

        stop("If date_from/to are numeric, it should be in year format yyyy between 1979 and 2100")

      }

    }

    # date as character
    if (is.character(date_from)) {

      if (is.na(as_date(date_from, format = date_formats))) {

        stop(paste("Please make sure that date_from/to were provided in properly formats according to date_formats variable: ", paste(date_formats, collapse = " or ")))

      } else {

        date_from <- as_date(date_from, format = date_formats)

      }
    }
    # date as character
    if (is.character(date_to)) {

      if (is.na(as_date(date_to, format = date_formats))) {

        stop(paste("Please make sure that date_from/to were provided in properly formats according to date_formats variable: ", paste(date_formats, collapse = " or ")))

      } else {

        date_to <- as_date(date_to, format = date_formats)

      }
    }

    # Historical surveys
    surveys <- preproc_raw_surveys(historical_raw_surveys)

    # Current surveys
    current_raw_surveys <-
      (1:n_current_surveys) |>
      map_dfr(function(x) {
        import_raw_survey_wiki(date_elec = as_date(NA), link = link_current_survey,
                               id_wiki_table = x) |>
          mutate(type_elec = "congress", type_survey = "national",
                 field_date = glue("{field_date} {year(today()) - x + 1}"))
        })

    surveys <-
      surveys |>
      bind_rows(preproc_raw_surveys(current_raw_surveys)) |>
      distinct(id_survey, .keep_all = TRUE) |>
      mutate(exit_poll = ifelse(is.na(exit_poll), FALSE, exit_poll))

    # Filter by date
    if (!is.null(date_from)) {

      surveys <-
        surveys |>
        filter(field_date_from >= date_from | field_date_to >= date_from)

    }
    if (!is.null(date_to)) {

      surveys <-
        surveys |>
        filter(field_date_from <= date_to | field_date_to <= date_to)

    }

    # Filter by date_elec
    if (!is.null(select_date_elec)) {

      if (is.character(select_date_elec)) {
          if (is.na(as_date(select_date_elec, format = date_formats))) {

            warning(paste("No date_elec filters were applied. Please make sure that select_date_elec were provided in properly formats according to date_formats variable: ", paste(date_formats, collapse = " or ")))

          } else {

            select_date_elec <- as_date(select_date_elec, format = date_formats)

            if ((surveys |>
                 filter(date_elec %in% select_date_elec) |> nrow()) == 0) {

              warning("No date_elec filters were applied. Please make sure that select_date_elec matches with elections of type provided")

            } else {

              surveys <-
                surveys |>
                filter(date_elec %in% select_date_elec)

            }
          }
        } else {

          warning(paste("No date_elec filters were applied. Please make sure that select_date_elec were provided in properly formats according to date_formats variable: ", paste(date_formats, collapse = " or ")))

      }

    }

    # Filter by type_elec and type_survey
    if (type_elec != "congress" | !(type_survey %in% c("national"))) {

      stop("At this time, just NATIONAL surveys for CONGRESS eleccions are available")

    } else {

      surveys <-
        surveys |>
        filter(type_elec == type_elec & type_survey == type_survey)

    }


    # Filter by pollster
    if (any(select_pollster != "all")) {

      select_pollster <- str_to_upper(select_pollster)
      if ((surveys |>
           filter(pollster %in% select_pollster) |> nrow()) == 0) {

        warning("None provided pollster has been found. No pollster filters were applied")

      } else {

        surveys <-
          surveys |> filter(pollster %in% select_pollster)

      }

    }

    # Filter by media
    if (any(select_media != "all")) {

      select_media <- str_to_upper(select_media)
      surveys <-
        surveys |>
        filter(media %in% select_media)

    }

    # Filter by parties
    if (any(select_parties != "all")) {

      select_parties <- str_to_upper(select_parties)

      if (!any(select_parties %in% names(surveys))) {

        warning("None provided parties has been found. No parties filters were applied")

      } else if (!all(select_parties %in% names(surveys))) {

        warning("Some of provided parties has not been found")

        select_parties <- select_parties[select_parties %in% names(surveys)]
        surveys <-
          surveys |>
          select(c(longer_var_init:longer_var_end, select_parties))
      }

      else {

        surveys <-
          surveys |>
          select(c(longer_var_init:longer_var_end, select_parties))

      }

    }

    # Filter by field days
    if (!is.null(min_field_days)) {

      if (min_field_days <= 0) {

        warning("No min_field_days filter was applied: parameter should be greater than 0")

      } else {

        surveys <-
          surveys |>
          filter(n_days_surv >= min_field_days)
      }

    }
    if (!is.null(max_field_days)) {

      if (max_field_days <= 0 | max_field_days < min_field_days) {

        warning("No max_field_days filter was applied: parameter should be greater than 0 and less (or equal) than min_field_days")

      } else {

        surveys <-
          surveys |>
          filter(n_days_surv <= max_field_days)
      }

    }

    # Filter by size
    if (!is.null(min_size)) {

      if (min_size <= 0) {

        warning("No min_size filter was applied: parameter should be greater than 0")

      } else {

        surveys <-
          surveys |> filter(size >= min_size)

      }

    }

    # Filter by days to elec
    if (!is.null(min_days_to_elec)) {

      if (min_days_to_elec <= 0) {

        warning("No min_days_to_elec filter was applied: parameter should be greater than 0")

      } else {

        surveys <-
          surveys |>
          filter(days_to_elec >= min_days_to_elec)

      }
    }
    if (!is.null(max_days_to_elec)) {

      if (max_days_to_elec <= 0 | max_days_to_elec < min_days_to_elec) {

        warning("No max_days_to_elec filter was applied: parameter should be greater than 0 and less (or equal) than min_days_to_elecfie")

      } else {

        surveys <-
          surveys |>
          filter(days_to_elec <= max_days_to_elec)

      }
    }

    # Drop NA size
    if (exclude_NA_size) {

      surveys <-
        surveys |> drop_na(size)

    } else if (!is.null(min_size)) {

      warning("Since min_size parameter was provied, exclude_NA_size = FALSE has not be considered")

    }

    # Drop exit polls
    if (exclude_exit_poll) {

      surveys <-
        surveys |> filter(!exit_poll)

    }

    # Drop poll's parties
    if (exclude_parties_poll) {

      surveys <-
        surveys |>
        filter(!(pollster %in% historical_parties$abbrev_candidacies |
                   media %in% historical_parties$abbrev_candidacies))

    }

    # Drop media variable
    if (!include_media) {

      surveys <- surveys |> select(-media)

    }

    # Tidy - long - format
    if (long_format) {

      surveys <-
        surveys |>
        pivot_longer(cols = -c(longer_var_init:longer_var_end),
                     names_to = "parties",
                     values_to = "est_vote",
                     values_drop_na = TRUE) |>
        # Preproc parties
        recod_parties(col_name_abbrev = "parties")

      # Most (est) voted
      surveys <-
        surveys |>
        left_join(surveys |>
                    slice_max(est_vote, by = id_survey, with_ties = FALSE) |>
                    select(id_survey, abbrev_candidacies) |>
                    rename(lead_party = abbrev_candidacies), by = "id_survey")

    } else { # Remove NA variables after filtering if wide format

      surveys <-
        surveys |>
        select(where(~!all(is.na(.x))))

    }

    # Output
    return(surveys)

}

# Wrong dates
# wrong_dates <- get_surveys(date_from = 1920, date_to = 2024)
# wrong_dates <- get_surveys(date_from = 95, date_to = 2024)
# wrong_dates <- get_surveys(date_from = "01-01-96", date_to = "12-03-2021")
# wrong_type_elec_survey <-
#   get_surveys(date_from = "01-01-1996", date_to = "12-03-2019",
#               type_elec = "senate")
# wrong_type_elec_survey <-
#   get_surveys(date_from = "01-01-1996", date_to = "12-03-2019",
#               type_survey = "mun")
#
# warning_date_elec <- get_surveys(date_from = "01-01-1996", date_to = "12-03-2019",
#                                  select_date_elec = "2019")
# warning_date_elec <- get_surveys(date_from = "01-01-1996", date_to = "12-03-2019",
#                                  select_date_elec = 2019)
# warning_date_elec <- get_surveys(date_from = "01-01-1996", date_to = "12-03-2019",
#                                  select_date_elec = "01-01-1997")
# warning_pollster <- get_surveys(date_from = "01-01-1996", date_to = "12-03-2019",
#                                 select_pollster = "INVENT")
# warning_parties <- get_surveys(date_from = "01-01-1996", date_to = "12-03-2019",
#                                select_parties = "INVENT")
# warning_parties <- get_surveys(date_from = "01-01-1996", date_to = "12-03-2019",
#                                select_parties = c("PSOE", "INVENT"))
# warning_field_days <- get_surveys(date_from = "01-01-1996", date_to = "12-03-2019",
#                                   select_pollster = c("GAD3", "CIS"),
#                                   min_field_days = -1)
# warning_field_days <- get_surveys(date_from = "01-01-1996", date_to = "12-03-2019",
#                                   select_pollster = c("GAD3", "CIS"),
#                                   min_field_days = 7, max_field_days = 4)
# warning_min_size <- get_surveys(date_from = "01-01-1996", date_to = "12-03-2019",
#                                 select_pollster = c("GAD3", "CIS"),
#                                 min_field_days = 7, max_field_days = 20,
#                                 min_size = -1)
# warning_days_to_elec <-
#   get_surveys(date_from = "01-01-1996", date_to = "12-03-2019",
#               select_pollster = c("GAD3", "CIS"),
#               min_field_days = 7, max_field_days = 30, min_size = 1000,
#               min_days_to_elec = 15, max_days_to_elec = 10)
#
# ok_dates <- get_surveys()
# ok_dates <- get_surveys(date_from = 1995, date_to = 2024)
# ok_dates <- get_surveys(date_from = "01-01-1996", date_to = "2019.03.12")
# ok_date_elec <- get_surveys(date_from = "01-01-1996", date_to = "12-03-2019",
#                             select_date_elec = NULL)
# ok_date_elec <- get_surveys(select_date_elec = c("2019-04-28"))
# ok_pollster <- get_surveys(date_from = "01-01-1996", date_to = "12-03-2019",
#                            select_pollster = c("GAD3", "CIS"))
# ok_parties <-
#   get_surveys(date_from = "01-01-1996", date_to = "12-03-2019",
#               select_parties = c("PSOE", "PP"), long_format = FALSE)
# ok_field_days <- get_surveys(date_from = "01-01-1996", date_to = "12-03-2019",
#                              select_pollster = c("GAD3", "CIS"),
#                              min_field_days = 7)
# ok_min_size <- get_surveys(date_from = "01-01-1996", date_to = "12-03-2019",
#                            select_pollster = c("GAD3", "CIS"),
#                            min_field_days = 7, max_field_days = 20,
#                            min_size = 2500)
# ok_days_to_elec <- get_surveys(date_from = "01-01-1996", date_to = "12-03-2019",
#                                select_pollster = c("GAD3", "CIS"),
#                                min_field_days = 7, max_field_days = 30,
#                                min_size = 1000,
#                                min_days_to_elec = 15, max_days_to_elec = 60)
# ok_exclude_parties_poll <-
#   get_surveys(date_from = "01-01-1996", date_to = "12-03-2019",
#               min_size = 500, min_days_to_elec = 15, max_days_to_elec = 120,
#               exclude_parties_poll = TRUE)
# ok_wide_format <-
#   get_surveys(date_from = "01-01-1996", date_to = "12-03-2019",
#               min_size = 2500, min_days_to_elec = 15, max_days_to_elec = 60,
#               long_format = FALSE)
# all_surveys <- get_surveys(exclude_NA_size = FALSE,
#                            exclude_exit_poll = FALSE, exclude_parties_poll = FALSE,
#                            long_format = TRUE)
# all_surveys_wide <- get_surveys(exclude_NA_size = FALSE,
#                                 exclude_exit_poll = FALSE, exclude_parties_poll = FALSE,
#                                 long_format = FALSE)

last_surveys <-
  function(last_days = 30, n_surveys = NULL,
           type_elec = "congress", type_survey = "national",
           select_pollster = "all", select_media = "all",
           select_parties = "all",
           min_size = NULL, include_media = TRUE, exclude_NA_size = TRUE,
           exclude_exit_poll = TRUE, exclude_parties_poll = TRUE,
           long_format = TRUE, longer_var_init = "id_survey",
           longer_var_end = "lead",
           date_formats = c("%d-%m-%Y", "%d/%m/%Y", "%d.%m.%Y",
                            "%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d"),
           n_current_surveys = 2,
           link_current_survey = "https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_Spanish_general_election") {

    # Select last surveys
    last_surveys <-
      get_surveys(date_from = today() - last_days, date_to = today(),
                  type_elec = type_elec, type_survey = type_survey,
                  select_pollster = select_pollster,
                  select_media = select_media,
                  select_parties = select_parties,
                  min_size = min_size,
                  include_media = include_media,
                  exclude_NA_size = exclude_NA_size,
                  exclude_exit_poll = exclude_exit_poll,
                  exclude_parties_poll = exclude_parties_poll,
                  long_format = long_format, longer_var_init = longer_var_init,
                  longer_var_end = longer_var_end, date_formats = date_formats,
                  n_current_surveys = n_current_surveys,
                  link_current_survey = link_current_survey)

    if (!is.null(n_surveys)) {

      if (!is.numeric(n_surveys) | n_surveys < 1) {

        warning("All surveys were provided: n_surveys should be
                a strictly positive integer")

      } else {

        # Select id for last n surveys
        last_n_surveys <-
          last_surveys |>
          distinct(id_survey, .keep_all = TRUE) |>
          slice_max(order_by = field_date_to, n = n_surveys) |>
          select(id_survey, field_date_to)

        last_surveys <-
          last_surveys |>
          inner_join(last_n_surveys, by = "id_survey")

      }
    }


    # output
    return(last_surveys)

}
