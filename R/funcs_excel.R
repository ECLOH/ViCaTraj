#' @author Elie.
#' @title La fonction "func_find_start_row"
#' @description  charge des données issues d'un fichier excel en détectant la ligne de départ grâce à la présence de noms de variables déterminés
#' @param fichier fichier excel
#' @param patname vecteur de chaines de catactères avec les pattern des noms de variables à détecter
#' @param ...  Elements de openxlsx2::read_xlsx() ou readxl::read_xls() => en particulier sheet
#' @return un tibble
#' @export
func_find_start_row <- function(fichier, patname = c("NOM","Prénom","date de naissance", "age"), N_MAX = 10, ...) {
    tst <- suppressMessages(
      readxl::read_excel(fichier, col_names=F, , 
      range = cellranger::cell_limits(c(1, NA), c(N_MAX,NA)), ... )
    )
  row_start <- which(sapply(1:nrow(tst), function(i) {
    ch_tst <- paste0(tst[i, ], collapse = " __ ")
    res <- sum(sapply(patname, function(x){grepl(x, ch_tst, ignore.case = T)})) >= (length(patname)-1)
    return(res)
  }))
  message("row détéctée avec readxl::read_excel()")
  message(row_start)
  message("-")
  row_start <- row_start - 1
  message("row_start:")
  print(row_start)

  if (!length(row_start) == 0) {
       tst <- readxl::read_excel(
         skip = row_start,
         col_names = T,
         path = fichier,
         ...
       )|> 
         janitor::clean_names()
    return(tst)
  }
}

#' @author Elie.
#' @title La fonction "func_find_start_row_simple"
#' @description  renvoie les lignes de noms
#' @param fichier fichier excel
#' @param patname vecteur de chaines de catactères avec les pattern des noms de variables à détecter
#' @param ...  Elements de openxlsx2::read_xlsx() ou readxl::read_xls() => en particulier sheet
#' @return un tibble
#' @export
func_find_start_row_simple <- function(fichier, patname = c("NOM","Prénom","date de naissance", "age"), N_MAX = 10, ...) {
    tst <- suppressMessages(
      readxl::read_excel(fichier, col_names=F, , 
      range = cellranger::cell_limits(c(1, NA), c(N_MAX,NA)), ... )
    )
  row_start <- which(sapply(1:nrow(tst), function(i) {
    ch_tst <- paste0(tst[i, ], collapse = " __ ")
    res <- sum(sapply(patname, function(x){grepl(x, ch_tst, ignore.case = T)})) >= (length(patname)-1)
    return(res)
  }))
  message("row détéctée avec readxl::read_excel()")
  message(row_start)
  message("-")
  row_start <- row_start - 1
  message("row_start:")
  print(row_start)
  return(row_start)
}




#' @author Elie.
#' @title La fonction "func_import_multisheet"
#' @description  charge des données issues de fichier avec plusieurs sheets et applique func_find_start_row()
#' @param fichier fichier excel
#' @return un tibble
#' @export
func_import_multisheet <- function(fichier, ... ) {
  wb <- openxlsx2::wb_load(file = fichier)
  sheets <- openxlsx2::wb_get_sheet_names(wb = wb)

  if (length(sheets) < 1) {
    message("0 sheets")
    func_find_start_row(fichier, ... )
  } else {
    message("MULTI sheets")
    lapply(sheets, function(si) {
      
      message(si)

      res <- func_find_start_row(fichier, sheet = si, ... )

      if (!is.null(names(res))) {
        res <- res |>
          mutate(across(everything(), as.character)) |> 
          mutate(SHEET = si) |> 
          janitor::clean_names() 

         names(res) <- stringi::stri_trans_general(
           str = names(res), 
           id = "Latin-ASCII"
         )

      }
      return(res)
    }) |>
      dplyr::bind_rows()
  }
}

#' @author Elie.
#' @title La fonction "func_readPDIE"
#' @description lit tous les fichiers xlsx et xls d'un dossier
#' @param dir dossier à balayer 
#' @param table_name nom de "tableau" excel à aller chercher
#' @return une liste
#' @export
func_readPDIE <- function(
  dir = "G:/Metiers/PublicInsertion/EVALUATIONS - DPM/LISTES PARTICIPANTS BILANS 2024/",
  table_name = "Tableau3"
) {
  li_fi <- list.files(path = dir, recursive = T)
  li_dat <- lapply(li_fi, function(fi) {
    print(fi)
    if (grepl(".xlsx", fixed = T, x = fi)) {
      message("--> Nom de fichier combiné : ")
      message(paste0(dir, fi))

      WB <- tryCatch(
        {
          openxlsx2::wb_load(file = paste0(dir, fi))
        },
        error = function(e) {
          return(NULL)
        }
      )
      if (!is.null(WB)) {
        DF <- WB$get_named_regions(tables = T)
        if ("Tableau3" %in% DF$name) {
          tib <- openxlsx2::wb_to_df(
            check_names = T,
            keep_attributes = F,
            skip_empty_cols = T,
            skip_empty_rows = T,
            detect_dates = T,
            convert = F,
            file = paste0(dir, fi),
            named_region = table_name
          ) |>
            janitor::clean_names() |>
            mutate(FILE = fi, DIR = dir) |>
            mutate(STATUT = "ok") |>
            mutate(across(everything(), ~ as.character(.x)))

          message("OK")
        } else {
          tib <- tryCatch(
            {
              restemp<- func_import_multisheet(fichier = paste0(dir, fi)) |>
                mutate(FILE = fi, DIR = dir) |>
                mutate(STATUT = "ok")
              return(restemp)

            },
            error = function(e) {
              message("XLSX --> Pas de Tableau3")
              message(as.character(e))

              return(
                tibble(FILE = fi, DIR = dir) |>
                  mutate(STATUT = "Pas de Tableau3")
              )
            }
          )
        }
      } else {
        tib <- tryCatch({
          message("xls 1 --> func_find_start_row()")
      restemp<- func_find_start_row(fichier = paste0(dir, fi))|>
        mutate(FILE = fi, DIR = dir) |>
        mutate(STATUT = "ok")
      return(restemp)
        },
        error = function(e) {
          message("xls 1 --> Pas de Tableau3")

          return(
            tibble(FILE = fi, DIR = dir) |>
              mutate(STATUT = "xls")
          )
        })
        #tib <- tibble(FILE = fi) |>
        #  mutate(STATUT = "ERROR : xls")
        }
    } else {
      tib <- tryCatch({
        message("xls 2 --> func_find_start_row()")
        restemp<- func_find_start_row(fichier = paste0(dir, fi))|>
      mutate(FILE = fi, DIR = dir) |>
      mutate(STATUT = "ok")
    return(restemp)
      },
      error = function(e) {
        message("xls 2 -->Pas de Tableau3")

        return(
          tibble(FILE = fi, DIR = dir) |>
            mutate(STATUT = "xls")
        )
      })
    }
    return(tib)
  })
  li_dat <- li_dat[lengths(li_dat) > 0]

  li_dat <- li_dat |> dplyr::bind_rows()

  error_dat <- li_fi[!li_fi %in% li_dat$FILE]

  res <- list("li_dat" = li_dat, "error_dat" = error_dat)
  return(res)
}
