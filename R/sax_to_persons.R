# sax_to_persons.R
# Requirements: install.packages(c("XML","data.table","arrow"))
library(XML)
library(data.table)
library(arrow)

# normalize name helper
normalize_name <- function(k, remove_prefix = NULL) {
  col <- k
  if (!is.null(remove_prefix) && startsWith(col, remove_prefix)) col <- substring(col, nchar(remove_prefix) + 1)
  col <- gsub("\\.", "_", col)
  col <- gsub("@", "_attr_", col)
  col <- gsub("\\s+", "_", col)
  col <- gsub("_+", "_", col)
  col <- gsub("^_|_$", "", col)
  col
}

# flatten a simple node (leaf) to named list of immediate children -> text
flatten_children_to_named <- function(node) {
  # node is a list representing element: $children may be named elements or list of elements
  res <- list()
  if (is.null(node$children) || length(node$children) == 0) return(res)
  for (nm in names(node$children)) {
    val <- node$children[[nm]]
    # if val is a list of multiple entries, pick first non-empty text or produce collapse? Here pick first non-empty
    if (is.list(val) && !is.null(names(val))) {
      # nested single child (a named list)
      # try to get text if exists as val$text
      if (!is.null(val$text) && nzchar(trimws(as.character(val$text)))) {
        res[[ normalize_name(nm) ]] <- as.character(val$text)
      } else {
        # attempt depth 1 children
        leaf <- NULL
        for (subnm in names(val)) {
          if (is.character(val[[subnm]]) && nzchar(trimws(as.character(val[[subnm]])))) {
            leaf <- as.character(val[[subnm]]); break
          }
        }
        res[[ normalize_name(nm) ]] <- ifelse(is.null(leaf), NA_character_, leaf)
      }
    } else if (is.list(val) && is.null(names(val))) {
      # list of repetitions -> take first non-empty occurrence
      found <- NULL
      for (entry in val) {
        # entry may be list with text or children
        if (!is.null(entry$text) && nzchar(trimws(as.character(entry$text)))) {
          found <- as.character(entry$text); break
        } else {
          # search in entry for first atomic child
          if (is.list(entry) && length(entry) > 0) {
            for (x in entry) {
              if (is.character(x) && nzchar(trimws(as.character(x)))) { found <- as.character(x); break }
            }
            if (!is.null(found)) break
          }
        }
      }
      res[[ normalize_name(nm) ]] <- ifelse(is.null(found), NA_character_, found)
    } else if (is.character(val)) {
      res[[ normalize_name(nm) ]] <- val
    } else {
      # fallback try to coerce
      res[[ normalize_name(nm) ]] <- as.character(val)
    }
  }
  return(res)
}

# convert person node (list) -> named vector of fields (dedupe by first non-empty)
person_node_to_row <- function(person_node) {
  # person_node$children should hold fields
  if (is.null(person_node$children)) return(list())
  # build map of normalized name -> first non-empty
  seen <- list()
  for (nm in names(person_node$children)) {
    val <- person_node$children[[nm]]
    # if repeated, val may be list of nodes
    if (is.list(val) && is.null(names(val))) {
      # iterate occurrences, for each attempt to get text or child text
      for (occ in val) {
        if (!is.null(occ$text) && nzchar(trimws(as.character(occ$text)))) {
          cand <- as.character(occ$text)
        } else {
          # search any leaf in occ
          cand <- NULL
          for (sub in occ) {
            if (is.character(sub) && nzchar(trimws(as.character(sub)))) { cand <- as.character(sub); break }
          }
        }
        if (!is.null(cand) && nzchar(trimws(cand))) {
          key <- normalize_name(nm, remove_prefix = "Personne.")
          if (is.null(seen[[key]])) seen[[key]] <- cand
          # keep first non-empty then break
          break
        }
      }
    } else if (is.list(val) && !is.null(names(val))) {
      # single nested element
      if (!is.null(val$text) && nzchar(trimws(as.character(val$text)))) {
        cand <- as.character(val$text)
        key <- normalize_name(nm, remove_prefix = "Personne.")
        if (is.null(seen[[key]])) seen[[key]] <- cand
      } else {
        # scan children
        cand <- NULL
        for (sub in val) {
          if (is.character(sub) && nzchar(trimws(as.character(sub)))) { cand <- as.character(sub); break }
        }
        key <- normalize_name(nm, remove_prefix = "Personne.")
        if (!is.null(cand) && is.null(seen[[key]])) seen[[key]] <- cand
      }
    } else if (is.character(val)) {
      key <- normalize_name(nm, remove_prefix = "Personne.")
      if (is.null(seen[[key]])) seen[[key]] <- val
    } else {
      # fallback
      key <- normalize_name(nm, remove_prefix = "Personne.")
      seen[[key]] <- as.character(val)
    }
  }
  return(seen)
}

# select address from list of address nodes (each address_node -> children)
select_address_from_nodes <- function(addr_nodes) {
  if (length(addr_nodes) == 0) return(list())
  # compute normalized address rows for each node
  addr_rows <- lapply(addr_nodes, function(nd) {
    flatten_children_to_named(nd)
  })
  # find RGADR key if present
  rg_key <- NULL
  if (length(addr_rows) > 0) {
    keys0 <- names(addr_rows[[1]])
    rg_candidates <- keys0[toupper(keys0) %in% toupper(keys0) & grepl("RGADR", toupper(keys0))]
    if (length(rg_candidates) > 0) rg_key <- rg_candidates[1]
  }
  if (!is.null(rg_key)) {
    for (ar in addr_rows) {
      v <- ar[[rg_key]]
      if (!is.null(v) && !is.na(v) && trimws(as.character(v)) == "01") return(ar)
    }
  }
  # fallback first
  return(addr_rows[[1]])
}

# Build household row from root node (list)
build_household_row <- function(root_node, selected_address) {
  hh <- list()
  # try to extract some top-level fields (Identification etc.)
  if (!is.null(root_node$children)) {
    for (nm in names(root_node$children)) {
      if (!(nm %in% c("Personne", "DonneesAdministratives"))) {
        # flatten first occurrence
        val <- root_node$children[[nm]]
        if (is.list(val) && is.null(names(val))) {
          # repetition -> take first
          val0 <- val[[1]]
          if (!is.null(val0$text) && nzchar(trimws(as.character(val0$text)))) hh[[normalize_name(nm)]] <- as.character(val0$text) else hh[[normalize_name(nm)]] <- NA_character_
        } else if (is.list(val) && !is.null(names(val))) {
          if (!is.null(val$text) && nzchar(trimws(as.character(val$text)))) hh[[normalize_name(nm)]] <- as.character(val$text) else {
            # search children for leaf
            leaf <- NA_character_
            for (s in val) if (is.character(s) && nzchar(trimws(as.character(s)))) { leaf <- as.character(s); break }
            hh[[normalize_name(nm)]] <- leaf
          }
        } else if (is.character(val)) {
          hh[[normalize_name(nm)]] <- val
        } else {
          hh[[normalize_name(nm)]] <- as.character(val)
        }
      }
    }
  }
  # attach address fields prefixed Adresse.
  if (length(selected_address) > 0) {
    for (k in names(selected_address)) {
      hh[[ paste0("Adresse.", k) ]] <- selected_address[[k]]
    }
  }
  return(hh)
}

# Process a completed fragment represented by root_node (list) -> returns list(hh_row, person_rows)
process_fragment_node <- function(root_node) {
  # root_node$children should contain Personne and DonneesAdministratives etc.
  person_rows <- list()
  house_row <- list()
  # addresses
  addr_nodes <- list()
  if (!is.null(root_node$children$DonneesAdministratives)) {
    da <- root_node$children$DonneesAdministratives
    # da$Adresse may be single or multiple
    if (!is.null(da$children$Adresse)) {
      addr_val <- da$children$Adresse
      if (is.list(addr_val) && is.null(names(addr_val))) {
        addr_nodes <- addr_val
      } else {
        addr_nodes <- list(addr_val)
      }
    }
  }
  selected_address <- select_address_from_nodes(addr_nodes)

  # build household
  house_row <- build_household_row(root_node, selected_address)

  # build persons: root_node$children$Personne may be list of person nodes or single
  if (!is.null(root_node$children$Personne)) {
    pers_val <- root_node$children$Personne
    pers_list <- if (is.list(pers_val) && is.null(names(pers_val))) pers_val else list(pers_val)
    for (pnode in pers_list) {
      prow <- person_node_to_row(pnode)
      # attach household id if present in house_row (try common keys)
      hid <- NULL
      for (k in c("Identification_IDFOYER","IDENTIFICATION_IDFOYER","Identification_IDFoyer","id_foyer","FOYER_ID")) {
        if (!is.null(house_row[[k]])) { hid <- house_row[[k]]; break }
      }
      if (is.null(hid)) hid <- paste0("hf_", as.integer(Sys.time())) # fallback unique-ish
      prow$household_id <- hid
      # attach selected address fields
      if (length(selected_address) > 0) {
        for (ak in names(selected_address)) prow[[ paste0("Adresse.", ak) ]] <- selected_address[[ak]]
      }
      person_rows[[ length(person_rows) + 1L ]] <- prow
    }
  }
  return(list(house = house_row, persons = person_rows))
}

# SAX parser building small tree per fragment
process_xml_sax <- function(xml_file, record_tag = "InfosFoyerRSA",
                            out_dir = "out_parquet", person_batch = 5000, house_batch = 1000,
                            write_households = TRUE, report_every = 500) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  # buffers
  persons_buf <- list(); houses_buf <- list()
  person_part <- 0L; house_part <- 0L
  total_persons <- 0L; total_houses <- 0L

  # state environment
  state <- new.env(parent = emptyenv())
  state$in_frag <- FALSE
  state$stack <- list()  # stack of nodes; each node is a list(name=..., attrs=list(), children=list(), text="")
  state$frag_count <- 0L
  state$line_count <- 0L

  startElement <- function(name, attrs, ...) {
    # name may include namespace prefix; we want localname only
    localname <- sub(".*:", "", name)
    if (!state$in_frag) {
      if (localname == record_tag) {
        state$in_frag <- TRUE
        # initialize root node
        node <- list(name = localname, attrs = as.list(attrs), children = list(), text = "")
        state$stack <- list(node)
        state$frag_count <- state$frag_count + 1L
      } else {
        # ignore outside nodes
      }
    } else {
      # push new node
      node <- list(name = localname, attrs = as.list(attrs), children = list(), text = "")
      state$stack[[ length(state$stack) + 1L ]] <- node
    }
  }

  textHandler <- function(text, ...) {
    if (!state$in_frag) return()
    if (length(text) == 0L) return()
    # append text to top node
    top_idx <- length(state$stack)
    if (top_idx >= 1L) {
      cur <- state$stack[[top_idx]]
      cur$text <- paste0(cur$text, text)
      state$stack[[top_idx]] <- cur
    }
  }

  endElement <- function(name, ...) {
    localname <- sub(".*:", "", name)
    if (!state$in_frag) return()
    # pop top
    top_idx <- length(state$stack)
    node <- state$stack[[ top_idx ]]
    state$stack <- state$stack[-top_idx]
    # build a compact representation of node: if children empty -> keep text (trim)
    val <- NULL
    if (length(node$children) == 0) {
      txt <- if (is.null(node$text) || trimws(node$text) == "") NULL else trimws(node$text)
      # if attributes exist and txt is null, include attributes
      if (!is.null(node$attrs) && length(node$attrs) > 0) {
        # attach attributes as named entries under node
        node_repr <- list()
        if (!is.null(txt)) node_repr$text <- as.character(txt)
        for (an in names(node$attrs)) node_repr[[ paste0("@", an) ]] <- as.character(node$attrs[[an]])
        val <- node_repr
      } else {
        val <- if (is.null(txt)) "" else as.character(txt)
      }
    } else {
      # node has children -> create named list
      node_children <- node$children
      val <- node_children
      # if attributes present, attach them as entries prefixed with @
      if (!is.null(node$attrs) && length(node$attrs) > 0) {
        for (an in names(node$attrs)) val[[ paste0("@", an) ]] <- as.character(node$attrs[[an]])
      }
    }

    # attach val into parent (if any) under name node$name
    if (length(state$stack) == 0) {
      # this was root fragment end
      root_node <- list(name = node$name, attrs = node$attrs, children = list())
      # node may itself have children represented in val; if val is list treat as children else create text child
      if (is.list(val) && length(val) > 0 && !is.character(val)) {
        root_node$children <- val
      } else {
        root_node$children[[ node$name ]] <- val
      }
      # process root_node: but we actually want node with children representing top children
      # simpler: reconstruct root as node with children equal to val if val is list
      root_node <- list(name = node$name, attrs = node$attrs, children = if (is.list(val)) val else list(text = val))
      # process fragment
      frag_res <- tryCatch(process_fragment_node(root_node), error = function(e) { warning("process_fragment_node error: ", e); NULL })
      if (!is.null(frag_res)) {
        # append household
        if (write_households && length(frag_res$house) > 0) {
          houses_buf[[ length(houses_buf) + 1L ]] <- frag_res$house
          total_houses <<- total_houses + 1L
        }
        if (length(frag_res$persons) > 0) {
          for (pr in frag_res$persons) persons_buf[[ length(persons_buf) + 1L ]] <- pr
          total_persons <<- total_persons + length(frag_res$persons)
        }
        # flush if buffers large
        if (length(persons_buf) >= person_batch) {
          dtp <- rbindlist(persons_buf, fill = TRUE)
          fname <- file.path(out_dir, sprintf("persons-part-%06d.parquet", person_part))
          arrow::write_parquet(dtp, sink = fname)
          person_part <<- person_part + 1L
          persons_buf <<- list()
        }
        if (write_households && length(houses_buf) >= house_batch) {
          dth <- rbindlist(houses_buf, fill = TRUE)
          fnameh <- file.path(out_dir, sprintf("households-part-%06d.parquet", house_part))
          arrow::write_parquet(dth, sink = fnameh)
          house_part <<- house_part + 1L
          houses_buf <<- list()
        }
      }
      state$in_frag <- FALSE
    } else {
      # attach to parent node's children. Parent is last on stack
      parent_idx <- length(state$stack)
      parent <- state$stack[[ parent_idx ]]
      # if same name exists in parent$children, append to list
      if (!is.null(parent$children[[ node$name ]])) {
        exist <- parent$children[[ node$name ]]
        # if exist is a list-of-multiples (unnamed list), append
        if (is.list(exist) && is.null(names(exist))) {
          parent$children[[ node$name ]] <- c(exist, list(val))
        } else {
          # make a list of entries
          parent$children[[ node$name ]] <- list(exist, val)
        }
      } else {
        parent$children[[ node$name ]] <- val
      }
      state$stack[[ parent_idx ]] <- parent
    }
    # reporting
    if (state$frag_count %% report_every == 0 && state$frag_count > 0) {
      message("Fragments parsed: ", state$frag_count, " (line approx unknown)")
    }
  }

  # run parser
  xmlEventParse(xml_file,
                handlers = list(startElement = startElement,
                                endElement = endElement,
                                text = textHandler),
                asText = FALSE, useTagName = FALSE)
  # final flush buffers
  if (length(persons_buf) > 0) {
    dtp <- rbindlist(persons_buf, fill = TRUE)
    fname <- file.path(out_dir, sprintf("persons-part-%06d.parquet", person_part))
    arrow::write_parquet(dtp, sink = fname)
    person_part <- person_part + 1L
    persons_buf <- list()
  }
  if (write_households && length(houses_buf) > 0) {
    dth <- rbindlist(houses_buf, fill = TRUE)
    fnameh <- file.path(out_dir, sprintf("households-part-%06d.parquet", house_part))
    arrow::write_parquet(dth, sink = fnameh)
    house_part <- house_part + 1L
    houses_buf <- list()
  }

  message("SAX parse completed. Total households: ", total_houses, ", total persons: ", total_persons,
          ", household parts: ", house_part, ", person parts: ", person_part)
  invisible(list(households = total_houses, persons = total_persons, house_parts = house_part, person_parts = person_part))
}