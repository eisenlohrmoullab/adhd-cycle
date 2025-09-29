#| label: setup-rmd-mirroring
# Helper: list all R chunk labels in an .Rmd (supports {r label,...} and {r, label="..."}).
show_chunk_labels <- function(rmd_path) {
  lines <- readLines(rmd_path, warn = FALSE)
  hdr_idx <- grep("^\\s*([`~]{3,})\\{[^}]*\\}\\s*$", lines, perl = TRUE)
  labs <- character(0)
  for (i in hdr_idx) {
    hdr <- lines[i]
    inside <- sub("^\\s*[`~]{3,}\\{(.*)\\}\\s*$", "\\1", hdr, perl = TRUE)
    inside_norm <- gsub("\\s+", " ", inside)
    if (!grepl("^\\s*r(\\s|,|\\})", inside_norm, perl = TRUE)) next
    # style A: {r mylabel, ...}
    mA <- regexec("^\\s*r\\s+([^,}]+)", inside_norm, perl = TRUE)
    gA <- regmatches(inside_norm, mA)[[1]]
    lab <- if (length(gA) >= 2) trimws(gA[2]) else NA_character_
    # style B: {r, label="mylabel"}
    if (is.na(lab) || lab == "" || grepl("^,", lab)) {
      mB <- regexec("label\\s*=\\s*(['\"]?)([^,'\"}]+)\\1", inside_norm, perl = TRUE)
      gB <- regmatches(inside_norm, mB)[[1]]
      if (length(gB) >= 3) lab <- trimws(gB[2]) else lab <- NA_character_
    }
    if (!is.na(lab) && nzchar(lab)) labs <- c(labs, lab)
  }
  unique(labs)
}

# Helper: extract a single labeled R chunk from an .Rmd into a temp child .Rmd
extract_chunk_as_child <- function(rmd_path, chunk_label) {
  lines <- readLines(rmd_path, warn = FALSE)
  hdr_idx <- grep("^\\s*([`~]{3,})\\{[^}]*\\}\\s*$", lines, perl = TRUE)
  if (!length(hdr_idx)) stop("No code-fence headers found.")
  esc <- function(x) gsub("([.^$|()*+?{}\\[\\]\\\\-])","\\\\\\1", x)

  match_i <- NA_integer_; fence <- NULL
  for (i in hdr_idx) {
    hdr    <- lines[i]
    fencei <- sub("^\\s*([`~]{3,})\\{.*", "\\1", hdr, perl = TRUE)
    inside <- sub("^\\s*[`~]{3,}\\{(.*)\\}\\s*$", "\\1", hdr, perl = TRUE)
    inside <- gsub("\\s+", " ", inside)
    if (!grepl("^\\s*r(\\s|,|\\})", inside, perl = TRUE)) next
    has_A <- grepl(sprintf("^\\s*r\\s+%s(\\s*,|\\s*\\})", esc(chunk_label)), inside, perl = TRUE)
    has_B <- grepl(sprintf("label\\s*=\\s*['\"]?%s['\"]?",            esc(chunk_label)), inside, perl = TRUE)
    if (has_A || has_B) { match_i <- i; fence <- fencei; break }
  }
  if (is.na(match_i)) stop(sprintf("Chunk '%s' not found in %s", chunk_label, rmd_path))

  close_pat <- paste0("^\\s*", fence, "\\s*$")
  after   <- lines[(match_i + 1):length(lines)]
  rel_end <- grep(close_pat, after, perl = TRUE)
  if (!length(rel_end)) stop("Closing fence not found.")
  j <- (match_i + 1) + rel_end[1] - 1

  out <- tempfile(fileext = ".Rmd")
  writeLines(lines[match_i:j], out)
  out
}
