#' Grab a network of coauthors from Google Scholar profile
#'
#' @param scholar_id The endline of a Google Scholar profile. For example, 'citations?user=amYIKXQAAAAJ&hl=en' comes
#' from the profile https://scholar.google.com/citations?user=amYIKXQAAAAJ&hl=en
#' @param n_coauthors Number of coauthors to explore. This number should usually be between 1 and 10 as
#' choosing many coauthors can make the network graph too messy.
#' @param n_deep The number of degrees that you want to go down the network. When \code{n_deep} is equal to \code{1}
#' then \code{grab_network} will only grab the coauthors of Joe and Mary, so Joe -- > Mary --> All coauthors. This can get
#' out of control very quickly if \code{n_deep} is set to \code{2} or above. The preferred number is \code{1}, the default.
#'
#' @return A \code{tibble} with the all authors and coauthors.
#'
#' @importFrom dplyr %>%
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' library(coauthornetwork)
#'
#' final_network <- grab_network('citations?user=amYIKXQAAAAJ&hl=en')
#' plot_coauthors(final_network)
#'
#' }
#'
grab_network <- function(scholar_id, n_coauthors = 5, n_deep = 1) {

  stopifnot(is.numeric(n_deep), length(n_deep) >= 1, n_deep != 0)
  stopifnot(is.numeric(n_coauthors), length(n_coauthors) >= 1, n_coauthors != 0)
  all_coauthors <- get_coauthors(scholar_id, n_coauthors)

  empty_network <- purrr::rerun(n_deep, list())

  for (i in seq_len(n_deep)) {
    if (i == 1)  {
      empty_network[[i]] <- clean_network(all_coauthors$coauthors_href, n_coauthors)
    } else {
      empty_network[[i]] <- clean_network(empty_network[[i - 1]]$coauthors_href, n_coauthors)
    }
  }

  final_network <- rbind(all_coauthors, purrr::reduce(empty_network, rbind))
  final_network
}

# Recursively try to GET Google Scholar Page
get_resp <- function(url, attempts_left = 5) {

  stopifnot(attempts_left > 0)

  resp <- httr::GET(url)

  # On a successful GET, return the response
  if (httr::status_code(resp) == 200) {
    resp
  }

  # When attempts run out, stop with an error
  else if (attempts_left == 1) {
    stop("Cannot connect to Google Scholar. Is the URL you provided correct?")
  }

  # Otherwise, sleep a second and try again
  else {
    Sys.sleep(1)
    get_resp(url, attempts_left - 1)
  }
}

get_coauthors <- function(scholar_id, n_coauthors) {

  if (scholar_id == "") {
    return(
      dplyr::tibble(
        author = character(),
        href = character(),
        coauthors = character(),
        coauthors_href = character())
    )
  }

  resp <- get_resp(paste0("https://scholar.google.es/", scholar_id), 5)

  google_scholar <- httr::content(resp)

  author_name <- google_scholar %>%
    xml2::xml_find_all(xpath = "//div[@id = 'gsc_prf_in']") %>%
    xml2::xml_text()

  # Do no grab the text of the node yet because I need to grab the
  # href below.
  coauthors <- xml2::xml_find_all(google_scholar,
                                  xpath = "//a[@tabindex = '-1']")

  subset_coauthors <- if (n_coauthors > length(coauthors)) TRUE else seq_len(n_coauthors)

  coauthor_href <- xml2::xml_attr(coauthors[subset_coauthors], "href")

  coauthors <- xml2::xml_text(coauthors)[subset_coauthors]

  # If the person has no coauthors, return empty
  if (length(coauthor_href) == 0) {
    coauthors <- ""
    coauthor_href <- ""
  }

  dplyr::tibble(
    author = author_name,
    href = scholar_id,
    coauthors = coauthors,
    coauthors_href = coauthor_href
  )
}
