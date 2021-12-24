#' A Function for Scraping Amazon Product Reviews
#'
#' This function scrapes all Amazon product reviews from the UK and/or US sites for a specified product ID.
#' @param product_id What is the unique, 6 character product ID?
#' @param country Should the function run for UK, US or Both? Defaults to UK.
#' @param delay How long in seconds should the function pause between pages? Defaults to 5.
#' @importFrom dplyr "%>%"
#' @keywords amazon
#' @export
#' @examples
#' amazonScrape("B08SQNY4FS", country = "UK")



amazonScrape <- function(product_id = NULL, country = "UK", delay = 5){

  # Create an empty list
  datalist <- list()

  # Check if it should run on the UK site
  if(country %in% c("UK", "Both")){

      # Get the UK URL
      uk_url <- paste0("https://www.amazon.co.uk/product-reviews/", product_id)

      # How many pages of reviews are there
      num_of_pages_uk <- uk_url %>%
        xml2::read_html() %>%
        rvest::html_nodes("#cm_cr-product_info > div > div.a-text-left.a-fixed-left-grid-col.reviewNumericalSummary.celwidget.a-col-left > div.a-row.a-spacing-medium.averageStarRatingNumerical > span") %>%
        rvest::html_text() %>% as.character() %>% gsub(" .*$", "", .) %>%
        as.integer() %>% `/`(10) %>% ceiling()

      # Loop through each page
      for (page_number in 1:num_of_pages_uk) {

        loop_url <- paste0("https://www.amazon.co.uk/product-reviews/", product_id, "/?pageNumber=", page_number)

        Sys.sleep(runif(1, max(delay-2, 0.5), delay))

        df <- xml2::read_html(loop_url) %>%
          rvest::html_nodes('.view-point .a-col-left') %>%
          purrr::map_df(~list(Reviewer = rvest::html_nodes(.x, '#cm_cr-review_list .a-profile-name') %>%
                                rvest::html_text(),
            Date = rvest::html_nodes(.x, '#cm_cr-review_list .review-date') %>%
              rvest::html_text(),
            Title = rvest::html_nodes(.x, '.a-text-bold span') %>%
              rvest::html_text(),
            Review_Text = rvest::html_nodes(.x, '.review-text-content span') %>%
              rvest::html_text(),
            Review_Score = rvest::html_nodes(.x, '#cm_cr-review_list .review-rating') %>%
              rvest::html_text() %>% substr(1, 3) %>% as.numeric()
          )
          )

        df$Country <- "UK"
        df$Date <- lubridate::dmy(df$Date)
        datalist[[page_number]] <- df

      }

  }


  # Check if it should run on the US site
  if(country %in% c("US", "Both")){

    # Get the US URL
    us_url <- paste0("https://www.amazon.co.uk/product-reviews/", product_id)

    # How many pages of US reviews are there
    num_of_pages_us <- us_url %>%
      rvest::read_html() %>%
      rvest::html_nodes("#cm_cr-product_info > div > div.a-text-left.a-fixed-left-grid-col.reviewNumericalSummary.celwidget.a-col-left > div.a-row.a-spacing-medium.averageStarRatingNumerical > span") %>%
      rvest::html_text() %>% as.character() %>% gsub(" .*$", "", .) %>%
      as.integer() %>% `/`(10) %>% ceiling()

    # Loop through each US page
    for (page_number in 1:num_of_pages_us) {

      loop_url <- paste0("https://www.amazon.com/product-reviews/", product_id, "/?pageNumber=", page_number)

      Sys.sleep(runif(1, max(delay-2, 0.5), delay))

      df <- rvest::read_html(loop_url) %>%
        rvest::html_nodes('.view-point .a-col-left') %>%
        purrr::map_df(~list(Reviewer = rvest::html_nodes(.x, '#cm_cr-review_list .a-profile-name') %>%
                              rvest::html_text(),
                     Date = rvest::html_nodes(.x, '#cm_cr-review_list .review-date') %>%
                       rvest::html_text(),
                     Title = rvest::html_nodes(.x, '.a-text-bold span') %>%
                       rvest::html_text(),
                     Review_Text = rvest::html_nodes(.x, '.review-text-content span') %>%
                       rvest::html_text(),
                     Review_Score = rvest::html_nodes(.x, '#cm_cr-review_list .review-rating') %>%
                       rvest::html_text() %>% substr(1, 3) %>% as.numeric()
        )
        )

      df$Country <- "US"
      df$Date <- lubridate::mdy(df$Date)
      datalist[[page_number]] <- df

    }

  }

  # Bind the lists into a dataframe
  combined_review_data <- dplyr::bind_rows(datalist)
  return(combined_review_data)
}
