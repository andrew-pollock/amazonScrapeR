#' A Function for Amazon Review Scraping
#'
#' This function allows you to express your love of cats.
#' @param product_id What is the unique, 6 character product ID?
#' @param country Should the function run for UK, US or Both? Defaults to UK.
#' @param delay How long in seconds should the function pause between pages? Defaults to 5.
#' @keywords amazon
#' @export
#' @examples
#' amazonScrape()



amazonScrape <- function(product_id = NULL, country = "UK", delay = 5){

  # Create an empty list
  datalist <- list()

  # Check if it should run on the UK site
  if(country %in% c("UK", "Both")){

      # Get the UK URL
      uk_url <- paste0("https://www.amazon.co.uk/product-reviews/", product_id)

      # How many pages of reviews are there
      num_of_pages_uk <- uk_url %>%
        read_html() %>%
        html_nodes("#cm_cr-product_info > div > div.a-text-left.a-fixed-left-grid-col.reviewNumericalSummary.celwidget.a-col-left > div.a-row.a-spacing-medium.averageStarRatingNumerical > span") %>%
        html_text() %>% as.character() %>% gsub(" .*$", "", .) %>%
        as.integer() %>% `/`(10) %>% ceiling()

      # Loop through each page
      for (page_number in 1:num_of_pages_uk) {

        loop_url <- paste0("https://www.amazon.co.uk/product-reviews/", product_id, "/?pageNumber=", page_number)

        Sys.sleep(runif(1, max(delay-2, 0.5), delay))

        df <- read_html(loop_url) %>%
          html_nodes('.view-point .a-col-left') %>%
          map_df(~list(Reviewer = html_nodes(.x, '#cm_cr-review_list .a-profile-name') %>%
               html_text(),
            Date = html_nodes(.x, '#cm_cr-review_list .review-date') %>%
              html_text(),
            Title = html_nodes(.x, '.a-text-bold span') %>%
              html_text(),
            Review_Text = html_nodes(.x, '.review-text-content span') %>%
              html_text(),
            Review_Score = html_nodes(.x, '#cm_cr-review_list .review-rating') %>%
              html_text() %>% substr(1, 3) %>% as.numeric()
          )
          )

        df$Country <- "UK"
        df$Date <- dmy(df$Date)
        datalist[[page_number]] <- df

      }

  }


  # Check if it should run on the US site
  if(country %in% c("US", "Both")){

    # Get the US URL
    us_url <- paste0("https://www.amazon.co.uk/product-reviews/", product_id)

    # How many pages of US reviews are there
    num_of_pages_us <- us_url %>%
      read_html() %>%
      html_nodes("#cm_cr-product_info > div > div.a-text-left.a-fixed-left-grid-col.reviewNumericalSummary.celwidget.a-col-left > div.a-row.a-spacing-medium.averageStarRatingNumerical > span") %>%
      html_text() %>% as.character() %>% gsub(" .*$", "", .) %>%
      as.integer() %>% `/`(10) %>% ceiling()

    # Loop through each US page
    for (page_number in 1:num_of_pages_us) {

      loop_url <- paste0("https://www.amazon.com/product-reviews/", product_id, "/?pageNumber=", page_number)

      Sys.sleep(runif(1, max(delay-2, 0.5), delay))

      df <- read_html(loop_url) %>%
        html_nodes('.view-point .a-col-left') %>%
        map_df(~list(Reviewer = html_nodes(.x, '#cm_cr-review_list .a-profile-name') %>%
                       html_text(),
                     Date = html_nodes(.x, '#cm_cr-review_list .review-date') %>%
                       html_text(),
                     Title = html_nodes(.x, '.a-text-bold span') %>%
                       html_text(),
                     Review_Text = html_nodes(.x, '.review-text-content span') %>%
                       html_text(),
                     Review_Score = html_nodes(.x, '#cm_cr-review_list .review-rating') %>%
                       html_text() %>% substr(1, 3) %>% as.numeric()
        )
        )

      df$Country <- "US"
      df$Date <- dmy(df$Date)
      datalist[[page_number]] <- df

    }

  }

  # Bind the lists into a dataframe
  combined_review_data <- bind_rows(datalist)
}
