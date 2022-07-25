#' Get latest S3 data update
#'
#' This function will provide the latest date when S3 bucket has been updated.
#'
#' @import aws.s3
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @importFrom utils head
#' @param bucket_name name of the s3 bucket.
#' @param prefix_name prefix used on the s3 bucket.
#' @param aws_access_key_id AWS S3 bucket secret key id.
#' @param aws_secret_access_key AWS S3 bucket secret access key.
#' @param time_zone time zone to convert for date update (default Los Angeles Pacific Time).
#' @export

latest_s3_parquet_update <- function(bucket_name, prefix_name, aws_access_key_id,
                                     aws_secret_access_key, time_zone = "America/Los_Angeles"){


  s3_bucket_table <- aws.s3::get_bucket(
    bucket = bucket_name,
    prefix = prefix_name,
    "AWS_ACCESS_KEY_ID" = aws_access_key_id,
    "AWS_SECRET_ACCESS_KEY" = aws_secret_access_key
  ) %>%
    tidyr::as_tibble()

  last_parquet_file_update <- s3_bucket_table %>%
    mutate(LastModified = lubridate::as_datetime(LastModified)) %>%
    arrange(desc(LastModified)) %>% utils::head(1) %>%
    pull(LastModified) %>%
    with_tz(tzone = time_zone)

  return(last_parquet_file_update)

}
