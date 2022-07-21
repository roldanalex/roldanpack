#' Download S3 parquet data
#'
#' This function will download parquet data from S3 bucket and it will convert it to tibble for further analytics.
#'
#' @import aws.s3
#' @import dplyr
#' @import tidyr
#' @import arrow
#' @import lubridate
#' @importFrom utils head
#' @param bucket_name name of the s3 bucket.
#' @param prefix_name prefix used on the s3 bucket.
#' @param aws_access_key_id AWS S3 bucket secret key id.
#' @param aws_secret_access_key AWS S3 bucket secret access key.
#' @param partitions number of parquet partitions within s3 bucket.
#' @export

download_s3_parquet_data <- function(bucket_name, prefix_name, aws_access_key_id,
                                     aws_secret_access_key, partitions){


  s3_bucket_table <- aws.s3::get_bucket(
    bucket = bucket_name,
    prefix = prefix_name,
    "AWS_ACCESS_KEY_ID" = aws_access_key_id,
    "AWS_SECRET_ACCESS_KEY" = aws_secret_access_key
    ) %>%
    tidyr::as_tibble()

  last_parquet_file <- s3_bucket_table %>%
    mutate(LastModified = lubridate::as_datetime(LastModified)) %>%
    arrange(desc(LastModified)) %>% utils::head(partitions) %>%
    pull(Key)

  if (partitions == 1) {

    Raw.data <- aws.s3::s3read_using(
      FUN = arrow::read_parquet,
      object = last_tw_event_file,
      bucket = bucket_name)

  } else {

    i <- 1
    Raw.data <- NULL

    while (i < (length(last_parquet_file) + 1)) {

      Raw.data <- aws.s3::s3read_using(
        FUN = arrow::read_parquet,
        object = last_parquet_file[i],
        bucket = bucket_name) %>%
        rbind(Raw.data)

      i <- i + 1

    }
  }

  return(Raw.data)

}
