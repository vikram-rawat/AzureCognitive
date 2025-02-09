% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/speech2text_batch_api.R
\name{get_transcription_status}
\alias{get_transcription_status}
\title{Get the status of transcription process with this function}
\usage{
get_transcription_status(az_cognitive_service_speechservices_obj,
  create_transcription_data)
}
\arguments{
\item{az_cognitive_service_speechservices_obj}{an speech service object of class az_cognitive_service}

\item{create_trans_data}{the data you get from create_transcription function}
}
\value{
It returns the responce from get transcription
}
\description{
Get the status of transcription process with this function
}
\details{
This function tries to get the status of the transcription id
}
