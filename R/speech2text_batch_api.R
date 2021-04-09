# create_req_body_for_speech_transcription --------------------------------
#' 
#' Create a request body for speech transcription
#' 
#' @param audio_folder_SAS A URL with Shared access Signature for azure storage Where audio files are stored.
#' @param diarizationEnabled Boolean parameter for speech recognition.
#' @param punctuationMode defining punctuation options see speechtotext batch api for more options
#' @param profanityFilterMode defining profanityFilterMode options see speechtotext batch api for more options
#' @param locale defining language options see speechtotext batch api for more options
#' @param displayName displayName is a text that appears with json more like a comment
#' @param ... pass additional parameter either as a named vector or a named list
#' @details
#' This function creates a body for speech to text api for creating transcription.
#' @return
#' It returns a body that is needed to be sent in the request.
#'
#' @export
create_transcription_req_body <- function(
  audio_folder_SAS,
  diarizationEnabled = FALSE,
  wordLevelTimestampsEnabled = FALSE,
  punctuationMode = "DictatedAndAutomatic",
  profanityFilterMode = "Masked",
  locale = "en-US",
  displayName = "Transcription from azure directly within R programming",
  ...
){
  req_body <- list(
    "contentUrls" = I(audio_folder_SAS),
    "properties" = list(
      "diarizationEnabled" = diarizationEnabled,
      "wordLevelTimestampsEnabled" = wordLevelTimestampsEnabled,
      "punctuationMode" = punctuationMode,
      "profanityFilterMode" = profanityFilterMode
    ),
    "locale" = locale,
    "displayName" = displayName
  )
  
  if(length(...) >= 1){
    req_body <- append(req_body, ...)
  }
  
  req_body <- jsonlite::toJSON(
    req_body,
    auto_unbox = TRUE
  )
  
  return(
    req_body
  )
}

# get transcription id ----------------------------------------------------
#' Extracts transcription ID from payload sent by create transcription API
#' 
#' @param create_transcription_data response you recieved from create transcription API
#' @details
#' This function returns the transcription id and operation that has to be used in subsequent function
#' @return
#' It returns a transcription id and the url for next operation to be used to get status 
#'
get_trans_id <- function(
  create_transcription_data
){
  
  transcription_operation <- 
    stringi::stri_extract_first_regex(
      str = create_transcription_data$self,
      pattern = "transcriptions/(.*)")
  
  transcription_id <- transcription_operation %>% 
    stringi::stri_replace_all_regex(
      pattern = "transcriptions/",
      replacement = "")

  return(
    list(
      transcription_operation = transcription_operation,
      transcription_id = transcription_id
    )
  )
}

# create transcriptions ----------------------------------------------------
#' Create a transcription by sending audio files.
#' 
#' @param az_cognitive_service_speechservices_obj an speech service object of class az_cognitive_service
#' @param transcription_body you can even create your own request body by using the function create_transcription_req_body
#' @param audio_folder_SAS This is used for sending data 
#' @details
#' This function creates a transcription event in azure and returns you the transcription ID that should be used again.
#' @return
#' the transcription id and operation that is to be used in next step for get_transcription_status
#'
#' @export
create_transcription <- function(
  az_cognitive_service_speechservices_obj,
  transcription_body = NULL,
  audio_folder_SAS,
  diarizationEnabled = FALSE,
  wordLevelTimestampsEnabled = FALSE,
  punctuationMode = "DictatedAndAutomatic",
  profanityFilterMode = "Masked",
  locale = "de-AT",
  displayName = "German Transcription for Austria"
  ){
  
  speech <- az_cognitive_service_speechservices_obj
  
  if ( is.null(transcription_body) ) {
    transcription_body <- create_transcription_req_body(
      audio_folder_SAS = audio_folder_SAS,
      diarizationEnabled = FALSE,
      wordLevelTimestampsEnabled = FALSE,
      punctuationMode = "DictatedAndAutomatic",
      profanityFilterMode = "Masked",
      locale = "de-AT",
      displayName = "German Transcription for Austria"
    )
  }
  
  create_transcription_response <- call_cognitive_endpoint(
    endpoint = speech$get_endpoint(),
    operation = "transcriptions",
    body = transcription_body,
    http_verb = "POST",
    encode = "json"
  )
  
  transcription_data <- get_trans_id(
    create_transcription_data = create_transcription_response
  )
    
  return(transcription_data)
}

# get transcription status ------------------------------------------------
#' Create a transcription by sending audio files.
#' 
#' @param az_cognitive_service_speechservices_obj an speech service object of class az_cognitive_service
#' @param create_trans_data the data you get from create_transcription function
#' @details
#' This function tries to get the status of the transcription id
#' @return
#' It returns the responce from get transcription
#'
#' @export
get_transcription_status <- function(
  az_cognitive_service_speechservices_obj,
  create_transcription_data
){

  get_transcription_response <- call_cognitive_endpoint(
      endpoint = az_cognitive_service_speechservices_obj$get_endpoint(),
      operation = create_transcription_data$transcription_operation,
      http_verb = "GET",
      encode = "json"
    )

  return( get_transcription_response )

}

# get transcription files -------------------------------------------------
#' Create a transcription by sending audio files.
#' 
#' @param az_cognitive_service_speechservices_obj an speech service object of class az_cognitive_service
#' @param create_trans_data the data you get from create_transcription function
#' @details
#' This function asks for files after transcription event is done running job
#' @return
#' It returns the response you get from Transcription files API
#'
#' @export
get_transcription_files_response <- function(
  az_cognitive_service_speechservices_obj,
  create_transcription_data
){
  
  speech <- az_cognitive_service_speechservices_obj
  
  operation <- paste0(
    create_trans_data$transcription_operation,
    "/files"
  )
  
  file_url <- call_cognitive_endpoint(
    endpoint = speech$get_endpoint(),
    operation = operation,
    http_verb = "GET",
    encode = "json"
  )

  return(file_url)

}
# get_transcription_files_data --------------------------------------------
#' Create a transcription by sending audio files.
#' 
#' @param transcription_files_response a file url response you get from get_transcription_files_response function
#' @details
#' This function gives you the output of all the files
#' @return
#' It returns the transcribed files data
#'
#' @export
get_transcription_files_url <- function(
  transcription_files_response
){
  
  file_data <- lapply(file_url$values, function(x){
    x$links$contentUrl %>% 
      httr::GET() %>% 
      httr::content()
  })
  
  file_data <- lapply(
    X = file_data,
    FUN = function(main_file){
      main_file$combinedRecognizedPhrases %>% 
        lapply(
          FUN = function(combinedPhrase){
          combinedPhrase$display
        })
    })

  return(file_data)

}
