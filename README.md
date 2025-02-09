
# AzureCognitive <img src="man/figures/logo.png" align="right" width="150"/>

[![CRAN](https://www.r-pkg.org/badges/version/AzureCognitive)](https://cran.r-project.org/package=AzureCognitive)
![Downloads](https://cranlogs.r-pkg.org/badges/AzureCognitive)
![R-CMD-check](https://github.com/Azure/AzureVM/workflows/R-CMD-check/badge.svg)

A package to work with [Azure Cognitive
Services](https://azure.microsoft.com/en-us/services/cognitive-services/).
Both a Resource Manager interface and a client interface to the REST API
are provided.

The primary repo for this package is at
<https://github.com/Azure/AzureCognitive>; please submit issues and PRs
there. It is also mirrored at the Cloudyr org at
<https://github.com/cloudyr/AzureCognitive>. You can install the
development version of the package with
`devtools::install_github("Azure/AzureCognitive")`.

## Resource Manager interface

AzureCognitive extends the class framework provided by
[AzureRMR](https://github.com/Azure/AzureRMR) to support Cognitive
Services. You can create, retrieve, update, and delete cognitive service
resources by calling the corresponding methods for a resource group.

``` r
az <- AzureRMR::get_azure_login()
sub <- az$get_subscription("sub_id")
rg <- sub$get_resource_group("rgname")

# create a new Computer Vision service
rg$create_cognitive_service("myvisionservice",
    service_type="ComputerVision", service_tier="S1")

# retrieve it
cogsvc <- rg$get_cognitive_service("myvisionservice")

# list subscription keys
cogsvc$list_keys()
```

## Client interface

AzureCognitive implements basic functionality for communicating with a
cognitive service endpoint. While it can be called by the end-user, it
is meant to provide a foundation for other packages that will support
specific services, like Computer Vision, LUIS (language understanding),
etc.

``` r
# getting the endpoint from the resource object
endp <- cogsvc$get_endpoint()

# or standalone (must provide subscription key or other means of authentication)
endp <- cognitive_endpoint("https://myvisionservice.cognitiveservices.azure.com/",
    service_type="ComputerVision", key="key")

# analyze an image
img_link <- "https://news.microsoft.com/uploads/2014/09/billg1_print.jpg"
call_cognitive_endpoint(endp,
    operation="analyze",
    body=list(url=img_link),
    options=list(details="celebrities"),
    http_verb="POST")
```

    $categories
    $categories[[1]]
    $categories[[1]]$name
    [1] "people_"

    $categories[[1]]$score
    [1] 0.953125

    $categories[[1]]$detail
    $categories[[1]]$detail$celebrities
    $categories[[1]]$detail$celebrities[[1]]
    $categories[[1]]$detail$celebrities[[1]]$name
    [1] "Bill Gates"

    $categories[[1]]$detail$celebrities[[1]]$confidence
    [1] 0.9999552

## Speech to Text azure services

This package is forked from the original azure cognitive service package
by different authors. I just added speech to text batch API
functionality on top of it. That package is already on cran and on
version 1.0+. I don’t plan to release this package on cran as ever.
Because I am not at all familiar with azure and I created this only for
a single project. I don’t know much about azure or cognitive services.
But it’s good enough to use if you have the same needs as I have to call
a speech to text batch API on audio files and get the data transcribed.
Feel free to use it fork it and update it if you like. Below are a few
pointers I learned during the package. I know nothing more than this.

Azure speech 2 Services fall under `SpeechServices` Kind and you also
have to pass a list with a key `endpoint` and value `speechtotext/v3.0`.
This helps in creating a speech service in azure cloud.

Now there are only 3 things you need to do.

1.  create a transcription by sending URL of the audio files.

    1.  make sure you provide SAS access to the files

2.  It takes time to run a service you need to check multiple times if
    the process has been completed

3.  Ask for a file url

4.  get the file from URL provided

5.  you can also extract the name of file with a function.

In Short this whole process goes like this.

``` r
speech <- rg$
  create_cognitive_service(
    name = "speech2texttest123",
    service_type = "SpeechServices",
    service_tier = "S0",
    properties = list(endpoint = "speechtotext/v3.0")
  )

speech <- rg$get_cognitive_service(
  name = "speech2texttest123"
  )

create_trans_data <- create_transcription(
  az_cognitive_service_speechservices_obj = speech,
  audio_folder_SAS = "blob_sas_file", ### Blos url after SAS activation
  profanityFilterMode = "None",
  locale = "de-AT",
  displayName = "German Transcription for testing"
)

get_transcription_response <- get_transcription_status(
  az_cognitive_service_speechservices_obj = speech,
  create_transcription_data = create_trans_data
)

if( get_transcription_response$status == "Succeeded" ){

  transcription_files_response <- get_transcription_files_response(
    az_cognitive_service_speechservices_obj = speech ,
    create_transcription_data = create_trans_data
  )
  
  get_files_data <- get_transcription_files(
    transcription_files_response = transcription_files_response 
  )
  
  filenames <- extract_file_name(
    file_data = get_files_data,
    start_word = "yourcontainerid",
    end_word = "yourfiletype"
  )

}
```

------------------------------------------------------------------------

<p align="center">
<a href="https://github.com/Azure/AzureR"><img src="https://github.com/Azure/AzureR/raw/master/images/logo2.png" width=800 /></a>
</p>
