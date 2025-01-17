% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validateRDBESDataObjectContent.R
\name{validateRDBESDataObjectContent}
\alias{validateRDBESDataObjectContent}
\title{check RDBES Raw Object Content
 Private function to do some basic checks on
the content of the RDBESDataObject (e.g. all required field names are
present).  Function is only used by checkRDBESDataObject and should only
be passed a list of non-null objects}
\usage{
validateRDBESDataObjectContent(objectToCheck)
}
\arguments{
\item{objectToCheck}{- RDBESDataObject i.e. a list of data.tables}
}
\value{
list with  first element as the object and the second the warnings
}
\description{
check RDBES Raw Object Content
 Private function to do some basic checks on
the content of the RDBESDataObject (e.g. all required field names are
present).  Function is only used by checkRDBESDataObject and should only
be passed a list of non-null objects
}
