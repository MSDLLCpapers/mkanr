% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_png_to_rtf.R
\name{png_to_rtf}
\alias{png_to_rtf}
\title{Create RTF plot from .png file}
\usage{
png_to_rtf(
  outgraph_path,
  rename_output,
  rtf_title = c("Title 1", "(Population)"),
  rtf_footnote = c("{^a} Footnote 1", "{^b} Footnote 2"),
  fig_height = 4,
  fig_width = 6,
  orientation = "portrait"
)
}
\arguments{
\item{outgraph_path}{Directory for the .png output (Default path$outgraph)}

\item{rename_output}{Name of output files}

\item{rtf_title}{Figure title(s) for rtf output (Default c("Title 1","(Population)"))}

\item{rtf_footnote}{Figure footnote(s) for rtf output (Default c("{^a} Footnote 1","{^b} Footnote 2"))}

\item{fig_height}{Height (inch) of the figure in rtf output (default 4)}

\item{fig_width}{Width (inch) of the figure in rtf output (default 6)}

\item{orientation}{Orientation in 'portrait' or 'landscape' (Default "portrait")}
}
\value{
A text containing the path to the created RTF plot.
}
\description{
Creates an RTF plot with title and footer from a .png output.
}
\section{Specification}{

\if{latex}{ This function creates an RTF plot with title and footer from a .png output.

Logical Specifications:

  \enumerate{
  \item Check input parameters.
  \item Assemble the title, footnote and png format plot into an RTF output.
  }
}
\if{html}{
The contents of this section are shown in PDF user manual only.
}
}

