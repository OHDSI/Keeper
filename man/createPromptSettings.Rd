% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PromptCraft.R
\name{createPromptSettings}
\alias{createPromptSettings}
\title{Create settings for generating prompts}
\usage{
createPromptSettings(
  writeNarrative = TRUE,
  testingReminder = TRUE,
  uncertaintyInstructions = TRUE,
  discussEvidence = TRUE,
  provideExamples = FALSE,
  maxParts = 100,
  maxDays = 5
)
}
\arguments{
\item{writeNarrative}{Ask the LLM to write a clinical narrative matching
the provided data?}

\item{testingReminder}{Remind the LLM that a diagnosis can be recorded
just to justify a test, and therefore by itself
is not sufficient evidence?}

\item{uncertaintyInstructions}{Provide instructions to the LLM on how to deal
with uncertainty?}

\item{discussEvidence}{Prompt the LLM to first discuss evidence in favor
and against the disease of interest?}

\item{provideExamples}{Provide examples? (few-shot prompting)}

\item{maxParts}{How many parts can a category have? For example,
if \code{maxParts = 100} and there are more than 100
measurements, a random sample of 100 will be
taken. Set to \code{0} if there is no maximum.}

\item{maxDays}{How many days can a single code have? For example,
if \code{maxDays = 5} and there is a measurement code
that appears on more than 5 days, a random sample
of 5 days will be taken. Set to \code{0} if there is no maximum.}
}
\value{
A settings object, to be used in \code{createSystemPrompt()} and \code{createPrompt()}.
}
\description{
Create settings for generating prompts
}
