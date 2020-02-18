# predictNextWord

Predict next word based on n-Gram model

## Goal

Predict the next word the user is most likely to type in a sentense.

## Limitations

Only works if atleast 3 words are entered in a sentense. A full stop will terminate the sentense.

## Support model

The prediction is based on a model which predicts next word based on the results data of an n-gram model. The base data is stored as 1-Gram, 2-Gram, and 3-Gram probability models on the server. 

## Usage

Please refer to the instructions page for how to use this App, while running the shiny app.

## Files for generating model

Please note that the files given in the folder "Preparation" are for informational purposes and may not be executable without modifications based on system capabilities. More specifically the trainModel.R can't be run in its present status AS IS. Only portions of code can be run and are used for iterating over to find suitable Lambda values.  Function calcPerp6 was used for evaluating the perplexity of the model for a given set of Lambda values.