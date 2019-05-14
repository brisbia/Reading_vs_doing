The .odc files are designed for use in WinBUGS.  Each "Model" file contains the model specifications, and has a corresponding "inits" file containing the initial values of the parameters.
The "Data" files contain the data corresponding to the Model with the same name.  DataAllChapters contains the data for ModelBase, ModelHighPrecision, and ModelLowPrecision.

The following list summarizes the analysis performed in each Model file:

ModelBase:  All chapters (5,7,9,10,11,12), precision = .1
ModelHighPrecision:  All chapters, precision = 1
ModelLowPrecision:  All chapters, precision = .01
ModelNoChapter5:  Chapters 7,9,10,11,12
ModelPracticeFirst:  Chapters in which practice problems were used first (5,7,11,12)
ModelWorkedFirst:  Chapters in which worked examples were used first (9,10)