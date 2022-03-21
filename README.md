# Binaural Audio in Hybrid Meetings
This repository contains files created and used for Loïc Rosset's Master Thesis: Advancing the User Experience of Hybrid Meetings with Binaural Audio (2021) and the extended work from the co-authors from the related papers.

The folders 1 to 3 contains all the materials used in the user study and consequent data analysis. The content of the folders are described here:

## Folder 1 : Dialogs
* Different files used for the recording of the dialogs. [/Experiment instructures/Dialogues X_xxx.pdf and procedure_enregistrements.docx]
* The transcription of the dialogs [/Experiment instructures/Dialogues_transcription.pdf]
* The timestampped events of the dialogs (including the sound directions) [event_timestamp].
* The timestampped events of the dialogs (including the sound directions of peudo sound when it was binaural-with-head-rotation format) [event_timestamp_new].
* The timestampped speech acts of the dialogs [speaker_events]

## Folder 2 : The videos 
(This is not uploaded due to privacy concerns. However, if you need it for research purpose, please contact sailin.zhong@unifr.ch to discuss on it.) 
* The videos created for the user study, in two format: with post-processed audio or mono audio
* The video used for the auditive test at the beginning of the user study

## Folder 3 : The surveys
* One of the surveys (as an example) used for the user study of the thesis. 

## Folder 4 : Analysis

* In the folder [statistical tests] containing the statistical analysis code (R code only, the SPSS part is not included) and summary of results.

* In the folder [eyetracking data]: example data (from dialog 1) recorded on realeye.io

* In the folder [eyetracking analysis]: all the files needed for the analysis of the eyetracking data. 
  - [Eye-tracking analysis - Extract fixations]: code to adjust raw fixation data (recorded on realeye.io) with respect to the coordination of video image size. 
  - [Eye-tracking - Analysis.ipynb]: code for the eye-tracking metrics calculation and (visual) analysis
  - [Individual differences analysis.ipynb]: code for preparing consolidated data (from tests, subjective ratings, and eye-tracking metrics) for statistical analysis (in R), preparing data for mHMM model (in R) and calculating the ratio of states (resulted from mHMM model in R).   
* In the folder [mHMM]: R code for the analysis of individual differences using multilevel HMM (mHMM). 
  - [mHMM.R]: mHMM for binaural conditions (x4)
  - [mHMM_mono.R]: mHMM for mono conditions (x2)


## Contact
Loïc Rosset: https://github.com/LoRosset
Sailin Zhong: sailin.zhong@unifr.ch 

## Cite this work
Rosset, L., Alavi, H., Zhong, S., & Lalanne, D. (2021, May). Already It Was Hard to Tell Who’s Speaking Over There, and Now Face Masks! Can Binaural Audio Help Remote Participation in Hybrid Meetings?. In Extended Abstracts of the 2021 CHI Conference on Human Factors in Computing Systems (pp. 1-7).

Zhong, S., Rosset, L., Lalanne, D., & Alavi, H., (to be appeared in CSCW 2022). Binaural Audio in Hybrid Meetings: Effects on Speaker Identification, Comprehension, and User Experience. 

