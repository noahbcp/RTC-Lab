# RTC-Lab
Scripts in use at the [RTC Lab](https://www.rtclab.org/) for the processing of data generated by some common cellular/molecular biology assays.

## TRUPATH Analysis
For the processing of [TRUPATH](https://www.nature.com/articles/s41589-020-0535-8) data exported by BMG Labtech's MARS software.

Requires experiments to be completed in a 96 well format and luminescence and fluorescence data to be 'exported for excel' on a single worksheet that has then been converted to a .csv format.

## BRET / CAMYEN Analysis
Scripts for the processing of general BRET experiment data exported by BMG Labtech's MARS software into a Graphpad Prism-friendly format.

CAMYEN.R assumes that data is in a format with technical triplicates.

## FLIPR Calcium
For the processing of 384 well Calcium data. 

Assumes the following plate layout:
![image](https://user-images.githubusercontent.com/98575657/170615833-00071a75-0cae-4fc7-8255-24d0318cbc0b.png)
