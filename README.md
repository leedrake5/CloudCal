# CloudCal

This app will allow you to build & apply calibrations for the Tracer & Artax series of XRF devices. It works with .csv files produced from S1PXRF and .csv files of net counts produced from Artax (7.4 or later). 

## How it works
It is based on the Lucas-Tooth and Price (1961) algorithm<sup>1</sup>, though I have added modifications to make it more robust (it's really hard to get a validation plot slope that isn't 1). The algorithm goes like this:

C<sub>i</sub> = r<sub>0</sub> + I<sub>i</sub>[r<sub>i</sub> + Σ(r<sub>in</sub>I<sub>n</sub>]

Where C<sub>i</sub> represents the concentration of element, r<sub>0</sub> is the intercept/empirical constant for element i, r<sub>i</sub> is the slope/empirical coefficient for intensity of element i, r<sub>in</sub> is the slope/empirical constant for effect of element n on element i, I <sub>i</sub> is the net intensity of element i, and I<sub>n</sub> is the net intensity of element n. This equation is based on a simple linear model, 

y = mx + b

In which y is C<sub>i</sub>, b is r<sub>0</sub>, m is r<sub>i</sub>, and I<sub>i</sub> is x (eg. C<sub>i</sub> = r<sub>i</sub>I<sub>i</sub> + r<sub>0</sub>). The additional variables present in the Lukas-Tooth equation indicate a slope correction for an element which influences the fluorescence of the element to be analyzed (subscript i represents the element being analyzed, subscript n represents the influencing element). 

The algorithm is simply a way to a) estimate concentrations from the x-ray spectrum, b) account for variation from the spectrum itself, and c) make results from instruments comparable to one another reliably<sup>2</sup>. That said, you do not have to use a Lucas-Tooth model to calibrate - in the app you can also choose linear/nonlinear models. 


## How to use the app

First, you will need to download a copy of R appropriate for your computer (Mac, Windows, or Linux): 

>https://www.r-project.org/

Next, you will need to install a package called 'shiny' to run it locally. You can do so by pasting this line into the R consul when you launch it:

>install.packages("shiny")


It will ask you to choose a download mirror (you can choose anyone, the result is the same). Then, to run the software:

>shiny::runGitHub("leedrake5/CloudCal")

The first time it runs, it may take some time to download the supporting software. After that, you should be good to go. If you'd like to download a copy and run it offline, you can instead download it from GitHub (https://github.com/leedrake5/CloudCal) and then run it locally:

>shiny::runApp("your/computer/directory/CloudCal")

## What format for the data?
Currently, the data operates with .csv files exported from either S1PXRF or Artax. It can't read binary files like .pdz yet, but I'm hoping to get there. 

To export from S1PXRF, go to Setup -> Group Conversion. Then choose a directory with PDZ files and select 'CSV' as the output. I'd also choose 'Replace duration with live time' to make sure dead time in the spectra is not a problem. This will produce the CSV files that you need to run in the app.

For Artax, follow all the same steps you know (if you don't know, visit www.xrf.guru). Instead of going to Export -> All Results to Excel, go to Export -> All Results. This will create CSV files that can be read into the app as well. 

Quick note - with the new Tracer 5i, Bruker has changed the format of PDZ files. That means data taken with the 5i is not compatible with the IISD/IVSD/IIV+, and vice-versa. Good news is that the most recent copy of Artax (8+) can handle both, but older copies won't. You'll need to convert from v25 (5i version) to v24 (Classic Tracer) to bring them in to S1PXRF - you can do this with CalToolkit or any other software Bruker provides. 

## Will this support other formats/XRF instruments?

No reason not to - I started with the Tracer because it was the easiest to work with. If you have other instruments you'd like to add, let me know and I can see if I can do it. Keep in mind that the software would need a heavy lift from multiple folks to read binary files - these are the proprietary company files that have non-open source file types.

## How to cite this software
Beats me - this is the first time I've tried to do anything like this. Drop me an email at b.lee.drake@gmail.com if you have questions or need to address this step. 


## References
Lucas-Tooth, H.J., Price, B.J. 1961. A Mathematical Method for the Investigation of Interelement Effects in X-Ray Fluorescence Analysis Metallurgia 64, 149–152.


Speakman, R.J., Shackley, M.S. 2013. Silo science and portable XRF in archaeology: a response to Frahm. Journal of Archaeological Science 40: 1435-1443


