# PEGSpol

Welcome to the PPOL+ user's manual.
The relevant material has been broken into sections
corresponding to each the PPOL+ tab.

TABLE OF CONTENTS
=================
2. Introduction to the tab system
1. The 'Load PPOL Project' Tab
  * The 'Open Project' button
  * The 'Rename Object' button
  * The 'Edit Image Usage Flags' button
3. The 'PPOL Assist' Tab
   * The 'Image Astrometry' tab
   * The 'Supersky Subtraction' tab
   * The 'Restricted Photometry' tab
3. The Post Processing Tab
   * The 'Final Images' tab
   * The 'Smooth and/or Rebin' tab

## Introduction to the tab system

The PPOL+ GUI design uses tabs to manage processing flow. This is in analogy
with the design of the PPOL GUI. The tabs along the left side of the GUI
correspond to the various stages in the processing and reduction pipeline.

The 'Load PPOL Project' tab allows you to start a new PPOL+ project, which
directly connects to an existing regular PPOL project. As you progress through
the parallel PPOL and PPOL+ processing, you will move down through the 'PPOL
assist' tab and finally conclude with the 'Post processing' tab.

Within each of the left-side tabs are a set of sub-tabs located along the top
of the GUI. These tabs are further explained below, but progress through
PPOL/PPOL+ processing will proceed from left to right.

When you first load PPOL+, you can click on the 'PPOL assist' tab and the
'Post processing tab,' but the contents are desensitized until an existing
PPOL/PPOL+ project has been loaded.

## The 'Load PPOL Project' Tab

This tab is where all processing will begin. After exiting out of PPOL+,
processing can be resumed where you left off via this tab.


### The 'Open Project' button
Clicking the 'Open Project' will pop-up a browsing dialog.
Using this dialog, browse to the location (directory) of the PPOL project
you wish to process. The first time a project is opened in PPOL+, the median
telescope pointing will be calculated and relevant 2MASS data for that object
will be downloaded. Finally, you should enter the name of your object in the
'Object Name' field.

NB: The nature of the problems addressed by PPOL+ requires that this PPOL
project only contain observations of ONE object.


### The 'Rename Object' button
If you accidentally misnamed your object, or you would like to enter a
different name, then clicking this button will allow  you to rename the object.


### The "Edit Image Usage Flags' button
TBD

## The 'PPOL Assist' Tab


### The 'Image Astrometry' tab
Some fields do not have enough bright stars to provide reliable automatic
astrometric registration. In this case, you can use the Image Astrometry Tool
to determine which images failed astrometry, and which stars will provide a
reliable estimate of the astrometry.

NB: Because there are at least three technically independent astrometric
parameters to be determined (central pointing, dRA/dxdy, and dDec/dxdy), you
should use the magnitude slider to select at least three stars to use for
astrometry. In the case of two star astrometry, pixels are assumed to be
perfectly square (this no geometric distortion assumption is probably accurate)
and the central pointing and rotation angle of the image are determined. One
star astrometry assumes square pixels and perfectly equatorially aligned images.
This should only be used as a VERY LAST RESORT!

Once a set of stars have been selected using the magnitude double slider,
clicking the 'Repair Astrometry' button will initalize the astrometry procedure.
The approximate telescope pointing is used to overplot estimated star positions
on the screen. By pinpointing the brightest star, you will help PPOL+ locate
the exact position of all other selected astrometry stars. Ocassionally a
gaussian cannot be reliably fit to dimmer stars. In this case, the user may be
called upon to help PPOL+ locate that star. This procedure can be saved and
exited at the end of the successful astrometric registration of any image.


### The 'Supersky Subtraction' tab
Once the images have been astrometrically registered (either automatically by
PPOL or manually using the PPOL+ 'Image Astrometry Tool'), a supersky image
can be generated. If there is a bright, extended source in the image (most
commonly a galaxy), then it will need to be masked. This can be done by clicking
the 'Mask Galaxy' checkbox. This will tell PPOL+ to import a mask when computing
the supersky image. With the 'Mask Galaxy' checkbox selected, click the 'Build
Galaxy Mask' button. This will open a mask building tool. Simply help PPOL+
identify the galaxy major and minor axes of the galaxy, and a mask will be
constructed based on those parameters.

Regardless of whether a mask has been constructed, the next step is to click the
'Supersky Subtraction' button. This will initalize the supersky routine, which
builds HWP specific supersky images using the six dither positions. These
supersky images are then subtracted from the constituent HWP images to produce
ultra-flattened science images. **This routine takes many hours to complete!**

Now that the images have been flattered, the SNR of stars in the image is
significantly improved. This allows another round of astrometry to be performed.
Simply click teh 'Refine Astrometry' button to automatically attempt astrometry
on all high quality 2MASS point-source-catalog entries in the field. Stars that
are reliably fit will be used to compute astrometry. This is generally a
significant improvement over the astrometry from the step before, as it is
computed from many stars (generally more than 10) rather than a few (generally
fewer than 10).

Once the astrometry has been refined, click 'Swap Files' to backup the original
PPOL step_3 files and transfer the new, ultra-flat images into the PPOL step_3
directory. Normal PPOL processing can then proceed using these ultra-flat
astrometrically registered images.


### The 'Restricted Photometry' tab
If you needed to use the 'Image Astrometry Tool' because there were few bright
stars, or if there is a galaxy in the image, then the automatic photometry
performed by PPOL will probably provide an erroneous photometric calibration
for your images. This can be addressed using the 'Restricted Photometry Tool.'

First, you must select the magnitude range of stars to be used in this initial
photometry. The magnitude double slider in this tab is force to allow a maximum
rannge of 2-magnitudes because this guarantees that the stars will sample
similar portions of the PSF, allowing for a single aperture to be selected
for image-to-image photometric calibration.

There are a few factors to consider when making this choice.

* Bright stars will return more reliably photometry
* Fewer stars allows for problems in the photometry of a single star
     to dominate the results.

My recomendation is generally to select the magnitude range that provides the
maximum number of stars. I trust an average quantity determined from many stars
even if each individual measurement was low quality --- this is the advantage
provided by the central limit theorem.

Once you have selected a magnitude range, click the 'Perform Photometry' button
to begin automatic photometry of the selected stars. The photometry tables will
be written to files matching the format of PPOL photometry files. **This routine
generally takes less than 1 hour to complete.**

(optional step)
If you would like to examine the shape of the average PSF as determined by the
'Perform Photometry' routine, click the "Check Photometry" button. This will
quickly loop through all the photometry tables and display the PSF shape on the
screen. If there is a file where more than one aperture returned a individual
stellar magnitude more than 3-sigma from the average value, then it the loop is
paused, and the user is asked to either 'accept' the photometry table, 'reject'
the photometry table (i.e. delete the entire file), or to 'edit' the photometry
table (i.e. delete a specific star from the table).

## The Post Processing Tab
Once you have returned to PPOL and proceeded through steps 5; 6; 7; and 9 or 11,
then you can use the tools in this tab to compute high quality polarization
maps.


### The 'Final Images' tab
This tab is straightfoward. Click the 'Start Combining' button to stack and
average all stokes images (intensity, Stokes U, and Stokes Q).

Once the stacking has been completed, click the 'Start Astrometry' button. This
will look for ALL high quality 2MASS entries in the image. Using the determined
positions of these stars, the astrometry of the final image will be computed
independently from the astrometry of the constitutent images. The determined
astrometry will also be applied to the Stokes U and Stokes Q images.

Finally, click the 'Start Photometry' button to compute maps of intensity
(micro Jy/arcsec^2) and surface brightness (mag/arcsec^2). The same 2-magnitude range of
stars used in the 'Restricted Photometry Tool' will be used to compute the final
photometry. If you did not select a magnitude range before, go back and do thi
now. First, the stellar curve-of-growth will be measured (see Stetson 1990).
Second, the aperture at which the selected photometry stars have a maximum SNR
will be determined and the instrumental magnitudes of the stars will be measured
at the determined aperture. Finally, the measured curve-of-growth will be used
to apply aperture corrections to the photometry star magnitudes.

The instrumental magnitudes from this process will be companed to the
corresponding 2MASS magnitudes. The differences between the instrumental
and 2MASS magnitudes will be used to determine a zero-point magnitude of the
image. This zero-point magnitude will then be used to generate the intensity
and surface brightness images.

Histograms of star position (relative to 2MASS star position entries) and
magnitude differences (relative to 2MASS magnitude entries) will be displayed.
The King-model of the curve-of-growth will also be displayed along with the
photometric data used to determine the curve-of-growth.


### The 'Smooth and/or Rebin' tab
Now that combined Stokes U and Stokes Q images have been computed, polarization
maps can be made from these. This tab provides several methods of computing
polarization maps. These are described in the following:

* The 'Integer Pixel Rebin' method allows the user to specify how many pixels should be used to rebin the Stokes U and Q images. The inverse variance weighted mean of each bin is computed, and the polarization percentage and position angle maps are computed from the rebinned stokes U and Q images.

* The 'Adaptive Mesh' routine allows the user to specify the smallest allowed number (Nmin) of Nmin x Nmin pixels to rebin. The user must also specify the number of rebinning levels to use (Nlevel). The maximum rebinning pixel size is then computed as Nmax = Nmin*2^(Nlevel). Finally, The minimum allowed signal-to-noise ratio (SNR) must also be specified. This final parameter is used to determine when a smaller or larger rebinning ought to be used. With these parameters provided, the procedure computes polarization maps and SNR maps for *ALL* rebinning levels. Then, beginning with the coarest rebinning (where the SNR is expected to be highest), the procedure examines whether the *NEXT FINEST* rebinning will return at least 3 sub-pixels above the specified minimum SNR. If it will, then the sub-pixels are further sub-divided until fewer than 3 sub-sub-pixels are above the minimum SNR. The finest rebinning level at which this test is passed is used as the binning level of that region of the polarization map.

* The 'Gaussian Smooth and Rebin' method can be described as a convolution of the inverse variance weighted Stokes U and Q images with a gaussian kernal. The convolved image is then re-sampled at an arbitrary pixel spacing (pitch), and polarization maps are computed from the convolved, resampled images. The FWHM of the convolving kernal and the resampled pitch must both be specified by the user.
