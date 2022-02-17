# Paired, georeferenced fish and benthic surveys
Contains code and documentation to process and validate data collected using the GPS-enabled, paired fish and benthic surveys described in ______.  It is assumed that the reader has read this paper and supplement because some terminology is used that is not repeated here.  Raw data from the two example transects highlighted in the paper are included to allow the reader to see how the different scripts interact.

---

## Step 0. Ensure data are in the correct format
Although these scripts can be modified to fit specific goals or workflows, as they are written, they assume that data were collected according to the companion paper and that certain pre-processing steps were taken.

1. Photographs should be organized as follows: "root" folder --> "camera" folder --> "date" folder --> "foray" folder.
  - "root" folder contains all the "camera" folders.  In this example, our "root" folder is "Organized".

![Example of our root folder, called "Organized", which contains 4 folders for our 4 cameras used](images/root_folder.png)
  - "camera" folders each contain all the sub-folders collected by each individual camera (we used 4 cameras in our field season, so we have 4 "camera" folders)

![Example of a camera folder which contains folders named with observer and date](images/camera_folder.png)
  - "date" folders are one step below "camera" folders and contain all the "forays" completed on each day by each observer on that camera

![Example of a date folder which contains folders named based on foray and an "extra" category for photos not included in the timelapse](images/date_folder.png)
  - "foray" folders contain all photographs collected in each trip from the boat (i.e., off a single timelapse).  Multiple transects can be included in one "foray", but they need to be grouped together to ensure the georeferencing works properly.  These should begin with a clear photograph of the watch face.  Photographs before this first clear image can be placed in a separate folder inside the "date" folder (we called ours "Extra").

![Example of a foray folder with the first few photographs shown](images/foray_folder.png).

2. The time from the watch in the first photograph of each foray should be saved in a .csv (in our case, watchData.csv).  The "Folder" column should refer to the folder directly containing all the images for that foray, and the "Track" column should contain the entire GPS track name, including the .gpx at the end.

3. All GPS tracks should be in a single folder.
4. You should have a csv with metadata about each site

---

## Step 1. Associating photographs with time and GPS coordinates
These are the steps required to take these images, associate them with a time based on the time visible on the watch face at the beginning of each foray, then associate these times with coordinates from the GPS tracks.

1. Photographs need to be renamed when large numbers are used.  Assuming a GoPro was used with the default naming scheme, there is bound to be some duplicate names of photographs across the dataset.  To do this, you can use the batchRenamer.R script to automatically assign each photograph a unique name.  It does so by adding the name in the "Folder" field of your "watchData.csv" to the front of the image name. **NOTE: This will permanently change the names of your images.  It is highly recommended that the photographs are backed up at some point prior to this step if they have not been already.**  
2. Once photographs are named something unique, the next step is to actually generate the time and coordinate metadata for each photograph.  To do this, open the timeLapseTimer.R script and change the fields at the top (put in the filepath for your "watchData.csv" and change your root path to the entire path leading to your "root" folder from Step 0).  This may take some time to run, but the end product is a dataframe with each row representing an individual photograph, and the time and coordinates from the GPS tracks associated with them.

---

## Step 2. Pairing photographs with fish surveys
In this step, we take information regarding the time photographs were taken from the newly-generated image_metadata.csv and use it to pair the photographs with their respective fish survey minute.  From there, 64 photographs are randomly selected (out of the possible 90) to be automatically-annotated by CoralNet.  The random selection can be ommitted if you desire to have all photographs associated with fish surveys.

For this step, use the "photo_extractor.R" script.  This script first generates a dataframe that contains a unique identifier for each survey transect segment (i.e., minute of fish counting).  

## Step 3. Copying sampled photographs to another folder for easy uploading
In the previous step, photographs were associated with fish transect segments based on the time each photograph was taken and the time that fish surveys were conducted via the creation of metadata csv files.  However, the photographs themselves have not been altered since they were renamed in Step 1.  This step uses a .bat file to locate the photographs and copy them (not move...the originals are preserved) to a different folder to enable easy uploading.

This is currently only setup to run on a Windows machine -- users on Mac or Linux will have to find their own solution to copy the files.  Provided is the "fileMover.bat" file, which takes a .txt file of the photographs (minus the file extension) and recursively searches through a folder to find matches in the filename of each row of the .txt file and copies these to another folder.  This needs three fields at the top to be modified (**NOTE: double-clicking will run the file.  To edit these paths, right-click and select "Edit"**):
- set FIILELIST= should be the pathway to a .txt file containing just a list of photograph names without the (this the "extracted_images" file produced by the "photo_extractor.R" script) -- ex., set FIILELIST=D:\example_folder\extracted_images.txt
- set FILESPATH= should be the pathway to your "root" folder that contains all the photographs nested within the subfolders.  The program will recursively search through all the subfolders to locate the files with the correct name -- ex., set FILESPATH=D:\Moorea\GoPros\Organized
- set DESTPATH= should be the pathway to the folder you want to copy the photographs to -- ex., set FILESPATH=D:\images_to_upload

## Step 4a. Training of an automatic classifier on CoralNet
**NOTE: If you already have a trained classifier, move to Step 4b.**

Now that the photographs to be annotated are in a separate folder, they can easily be selected for annotation.  If you are starting completely fresh and do not have a trained automatic classifier on a CoralNet source, then you must train one.  To do this, you need to create a CoralNet source, upload images to the web platform of CoralNet (or provide it with annotations from another program), and perform manual annotations until you are satisfied with the progress or it stops retraining.  These steps will not be covered in detail here, but users are encouraged to check out the CoralNet about page and the references contained within: https://coralnet.ucsd.edu/about/.  When we trained our source initially, we uploaded 2 photographs per minute of fish surveys (~8000 photographs), yet our classifier's performance plateaued after ~3450 photographs.

## Step 4b. Automatic-annotation of benthic photographs
Once the classifier is trained, you are now set to have your photographs automatically-annotated! To do this, you may either use the web version of CoralNet similar to how you did with manual annotation, only you would export the data without confirming any points by-hand (i.e., just take the first machine suggestion).  This will work for small datasets but is not recommended for larger number of images.  In these cases, it is highly-recommended to use the CoralNet API functionality.  More details and links to official documentation and code specific to CoralNet can be found in this blog post: https://coralnet.ucsd.edu/blog/automatically-annotating-175000-images-with-the-coralnet-api/

---

# Validation of the CoralNet automatic annotations
This section will cover the steps needed to run our validation procedure to determine the ability of CoralNet's automatic annotations to estimate percent cover of key substrates at the scale of a transect segment.  The purpose of this is to determine how well the CoralNet algorithm estimates percent cover of substrates compared to a unique reference set.  This reference set is created using 12 manually-annoated photographs to estimate the percent cover at the scale of a transect segment (roughly 10m of linear reef area).  The percent cover from different numbers of unique manually-annotated and CoralNet-annotated photographs are then calculated, creating different sample sets.  For each transect segment, a regression is calculated using the percent cover from each sample set as the predictor and the percent cover from the reference set as the response.  The process is repeated thousands of times with different photographs used for each sample set to estimate the variability around the selection of different numbers of manual and CoralNet-annotated photographs.  The end result is the ability to determine how well the different numbers of photographs annotated using different methods estimate the actual percent cover of substrates in a given unit of interest.

It is recommended to run this and make sure you are comfortable with the results prior to beginning full-scale automatic annotation of your photographic dataset.  This requires some additional investment in terms of manual annotation, but does provide useful metrics that show how well the algorithm is performing at the scale of a transect segment for each substrate rather than how accurate it is at the level of a point.

## Step 1. Select photographs to use
For our validation procedure, all photographs from random transect segments (we used 50 transect segments) need to be annotated; 20 are done manually and the other 70 are automatically annotated by CoralNet.  These photograph associated with individual transect segments can be identified from the metadata generated by _____.  When selecting the 20 photographs to be manually annotated, we used images evenly spaced throughout the transect to ensure proper spatial coverage of the reference images.  Once these are calculated, the dataframes need to be converted to "wide" format, with columns for the percent covers of the different substrates and additional columns for an identifier for the photographs ("Name") and one called "SurveyCode" that is an identifier for the transect segment.  Each row represents a unique photograph.

## Step 2. Performing the validation
Performing the validation procedure involves running a few custom-built functions from the "validation_functions.R" script in sequence to:
1. Generate the reference set ("generate_reference()").
2. Generate the sample set ("sample_human()" for the manually-annotated photographs and "sample_robot()" for the CoralNet-annotated photographs).  These are different because the "sample_human()" function has an additional argument to exclude photographs used to generate the reference set.
3. Calculate the regressions between the reference and sample sets and save the outputs ("generate_relationships()").
4. When these are completed, the function "plot_octave()" can be used to visualize the results.

Included in this repository is an R script, "sample_validation.R" and accompanying data that shows how these can be used to replicate the results from the accompanying paper and can be used as a starting point for your own validations.  The behavior of the functions is described in detail below:
- "generate_reference()" takes the manually-annotated dataframe as its first argument ("dat"), the number of images to be used for the reference set ("num", which we set as 12), and a vector of the names of the substrate columns you wish to have analyzed ("cover").  It randomly selects "num" number of photographs from each transect segment, calculates the percent cover of each substrate in each of these transect segments, and outputs a list of length 2 containing the results (first element) and a dataframe identifying which photographs were used to create this reference set so they can be removed from the image pool in future steps.
- "sample_human()" takes the same manually-annotated dataframe as it's first argument, "dat".  The second argument is "files_to_exclude", which is just the second element of the list returned from "generate_reference()".  The third argument is "sample_n" which signifies how many photographs should be sampled from each transect segment to calculate the percent cover of each substrate.  The fourth argument is "reps" which identifies how many replicates should be done, each time randomly sampling from every transect segment.  The final argument is "cover" which should be the same vector used in "generate_reference()".  This function outputs a dataframe 
- "sample_robot()" operates similarly to "sample_human()" except it does not have the "files_to_exclude" argument and instead of using the dataframe from manually-annotated photographs for the first argument, you should use the dataframe from the CoralNet-annotated photographs as the first argument, "dat".  Thus, all photographs that were annotated by CoralNet are available to use.
- "generate_relationships()" takes the outputs from "generate_reference()" as the first argument and the output from either "sample_human()" or "sample_robot()" as the second argument.  The third argument is "cover" which is the same as in the previous functions.  This calculates a regression using the sample as the predictor and the reference as the response for each replicate in the sample data, saving the model coefficients and r^2 values.  
-"plot_octave()" takes the data generated from "generate_relationships()" using manually-annotated photographs (first argument) and CoralNet-annotated photographs (second argument), the substrates desired to be plotted (third argument - can either be a singular character value or a vector of characters), and the metrics that you wish to plot (can accept "R_square", "Slope", and "Intercept" or a vector with a combination of these), and it makes the octave plots comparing the two methods for annotating photographs across the number of photographs you established in earlier steps.

