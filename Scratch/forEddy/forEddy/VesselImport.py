##---------------------------------------------------------------------
## jef41_fullscript.py
##
## Usage: ArgosVesselIntersectTool (ARGOS_File, Vessel_File, Output_Feature_Class, {Output_Spatial_Reference}, {Location_Class_Filter}, {Buffer_Distance__m_}) 
##
## Created: December 2016
## Author: Joe Fader, joseph.fader@duke.edu (for ENV859)
##---------------------------------------------------------------------

# Description:
#  User locates ARGOS .csv file and fishing vessel .csv file and sets output location/file name for final product.
#  User also has option to set output spatial reference, LC filter for ARGOS, and the buffer distance
#  that will be created around satellite tag locations to identify intersections with fishing vessels.
#  The tool creates a feature class of the ARGOS tracking data that meets the requirements, applies a land mask,
#  and creates the specified buffer. It then creates a feature class from the vessel location data in the same
#  projection and identifies any tagged animal that spatially overlaps (using the buffer) with a fishing vessel.
#  Finally, the tool takes the spatially intersecting ARGOS and vessel points and outputs a new feature class
#  of points that also occur on the same day. These points provide the animal ID and vessel ID for simultaneous
#  occurrences of pilot whales and vessels. This information can then be used to inform additional analyses
#  regarding pilot whales known to spatially associate with longline fishing vessels.


#________setting relative paths, environments, modules, etc._________________
import sys, os, arcpy

# Get relative paths
scriptPath = sys.argv[0]                # The script filename is the first item in sys.argv
scriptWS = os.path.dirname(scriptPath)  # Get the script folder using the os.path.dirname function
rootWS = os.path.dirname(scriptWS)      # Up one folder from thereis the project root folder
dataWS = os.path.join(rootWS,"Data")    # Now the Data folder...
tempWS = os.path.join(rootWS,"Scratch") # ...and the scratch folder

# Set env variables
arcpy.env.workspace = dataWS
arcpy.env.scratchWorkspace = tempWS

# Allow outputs to be overwritten
arcpy.env.overwriteOutput = True

#_____________import data____________________________________

#Set input variables (User Input)
#ARGOSInputFile = sys.argv[1]
VesselInputFile = sys.argv[1]
inputSR = arcpy.SpatialReference('WGS 1984') # using same input SR for ARGOS and vessel
outputFC = sys.argv[2] # final output file of ARGOS+Vessel intersected in time and space
outputSR = sys.argv[3] # coded to use same output SR for both ARGOS and vessel
#LCFilter = sys.argv[5] # user selects LC to filter ARGOS data by, defaults set below
#buffDist = sys.argv[6] # user sets buffer distance for ARGOS points

# defaults to use if user doesn't put in input
# setting defaults to equidistant cylindrical, top 3 LCs, and buffer of 500m
if outputSR == "#":
    outputSR = arcpy.SpatialReference(54002)
#if LCFilter == "#":
#    LCFilter = "1;2;3"
#if buffDist == "#":
#    buffDist = "500 meters"

#arcpy.AddMessage(type(VesselInputFile[5]))
#arcpy.ConvertTimeField_management(VesselInputFile, "SET_BEGIN_DATETIME", "MM/dd/yyyy HH:mm:ss", "SET_BEGIN_DATETIME", "DATE","MM/dd/yyyy HH:mm:ss")

#________________Vessel data import/processing______________________
# Create an empty feature class for vessel location data; requires the path and name as separate parameters
VesselFC_nomask = "Vessel_nomasks.shp"
#VesselFC_nomask = sys.argv[2] + "_nomask.shp"
VesselFC_nomask_path = tempWS + "\\" + VesselFC_nomask
arcpy.CreateFeatureclass_management(tempWS, VesselFC_nomask, "POINT","","","", outputSR)

# Add vesselID, and Date fields to the vessel output feature class
arcpy.AddField_management(VesselFC_nomask_path,"UID","TEXT")
arcpy.AddField_management(VesselFC_nomask_path,"TRIP_ID","TEXT")
arcpy.AddField_management(VesselFC_nomask_path,"VESSEL_ID","TEXT")
arcpy.AddField_management(VesselFC_nomask_path,"TRIP_TYPE","TEXT")
arcpy.AddField_management(VesselFC_nomask_path,"SET_NUM","TEXT")
#arcpy.AddField_management(VesselFC_nomask_path,"SET_BEG_DT","DATE") #stored as string, currently cant figure how to get as date, using DATE only shows date with time being dropped
#arcpy.AddField_management(VesselFC_nomask_path,"SetTime","TEXT")
arcpy.AddField_management(VesselFC_nomask_path,"HaulDate","DATE")

# Create an input cursor for the vessel feature class so we can add feature records
cur = arcpy.InsertCursor(VesselFC_nomask_path, inputSR)

# ErrorCounter to track errors adding Vessel data lines
Vessel_errorCount = 0

# Open the Vessel input filename for reading, creating a file object
inputFileObj = open(VesselInputFile,'r')

# Construct a while loop to iterate through all lines in the datafile
# Get the first line of data, so we can use a while loop
lineString = inputFileObj.readline()
while lineString:
    if lineString[0].isdigit():
        # Parse the line into a list
        lineData = lineString.split(',')
        # Extract attributes from the csv lines
        UID =lineData[0]
        TRIP_ID = lineData[1]
        VESSEL_ID = lineData[3]
        SET_NUM = lineData[2]
        TRIP_TYPE = lineData[4]
        #SET_BEG_DT = lineData[7]

        #SetDate = lineData[6]
        #SetTime = lineData[7]
        #SetLat = lineData[8]
        #SetLon = lineData[9]
        HaulDate = lineData[14]
        #haulTime = lineData[10]
        HaulLat = lineData[16]
        HaulLon = lineData[17]
        

        try:
            obsLatDD = float(HaulLat)
            obsLonDD = float(HaulLon)
            # Construct a point object from the feature class
            obsPoint = arcpy.Point()
            obsPoint.X = obsLonDD
            obsPoint.Y = obsLatDD

            # Create a feature object to add to the feature class
            feat = cur.newRow()
                
            # Set the feature's shape and other attribute values
            feat.shape = obsPoint
            feat.setValue("UID",UID)
            feat.setValue("TRIP_ID",TRIP_ID)
            feat.setValue("VESSEL_ID",VESSEL_ID)
            feat.setValue("SET_NUM",SET_NUM)
            feat.setValue("TRIP_TYPE",TRIP_TYPE)
            feat.setValue("HaulDate", HaulDate)

            # Commit the feature to the feature class
            cur.insertRow(feat)
        except Exception as e:
            arcpy.AddWarning("Error adding set" + UID + " to the file")
            Vessel_errorCount = Vessel_errorCount + 1
                
    # Move to the next line so the while loop progresses
    lineString = inputFileObj.readline()

#Close the file object
inputFileObj.close()

# Report the error count
if Vessel_errorCount > 0:
    arcpy.AddError(str(Vessel_errorCount)+" Vessel tracking errors in file " +VesselInputFile)
del cur

# Mask(erase) any points on land
landmask = "NorthAmericaLand.shp"
#VesselFC_mask_path = tempWS + "\\Vessel_mask.shp"
VesselFC_mask_path = outputFC
arcpy.Erase_analysis(VesselFC_nomask_path, landmask, VesselFC_mask_path)

arcpy.AddMessage("Vessel data successfully added!")