! Program no.: F-95
! Fortran 95 program to calculate Canadian forest
! Fire weather index for a dec pdp 11 at P.F.E.S.
! Reads data and prints out in metric units.
program ffwi
   use FFWIndecies
   implicit none

   ! Declaring variables
   integer, dimension(12) :: lenMonth
   real, dimension(12) :: daylenDmc,daylenDc
   integer :: readHumid,readWind,startDay,startMonth,maxDaysInMonth,row,column,wind,humid,counter,numDays
   integer :: outputDc,outputFfmc,outputDmc,outputSpread,outputBuildup,outputFwi,endReason
   real :: startFfmc,startDmc,startDc,readTemp,readRain,temp,rain,finalMoist
   real :: droughtCode,todayFfmc,duffMoist,initSpreadCalc,buildIndex,fireWeatherIndex

   ! Prompting user to enter an input file name until they enter a file that exists
   call getUserInputFileFunc()

   ! Prompting user to enter an output file name, will create a new file if it doesn't exist, will overwrite if it does exist
   call getUserOutputFileFunc()

   ! Outputting the program name and legend
   call printLegendFunc()

   ! Looping through the first 13 rows of the file
   call readFirstThirteenRows(lenMonth,daylenDmc,daylenDc,row,startMonth,numDays,startFfmc,startDmc,startDc)

   ! Looping through the months, starting at the start month that was read from the file
   do row=startMonth,12
      maxDaysInMonth=lenMonth(row)
      if(row.eq.startMonth) then
         startDay=lenMonth(row)-numDays+1
      else
         startDay=1
      endif
      
      ! Reading daily weather data
      ! Start looping from the selected day of the month (either day read from file, or the 1st), until the last day of the month
      counter=0
      do column=startDay,maxDaysInMonth
         counter=counter+1

         ! Reading the row from the file after the 13th row until the file is fully read and checking for EOF
         call readAnyOtherRow(readTemp,readHumid,readWind,readRain,endReason)
         call checkEndOfFile(endReason)

         ! If a new month is started, then print the column headers
         call writeTableHeader(counter)

         ! Saving some variables in case they get modified with the calculations
         temp=readTemp
         humid=readHumid
         wind=readWind
         rain=readRain

         ! Calculating the fine fuel moisture code
         todayFfmc=fineFuelMoistureCodeFunc(readTemp,readRain,startFfmc,humid,wind,finalMoist)
      
         ! Calculating the duff moisture code
         duffMoist=duffMoistureCodeFunc(readTemp,readRain,row,daylenDmc,startDmc,humid)

         ! Calculating the drought code
         droughtCode=droughtCodeFunc(readTemp,readRain,row,daylenDc,startDc)

         ! Calculating the initial spread index
         initSpreadCalc=initialSpreadIndexFunc(todayFfmc,wind)

         ! Calculating the buildup index
         buildIndex=buildupIndexFunc(duffMoist,droughtCode)

         ! Calculating the fire weather index
         fireWeatherIndex=fireWeatherIndexFunc(buildIndex,initSpreadCalc)

         ! Preparing some variables for outputting to a new row in the output file
         outputFfmc=int(todayFfmc+0.5)
         outputDmc=int(duffMoist+0.5)
         outputDc=int(droughtCode+0.5)
         outputSpread=int(initSpreadCalc+0.5)
         outputBuildup=int(buildIndex+0.5)
         outputFwi=int(fireWeatherIndex+0.5)

         ! Writing the calculated data to file
         call printOutput(row,column,temp,humid,wind,rain,outputFfmc,outputDmc,outputDc,outputSpread,outputBuildup,outputFwi)

         ! Resetting a few variables for the next itteration
         startFfmc=todayFfmc
         startDmc=duffMoist
         startDc=droughtCode

      end do
   end do
end program ffwi
