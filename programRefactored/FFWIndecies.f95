! Modules for ffwi.95
module FFWIndecies
   implicit none
   
contains
   
   ! Function that reads the first 13 rows of the input file
   subroutine readFirstThirteenRows(lenMonth,daylenDmc,daylenDc,row,startMonth,numDays,startFfmc,startDmc,startDc)
      implicit none
      integer, dimension(12), intent(inout) :: lenMonth
      real, dimension(12), intent(inout) :: daylenDmc,daylenDc
      integer, intent(inout) :: row,startMonth,numDays
      real, intent(inout) :: startFfmc,startDmc,startDc

      ! Looping through the first 12 rows of the input file, getting the lengths of each month, and the day length factors
      do row=1,12
         read(8,100) lenMonth(row),daylenDmc(row),daylenDc(row)
         100 format(I2,F4.1,F4.1)
      end do
      
      ! Reading the 13th row of input, which is the starting FFMC, DMC, DC, month, and number of days
      read(8,102) startFfmc,startDmc,startDc,startMonth,numDays
      102 format(F4.1,F4.1,F5.1,I2,I2)

      return
   end subroutine readFirstThirteenRows

   ! Function that reads rows after the 13th row
   subroutine readAnyOtherRow(readTemp,readHumid,readWind,readRain,endReason)
      implicit none
      integer, intent(inout) :: readHumid,readWind,endReason
      real, intent(inout) :: readTemp,readRain

      ! Reading the row from the file after the 13th row until the file is fully read
      read(8,101,iostat=endReason) readTemp,readHumid,readWind,readRain
      101 format(F4.1,I4,I4,F4.1)

      return
   end subroutine readAnyOtherRow

   ! Function that writes the table header to file
   subroutine writeTableHeader(counter)
      implicit none
      integer, intent(in) :: counter
      
      ! If a new month is started, then print the column headers
      if(counter.eq.1) then
         write(9,103)
         write(9,*) " ---------------------------------------------------------------"
         103 format(3(/),'  DATE  TEMP  RH   WIND   RAIN  FFMC   DMC   DC   ISI   BUI   FWI')
      end if

      return
   end subroutine writeTableHeader

   ! Function that writes the calculated data to file
   subroutine printOutput(row,column,temp,humid,wind,rain,outputFfmc,outputDmc,outputDc,outputSpread,outputBuildup,outputFwi)
      integer, intent(in) :: row,column,humid,wind,outputFfmc,outputDmc,outputDc,outputSpread,outputBuildup,outputFwi
      real, intent(in) :: temp,rain

      ! Writing the calculated data to file
      write(9,104) row,column,temp,humid,wind,rain,outputFfmc,outputDmc,outputDc,outputSpread,outputBuildup,outputFwi
      104 format(2I3,F6.1,I4,I6,F7.1,6I6)

      return
   end subroutine printOutput

   ! Function that checks for correct end of file
   subroutine checkEndOfFile(endReason)
      implicit none
      integer, intent(in) :: endReason

      ! Checking the iostat value
      if (endReason > 0)  then
         write(*,*) ""
         write(*,*) "Program terminated with an error"
         close(8,status='keep')
         close(9,status='keep')
         stop
      else if (endReason < 0) then
         write(*,*) ""
         write(*,*) "Program terminated successfully, read output file for results"
         close(8,status='keep')
         close(9,status='keep')
         stop
      end if

      return
   end subroutine checkEndOfFile

   ! Function that gets the user input file
   subroutine getUserInputFileFunc()
      implicit none
      character (len=32) :: fileNameIn
      logical :: fileExist

      ! Giving the user some information on correct input file format
      write(*,*) "┏-----------------------------------------------------------------------------------┓"
      write(*,*) "|  Welcome to a fortran 95 program to calculate Canadian forest Fire weather index  |"
      write(*,*) "|      for a dec pdp 11 at P.F.E.S. Reads data and prints out in metric units.      |"
      write(*,*) "┗-----------------------------------------------------------------------------------┛"
      write(*,*) ""
      write(*,*) "Regarding the input file..."
      write(*,*) ""
      write(*,*) "Lines 1-12 (line 1 is Jan, line 2 is Feb, etc) will be formatted such that"
      write(*,*) "Column 1 will be the number of days for the corresponding month"
      write(*,*) "Column 2 will be the DMC daylength factors"
      write(*,*) "Column 3 will be the DC daylength factors"
      write(*,*) ""
      write(*,*) "Line 13 will be formatted such that"
      write(*,*) "Column 1 is the starting FFMC"
      write(*,*) "Column 2 is the starting DMC"
      write(*,*) "Column 3 is the starting DC"
      write(*,*) "Column 4 is the starting month"
      write(*,*) "Column 5 is the number of days in the starting month"
      write(*,*) ""
      write(*,*) "Lines 14 and on will be formatted such that"
      write(*,*) "Column 1 is the day's temperature in degrees celcius"
      write(*,*) "Column 2 is the day's relative humidity as a percentage"
      write(*,*) "Column 3 is the day's wind speed in kilometers per hour"
      write(*,*) "Column 4 is the day's rainfall in millimetres"
      write(*,*) ""
      write(*,*) "Enter the input file name: "

      ! Prompting user to enter an input file name until they enter a file that exists
      fileExist = .false.
      do while(.not.fileExist)
         read(*,"(A)") fileNameIn
         inquire(file=fileNameIn, exist=fileExist)
         if (.not.fileExist) then
            write(*,*) "Requested input file doesn't exist - try entering again"
         else
            open(unit=8,file=fileNameIn,status="old",action="read")
         end if
      end do
      return
   end subroutine getUserInputFileFunc

   ! Function that gets the user output file
   subroutine getUserOutputFileFunc()
      implicit none
      character (len=32) :: fileNameOut
      logical :: fileExist

      ! Prompting user to enter an output file name, will create a new file if it doesn't exist, will overwrite if it does exist
      write(*,*) ""
      write(*,*) "Enter the output file name: "
      read(*,"(A)") fileNameOut
      inquire(file=fileNameOut, exist=fileExist)
      if (fileExist) then
         open(unit=9,file=fileNameOut,status="replace",action="write")
      else
         open(unit=9,file=fileNameOut,status="new",action="write")
      end if

      return
   end subroutine getUserOutputFileFunc
   
   ! Function that prints the legend
   subroutine printLegendFunc()
      implicit none

      ! Print the legend
      write(9, *) " Program NO.: F-95"
      write(9, *) " ----------------------------------------------------------------------------------"
      write(9, *) " Legend"
      write(9, *) " DATE:      The date to which the data corresponds to (month, then day)"
      write(9, *) " TEMP:      The temperature in celcius that was read from the input file"
      write(9, *) " RH:        The relative humidity as a percentage that was read from the input file"
      write(9, *) " WIND:      The wind speed in kilometres per hour that was read from the input file"
      write(9, *) " RAIN:      The rainfall in millimetres that was read from the input file"
      write(9, *) " FFMC:      The Fine Fuel Moisture Code that was calculated"
      write(9, *) " DMC:       The Duff Moisture Code that was calculated"
      write(9, *) " DC:        The Drought Code that was calculated"
      write(9, *) " ISI:       The Initial Spread Index that was calculated"
      write(9, *) " BUI:       The Buildup Index that was calculated"
      write(9, *) " FWI:       The Fire Weather Index that was calculated"
      write(9, *) " ----------------------------------------------------------------------------------"
      
      return
   end subroutine printLegendFunc

   ! Function that deals with the Fine Fuel Moisture Code
   real function fineFuelMoistureCodeFunc(readTemp,readRain,startFfmc,humid,wind,finalMoist)
      implicit none
      real, intent(in) :: startFfmc,readTemp
      integer, intent(in) :: wind,humid
      real, intent(inout) :: readRain,finalMoist
      real :: rainFunc,curRain,correctionTerm,FfmcAfterRain,emcDry,emcWet,startMoist
      real :: xIntermed,dryrateLog,todayFfmc

      ! Calculating the fine fuel moisture code
      if(readRain.le.0.5) then
         readRain=0.0
         FfmcAfterRain=startFfmc
      else
         curRain=readRain

         ! Applying various formulas based on the value of the current rain in mm
         if(curRain.le.1.45) then
            rainFunc=123.85-(55.6*alog(curRain+1.016))
         else
            if(curRain-5.75.le.0.) then
               rainFunc=57.87-(18.2*alog(curRain-1.016))
            else
               rainFunc=40.69-(8.25*alog(curRain-1.905))
            end if
         end if

         ! Calculating a correct term to be used for calculating the FFMC after rain
         correctionTerm=8.73*exp(-0.1117*startFfmc)
         FfmcAfterRain=(startFfmc/100.)*rainFunc+(1.0-correctionTerm)

         ! Ensuring that FFMC after rain must be 0 or greater
         if(FfmcAfterRain.lt.0.) then
            FfmcAfterRain=0.0
         end if
      end if

      ! Calculate the starting moisture and EMC dry factor using the himidity and temperature
      startMoist=101.-FfmcAfterRain
      emcDry=0.942*(humid**0.679)+(11.*exp((humid-100.)/10.))+0.18*(21.1-readTemp)*(1.-1./exp(0.115*humid))
      
      ! Checking the value of the starting moisture - the EMC dry factor, and performing various calculations
      ! to calculate the final moisture value based on if the result is less than 0, equal to 0, or greater than 0
      if(startMoist-emcDry.lt.0.) then
         emcWet=0.618*(humid**0.753)+(10.*exp((humid-100.)/10.))+0.18*(21.1-readTemp)*(1.-1./exp(0.115*humid))
         if(startMoist.lt.emcWet) then
            finalMoist=emcWet-(emcWet-startMoist)/1.9953
         end if
      else if(startMoist-emcDry.eq.0.) then
         finalMoist=startMoist
      else if(startMoist-emcDry.gt.0.) then

         ! Getting an intermediary value, then getting a dry rate to calculate the final moisture value
         xIntermed=0.424*(1.-(humid/100.)**1.7)+(0.0694*(wind**0.5))*(1.-(humid/100.)**8)
         dryrateLog=xIntermed*(0.463*(exp(0.0365*readTemp)))
         finalMoist=emcDry+(startMoist-emcDry)/10.**dryrateLog
      end if
      
      todayFfmc=101.-finalMoist

      ! Ensuring that FFMC can only be between 0 and 101
      if(todayFfmc.gt.101.) then
         todayFfmc=101.
      else
         if(todayFfmc.lt.0) then
            todayFfmc=0.0
         end if
      end if

      fineFuelMoistureCodeFunc = todayFfmc
   end function fineFuelMoistureCodeFunc

   ! Funciton that deals with the Duff Moisture Code
   real function duffMoistureCodeFunc(readTemp,readRain,row,daylenDmc,startDmc,humid)
      implicit none
      real, dimension(12), intent(in) :: daylenDmc
      real, intent(in) :: readRain,startDmc
      real, intent(inout) :: readTemp
      integer, intent(in) :: row,humid
      real :: duffMoist,afterDmc,curRain,effectiveRain,moistureRate,fwiIntermed,contentMoist,dryFactor

      ! If the temperature + 1.1 is less than 0, set it to -1.1
      if(readTemp+1.1.lt.0.) then
         readTemp=-1.1
      else
         ! Calculate a dry factor value
         dryFactor=1.894*(readTemp+1.1)*(100.-humid)*(daylenDmc(row)*0.0001)

         ! If the rain is less than 1.5mm, set the DMC to the start DMC, otherwise calculate the effective rain,
         ! Content moisture, and save them for later
         if(readRain.le.1.5) then
            afterDmc=startDmc
         else
            curRain=readRain
            effectiveRain=0.92*curRain-1.27
            contentMoist=20.0+280./exp(0.023*startDmc)

            ! Check if the starting DMC is less than 33, if so apply the first formula to the intermediary FWI
            if(startDmc.le.33.) then
               fwiIntermed=100./(0.5+0.3*startDmc)
            else

               ! Apply one of two formulas to calculate the intermediary FWI, based on if the starting DMC - 65 is less than 0
               if(startDmc-65.le.0.) then
                  fwiIntermed=14.-1.3*alog(startDmc)
               else
                  fwiIntermed=6.2*alog(startDmc)-17.2
               end if
            end if

            ! Calculate the moisture rate based on the content moisture, effective rain, and intermediary FWI
            moistureRate=contentMoist+(1000.*effectiveRain)/(48.77+fwiIntermed*effectiveRain)

            ! Calculate the DMC using the previously calculated moisture rate
            afterDmc=43.43*(5.6348-alog(moistureRate-20.))
         end if
         
         ! Set the DMC to 0 if its less than 0
         if(afterDmc.lt.0.) then
            afterDmc=0.0
         end if
         
         duffMoist=afterDmc+dryFactor
      end if
      
      duffMoistureCodeFunc=duffMoist
   end function duffMoistureCodeFunc
   
   ! Function that deals with the Drought Code
   real function droughtCodeFunc(readTemp,readRain,row,daylenDc,startDc)
      implicit none
      real, dimension(12), intent(in) :: daylenDc
      real, intent(in) :: readRain,startDc
      real, intent(inout) :: readTemp
      integer, intent(in) :: row
      real :: moistEquivDc,dryFactor,effectiveRain,afterRainDc,curRain,droughtCode

      ! Calculating the dry factor
      if(readTemp+2.8.ge.0.) then
         dryFactor=(.36*(readTemp+2.8)+daylenDc(row))/2.
      else
         readTemp=-2.8
      endif
      
      ! If the rain is less than 2.8mm, then calculate the effective rain, and moist equivalent drought code,
      ! Then set the drought code that is calculated after the rain equal to a value that uses the previous two values,
      ! Otherwise just set it to the starting drought code value
      if(readRain.le.2.8) then
         afterRainDc=startDc
      else
         curRain=readRain
         effectiveRain=0.83*curRain-1.27
         moistEquivDc=800.*exp(-startDc/400.)
         afterRainDc=startDc-400.*alog(1.+((3.937*effectiveRain)/moistEquivDc))

         ! If the after rain drought code value is less than 0, set it to 0
         if(afterRainDc.le.0.) then
            afterRainDc=0.0
         end if
      end if
      
      droughtCode=afterRainDc+dryFactor

      ! Setting it to 0 if the calculated value is negative
      if(droughtCode.lt.0.) then
         droughtCode=0.0
      end if

      droughtCodeFunc = droughtCode
   end function droughtCodeFunc

   ! Function that deals with the Initial Spread Index
   real function initialSpreadIndexFunc(todayFfmc,wind)
      implicit none
      real, intent(in) :: todayFfmc
      integer, intent(in) :: wind
      real :: fineMoisture,fineFuelFunc,initSpreadCalc

      ! Calculating the initial spread index
      fineMoisture=101.-todayFfmc
      fineFuelFunc=19.1152*exp(-0.1386*fineMoisture)*(1.+fineMoisture**4.65/7950000.)
      initSpreadCalc=fineFuelFunc*exp(0.05039*wind)

      initialSpreadIndexFunc = initSpreadCalc
   end function initialSpreadIndexFunc

   ! Function that deals with the Fire Weather Index
   real function buildupIndexFunc(duffMoist,droughtCode)
      implicit none
      real, intent(in) :: duffMoist,droughtCode
      real :: buildIndex,correctionFuncRatio,correctionFuncCc
      
      ! Calculating the buildup index using the drought code and duff moisture code, then if it is less than
      ! the duff moisture code, apply a correction value
      buildIndex=(0.8*droughtCode*duffMoist)/(duffMoist+0.4*droughtCode)
      if(buildIndex.lt.duffMoist) then
         correctionFuncRatio=(duffMoist-buildIndex)/duffMoist
         correctionFuncCc=0.92+(0.0114*duffMoist)**1.7
         buildIndex=duffMoist-(correctionFuncCc*correctionFuncRatio)

         ! Setting it to 0 if the calculated value is negative
         if(buildIndex.lt.0.) then 
         buildIndex=0.
         end if
      end if
      
      buildupIndexFunc = buildIndex
   end function buildupIndexFunc

   ! Function that deals with the Fire Weather Index
   real function fireWeatherIndexFunc(buildIndex,initSpreadCalc)
      implicit none
      real, intent(in) :: buildIndex,initSpreadCalc
      real :: fwiIntermed,finalLogFwi,fireWeatherIndex

      ! Calculating the intermediary fire weather index
      if(buildIndex.le.80.) then
         fwiIntermed=0.1*initSpreadCalc*(0.626*buildIndex**0.809+2.)
      else
         fwiIntermed=0.1*initSpreadCalc*(1000./(25.+108.64/exp(0.023*buildIndex)))
      end if

      ! Setting the final value to the logged version of the intermediary fire weather index if when subtracted 
      ! By 1 is greater than 0, if it isnt greater, set it to the intermediary value
      if(fwiIntermed-1.0.gt.0.) then
         finalLogFwi=2.72*(0.43*alog(fwiIntermed))**0.647
         fireWeatherIndex=exp(finalLogFwi)
      else
         fireWeatherIndex=fwiIntermed
      end if

      fireWeatherIndexFunc = fireWeatherIndex
   end function fireWeatherIndexFunc
end module FFWIndecies
