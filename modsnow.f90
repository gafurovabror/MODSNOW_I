Copyright (C) [2016] Dr. Abror Gafurov, GFZ Helmholtz Centre for Geosciences

Licensed under AGPL-3.0-only (GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007).
https://spdx.org/licenses/AGPL-3.0-only.html

Copyright (C) 2007 Free Software Foundation, Inc. <https://fsf.org/>

Everyone is permitted to copy and distribute verbatim copies of this license document, but changing it is not allowed. 


	!*****************************************************************************************************************************
	! MODSNOW program for eliminating clouds from MODIS data. Step by step method is used where different assumptions are taken.
	! Read "Cloud removal methodology for MODIS snow cover product" by Gafurov and Bardossy (2009) for more information on the methodology.
	!        
	! Input: MODIS Terra and Aqua snow cover data in ASCII format
	!		 DEM with the same resolution and extension (same header files) as MODIS original snow cover data in ASCII format
	!
	! Output: 1. Ascii file with header for new snow cover data without cloud cover.
	!
	! Instruction to use this program: To apply this program for a different region, change folowing information:
	!	1. Integer parameters StDay, NrDays, NrCols, and NrRows according to starting and ending date and ascii file header
	!	2. Result text file (6000) location where percentage of clouds after each step for each day will be recorded
	!	3. "path_input_aqua" string with the folder directory where aqua modis snow data are stored in ascii format including back-slash (\)
	!	4. "path_input_terra" string with the folder directory where terra modis snow data are stored in ascii format including back-slash (\)
	!	5. "path_output" string where the directory for output files is given including back-slash (\)
	!	6. "path_dem" where directory for digital elevation model of a region is given
	!	7. Jahr according to the period where MODIS snow product should be processed using MODSNOW
	!
	! Written by: A. Gafurov
	! Date: 11-2007									Last Update: 04-2012
	!*******************************************************************************************
	
	! Two dimentional version. Last version that works without matrix dimension limitation. Abror 14.10.2011


	program modsnow
	
	implicit none
	
	integer	:: StDay, NrDays, NrCols, NrRows
	integer j, i, k, Jahr, year_start, year_end, exists, schritt, NoData
	character (len=3) :: day
	character (len=4) :: Year
	character (len=5) :: FolderName, NrColsChar
	character (len=15) :: collen
	character (len=200):: path_input_aqua, path_input_terra, path_output, path_dem_file
	character (len=50) :: extention_in, extention_out
!	real, dimension(:,:), allocatable :: CounterPers, SnowPers
!	real, dimension(:), allocatable :: CounterPersTerra, SnowPersTerra
	real ::  counter, counterTerra, CounterTotal, CounterPers(366,6), snowcount, snowcountTerra, SnowPers(366,6), CounterPersTerra(366), SnowPersTerra(366)
	logical :: write_output_step1, write_output_step2, write_output_step3, write_output_step4, write_output_step5, write_output_step6

	open(999,file='parameter.dat', STATUS='old')

	read(999,*)  ! a dummy line
	read(999,*) path_input_terra
	read(999,*) path_input_aqua
	read(999,*) path_output
	read(999,*) path_dem_file
	read(999,*) year_start
	read(999,*) year_end
	read(999,*) StDay
	read(999,*) NrDays
	read(999,*) write_output_step1
	read(999,*) write_output_step2
	read(999,*) write_output_step3
	read(999,*) write_output_step4
	read(999,*) write_output_step5
	read(999,*) write_output_step6
	read(999,*) extention_in
	read(999,*) extention_out
	read(999,*) NrCols
	read(999,*) NrRows
	read(999,*) NoData

	if (NrCols.ge.1000) then
		write(NrColsChar,'(I4.4)') NrCols			! this is for column number with 4 digit value
		collen='('//trim(NrColsChar)//'(i1,1x))'
	else
		write(NrColsChar,'(I3.3)') NrCols				! this is for column number with 3 digit value
		collen='('//trim(NrColsChar)//'(i1,1x))'
	endif

	open(99, file=trim(path_output)//'\logfile', STATUS='replace')

	Do Jahr=year_start,year_end
		
		write(Year,'(I4.4)') Jahr

		INQUIRE(FILE = trim(path_output)//'\'//Year, EXIST = exists )    !this looks for wether FolderName exists!
	
		if (exists==0) then  ! if FolderName does no exist, new FolderName is created.
			call system('mkdir '//trim(path_output)//'\'//Year)
		endif

!		CALL STEP_1(StDay, NrDays, NrRows, NrCols, NoData, path_output, path_input_aqua, path_input_terra, Year, collen, CounterPers, SnowPers, CounterPersTerra, SnowPersTerra, write_output_step1, extention_in, extention_out) 
!		CALL STEP_2(StDay, NrDays, NrRows, NrCols, NoData, path_output, Year, collen, CounterPers, SnowPers, write_output_step2, extention_in, extention_out)
!		CALL STEP_3(StDay, NrDays, NrRows, NrCols, NoData, path_output, path_dem_file, Year, collen, CounterPers, SnowPers, write_output_step3, extention_in, extention_out)
!		CALL STEP_4(StDay, NrDays, NrRows, NrCols, NoData, path_output, Year, collen, CounterPers, SnowPers, write_output_step4, extention_in, extention_out)
!		CALL STEP_5(StDay, NrDays, NrRows, NrCols, NoData, path_output, path_dem_file, Year, collen, CounterPers, SnowPers, write_output_step5, extention_in, extention_out)
		CALL STEP_6(StDay, NrDays, NrRows, NrCols, NoData, path_output, Year, Jahr, collen, CounterPers, SnowPers, write_output_step6, extention_in, extention_out)

		open(6000, file=trim(path_output)//'\'//Year//'\cloud_snow_fraction_'//Year//'_central.txt')  ! Result text file with cloud percentage
	
		do k=StDay,NrDays
			if (k.eq.StDay) then
				write(6000, '(A130)') 'Year  *  Day   *  Terra cloud fraction  *  Cloud Fraction Step 1-6     *   Terra snow fraction   *   Snow Fraction Steps 1-6'
			endif

			write(6000,'(I4,2x,I3,2X,7(F6.2,2X),A3,2X,7(F6.2,2X))') Jahr, k, CounterPersTerra(k), (CounterPers(k,schritt), schritt=1,6), ' * ', SnowPersTerra(k), (SnowPers(k,schritt), schritt=1,6)
		enddo

	Enddo	

	write(*,*) 'cloud elimination finished succesfully'
	pause

	End Program modsnow
