	! In this step cloud removal occurs according to maximum and minimum elevation within this domain. 
												
	SUBROUTINE STEP_3(StDay, NrDays, NrRows, NrCols, NoData, path_output, path_dem_file, Year, collen, CounterPers, SnowPers, write_output_step3, extention_in, extention_out)
	implicit none
  	integer :: StDay, NrDays, NrRows, NrCols, i, j, k, exists, stat, El_Max, El_Min, h, schritt, NoData, sign, El_Max1, El_Min1
	integer, dimension(:,:), allocatable :: SnowID, DemID
!	integer, dimension(NrRows,NrCols) :: DemID
	integer, dimension(NrDays):: MinDem, MaxDem, SnowBand
	character (len=3) :: day
	character (len=4) :: Year
	character (len=5) :: FolderName
	character (len=15) :: collen
	character (len=200):: path_output, path_dem_file
	character (len=50) :: header1, header2, header3, header4, header5, header6
	character (len=50) :: extention_in, extention_out
	real ::  counter, CounterTotal, CounterPers(366,6), snowcount, SnowPers(366,6)
	logical :: write_output_step3
	real :: start, finish

	schritt=3

	write(*,*) 'Processing STEP_3...'

	FolderName='Step3'
	INQUIRE(FILE = trim(path_output)//'\'//Year//'\'//FolderName, EXIST = exists )    !this looks for wether FolderName exists!
	if (exists==0) then  ! if FolderName does no exist, new FolderName is created.
		call system('mkdir '//trim(path_output)//'\'//Year//'\'//FolderName)
	endif
	allocate(DemID(NrRows,NrCols))
	open(3000,file=trim(path_dem_file), STATUS='old')
		read(3000,*) 
		read(3000,*)
		read(3000,*)
		read(3000,*)
		read(3000,*)
		read(3000,*)
	do j=1,NrRows
		read(3000,*) (DemID(j,i), i=1,NrCols)
	enddo
	
	close(3000)

	El_Max=0  !initialize these variables to compute maxmimum and minimum elevation in following do loop.
	El_Min=10000
	
	El_Max=maxval(DemID)					! get maximum elevation 
	El_Min=minval(DemID, mask = DemID>0)	! get minimum elevation with elevation higher than 0 (for nodata areas with -9999 values)
			
	sign=0 ! a sign to count CounterTotal only once (first day where data available)
	CounterTotal=0

	do  k=StDay,NrDays

		allocate(SnowID(NrRows,NrCols))
	
		write(*,*) Year, ' step 3  ', k
		
		write(day,'(I3.3)') k
		
		open(1, file=trim(path_output)//'\'//Year//'\Step2\'//Year//day//trim(extention_out), STATUS='old', IOSTAT=stat)  !reads results from folder Step2 as input for this step 3.

		if (stat.ne.0) then        ! this identifies the error when the file is not found

			call SYSTEM("copy "//trim(path_output)//'\'//Year//"\Step2\"//Year//day//trim(extention_out)//" "// trim(path_output)//'\'//Year//'\'//FolderName)
			CounterPers(k,schritt)=NoData
			SnowPers(k,schritt)=NoData

			deallocate(SnowID)
			close(1)
			close(10)
			cycle
		endif
		
		read(1,'(A50)') header1
		read(1,'(A50)') header2
		read(1,'(A50)') header3
		read(1,'(A50)') header4
		read(1,'(A50)') header5
		read(1,'(A50)') header6

		open(10, file=trim(path_output)//'\'//Year//'\'//FolderName//'/'//Year//day//trim(extention_out))   !output file

		do j=1,NrRows
			read(1,*) (SnowID(j,i), i=1,NrCols)
			do i=1,NrCols
				if (SnowID(j,i).eq.1.or.SnowID(j,i).eq.0.or.SnowID(j,i).eq.254) then    ! Treat pixel values '1', '0' and '254' as cloud pixel (50)
					SnowID(j,i)=50
				endif
				if (SnowID(j,i).ne.NoData.or.SnowID(j,i).ne.255) then
					if (sign.eq.0) then  ! CounterTotal will be calculated only once (first day where data available) 
						CounterTotal=CounterTotal+1
					endif
				endif
				if (SnowID(j,i).eq.50) then  ! For calculation of cloud coverage fraction before this step which will be used as one of the criteria to execute this step.
					counter=counter+1
				endif
			enddo
		enddo
		CounterPers(k,schritt)=counter/CounterTotal*100
		counter=0
		sign=1  ! indicator so that CounterTotal will not be calculated again

		if (CounterPers(k,schritt).le.30) then
			MinDem(k)=10000  ! 10000 is to initialize the value of MinDem(k) so it is not zero
			do j=1,NrRows
				do i=1,NrCols
					if (SnowID(j,i).eq.200) then
						if (DemID(j,i).lt.MinDem(k)) then
							MinDem(k)=DemID(j,i)
						endif
					endif
				enddo
			enddo
			do H=El_Max,El_Min,-1
				do j=1,NrRows 
					do i=1,NrCols
						if (DemID(j,i).eq.H) then   !the test will be begin from upper edge of elevation and will continue downwards. As soon as land cover detected, the El_Max will be set to the elevation of this cell.
							if (SnowID(j,i).eq.25) then
								MaxDem(k)=H
								goto 1
							endif
						endif
					enddo
				enddo
			enddo
1			if (MaxDem(k).gt.0) then
				SnowBand(k)=MaxDem(k)
			endif
			write(99,*) Year, day, ' snowline_max =', SnowBand(k), ' snowline_min =', MinDem(k)

			do j=1,NrRows
				do i=1,NrCols
					if (SnowID(j,i).ne.NoData.or.SnowID(j,i).ne.255) then	
						if (SnowID(j,i).eq.50.and.DemID(j,i).lt.MinDem(k).and.MinDem(k).lt.10000) then   ! For "cloud (50)" cells
							SnowID(j,i)=25
						endif
						if (SnowID(j,i).eq.50.and.DemID(j,i).gt.SnowBand(k).and.SnowBand(k).gt.0) then
							SnowID(j,i)=200
						endif

						if (SnowID(j,i).eq.50) then  ! For calculation of cloud coverage fraction after this step.
							counter=counter+1
						endif
						if (SnowID(j,i).eq.200) then  ! For calculation of snow coverage fraction after this step.
							snowcount=snowcount+1
						endif
					else
						SnowID(j,i)=NoData
					endif
				enddo
				if (write_output_step3) then
					if (j.eq.1) then
						write(10, '(A50)') header1
						write(10, '(A50)') header2
						write(10, '(A50)') header3
						write(10, '(A50)') header4
						write(10, '(A50)') header5
						write(10, *) 'NODATA_value ', NoData
						write(10, collen) (SnowID(j,i), i=1,NrCols)  
					else
						write(10, collen) (SnowID(j,i), i=1,NrCols)  
					endif
				endif
			enddo
			MinDem(k)=0
			SnowBand(k)=0
		else  ! this step is not executed and SnowID array that was read from previous step is written as an output for the next step
			do j=1,NrRows			
				if (write_output_step3) then
					if (j.eq.1) then
						write(10, '(A50)') header1
						write(10, '(A50)') header2
						write(10, '(A50)') header3
						write(10, '(A50)') header4
						write(10, '(A50)') header5
						write(10, *) 'NODATA_value ', NoData
						write(10, collen) (SnowID(j,i), i=1,NrCols)  
					else
						write(10, collen) (SnowID(j,i), i=1,NrCols)  
					endif
				endif
			enddo
			write(99,*) 'cloud fraction on day ', k, ' is over 30 % (', CounterPers(k,schritt), '%)'
		endif
		deallocate(SnowID)
		CounterPers(k,schritt)=counter/CounterTotal*100
		SnowPers(k,schritt)=snowcount/CounterTotal*100
		snowcount=0
		counter=0
		close(1)
		close(10)
	enddo
	deallocate(DemID)

	END SUBROUTINE STEP_3
