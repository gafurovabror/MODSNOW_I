	! In this step cloud removal occurs according to neghboring pixel coverage and its elevation. 

	SUBROUTINE STEP_5(StDay, NrDays, NrRows, NrCols, NoData, path_output, path_dem_file, Year, collen, CounterPers, SnowPers, write_output_step5, extention_in, extention_out)
	integer, dimension(:,:), allocatable :: SnowID, DemID
!	integer, dimension(NrRows,NrCols) :: DemID
	integer :: StDay, NrDays, NrRows, NrCols, exists, stat, schritt, NoData, sign
	character (len=3) :: day
	character (len=4) :: Year
	character (len=5) :: FolderName
	character (len=15) :: collen
	character (len=200):: path_output, path_dem_file
	character (len=50) :: header1, header2, header3, header4, header5, header6
	character (len=50) :: extention_in, extention_out
	real ::  counter, CounterTotal, CounterPers(366,6), snowcount, SnowPers(366,6)
	logical :: write_output_step5
	

	schritt=5
	
	write(*,*) 'Processing STEP_5...'
	
	FolderName='Step5'
	
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
	
	sign=0 ! a sign to count CounterTotal only once (first day where data available)
	CounterTotal=0

	do  k=StDay,NrDays

		allocate(SnowID(NrRows,NrCols))

		write(*,*) Year, ' step 5  ', k
		write(day,'(I3.3)') k

		open(1, file=trim(path_output)//'\'//Year//'\Step4\'//Year//day//trim(extention_out), STATUS='old', IOSTAT=stat)  !reads results from folder Step2 as input for this step 3.

		if (stat.ne.0) then        ! this identifies the error when the file is not found
			call SYSTEM("copy "//trim(path_output)//'\'//Year//"\Step4\"//Year//day//trim(extention_out)//" "// trim(path_output)//Year//'\'//FolderName)
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
				if (SnowID(j,i).eq.1.or.SnowID(j,i).eq.0.or.SnowID(j,i).eq.254.or.SnowID(j,i).eq.50) then    ! Treat pixel values '1', '0' and '254' as cloud pixel (50) and set cloud index into 5
					SnowID(j,i)=5
				endif
				if (SnowID(j,i).eq.255) then  ! in the following codes, the 3 digit numbers are converted into 1 digit to minimize the output file size since with 3 digit data very large space is needed.
					SnowID(j,i)=0
				endif
				if (SnowID(j,i).eq.25) then
					SnowID(j,i)=2
				endif
				if (SnowID(j,i).eq.200) then
					SnowID(j,i)=8
				endif
				if (SnowID(j,i).eq.37) then
					SnowID(j,i)=3
				endif
				if (SnowID(j,i).eq.100) then
					SnowID(j,i)=1
				endif

				if (SnowID(j,i).ne.0) then
					if (sign.eq.0) then  ! CounterTotal will be calculated only once (first day where data available) 
						CounterTotal=CounterTotal+1
					endif
					if (j.gt.1.and.i.gt.1.and.j.lt.(NrRows-1).and.i.lt.(NrCols-1)) then
						if (SnowID(j,i).eq.5.and.SnowID(j-1,i).eq.8.and.DemID(j,i).gt.DemID(j-1,i)) then    
							SnowID(j,i)=8																	   
						endif																					   
						if (SnowID(j,i).eq.5.and.SnowID(j+1,i).eq.8.and.DemID(j,i).gt.DemID(j+1,i)) then  
							SnowID(j,i)=8																   
						endif																					   
						if (SnowID(j,i).eq.5.and.SnowID(j,i-1).eq.8.and.DemID(j,i).gt.DemID(j,i-1)) then  
							SnowID(j,i)=8																	   
						endif																					   
						if (SnowID(j,i).eq.5.and.SnowID(j,i+1).eq.8.and.DemID(j,i).gt.DemID(j,i+1)) then  
							SnowID(j,i)=8																	   
						endif

						if (SnowID(j,i).eq.5.and.SnowID(j-1,i).eq.2.and.DemID(j,i).lt.DemID(j-1,i)) then    
							SnowID(j,i)=2																	   
						endif																					   
						if (SnowID(j,i).eq.5.and.SnowID(j+1,i).eq.2.and.DemID(j,i).lt.DemID(j+1,i)) then  
							SnowID(j,i)=2																	   
						endif																					   
						if (SnowID(j,i).eq.5.and.SnowID(j,i-1).eq.2.and.DemID(j,i).lt.DemID(j,i-1)) then  
							SnowID(j,i)=2																	   
						endif																					   
						if (SnowID(j,i).eq.5.and.SnowID(j,i+1).eq.2.and.DemID(j,i).lt.DemID(j,i+1)) then  
							SnowID(j,i)=2																	   
						endif
					endif
					if (SnowID(j,i).eq.5) then  ! For calculation of cloud coverage fraction after this step.
						counter=counter+1
					endif
					if (SnowID(j,i).eq.8) then  ! For calculation of snow coverage fraction after this step.
						snowcount=snowcount+1
					endif
				else
					SnowID(j,i)=NoData
				endif
			enddo
			if (write_output_step5) then
				if (j.eq.1) then
					write(10, '(A50)') header1
					write(10, '(A50)') header2
					write(10, '(A50)') header3
					write(10, '(A50)') header4
					write(10, '(A50)') header5
					write(10, *) 'NODATA_value ', NoData
					write(10, collen) (SnowID(j,i), i=1,NrCols)  ! Format should be changed according to NrCols
				else
					write(10, collen) (SnowID(j,i), i=1,NrCols)  ! Format should be changed according to NrCols
				endif
			endif
		enddo
		sign=1  ! indicator so that CounterTotal will not be calculated again
		deallocate(SnowID)
		CounterPers(k,schritt)=counter/CounterTotal*100
		SnowPers(k,schritt)=snowcount/CounterTotal*100
		snowcount=0
		counter=0
		close(1)
		close(10)
	enddo
	deallocate(DemID)

	END SUBROUTINE STEP_5