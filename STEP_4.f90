	! In this step cloud removal occurs according to spatial filtering. Neighboring pixels are checked for its coverage and if at least 3 say snow, than middle also snow. 

	SUBROUTINE STEP_4(StDay, NrDays, NrRows, NrCols, NoData, path_output, Year, collen, CounterPers, SnowPers, write_output_step4, extention_in, extention_out)
	integer :: StDay, NrDays, NrRows, NrCols, exists, stat, schritt, NoData, sign
	integer, dimension(:,:), allocatable :: SnowID
	character (len=3) :: day
	character (len=4) :: Year
	character (len=5) :: FolderName
	character (len=15) :: collen
	character (len=200):: path_output
	character (len=50) :: header1, header2, header3, header4, header5, header6
	character (len=50) :: extention_in, extention_out
	real ::  counter, CounterTotal, CounterPers(366,6), snowcount, SnowPers(366,6)
	logical :: write_output_step4


	schritt=4

	write(*,*) 'Processing STEP_4...'
	
	FolderName='Step4'
	
	INQUIRE(FILE = trim(path_output)//'\'//Year//'\'//FolderName, EXIST = exists )    !this looks for wether FolderName exists!
	
	if (exists==0) then  ! if FolderName does no exist, new FolderName is created.
		call system('mkdir '//trim(path_output)//'\'//Year//'\'//FolderName)
	endif

	sign=0 ! a sign to count CounterTotal only once (first day where data available)
	CounterTotal=0

	do  k=StDay,NrDays

		allocate(SnowID(NrRows,NrCols))

		write(*,*) Year, ' step 4  ', k
		write(day,'(I3.3)') k
		
		open(1, file=trim(path_output)//'\'//Year//'\Step3\'//Year//day//trim(extention_out), STATUS='old', IOSTAT=stat)  !reads results from folder Step2 as input for this step 3.

		if (stat.ne.0) then        ! this identifies the error when the file is not found
			
			call SYSTEM("copy "//trim(path_output)//'\'//Year//"\Step3\"//Year//day//trim(extention_out)//" "// trim(path_output)//'\'//Year//'\'//FolderName)
			CounterPers(k,schritt)=NoData
			SnowPers(k,schritt)=NoData

			deallocate(SnowID)
			close(1)
			close(10)
			cycle
		endif

		open(10, file=trim(path_output)//'\'//Year//'\'//FolderName//'\'//Year//day//trim(extention_out))   !output file
		
		read(1,'(A50)') header1
		read(1,'(A50)') header2
		read(1,'(A50)') header3
		read(1,'(A50)') header4
		read(1,'(A50)') header5
		read(1,'(A50)') header6

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
					if (j.gt.2.and.i.gt.2.and.j.lt.(NrRows-2).and.i.lt.(NrCols-2)) then
						if (SnowID(j,i).eq.50.and.SnowID(j-1,i).eq.25.and. &
							SnowID(j,i-1).eq.25.and.SnowID(j,i+1).eq.25) then				
							SnowID(j,i)=25														
						endif																		
						if (SnowID(j,i).eq.50.and.SnowID(j+1,i).eq.25.and. &				
							SnowID(j-1,i).eq.25.and.SnowID(j,i+1).eq.25) then					
							SnowID(j,i)=25														
						endif																		
						if (SnowID(j,i).eq.50.and.SnowID(j+1,i).eq.25.and. &				
							SnowID(j,i-1).eq.25.and.SnowID(j,i+1).eq.25) then					
							SnowID(j,i)=25
						endif
						if (SnowID(j,i).eq.50.and.SnowID(j-1,i).eq.25.and. &
							SnowID(j,i-1).eq.25.and.SnowID(j+1,i).eq.25) then
							SnowID(j,i)=25
						endif

						if (SnowID(j,i).eq.50.and.SnowID(j-1,i).eq.200.and.SnowID(j,i-1).eq.200.and.SnowID(j,i+1).eq.200) then
							SnowID(j,i)=200
						endif
						if (SnowID(j,i).eq.50.and.SnowID(j+1,i).eq.200.and.SnowID(j-1,i).eq.200.and.SnowID(j,i+1).eq.200) then
							SnowID(j,i)=200
						endif
						if (SnowID(j,i).eq.50.and.SnowID(j+1,i).eq.200.and.SnowID(j,i-1).eq.200.and.SnowID(j,i+1).eq.200) then
							SnowID(j,i)=200
						endif
						if (SnowID(j,i).eq.50.and.SnowID(j-1,i).eq.200.and.SnowID(j,i-1).eq.200.and.SnowID(j+1,i).eq.200) then
							SnowID(j,i)=200
						endif

						if (SnowID(j,i).eq.50) then  ! For calculation of cloud coverage fraction after this step.
							counter=counter+1
						endif
						if (SnowID(j,i).eq.200) then  ! For calculation of snow coverage fraction after this step.
							snowcount=snowcount+1
						endif

					endif
				else
					SnowID(j,i)=NoData
				endif
			enddo
			if (write_output_step4) then
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
		counter=0
		snowcount=0
		close(1)
		close(10)
	enddo

	END SUBROUTINE STEP_4