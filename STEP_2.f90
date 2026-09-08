	! In this step cloud removal occurs according to temporal information. 2 days forwards and 2 days backwards method.
	
	SUBROUTINE STEP_2(StDay, NrDays, NrRows, NrCols, NoData, path_output, Year, collen, CounterPers, SnowPers, write_output_step2, extention_in, extention_out)
	integer, dimension(:,:), allocatable :: SnowID1, SnowID2, SnowID, SnowID4, SnowID5
	integer :: StDay, NrDays, NrRows, NrCols, exists, i, j, k, t, stat, schritt, NoData, sign
	character (len=3) :: day1, day2, day3, day4, day5
	character (len=4) :: Year
	character (len=5) :: FolderName
	character (len=15) :: collen
	character (len=200):: path_output
	character (len=50) :: header1, header2, header3, header4, header5, header6
	character (len=50) :: extention_in, extention_out
	real ::  counter, CounterTotal, CounterPers(366,6), snowcount, SnowPers(366,6)
	logical :: write_output_step2

	
	schritt=2

	write(*,*) 'Processing STEP_2....'
	
	FolderName='Step2'
	
	INQUIRE(FILE = trim(path_output)//'\'//Year//'\'//FolderName, EXIST = exists )    !this looks for whether FolderName exists!
	
	if (exists==0) then  ! if FolderName does no exist, new FolderName is created.
		call system('mkdir '//trim(path_output)//'\'//Year//'\'//FolderName)
	endif

	sign=0 ! a sign to count CounterTotal only once (first day where data available)
	CounterTotal=0

	do  k=StDay,NrDays
		
		write(*,*) Year, ' step 2  ', k
		t=k-2
		write(day1,'(I3.3)') t !k-2
		t=k-1
		write(day2,'(I3.3)') t !k-1
			
		write(day3,'(I3.3)') k
		t=k+1
		write(day4,'(I3.3)') t !k+1
		t=k+2
		write(day5,'(I3.3)') t !k+2


		open(1, file=trim(path_output)//'\'//Year//'\Step1\'//Year//day1//trim(extention_out), STATUS='old', IOSTAT=stat)
		if (stat.ne.0) then        ! this identifies the error when the file is not found
			write(99, *) 'Data on day ', Year, k-2, 'does not exist'
			close(1)

			call SYSTEM("copy "//trim(path_output)//'\'//Year//"\Step1\"//Year//day3//trim(extention_out)//" "// trim(path_output)//'\'//Year//'\'//FolderName) 
			CounterPers(k,schritt)=NoData
			SnowPers(k,schritt)=NoData
			cycle
		endif
		open(2, file=trim(path_output)//'\'//Year//'\Step1\'//Year//day2//trim(extention_out), STATUS='old', IOSTAT=stat)
		if (stat.ne.0) then        ! this identifies the error when the file is not found
			write(99, *) 'Data on day ', Year, k-1, 'does not exist'
			close(1)
			close(2)

			call SYSTEM("copy "//trim(path_output)//'\'//Year//"\Step1\"//Year//day3//trim(extention_out)//" "// trim(path_output)//'\'//Year//'\'//FolderName)   ! copy file for current day from previous step.
			CounterPers(k,schritt)=NoData
			SnowPers(k,schritt)=NoData
			cycle
		endif
			
		open(4, file=trim(path_output)//'\'//Year//'\Step1\'//Year//day4//trim(extention_out), STATUS='old', IOSTAT=stat)
		if (stat.ne.0) then        ! this identifies the error when the file is not found
			write(99, *) 'Data on day ', Year, k+1, 'does not exist'
			close(1)
			close(2)
			close(4)

			call SYSTEM("copy "//trim(path_output)//'\'//Year//"\Step1\"//Year//day3//trim(extention_out)//" "// trim(path_output)//'\'//Year//'\'//FolderName)   ! copy file for current day from previous step.
			CounterPers(k,schritt)=NoData
			SnowPers(k,schritt)=NoData
			cycle
		endif
		open(5, file=trim(path_output)//'\'//Year//'\Step1\'//Year//day5//trim(extention_out), STATUS='old', IOSTAT=stat)
		if (stat.ne.0) then        ! this identifies the error when the file is not found
			write(99, *) 'Data on day ', Year, k+2, 'does not exist'
			close(1)
			close(2)
			close(4)
			close(5)

			call SYSTEM("copy "//trim(path_output)//'\'//Year//"\Step1\"//Year//day3//trim(extention_out)//" "// trim(path_output)//'\'//Year//'\'//FolderName)   ! copy file for current day from previous step.
			CounterPers(k,schritt)=NoData
			SnowPers(k,schritt)=NoData
			cycle
		endif

		open(3, file=trim(path_output)//'\'//Year//'\Step1\'//Year//day3//trim(extention_out), STATUS='old', IOSTAT=stat)

		if (stat.ne.0) then        ! this identifies the error when the file is not found
			write(99, *) 'Data on day ', Year, k, 'does not exist'
			close(1)
			close(2)
			close(3)
			close(4)
			close(5)

			call SYSTEM("copy "//trim(path_output)//'\'//Year//"\Step1\"//Year//day3//trim(extention_out)//" "// trim(path_output)//'\'//Year//'\'//FolderName) 
			CounterPers(k,schritt)=NoData
			SnowPers(k,schritt)=NoData
			cycle
		endif

		allocate(SnowID(NrRows,NrCols))
		allocate(SnowID1(NrRows,NrCols))
		allocate(SnowID2(NrRows,NrCols))
		allocate(SnowID4(NrRows,NrCols))
		allocate(SnowID5(NrRows,NrCols))
		
		do j=1, 6
			read(1,*)
			read(2,*)
			read(4,*)
			read(5,*)
		enddo

		read(3,'(A50)') header1 
		read(3,'(A50)') header2
		read(3,'(A50)') header3
		read(3,'(A50)') header4
		read(3,'(A50)') header5
		read(3,'(A50)') header6

		open(10, file=trim(path_output)//'\'//Year//'\'//FolderName//'\'//Year//day3//trim(extention_out))   !output file
		
		do j=1,NrRows
			read(3,*) (SnowID(j,i), i=1,NrCols)
		
			if (k.gt.2.and.k.lt.(NrDays-2)) then   ! Due to 2 days of forward and backward moving days, first two and last two days can be preprocessed using this step
			
				read(1,*) (SnowID1(j,i), i=1,NrCols)
				read(2,*) (SnowID2(j,i), i=1,NrCols)
				read(4,*) (SnowID4(j,i), i=1,NrCols)
				read(5,*) (SnowID5(j,i), i=1,NrCols)
				do i=1,NrCols
					if (SnowID(j,i).eq.1.or.SnowID(j,i).eq.0.or.SnowID(j,i).eq.254) then    ! Treat pixel values '1', '0' and '254' as cloud pixel (50)
						SnowID(j,i)=50
					endif
					if (SnowID(j,i).ne.NoData.or.SnowID(j,i).ne.255) then
						if (sign.eq.0) then  ! CounterTotal will be calculated only once (first day where data available) 
							CounterTotal=CounterTotal+1
						endif
						if (SnowID(j,i).eq.50) then
							if (SnowID2(j,i).eq.200.and.SnowID4(j,i).eq.200) then			
								SnowID(j,i)=200
							endif																 
							if (SnowID2(j,i).eq.25.and.SnowID4(j,i).eq.25) then	 !SnowID2=k-1, SnowID1=k-2, SnowID=k, SnowID4=k+1, SnowID5=k+2	
								SnowID(j,i)=25
							endif																
							if (SnowID1(j,i).eq.200.and.SnowID4(j,i).eq.200) then			
								SnowID(j,i)=200
							endif															
							if (SnowID1(j,i).eq.25.and.SnowID4(j,i).eq.25) then		
								SnowID(j,i)=25											
							endif
							if (SnowID2(j,i).eq.200.and.SnowID5(j,i).eq.200) then		
								SnowID(j,i)=200
							endif															
							if (SnowID2(j,i).eq.25.and.SnowID5(j,i).eq.25) then		
								SnowID(j,i)=25											
							endif
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
				if (write_output_step2) then
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
			else  ! this part is for first two days where step 2 was not applied. SnowID that was read will be written again for these days without any computation.
				if (write_output_step2) then
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
			endif
		enddo
		sign=1  ! indicator so that CounterTotal will not be calculated again
		deallocate(SnowID1)
		deallocate(SnowID2)
		deallocate(SnowID)
		deallocate(SnowID4)
		deallocate(SnowID5)
		CounterPers(k,schritt)=counter/CounterTotal*100
		SnowPers(k,schritt)=snowcount/CounterTotal*100
		counter=0
		snowcount=0
		close(1)
		close(2)
		close(3)
		close(4)
		close(5)
		close(10)  !output file.
	enddo

	END SUBROUTINE STEP_2