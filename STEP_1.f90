	! In this step, snow maps of Terra and Aqua are combined, so that maximum cloud free coverage from both maps is obtained. 

	SUBROUTINE STEP_1(StDay, NrDays, NrRows, NrCols, NoData, path_output, path_input_aqua, path_input_terra, Year, collen, CounterPers, SnowPers, CounterPersTerra, SnowPersTerra, write_output_step1, extention_in, extention_out) 
	integer :: StDay, NrDays, NrRows, NrCols, exists, stat_a, stat_t, sign, schritt, NoData
	integer, dimension(:,:), allocatable :: SnowID, SnowID_AQUA
	character (len=3) :: day
	character (len=4) :: Year
	character (len=5) :: FolderName, NrColsChar
	character (len=200):: path_output, path_input_aqua, path_input_terra 
	character (len=50) :: header1, header2, header3, header4, header5, header6
	character (len=15) :: collen
	character (len=50) :: extention_in, extention_out
	CHARACTER(120) oldname, newname
	real ::  counter, counterTerra, CounterTotal, CounterPers(366,6), snowcount, snowcountTerra, SnowPers(366,6), CounterPersTerra(366), SnowPersTerra(366)
	LOGICAL(4) result
	logical :: write_output_step1

	
	schritt=1

	write(*,*) 'Processing STEP_1...'

	FolderName='Step1'
	
	INQUIRE(FILE = trim(path_output)//'\'//Year//'\'//FolderName, EXIST = exists )    !this looks for wether FolderName exists!
	
	if (exists==0) then  ! if FolderName does no exist, new FolderName is created.
		call system('mkdir '//trim(path_output)//'\'//Year//'\'//FolderName)
	endif
	
	sign=0 ! a sign to count CounterTotal only once (first day where data available)
	CounterTotal=0

	do  k=StDay,NrDays
	
		allocate(SnowID(NrRows,NrCols))
		allocate(SnowID_AQUA(NrRows,NrCols))

		write(*,*) Year, ' step 1  ', k
		write(day,'(I3.3)') k

		open(1, file = trim(path_input_aqua)//'\'//Year//'\myd'//Year//day//trim(extention_in), STATUS='old', IOSTAT=stat_a)

		open(2, file=trim(path_input_terra)//'\'//Year//'\mod'//Year//day//trim(extention_in), STATUS='old', IOSTAT=stat_t)

		if (stat_t.ne.0) then        ! this identifies the error when the file is not found
			if (stat_a.eq.0) then
				do j=1,6
					read(1,*) 
				enddo
				do j=1,NrRows
					read(1,*) (SnowID(j,i), i=1,NrCols)
					do i=1,NrCols
						if (SnowID(j,i).eq.1.or.SnowID(j,i).eq.0.or.SnowID(j,i).eq.254) then   ! Treat pixel values '1 (no decision)', '0 (data missing)' and '254 (detector saturated)' as cloud pixel (50)
							SnowID(j,i)=50
						endif
						if (SnowID(j,i).ne.-9999.or.SnowID(j,i).ne.255) then
							if (sign.eq.0) then  ! CounterTotal will be calculated only once (first day where data available) 
								CounterTotal=CounterTotal+1
							endif
							if (SnowID(j,i).eq.50) then
								counter=counter+1
							endif
							if (SnowID(j,i).eq.200) then
								snowcount=snowcount+1
							endif
						endif
					enddo
				enddo
				sign=1  ! indicator so that CounterTotal will not be calculated again
				CounterPers(k,schritt)=counter/CounterTotal*100
				SnowPers(k,schritt)=snowcount/CounterTotal*100

				CounterPersTerra(k)=CounterPers(k,schritt)    ! if Terra data is not available this step will not be executed and the snow fraction of Aqua will be assigned
				SnowPersTerra(k)=SnowPers(k,schritt)		  ! as snow fraction after step 1 and from Terra data in snow fraction output data. They will habe the same values for those days where no Terra data are available.

				counter=0
				snowcount=0

				call SYSTEM('copy '//trim(path_input_aqua)//'\'//Year//'\myd'//Year//day//trim(extention_in)//' '// trim(path_output)//'\'//Year//'\'//FolderName)   ! copy file for current day from previous step.
				oldname=trim(path_output)//'\'//Year//'\'//FolderName//'\myd'//Year//day//trim(extention_in)
				newname=trim(path_output)//'\'//Year//'\'//FolderName//'\'//Year//day//trim(extention_out)
				result = RENAMEFILEQQ(oldname, newname)
				write(99,*) 'Terra file on day ', k, ' missing, existing orig file copied into ', FolderName
				write(*,*) 'Terra file on day ', k, ' missing, existing orig file copied into ', FolderName
				close(1)
			endif
		endif
		
		if (stat_a.ne.0) then
			if (stat_t.eq.0) then
				do j=1,6
					read(2,*) 
				enddo
				do j=1,NrRows
					read(2,*) (SnowID(j,i), i=1,NrCols)
					do i=1,NrCols
						if (SnowID(j,i).eq.1.or.SnowID(j,i).eq.0.or.SnowID(j,i).eq.254) then   ! Treat pixel values '1 (no decision)', '0 (data missing)' and '254 (detector saturated)' as cloud pixel (50)
							SnowID(j,i)=50
						endif
						if (SnowID(j,i).ne.-9999.or.SnowID(j,i).ne.255) then
							if (sign.eq.0) then  ! CounterTotal will be calculated only once (first day where data available) 
								CounterTotal=CounterTotal+1
							endif
							if (SnowID(j,i).eq.50) then
								counter=counter+1
							endif
							if (SnowID(j,i).eq.200) then
								snowcount=snowcount+1
							endif
						endif
					enddo
				enddo
				sign=1  ! indicator so that CounterTotal will not be calculated again
				CounterPers(k,schritt)=counter/CounterTotal*100
				SnowPers(k,schritt)=snowcount/CounterTotal*100

				CounterPersTerra(k)=CounterPers(k,schritt)    ! if Aqua data is not available this step will not be executed and the snow fraction of Terra will be assigned
				SnowPersTerra(k)=SnowPers(k,schritt)		  ! as snow fraction after step 1. They will habe the same values for those days where no Aqua data are available.

				counter=0
				snowcount=0

				call SYSTEM('copy '//trim(path_input_terra)//'\'//Year//'\mod'//Year//day//trim(extention_in)//' '// trim(path_output)//'\'//Year//'\'//FolderName)   ! copy file for current day from previous step.
				oldname=trim(path_output)//'\'//Year//'\'//FolderName//'\mod'//Year//day//trim(extention_in)			!one for terra and one (below) for aqua
				newname=trim(path_output)//'\'//Year//'\'//FolderName//'\'//Year//day//trim(extention_out)
				result = RENAMEFILEQQ(oldname, newname)
				write(99,*) 'Aqua file on day ', k, ' missing, existing orig file copied into ', FolderName
				write(*,*) 'Aqua file on day ', k, ' missing, existing orig file copied into ', FolderName
				close(2)
			endif
		endif

		if (stat_a.ne.0.or.stat_t.ne.0) then 

			deallocate(SnowID)
			deallocate(SnowID_AQUA)

			cycle
		endif
		
		read(1,*)
		read(1,*)
		read(1,*)
		read(1,*)
		read(1,*)
		read(1,*)

		read(2,'(A50)') header1 
		read(2,'(A50)') header2
		read(2,'(A50)') header3
		read(2,'(A50)') header4
		read(2,'(A50)') header5
		read(2,'(A50)') header6

		open(10, file=trim(path_output)//'\'//Year//'\'//FolderName//'/'//Year//day//trim(extention_out))

		do j=1,NrRows
			read(1,*) (SnowID_AQUA(j,i), i=1,NrCols)
			read(2,*) (SnowID(j,i), i=1,NrCols)     ! This is deactivated in validation where step is not validated and Terra is read as SnowID
			do i=1,NrCols
				if (SnowID_AQUA(j,i).eq.1.or.SnowID_AQUA(j,i).eq.0.or.SnowID_AQUA(j,i).eq.254) then   ! Treat pixel values '1 (no decision)', '0 (data missing)' and '254 (detector saturated)' as cloud pixel (50)
					SnowID_AQUA(j,i)=50
				endif
				if (SnowID(j,i).eq.1.or.SnowID(j,i).eq.0.or.SnowID(j,i).eq.254) then    ! Treat pixel values '1 (no decision)', '0 (data missing)' and '254 (detector saturated)' as cloud pixel (50)
					SnowID(j,i)=50
				endif
				if (SnowID(j,i).ne.-9999.or.SnowID(j,i).ne.255) then
					if (sign.eq.0) then  ! CounterTotal will be calculated only once (first day where data available) 
						CounterTotal=CounterTotal+1
					endif
					if (SnowID(j,i).eq.50) then
						counterTerra=counterTerra+1
					endif
					if (SnowID(j,i).eq.200) then
						snowcountTerra=snowcountTerra+1
					endif
					if (SnowID(j,i).eq.50) then
						if (SnowID_AQUA(j,i).ne.50) then
							SnowID(j,i)=SnowID_AQUA(j,i)
						endif
					endif
					if (SnowID(j,i).eq.50) then
						counter=counter+1
					endif
					if (SnowID(j,i).eq.200) then
						snowcount=snowcount+1
					endif
				else
					SnowID(j,i)=NoData   ! nodata pixels
				endif
			enddo
			if (write_output_step1) then
				if (j.eq.1) then
					write(10, '(A50)') header1
					write(10, '(A50)') header2
					write(10, '(A50)') header3
					write(10, '(A50)') header4
					write(10, '(A50)') header5
					write(10, *) 'NODATA_value ', NoData
					write(10,collen) (SnowID(j,i), i=1,NrCols)  ! Format should be changed according to NrCols
				else
					write(10,collen) (SnowID(j,i), i=1,NrCols)  ! Format should be changed according to NrCols
				endif
			endif
		enddo
		sign=1  ! indicator so that CounterTotal will not be calculated again
		deallocate(SnowID_AQUA)
		deallocate(SnowID)
		CounterPersTerra(k)=counterTerra/CounterTotal*100
		SnowPersTerra(k)=snowcountTerra/CounterTotal*100

		CounterPers(k,schritt)=counter/CounterTotal*100
		SnowPers(k,schritt)=snowcount/CounterTotal*100
		counterTerra=0
		snowcountTerra=0
		counter=0
		snowcount=0
		
		close(1)
		close(2)
		close(10)
	enddo
	
	END SUBROUTINE STEP_1