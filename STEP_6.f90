	! In this step cloud removal occurs according to time series of each pixel coverage. 

	subroutine STEP_6(StDay, NrDays, NrRows, NrCols, NoData, path_output, Year, Jahr, collen, CounterPers, SnowPers, write_output_step6, extention_in, extention_out)

   	integer :: StDay, NrDays, NrRows, NrCols, i, j , k , t, exists, stat, schritt, NoData, sign
	integer, dimension(:,:), allocatable :: SnowID, countC, countS, countL, PreviousID, b, d, WaterMask
	integer, dimension(:,:,:), allocatable ::   MeltDay, SnowStartDay
	integer :: diff_m, diff_m_old, diff_s, diff_s_old, snowmelt, snowstart 
	character (len=3) :: day
	character (len=4) :: Year, Last_Year
	character (len=5) :: FolderName
	character (len=15) :: collen
	character (len=200):: path_output
	character (len=50) :: header1, header2, header3, header4, header5, header6
	character (len=50) :: extention_in, extention_out
	real ::  counter, CounterTotal, CounterPers(366,6), snowcount, SnowPers(366,6)
	logical :: write_output_step6


	schritt=6

	write(*,*) 'Processing STEP_6...'

	FolderName='Step6'
	
	INQUIRE(FILE = trim(path_output)//'\'//Year//'\'//FolderName, EXIST = exists )    !this looks for wether FolderName exists!
	
	if (exists==0) then  ! if FolderName does no exist, new FolderName is created.
		call system('mkdir '//trim(path_output)//'\'//Year//'\'//FolderName)
	endif

		! following part will read 30 day data from last year to assign previous pixel coverage on first day of current year.
	allocate(PreviousID(NrRows,NrCols))
	allocate(WaterMask(NrRows,NrCols))

	write(Last_Year,'(I4.4)') Jahr-1

	do k=335,NrDays
		allocate(SnowID(NrRows,NrCols))
		write(day,'(I3.3)') k
		
		open(1, file=trim(path_output)//'\'//Last_Year//'\Step5/'//Last_Year//day//trim(extention_out), STATUS='old', IOSTAT=stat)  !reads results from folder Step2 as input for this step 3.

		if (stat.ne.0) then 
			deallocate(SnowID)
			close(1)
			cycle
		endif
		read(1,'(A50)') header1
		read(1,'(A50)') header2
		read(1,'(A50)') header3
		read(1,'(A50)') header4
		read(1,'(A50)') header5
		read(1,'(A50)') header6

		do j=1,NrRows
			read(1,*) (SnowID(j,i), i=1,NrCols)
			do i=1,NrCols
				if (SnowID(j,i).ne.50) then
					PreviousID(j,i)=SnowID(j,i)
				endif
				if (SnowID(j,i).eq.37.or.SnowID(j,i).eq.100) then
					WaterMask(j,i)=SnowID(j,i)
				endif
			enddo
		enddo
		deallocate(SnowID)
		close(1)
	enddo

	open(88, file=trim(path_output)//'\'//Year//'\MeltDay'//Year//'.asc')
	open(888, file=trim(path_output)//'\'//Year//'\AccumDay'//Year//'.asc')

	allocate(countC(NrRows,NrCols))
	allocate(countS(NrRows,NrCols))
	allocate(countL(NrRows,NrCols))
	allocate(b(NrRows,NrCols))
	allocate(d(NrRows,NrCols))
	allocate(MeltDay(52,NrRows,NrCols))
	allocate(SnowStartDay(52,NrRows,NrCols))

	CountS=0.
	CountC=0.
	countL=0.
	b=0.
	d=0.
	
	do  k=StDay,NrDays

		allocate(SnowID(NrRows,NrCols))

		write(*,*) Year, ' step 6  ', k, '  1'
		write(day,'(I3.3)') k
		
		open(1, file=trim(path_output)//'\'//Year//'\Step5/'//Year//day//trim(extention_out), STATUS='old', IOSTAT=stat)  !reads results from folder Step2 as input for this step 3.

		if (stat.ne.0) then        ! this identifies the error when the file is not found
			CounterPers(k,schritt)=NoData
			SnowPers(k,schritt)=NoData
			deallocate(SnowID)
			close(1)
			cycle
		endif
		
		read(1,'(A50)') header1
		read(1,'(A50)') header2
		read(1,'(A50)') header3
		read(1,'(A50)') header4
		read(1,'(A50)') header5
		read(1,'(A50)') header6

		do j=1,NrRows
			read(1,*) (SnowID(j,i), i=1,NrCols)
			do i=1,NrCols
				if (SnowID(j,i).ne.NoData.or.SnowID(j,i).ne.255) then
					
					if (SnowID(j,i).eq.50) then
						countC(j,i)=countC(j,i)+1
					endif

					if (SnowID(j,i).eq.200.and.PreviousID(j,i).eq.200) then
						countS(j,i)=countS(j,i)+1
					endif

					if (SnowID(j,i).eq.50.and.PreviousID(j,i).eq.200) then
						countS(j,i)=countS(j,i)+1
!						if (SnowStartDay(b(j,i),j,i).lt.MeltDay(d(j,i),j,i).and.b(j,i).gt.0.and.d(j,i).gt.0) then
!							b(j,i)=b(j,i)+1
!							SnowStartDay(b(j,i),j,i)=k-1
!						endif
					endif

					if (SnowID(j,i).eq.25.and.PreviousID(j,i).eq.200.and.countS(j,i).ge.1) then !.and.countC(j,i).ge.1) then
						countS(j,i)=0
						d(j,i)=d(j,i)+1
						MeltDay(d(j,i),j,i)=k
						countL(j,i)=countL(j,i)+1
						countC(j,i)=0
					end if



					if (SnowID(j,i).eq.25.and.PreviousID(j,i).eq.25) then
						countL(j,i)=countL(j,i)+1
					endif

					if (SnowID(j,i).eq.50.and.PreviousID(j,i).eq.25) then
						countL(j,i)=countL(j,i)+1
!						if (MeltDay(d(j,i),j,i).lt.SnowStartDay(b(j,i),j,i).and.d(j,i).gt.0.and.b(j,i).gt.0) then
!							d(j,i)=d(j,i)+1
!							MeltDay(d(j,i),j,i)=k-1
!						endif
							
					endif

					if (SnowID(j,i).eq.200.and.PreviousID(j,i).eq.25.and.countL(j,i).ge.1) then !.and.countC(j,i).ge.1) then
						countL(j,i)=0
						b(j,i)=b(j,i)+1
						SnowStartDay(b(j,i),j,i)=k
						countS(j,i)=countS(j,i)+1
						countC(j,i)=0
					end if



					if (SnowID(j,i).ne.50) then
						PreviousID(j,i)=SnowID(j,i)
					endif
				else
					MeltDay(1,j,i)=NoData
					SnowStartDay(1,j,i)=NoData						
				endif
			enddo
		enddo
		close(1)
		deallocate(SnowID)
	enddo
	
	do k=StDay, NrDays
		allocate(SnowID(NrRows,NrCols))

		write(*,*) Year, ' step 6  ', k, '  1'
		write(day,'(I3.3)') k
		
		open(1, file=trim(path_output)//'\'//Year//'\Step5/'//Year//day//trim(extention_out), STATUS='old', IOSTAT=stat)  !reads results from folder Step2 as input for this step 3.

		if (stat.ne.0) then        ! this identifies the error when the file is not found
			CounterPers(k,schritt)=NoData
			SnowPers(k,schritt)=NoData
			deallocate(SnowID)
			close(1)
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
				diff_m=1000   !initial value to check the minimum value
				diff_s=1000
				if (SnowID(j,i).eq.50.and.WaterMask(j,i).ne.37.and.WaterMask(j,i).ne.100.and.SnowStartDay(1,j,i).gt.0.and.MeltDay(1,j,i).gt.0) then
					
					if (k.lt.SnowStartDay(1,j,i).and.k.lt.MeltDay(1,j,i).and.SnowStartDay(1,j,i).gt.MeltDay(1,j,i)) then
						SnowID(j,i)=200
						cycle
					endif
					if (k.lt.SnowStartDay(1,j,i).and.k.lt.MeltDay(1,j,i).and.SnowStartDay(1,j,i).lt.MeltDay(1,j,i)) then
						SnowID(j,i)=25
						cycle
					endif

					do l=1,52
						diff_m=ABS(k-MeltDay(l,j,i))
						diff_s=ABS(k-SnowStartDay(l,j,i))
						
						if (diff_m.lt.diff_m_old.and.MeltDay(l,j,i).gt.0) then
							snowmelt=MeltDay(l,j,i)
						endif
						if (diff_s.lt.diff_s_old.and.SnowStartDay(l,j,i).gt.0) then
							snowstart=SnowStartDay(l,j,i)
						endif
							
						diff_m_old=diff_m
						diff_s_old=diff_s
					enddo
					if (k.gt.snowstart.and.k.lt.snowmelt) then
						SnowID(j,i)=200
					endif
					if (k.gt.snowmelt.and.k.lt.snowstart) then
						SnowID(j,i)=25
					endif
										
				endif
	!			if (SnowID(j,i).ne.37.and.SnowID(j,i).ne.100) then
	!				write(777, '(a12,2x,55(I3,2x))') 'MeltDay', j, i, (MeltDay(l,j,i), l=1,52)
	!				write(777, '(a12,2x,55(I3,2x))') 'SnowStartDay', j, i, (SnowStartDay(m,j,i), m=1,52)
	!			endif
	
			enddo
			if (write_output_step6) then
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
	deallocate(SnowID)
	enddo


!	open(2222,file='W:\WP2\Preprocessing\P_remote_sensing\P_Modis_Snow_other_basins\aksu\output\2000\Step5\2000232_aksu_cloud_free.asc')
!	allocate(SnowID(NrRows,NrCols))
!	read(2222,*)
!	read(2222,*)
!	read(2222,*)
!	read(2222,*)
!	read(2222,*)
!	read(2222,*)

!	do j=1,NrRows
!		read(2222,*) (SnowID(j,i),i=1,NrCols)
!		do i=1,NrCols
!			if (SnowID(j,i).ne.37.and.SnowID(j,i).ne.100) then
!				write(777, '(a12,2x,55(I3,2x))') 'MeltDay', j, i, (MeltDay(l,j,i), l=1,52)
!				write(777, '(a12,2x,55(I3,2x))') 'SnowStartDay', j, i, (SnowStartDay(m,j,i), m=1,52)
!			endif
!		enddo
!	enddo
	
	deallocate(SnowID)

	deallocate(MeltDay)
	deallocate(SnowStartDay)
	deallocate(countC)
	deallocate(countS)
	deallocate(countL)
	deallocate(b)
	deallocate(d)

end subroutine STEP_6


