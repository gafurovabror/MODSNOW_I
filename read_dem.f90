subroutine read_dem(DemID, NrCols, NrRows, path_dem_file)

implicit none

integer :: NrRows, NrCols, i, j
integer, dimension(NrRows,NrCols) :: DemID 
character (len=200) :: path_dem_file

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

end subroutine read_dem
