program points_to_panels
	implicit none
	integer :: n,i,j,z
  integer,allocatable,dimension(:) :: m
	double precision,allocatable,dimension(:) :: x,y,ppx,ppy,poix,poiy


	open (100, file='points2.dat', status='old')

		allocate(x(n))
		allocate(y(n))

  	read(100,*) n
  		do i = 1,n
    		read(100,*) x(i), y(i)
			end do
						
	close(100)

	allocate(ppx(n))
	allocate(ppy(n))
	allocate(m(n))
	allocate(poix(n))
	allocate(poiy(n))

	write(*,*) 'how many panels points to points' 
  do i=1,n
   	read(*,*) m(i)
  end do

	do i = 1, n-1
    ppx(i)=(x(i+1)-x(i))/m(i)
    ppy(i)=(y(i+1)-y(i))/m(i)
	end do    

	ppx(n)=(x(1)-x(n))/m(n)
	ppy(n)=(y(1)-y(n))/m(n)

	open(110,file='groove_plate_vortex.dat', status='old')

		do i = 1, n
			write(110,'(f12.8,f12.8)')  x(i), y(i)
				z = m(i)-1
	  	do j = 1, z
				poix(j) = x(i) + ppx(i)*j
				poiy(j) = y(i) + ppy(i)*j
			  write(110,'(f12.8,f12.8)')  poix(j), poiy(j)
	  	end do
		end do

	close(110)

end program