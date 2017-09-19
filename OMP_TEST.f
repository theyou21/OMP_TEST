        IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      
        PARAMETER(IL = 100, JL = 100, KL = 10)
        DOUBLE PRECISION RES(IL,JL,KL)
        OPEN(9,FILE='/output/out.txt')
          NUM_MP = 1
          call omp_set_num_threads(NUM_MP)
          IF(mod(JL,NUM_MP).eq.0) THEN
              ICHUNK = JL/NUM_MP
          ELSE
              ICHUNK = JL/NUM_MP+1
          ENDIF
      do k = 1,KL
          start = omp_get_wtime()
          !call CPU_TIME(start)
!$omp parallel do private(i,nn,ip,im,jp,jm) schedule(static,ICHUNK)
      do 1 j = 1, JL
      do 2 i = 1, IL
          do nn = 1, 999999
          if (k .ne. 1) then
              ip = min(i+1,IL)
              im = max(i-1,1)
              jp = min(j+1,JL)
              jm = max(j-1,1)
            RES(i,j,k) = RES(ip,j,k-1)*RES(im,j,k-1)*
     *                    RES(i,jp,k-1)*RES(i,jm,k-1)
          else
            RES(i,j,k) = 1.0d0
          endif
          enddo


2     continue
1     continue
         finish = omp_get_wtime()
         !call CPU_TIME(finish)
         write(*,*) 'step = ',k,'cpu time = ',finish-start
         write(9,*) 'step = ',k,'cpu time = ',finish-start
      enddo
      close(9)
      END
