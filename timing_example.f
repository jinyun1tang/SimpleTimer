      program main

      use TimerMod, only : TStart
      use TimerMod, only : TStop
      use TimerMod, only : WriteTLogs
      use TimerMod, only : TInit,Tinitf
      use TimerMod, only : TClear
      implicit none
      integer :: inum
      real*8 :: t1,t2
      real*8 :: tinsec
      integer :: jj,nstep
      real*8 :: f

      call TInit('./')

      nstep=0
      do while(nstep<10)
      nstep=nstep+1

      call TStart(t1)
      do jj =1, 100000
      f=sqrt(dble(jj))
      enddo
      call TStop('sqrt',t1)

      call TStart(t1)
      do jj =1, 100000
      f=sin(dble(jj))+cos(dble(jj))
      enddo
      call TStop('sincos',t1)

      call TStart(t1)
      do jj =1, 100000
      f=exp(dble(jj)/100000.)
      enddo
      call TStop('exp',t1)


      if(nstep==1)call Tinitf()

      call WriteTLogs(nstep)
      enddo
      call Tclear()
      end program main
