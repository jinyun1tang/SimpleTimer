      module TimerMod

      implicit none
      private
      save
      integer, parameter :: varlen=30
      integer, parameter :: maxprocs=200
      character(len=varlen) :: procnames(maxprocs)
      real*8 :: tsecs(maxprocs)
      integer :: nprocs
      real*8 :: tinsec
      logical :: init_flag
      integer :: lun
      public :: TStart
      public :: TStop
      public :: WriteTLogs
      public :: TInit,Tinitf
      public :: TClear
      contains
C------------------------------------------------------------
      subroutine TInit(outdir)
      implicit none
      character(len=*), intent(in) :: outdir
      init_flag=.false.
      lun=1234
      nprocs=0
      tsecs(:)=0.
      call system('mkdir -p '//trim(outdir)//'/timing' )
      OPEN(UNIT=lun, FILE=trim(outdir)//"/timing/time.txt"
     2,STATUS='UNKNOWN')

      call gettscal(tinsec)

      end subroutine TInit
C------------------------------------------------------------
      subroutine TClear

      implicit none

      close(lun)

      end subroutine TClear
C------------------------------------------------------------

      subroutine TStart(t1)
C     DESCRIPTION
C     begin timing
      implicit none
      real*8, intent(out) :: t1

      call timec(t1)

      end subroutine TStart
C------------------------------------------------------------
      subroutine TStop(procname,t1)
C     DESCRIPTION:
C     end of timing
      implicit none
      character(len=*), intent(in) :: procname
      real*8, intent(in) :: t1

      integer :: jj
      real*8 :: t2

      call timec(t2)

      if (.not. init_flag)then
      nprocs=nprocs+1
      write(procnames(nprocs),'(A)')trim(procname)
      tsecs(nprocs)=tsecs(nprocs)+(t2-t1)/tinsec
      else
      do jj =1, nprocs
      if(trim(procnames(jj))==trim(procname))then
      tsecs(jj)=tsecs(jj)+(t2-t1)/tinsec
      exit
      endif
      enddo
      endif


      end subroutine TStop
C------------------------------------------------------------
      subroutine WriteTLogs(nsteps)

      implicit none
      integer, intent(in) :: nsteps

      character(len=4) :: fmt1
      character(len=30) :: fmt
      integer :: j

      write(fmt1,'(I3)')nprocs
      fmt='(I20,'//trim(fmt1)//'(A,E30.10))'


      write(lun,fmt)nsteps,(',',tsecs(j),j=1,nprocs)
      tsecs=0.
      end subroutine WriteTLogs
C------------------------------------------------------------
      subroutine Tinitf()

      implicit none

      character(len=4) :: fmt1
      character(len=30) :: fmt
      integer :: j

      init_flag = .true.

      write(fmt1,'(I3)')nprocs
      fmt='(A20,'//trim(fmt1)//'(A,A30))'
      write(lun,fmt)'steps',(',',procnames(j),j=1,nprocs)


      end subroutine Tinitf

      end module TimerMod
