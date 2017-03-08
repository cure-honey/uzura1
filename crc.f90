    module m_crc
      use :: m_kind
      implicit none
      private 
      public :: crc16
      integer, parameter :: igenerator =  32773  !Z'8005' ! 32773  !B'1000000000000101'  ! x^16+x^15+x^2+1
    contains
      subroutine crc16(n, inp, icrc)
        integer, intent(in    ) :: n, inp
        integer, intent(in out) :: icrc
        integer :: i, ibit1, ibit2
        do i = n - 1, 0, -1
          ibit1 = ibits(inp,   i, 1)
          ibit2 = ibits(icrc, 15, 1)
          icrc  = iand(int(Z'0000FFFF'), ishft(icrc, 1)) ! trim to 16bit  
          if ( ieor(ibit1, ibit2) == 1 ) icrc = ieor(icrc, igenerator)
        end do
      end subroutine crc16  
    end module m_crc
