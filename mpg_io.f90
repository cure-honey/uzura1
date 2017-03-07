    module m_mpg_io
      use m_kind
      use m_file_io
      use m_mpg
      use m_crc
      implicit none
      private 
      public :: t_mpgfile
      type, extends(t_file) :: t_mpgfile
        private
        integer :: iunit 
        integer :: ipos
        character(:), allocatable :: fn
        character (len = 20000) :: bit_string
      contains
        procedure :: open_file  => open_mpg_file
        procedure :: close_file => close_mpg_file
        final     :: destroy_file
        procedure :: write_bits_1frame
        procedure :: clear_bit_buff
        procedure :: encode_header
        procedure :: encode_crc
        procedure :: encode_body
        procedure :: put_bits
        procedure :: put_bits_c
      end type t_mpgfile    
    contains
      subroutine open_mpg_file(this, fn)
        class(t_mpgfile),   intent(in out) :: this
        character(len = *), intent(in) :: fn
        integer :: io
        open(newunit = this%iunit, file = fn, iostat = io, status = 'unknown', recordtype = 'stream')
        if (io /= 0) then
          write(*, *) 'i/o error ', io, ' occuerred. file =', this%iunit, 'file name ', fn
          stop 'check output file!'
        end if
      end subroutine open_mpg_file
      
      subroutine close_mpg_file(this)
        class(t_mpgfile), intent(in) :: this
        close(this%iunit)
      end subroutine close_mpg_file
      
      subroutine destroy_file(this)
        type(t_mpgfile), intent(in) :: this
        call this%close_file()
      end subroutine destroy_file  
     
      subroutine write_bits_1frame(this, n)
        class(t_mpgfile), intent(in) :: this
        integer, intent(in) :: n
        integer :: i, j, ipos, m
        character(len = 4) ::cm
        equivalence (m, cm) ! integer*4 assumed for m
        ipos = 0
        do i = 1, n, 8
          m = 0
          do j = 1, 8
            ipos = ipos + 1
            if (ipos > len(this%bit_string)) exit
            if (this%bit_string(ipos:ipos) == '1') m = m + 2**(8 - j)
          end do
          write(this%iunit, '(a1)', advance = 'no') cm(1:1)   ! little endian assumed
        end do
      end subroutine write_bits_1frame
    
      subroutine clear_bit_buff(this, n)
        class(t_mpgfile), intent(in out) :: this
        integer, intent(in) :: n
        this%bit_string = repeat(' ', n)
      end subroutine clear_bit_buff
    
      subroutine encode_header(this, mpg)
        class(t_mpgfile), intent(in out) :: this
        type (t_mpg)    , intent(in    ) :: mpg
        this%ipos = 1 ! reset position to the first bit
        call this%put_bits_c('11111111111'      )  !sync word
        call this%put_bits(2, mpg%mtype         )  !mpeg1
        call this%put_bits(2, mpg%layer         )  !layer 1
        call this%put_bits(1, mpg%icrc          )  !crc check no
        call this%put_bits(4, mpg%ibit_rate     )  !bitrate 
        call this%put_bits(2, mpg%isample_rate  )  !sampling frequency 44.1
        call this%put_bits(1, mpg%ipadding      )  !ipadding
        call this%put_bits(1, mpg%iprivate_bit  )  !private bit : unused
        call this%put_bits(2, mpg%mode          )  !stereo
        call this%put_bits(2, mpg%mode_extension)  !mode
        call this%put_bits(1, mpg%icopyright    )
        call this%put_bits(1, mpg%ioriginal     )
        call this%put_bits(2, mpg%iemphasis     )
      end subroutine encode_header
   
      subroutine encode_crc(this, mpg, ialloc_bits)
        class(t_mpgfile), intent(in out) :: this
        type (t_mpg)    , intent(in    ) :: mpg
        integer         , intent(in    ) :: ialloc_bits(:, :)
        integer :: iband, ichannel, icrc
        icrc = Z'0000FFFF' ! initialize crc 
        call crc16(4, mpg%ibit_rate     , icrc)
        call crc16(2, mpg%isample_rate  , icrc)
        call crc16(1, mpg%ipadding      , icrc)
        call crc16(1, mpg%iprivate_bit  , icrc)
        call crc16(2, mpg%mode          , icrc)
        call crc16(2, mpg%mode_extension, icrc)
        call crc16(1, mpg%icopyright    , icrc)
        call crc16(1, mpg%ioriginal     , icrc)
        call crc16(2, mpg%iemphasis     , icrc)
        do iband = 1, 32
          do ichannel = 1, size(ialloc_bits, 2)
            call crc16(4, ialloc_bits(iband, ichannel), icrc)
          end do
        end do
        call this%put_bits(16, icrc)
      end subroutine encode_crc
 
      subroutine encode_body(this, ialloc_bits, iscale_factor, isubband)
        class(t_mpgfile), intent(in out) :: this
        integer, intent(in) :: ialloc_bits(:, :), iscale_factor(:, :), isubband(:, :, :)
        integer :: iband, ichannel, j, k
        do iband = 1, 32
          do ichannel = 1, size(ialloc_bits, 2)
            call this%put_bits(4, ialloc_bits(iband, ichannel) )
          end do
        end do
        do iband = 1, 32
          do ichannel = 1, size(ialloc_bits, 2)
            if (ialloc_bits(iband, ichannel) /= 0) call this%put_bits(6, iscale_factor(iband, ichannel) )
          end do
        end do
        do j = 1, 12
          do iband = 1, 32
            do ichannel = 1, size(ialloc_bits, 2)
              k = ialloc_bits(iband, ichannel)
              if (k /= 0) call this%put_bits(k + 1, isubband(iband, j, ichannel) )
            end do
          end do
        end do
      end subroutine encode_body
 
      subroutine put_bits(this, n, inp)
        class(t_mpgfile), intent(in out) :: this
        integer, intent(in) :: n, inp
        integer :: i, m
        associate (ipos => this%ipos, bit_string => this%bit_string)
          do i = 1, n
            m = 2**(n - i)
            if (mod(inp / m, 2) == 1) then
              bit_string(ipos:ipos) = '1'
            else
              bit_string(ipos:ipos) = '0'
            end if
            ipos = ipos + 1
          end do
        end associate  
      end subroutine put_bits
 
      subroutine put_bits_c(this, str)
        class(t_mpgfile), intent(in out) :: this
        character(len = *), intent(in) :: str
        integer :: i
        associate (ipos => this%ipos, bit_string => this%bit_string)
          do i = 1, len_trim(str)
            if (str(i:i) /= '0' .and. str(i:i) /= '1') &
              call abort('invalid string: subroutine put_bit_c')
            bit_string(ipos:ipos) = str(i:i)
            ipos = ipos + 1
          end do
        end associate      
      end subroutine put_bits_c
    end module m_mpg_io