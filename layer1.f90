    module m_layer1
      use m_kind
      implicit none
      private
      public :: isubband_normalization, bit_allocation, iquantization
      integer, private :: i_
      ! ISO Table B.1 -- Layer I,II scalefactors
      real(kd), parameter :: scale_factor(0:63) = &
                              [(1.0e-14_kd * anint(1.0e14_kd * 2.0_kd * 2.0_kd**(-real(i_, kd) / 3.0_kd )), i_ = 0, 63)]
      
      ! ISO Table C.2 -- Layer I  Signal-to-Noise Ratios
      real(kd), parameter :: snr(0:*) =[ 0.00_kd,  7.00_kd, 16.00_kd, 25.28_kd, 31.59_kd, 37.75_kd, 43.84_kd, & 
                                        49.89_kd, 55.93_kd, 61.96_kd, 67.98_kd, 74.01_kd, 80.03_kd, 86.05_kd, 92.01_kd]
      ! empirically better SNR = 20 * log(2^n)
      !real(kd), parameter :: snr(0:14) = [(20.0_kd * log10(2.0_kd) * i_, i_ = 0, 14)]
            
      ! ISO Table C.3 -- Layer I Quantization Coefficients
      real(kd), parameter :: tmp(14) = [( 1.0_kd / 2.0_kd**real(i_ + 1, kd), i_ = 1, 14)]
      real(kd), parameter :: a(14) = [ 1.0e-9_kd * anint(1.0e9_kd * (1.0e0_kd - tmp))]
      real(kd), parameter :: b(14) = [-1.0e-9_kd * anint(1.0e9_kd * tmp)]
    contains
      pure function isubband_normalization(subband) result(iscale_factor)
        real(kd), intent(in) :: subband(0:, 0:, 0:)
        integer :: iscale_factor(0:31, 0:size(subband, 3))
        integer :: ichannel, iband
        do ichannel = 0, size(subband, 3)
          do iband = 0, 31
            iscale_factor(iband, ichannel) = iscale_fac_sub(subband(iband, :, ichannel)) 
          end do
        end do
      contains
        pure integer function iscale_fac_sub(x) result(ires)
          real(kd), intent(in) :: x(0:)
          do ires = 62, 0, -1
            if (maxval(abs(x), dim = 1) < scale_factor(ires)) exit  
          end do 
        end function iscale_fac_sub
      end function isubband_normalization

      subroutine bit_allocation(smr, max_bits, itot_bits, ialloc_bits)
        real(kd), intent(in) :: smr(:, :)
        integer , intent(in) :: max_bits
        integer , intent(   out) :: ialloc_bits(:, :)
        integer , intent(in out) :: itot_bits
        integer  :: max_pos(2), k
        real(kd) :: rmnr(32, 2)
        ialloc_bits = 0
        rmnr = smr - snr(0)
        do 
          select case (max_bits - itot_bits) 
            case (0:11) ! no more bits left 
              exit  
            case (12:29) ! cannot allocate to new place
              max_pos = maxloc(rmnr, mask = ialloc_bits < 13 .AND. ialloc_bits /= 0)
            case (30:)   !
              max_pos = maxloc(rmnr, mask = ialloc_bits < 13)
              if (rmnr( max_pos(1), max_pos(2) ) < -0.44d0) then ! threshold margin 10% -0.46 20% -1.0
                max_pos = maxloc(rmnr, mask = ialloc_bits == 0)
                if (max_pos(1) == 0) max_pos = maxloc(rmnr, mask = ialloc_bits < 13)
              end if
          case default 
            stop 'impossible!: bit_allocation'
          end select
          if (max_pos(1) == 0) exit
          k = ialloc_bits( max_pos(1), max_pos(2) )
          if (k == 0) then  
           itot_bits = itot_bits + 6 + 12 * 2 ! first time: scalefactor 6 bits + 12 * 2 bits   
          else
           itot_bits = itot_bits + 12         ! 12 * 1 bit 
          end if
          ialloc_bits( max_pos(1), max_pos(2) ) = k + 1
          rmnr       ( max_pos(1), max_pos(2) ) = smr( max_pos(1), max_pos(2) ) - snr(k +1)
        end do
      end subroutine bit_allocation

      pure function iquantization(ialloc_bits, subband, iscale_factor) result(isubband)
        integer , intent(in) :: ialloc_bits(0:, 0:), iscale_factor(0:, 0:)
        real(kd), intent(in) :: subband(0:, 0:, 0:)
        integer :: isubband(0:31, 0:11, 0:ubound(subband, 3))
        integer :: ichannel, iband, k, m
        real(kd) :: s(12)
        do ichannel = 0, ubound(subband, 3)
          do iband = 0, 31  
            k = ialloc_bits(iband, ichannel)
            m = iscale_factor(iband, ichannel)
            if (k /= 0) THEN 
              s = a(k) * subband(iband, :, ichannel)  / scale_factor(m) + b(k) 
              isubband(iband, :, ichannel) = floor( (s + 1.0_kd) * 2**k )
            else
              isubband(iband, :, ichannel) = 0
            end if          
          end do
        end do  
      end function iquantization
    end module m_layer1