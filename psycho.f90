        module fft_module
          use m_kind
          implicit none
          private
          public :: fft_window
          integer , parameter :: np2 = 9, nn = 2**np2, nn_2 = nn / 2   ! 2^9 = 512
          real(kd), parameter :: pi = 4 * atan(1.0_kd), pi2 = 2 * pi
          real(kd), parameter :: pi2_n = pi2 / nn
          integer, save :: indx(nn) 
          integer, private :: i_
          complex(kd), parameter :: omega(0:*) = [(exp(cmplx(0.0_kd, pi2_n * i_, kind = kd)), i_ = 0, nn - 1)] 
          real(kd)   , parameter :: hann_window(*) = [(0.5_kd * (1.0_kd - cos(pi2_n * i_))  , i_ = 0, nn - 1)]
        contains
          subroutine fft_initialize()
            integer :: i, j2, k, n
            do i = 1, nn
              n = 0
              k = i - 1
              do j2 = np2 - 1, 0, -1
                n = n + mod(k, 2) * 2**j2
                k = k / 2
              end do
              indx(i) = n + 1 ! indx = 1..n
            end do
          end subroutine fft_initialize
 
          subroutine fft_window(x, y)
            real    (kd), intent(in ) :: x(:)
            complex (kd), intent(out) :: y(:)
            logical :: qfirst = .true.
            if (qfirst) then
              qfirst = .false.
              call fft_initialize()
            end if
            y = hann_window * x / nn
            call fft2(y)
          end subroutine fft_window

          subroutine fft2(fft) ! fft_2^np2
            complex (kd), intent(in out) :: fft(:)
            complex (kd) :: tmp1, tmp2, c1
            integer :: i, j, k2, m1, m2, iphase, kn2
            fft = fft(indx)
            do k2 = 1, np2
              kn2 = 2**(k2 - 1)
              do i = 1, nn, 2* kn2
                do j = 1, kn2
                  iphase = 2**(np2 - k2)*(j - 1)
                  c1 = omega( mod(iphase, nn) )
                  m1 = i + j - 1
                  m2 = m1 + kn2
                  tmp1 =      fft(m1)
                  tmp2 = c1 * fft(m2)
                  fft(m1) = tmp1 + tmp2 ! 1 = x^2 ; x = 1 or -1
                  fft(m2) = tmp1 - tmp2
                end do
              end do
            end do
          end subroutine fft2
        end module fft_module

        module m_psycho
          use m_kind
          implicit none
          private
          public :: psychoacoustics
          real(kd), save :: freq_fft(0:256), pseud_bark(0:256), ath_fft(0:256)
          real(kd), save :: crbw_fft(0:256), cbwl_fft(0:256), zn(0:256)
          real(kd), save :: sp(256, 256) ! spreading function for masking
          real(kd), save :: scale
          real(kd), parameter :: alpha = 0.30d0 ! non-linear factor
        contains
          subroutine psychoacoustics(pcm, isample_rate, smr)
            use fft_module
            real (kd), intent(in ) :: pcm(:, :) 
            integer       , intent(in ) :: isample_rate
            real (kd), intent(out) :: smr(:, :)
            complex(kd) :: cfft(512)
            integer :: ichannel, i0, i1
            logical, save :: qfirst = .true.
            if (qfirst) then
              qfirst = .false.
              call init_absolute_threshold(isample_rate)
            end if
            i0 = 161 
            i1 = i0 + 512 - 1
            do ichannel = 1, size(pcm, 2)
              call fft_window(pcm(i0:i1, ichannel), cfft)
              call calc_smr(cfft, smr(:, ichannel))
            end do
          end subroutine
 
          subroutine init_absolute_threshold(isample_rate)
            integer, intent(in) :: isample_rate
            real (kd) :: freq, tmp
            integer :: i, m, i0, i1
            do i = 0, size(freq_fft) - 1
              freq = real(isample_rate, kind = kd) / 2.0_kd / 1000.0_kd &
                   * real(i, kind = kd) / real(size(freq_fft), kind = kd)
              ath_fft(i) = 3.64_kd * freq**(-0.8_kd)  & ! 
                         - 6.5_kd * exp(-0.6_kd * (freq - 3.3_kd)**2) + 0.001_kd * freq**4.0_kd   
              freq = freq * 1000.0_kd ! khz -> hz
              freq_fft(i) = freq
            end do
            scale =  real(isample_rate, kind = kd) / 2 / 256 ! freq to fft-line scale
            crbw_fft = critical_band_width( freq_fft )       ! critical band width in hz
            cbwl_fft = decibel( crbw_fft )                   ! critical band width in log
            do i = 1, 256
              m = nint( crbw_fft(i) / scale )
              i0 = max(i  - m / 2,   1)           !f0 - bw(f0) / 2 
              i1 = min(i0 + m - 1, 256)           !f0 + bw(f0) / 2
              zn(i) = sum( 10.0_kd**( ( cbwl_fft(i0:i1) * alpha - cbwl_fft(i0:i1) ) / 10.0_kd ) ) ! <- normalization factor
            end do

            tmp = 0.0_kd ! pseud bark: integrate critical band  
            do m = 1, 256
              tmp = tmp + ( 1.0_kd / crbw_fft(m - 1) + 1.0_kd / crbw_fft(m) ) / 2 ! trapezoidal integration
              pseud_bark(m) = tmp * scale 
            end do
            ! spreading function
            forall(i = 1:256, m = 1:256) sp(i, m) = spreading( pseud_bark( i ) - pseud_bark( m ) ) ! top normalized to 1.0
          end subroutine init_absolute_threshold

          pure elemental real(kd) function spreading(z)
            real(kd), intent(in) :: z ! pseud-bark
            if ( z > 0.0d0 ) then
              spreading = -25.0_kd * z 
            else
              spreading =  75.0_kd * z 
            end if
            spreading = max(-160.0_kd, spreading)
          end function spreading

          pure elemental real(kd) function critical_band_width(f)
            real(kd), intent(in) :: f ! hz
            real(kd), parameter :: gamma = 0.69_kd
            critical_band_width = 25.0_kd + 75.0_kd * ( 1.0_kd + 1.4_kd * (f / 1000.0_kd)**2 )**gamma 
          !  critical_band_width = 100.0d0 * ( 1.0d0 + 1.0d0 * (f / 1000.0d0)**2 )**gamma 
          end function critical_band_width

          pure elemental real(kd) function decibel(x) 
            real (kd), intent(in) :: x 
            real (kd) :: tmp
            tmp = max(1.0e-100_kd, x)
            decibel = 10.0e0_kd * log10(tmp)
          end function decibel
      
          subroutine calc_smr(cfft, smr)
            complex(kd), intent(in) :: cfft(0:)
            real   (kd), intent(out) :: smr(:)
            real   (kd), parameter :: pi = 4 * atan(1.0_kd), pi2 = 2 * pi ! fortran2003
            real   (kd) :: snr(32), rmnr(32)
            real   (kd) :: xa(0:256), ya(0:256), za(0:256)
            integer :: iband, i, m, i0, i1
            xa = 2 * decibel( abs(cfft(0:256)) ) + cbwl_fft
            ya = 0.0_kd
            do i = 1, 256 ! convolution of spreading function 
              do m = 1, 256                                                                                      ! i maskee, m masker
                ya(i) = ya(i) + 10.0_kd**( ( (sp(i, m) + xa(m) - ath_fft(m)) * alpha - cbwl_fft(m) ) / 10.0_kd ) ! non-linear sum
              end do   
            end do   
            ya = decibel( ya / zn ) + ath_fft * alpha  ! zn: normalization factor
        ! effective spl
            do i = 1, 256
              m = nint( crbw_fft(i) / scale )
              i0 = max(i  - m / 2,   1)       !f0 - bw(f0) / 2 
              i1 = min(i0 + m - 1, 256)       !f0 + bw(f0) / 2
              za(i) = sum( 10.0_kd**( (xa(i0:i1) * alpha - cbwl_fft(i0:i1) ) / 10.0_kd ) ) 
            end do
            za = decibel( za / zn )  ! zn: normalization factor
        ! smr = snr' - mnr'
            do iband = 1, 32
              m = (iband - 1) * 8 + 1
              i0 = m     - nint( crbw_fft(m    ) / 2 / scale ) ! fl - bw(fl) / 2  ; subband [fl..fh] 
              i0 = max(i0,   1)
              i1 = m + 7 + nint( crbw_fft(m + 7) / 2 / scale ) ! fh + bw(fh) / 2 
              i1 = min(i1, 256)
              snr(iband)  = maxval( za(i0:i1) )
              rmnr(iband) = minval( ya(i0:i1) ) - 11.5_kd ! 11.5 masking factor
            end do
            smr = snr - rmnr 
          end subroutine calc_smr
        end module m_psycho