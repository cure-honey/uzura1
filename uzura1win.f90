      program uzura1
        use m_kind
        use m_mpg
        use m_wav
        use m_mpg_io
        use m_polyphase
        use m_psycho
        use m_layer1
        implicit none
        integer , allocatable :: iscale_factor(:, :), isubband(:, :, :), ialloc_bits(:, :)
        real(kd), allocatable :: pcm(:, :), smr(:, :)
        integer :: max_bits, itot_bits
        integer :: nchannel, iframe = 1, ntotal_frames
        character(len = :), allocatable :: file_name, fn_in, fn_out
        type(t_mpg) :: mpg
        type(t_subband), allocatable :: subb
        type(t_wavfile), allocatable :: wav
        type(t_mpgfile), allocatable :: mp1
        call get_option(mpg, file_name)
        fn_in  = trim(file_name) // '.wav'
        fn_out = trim(file_name) // '.mp1'
        allocate(wav, mp1)
        call wav%open_file(fn_in ) ! read whole wav file
        call mp1%open_file(fn_out)
        call pr_info(mpg)
        call pr_play_time( wav%get_play_time() )
        nchannel = wav%get_channel()  
        allocate( pcm(864, nchannel), smr(32, nchannel) )
        allocate( iscale_factor(32, nchannel), isubband(32, 12, nchannel), ialloc_bits(32, nchannel) )
        allocate(subb)
        call subb%init(nchannel)
        ntotal_frames = wav%get_data_size() / (mpeg_frame_size(mpg%layer) * nchannel * 2)
        pcm = 0.0d0
        do while(iframe <= ntotal_frames )
          call get_maxbits(mpg, max_bits)
          call mp1%clear_bit_buff(max_bits)
          call mp1%encode_header(mpg)
          itot_bits = 32
          call wav%pcm1frame(pcm) 
          call subb%polyphase_filter12(pcm(:, :)) 
          smr = 0
          call psychoacoustics(pcm, wav%get_sampling_rate(), smr)
          iscale_factor = isubband_normalization(subb%subband)
          itot_bits = itot_bits + 4 * 32 * nchannel               ! 4*32*nch bits required for the scale factor bits : 
          if (mpg%icrc == 0) itot_bits = itot_bits + 16 ! 16bits required for the crc
          call bit_allocation(smr, max_bits, itot_bits, ialloc_bits)
          if (mpg%icrc == 0) call mp1%encode_crc(mpg, ialloc_bits)
          isubband = iquantization(ialloc_bits, subb%subband, iscale_factor)
          call mp1%encode_body(ialloc_bits, iscale_factor, isubband)
          call mp1%write_bits_1frame(max_bits)
          iframe = iframe + 1
          if (mod(iframe, 200) == 0) call update_status(iframe, ntotal_frames) 
        end do
        write(*, *) 'total frames', iframe - 1, '/', ntotal_frames
        deallocate(subb)
        deallocate(wav) ! close file & deallocte
        deallocate(mp1) ! close file & deallocte
      contains
        subroutine pr_info(mpg)
          type (t_mpg), intent(in) :: mpg
          write(*, *) 'uzura1 (mpeg-1 audio/layer-I encoder) ver.0.4 '
          write(*, *) 'psychoacoustic model ', mpeg_psy_names(mpg%ipsychoacoustic), &
                      ' bit rate (kbps)', mpeg_bit_rates(mpg%ibit_rate, mpg%layer)
          if (mpg%icrc == 0) write(*, *) 'crc16 error protection enabled'
        end subroutine pr_info

        subroutine pr_play_time(itot_sec)
          integer, intent(in) :: itot_sec
          integer :: ihour, imin, isec
          ihour =          itot_sec / 3600
          imin  =      mod(itot_sec, 3600) / 60
          isec  = mod( mod(itot_sec, 3600) , 60 )
          write(*, '(a, i3, a, i2, a, i2)') ' playtime ', ihour, ':', imin, ':', isec
        end subroutine pr_play_time
        
        subroutine update_status(iframe, itot_frames)
          integer, intent(in) :: iframe, itot_frames
          integer :: it(8), ielapsed, iel_min, iel_sec
          integer, save :: istart
          logical, save :: qfirst = .true.
          real    :: percent
          character (len = 10) :: time, date, zone
          call date_and_time(date, time, zone, it)
          if (qfirst) then
           istart   = it(5) * 3600 + it(6) * 60 + it(7)
           qfirst   = .false.
          end if
          ielapsed = it(5) * 3600 + it(6) * 60 + it(7) - istart
          iel_min  =     ielapsed / 60
          iel_sec  = mod(ielapsed , 60)
          percent = real(100 * iframe) / real(itot_frames)
          write(*, '(a, f6.2, a, i4, 2(a, i2), 3(a, i2.2), a, i4.2, a, i2.2, a)')  &
                '+processed...', percent, '%  ', &
                it(1), '/', it(2), '/', it(3), ' ', it(5), ':', it(6), ':', it(7), &
                ' time elapsed ', iel_min, 'min ', iel_sec, 'sec'
        end subroutine update_status

        subroutine get_maxbits(mpg, max_bits)
          type(t_mpg), intent(in out) :: mpg
          integer , intent(out) :: max_bits
          integer        , save :: islot_size
          real (kind = 8), save :: padding, fslot_size
          logical        , save :: qfirst = .true.
          if (qfirst) then
            qfirst = .false.
            padding = 0.0d0
            call calc_slot_size(mpg, islot_size, fslot_size)
          end if
          padding = padding + fslot_size
          if (padding > 1) then
            mpg%ipadding = 1
            padding = padding - 1.0d0
          else
            mpg%ipadding = 0
          end if
          max_bits = ( islot_size + mpg%ipadding ) * 32
        end subroutine get_maxbits  

        subroutine calc_slot_size(mpg, islot_size, fslot_size)
          type(t_mpg), intent(in) :: mpg
          integer , intent(out) :: islot_size
          real(kd), intent(out) :: fslot_size
          real(kd) :: aslot_size
          aslot_size = 12.0d0 * 1000.0d0 * real(mpeg_bit_rates(mpg%ibit_rate, mpg%layer), kind = kd) &
                     / real(mpeg_sample_rates(mpg%isample_rate), kind = kd)
          aslot_size = 1.0d-3 * anint(1.0d3 * aslot_size) 
          islot_size = int(aslot_size)
          fslot_size = aslot_size - islot_size
        end subroutine calc_slot_size
        
        subroutine get_option(mpg, fn_in)
          type(t_mpg), intent(in out) :: mpg
          character (len = :), allocatable, intent(out) :: fn_in
          character (len = 80) :: buffer
          character (len =  6) :: fmt
          integer :: narg, iarg, length
          iarg = 0
          narg = command_argument_count() + 1
          do
            iarg = iarg + 1
            if (iarg >= narg) call print_option()
            call get_command_argument(iarg, buffer)
            if (buffer(1:1) /= '-') exit  
            select case(trim(buffer))
              case ('-b') 
                iarg = iarg + 1
                if ( iarg >= narg ) call print_option()
                call get_command_argument(iarg, buffer, length)
                write(fmt, '(a, i1, a)') '(i', length, ')' 
                read(buffer, fmt) mpg%ibit_rate
                if (mpg%ibit_rate < 1 .or. mpg%ibit_rate > 14) call print_option()
              case ('-crc') 
                mpg%icrc = 0       ! crc16 on 
              case ('-c') 
                mpg%icopyright = 1 ! copyrigt on
              case ('-o') 
                mpg%ioriginal = 1  ! original on
              case default
                call print_option()
            end select
          end do
          fn_in = trim(buffer)
        end subroutine get_option   
 
       subroutine print_option()
         write(*, *) 'Usage : uzura -option file_name '
         write(*, *) '      : file_name.wav -> file_name.mp1'
         write(*, *) 'Option: -b 1..14  bitrate '
         write(*, *) '        -crc      CRC16 error protection on'
         write(*, *) '        -c        copyright flag on'
         write(*, *) '        -o        original  flag on'
         stop
       end subroutine print_option
     end program uzura1
