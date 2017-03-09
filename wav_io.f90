    module m_wav
      use m_kind
      use m_file_io
      implicit none
      private
      public :: t_wavfile, t_fmt
      type :: t_fmt
        sequence
        character(4):: chunk_id
        integer(int32) :: chunk_size
        integer(int16) :: format_id, channels
        integer(int32) :: sampling_rate
        integer(int32) :: bytes_per_sec
        integer(int16) :: block_size, bits_per_sample
      end type t_fmt

      type :: t_data
        sequence
        character(4) :: chunk_id
        integer(int32)  :: chunk_size
        !! integer(int16), allocatable :: pcm16(:)
      end type t_data

      type :: t_riffwav
        sequence
        character(4)  :: chunk_id
        integer(int32):: chunk_size
        character(4)  :: formattag
        type (t_fmt ) :: fmt
        type (t_data) :: dat
      end type t_riffwav

      type, extends(t_file) :: t_wavfile
        private
        integer :: iunit 
        integer :: ipos
        character(:), allocatable :: fn
        type (t_riffwav) :: riff
        integer(int16), allocatable :: pcm16(:) ! <- data of data_chunk
      contains
        procedure, public :: open_file  => open_wav_file
        procedure, public :: close_file => close_wav_file
        final             :: destroy_file
        procedure, public :: pcm1frame
        procedure, public :: get_channel
        procedure, public :: get_sampling_rate
        procedure, public :: get_data_size
        procedure, public :: get_play_time
      end type t_wavfile
    contains
      subroutine open_wav_file(this, fn)
        class(t_wavfile), intent(in out) :: this
        character(len = *), intent(in) :: fn
        integer :: io
        this%fn = fn
        open(newunit = this%iunit, file = trim(this%fn), access = 'stream', iostat = io, status = 'old', form = 'unformatted')
        if (io /= 0) then
          write(*, *) 'i/o error ', io, ' occuerred. file =', this%iunit, 'file name ', fn
          stop 'check output file!'
        end if
        ! read wave file little endian assumed
        associate (riff => this%riff, fmt => this%riff%fmt, dat => this%riff%dat)  
          ! riff-wave chunk
          read(this%iunit) riff 
          if ( riff%chunk_id  /= 'RIFF' ) stop 'this is not RIFF file'
          if ( riff%formattag /= 'WAVE' ) stop 'this RIFF file is not in WAVE format'
          ! fmt chunk
          if ( fmt%chunk_id   /= 'fmt ' ) stop 'fmt chunk not found'
          if ( fmt%format_id  /=  1     ) stop 'unknown wave format' ! 1 linear pcm
          if ( fmt%bits_per_sample /= 16) stop 'not 16bit data'
          select case ( fmt%channels )
            case (1)
              write(*, '(a, i3, a, i6, a)') 'monoral', fmt%bits_per_sample, 'bit sampling rate', fmt%sampling_rate, 'hz '
            case (2)
              write(*, '(a, i3, a, i6, a)') 'stereo' , fmt%bits_per_sample, 'bit sampling rate', fmt%sampling_rate, 'hz '
            case default
              stop 'wave channels must be 1 or 2'
          end select
          ! data chunk
          if (dat%chunk_id /= 'data') then
            do      
              inquire(this%iunit, pos = this%ipos)
              this%ipos = this%ipos + dat%chunk_size  ! skip non-data chunk
              read(this%iunit, pos = this%ipos, iostat = io) dat
              if (io == -1) stop 'end of file encounterd while searching for a data chunk'
              if (dat%chunk_id == 'data') exit
            end do
          end if
          ! now file position is at the beginning of pcm data
          allocate( this%pcm16( dat%chunk_size / 2 ) )
          ! read whole pcm data
          read(this%iunit) this%pcm16 
        end associate
        return
      end subroutine open_wav_file 

      subroutine close_wav_file(this)           
        class(t_wavfile), intent(in) :: this
        close(this%iunit)
      end subroutine close_wav_file
  
      subroutine destroy_file(this) ! finalization routine
        type(t_wavfile), intent(in) :: this
        call this%close_file()
      end subroutine destroy_file  
  
      subroutine pcm1frame(this, pcm) ! copy from array
        class(t_wavfile), intent(in) :: this
        real(kd), intent(in out) :: pcm(:, :)
        integer, save :: ipos = 1
        pcm = eoshift(pcm, -384, 0.0_kd, 1)
        select case (this%riff%fmt%channels)
          case (1) !mono
            pcm(384:1:-1, 1) = real( this%pcm16(ipos:ipos + 384 - 1), kind = kd ) / 2**15 
            ipos = ipos + 384
          case (2) !stereo
            pcm(384:1:-1, 1) = real( this%pcm16(ipos    :ipos + 2 * 384 - 1:2), kind = kd ) / 2**15 ! L 16bit int
            pcm(384:1:-1, 2) = real( this%pcm16(ipos + 1:ipos + 2 * 384 - 1:2), kind = kd ) / 2**15 ! R
            ipos = ipos + 2 * 384
          case default
            stop 'ichannel must be 1 or 2: subroutine wav_get' 
        end select
      end subroutine pcm1frame
     ! getters
      integer function get_channel(this)
        class(t_wavfile), intent(in) :: this
        get_channel = this%riff%fmt%channels ! mono:1 stereo:2
      end function get_channel
 
      integer function get_sampling_rate(this)
        class(t_wavfile), intent(in) :: this
        get_sampling_rate = this%riff%fmt%sampling_rate ! in Hz
      end function get_sampling_rate
 
      integer function get_data_size(this)
        class(t_wavfile), intent(in) :: this
        get_data_size = this%riff%dat%chunk_size ! in bytes
      end function get_data_size
    
      integer function get_play_time(this) result(isec)
        class(t_wavfile), intent(in) :: this
        isec = this%riff%dat%chunk_size / this%riff%fmt%bytes_per_sec ! play time in sec
      end function get_play_time
    end module m_wav
