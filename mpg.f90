    module m_mpg
      use m_kind
      implicit none
      public 
      type :: t_mpg
        integer :: mtype           =  3 ! 0:mpeg2.5, 1:---, 2:mpeg2, 3:mpeg1
        integer :: layer           =  3 ! layer { 1 = III, 2 = II, 3 = I }
        integer :: ibit_rate       = 12 ! 192 * 2 = 384kbps 
        integer :: isample_rate         ! 0:44.1kHz 1:48kHz 2:32kHz 3:reserved
        integer :: ipsychoacoustic =  0
        integer :: iemphasis       =  0
        integer :: ipadding        =  0 
        integer :: icrc            =  1 ! CRC16  0 enabled / 1 disabled
        integer :: mode            =  0 ! 0:stereo 1:joint_stereo 2:dual_channel 3:single_channel
        integer :: iprivate_bit    =  0 ! unused 
        integer :: mode_extension  =  0 ! 0:4-31 1:8-31 2:12-31 3:16-31 intensity stereo
        integer :: icopyright      =  0 ! 0:     1:copyright protected
        integer :: ioriginal       =  1 ! 0:copy 1:original
      end type t_mpg
      !mpeg1 / audio
      integer, parameter :: mpeg_frame_size(3)      = [1152, 1152, 384]
      integer, parameter :: mpeg_sample_rates(0:3)  = [44100, 48000, 32000, 0]
      integer, parameter :: mpeg_bit_rates(0:14, 3) = &
               reshape( [ 0, 32, 40, 48,  56,  64,  80,  96, 112, 128, 160, 192, 224, 256, 320,   &
                          0, 32, 48, 56,  64,  80,  96, 112, 128, 160, 192, 224, 256, 320, 384,   &
                          0, 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 414, 448 ], &
                        [15, 3] )
      character (len = *), parameter :: mpeg_mode_names(0:*)    = [character(len = 8)::'stereo', 'j-stereo', 'dual-ch', 'mono']
      character (len = *), parameter :: mpeg_layer_names(0:*)   = [character(len = 3)::'', 'III', 'II', 'I']
      character (len = *), parameter :: mpeg_version_names(0:*) = [character(len = 7)::'MPEG2.5', '', 'MPEG-II', 'MPEG-I']
      character (len = *), parameter :: mpeg_psy_names(0:*)     = [character(len = 6)::'', '', '', '']
      character (len = *), parameter :: mpeg_demp_names(0:*)    = [character(len = 7)::'none', '50/15us', '', 'CITT']
    end module m_mpg
    