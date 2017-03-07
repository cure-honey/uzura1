    module m_kind
      use, intrinsic :: iso_fortran_env
      public
      integer, parameter :: k2 = int16
      integer, parameter :: k4 = int32
      integer, parameter :: kd = real64
    end module m_kind
    
    module m_file_io ! abstract interface for file 
      implicit none
      type, abstract :: t_file
      contains  
        procedure(p_open ), deferred :: open_file
        procedure(p_close), deferred :: close_file
      end type t_file   
      
      abstract interface
        subroutine p_open(this, fn)
          import 
          class(t_file), intent(in out) :: this
          character(len = *), intent(in) :: fn
        end subroutine p_open
      
        subroutine p_close(this)
          import
          class(t_file), intent(in) :: this
        end subroutine p_close
      end interface
    end module m_file_io