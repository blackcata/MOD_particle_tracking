!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : mod_particle_tracking.f90                                        !
!                                                                              !
!   PURPOSE : Module for tracking the particle's trajectories and attributes   !
!                                                                              !
!                                                             2019.01.29 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!


        MODULE particle_tracking

          USE kinds
          USE pegrid
          IMPLICIT NONE

          INTEGER(iwp) ::  unit_par, num_par
          INTEGER(wp),DIMENSION(:),ALLOCATABLE :: par_id

          SAVE

        CONTAINS
          !--------------------------------------------------------------------!
          !  Function for reading the number of particles which want to track  !
          !--------------------------------------------------------------------!
          FUNCTION READ_PAR_NUM(path,filename)
            INTEGER(iwp) ::  READ_PAR_NUM, unit_list, io
            REAL(wp)     ::  tmp
            CHARACTER(LEN=100),INTENT(IN) :: path, filename

            unit_list = 200
            OPEN(unit_list,FILE=TRIM(path)//TRIM(filename),STATUS='OLD')

            READ_PAR_NUM = 0

            DO 
              READ(unit_list,*,IOSTAT=io) tmp
              IF (io < 0 ) EXIT
              READ_PAR_NUM = READ_PAR_NUM + 1
            END DO 

            CLOSE(unit_list)
          END FUNCTION READ_PAR_NUM

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : MAKE_PAR_FILES                                                !
!                                                                              !
!   PURPOSE : To make each tracking paricles dat files                         !
!                                                             2019.01.29 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!

          SUBROUTINE MAKE_PAR_FILES(path,filename)
            IMPLICIT NONE
            INTEGER(iwp)       :: unit_list, pn
            CHARACTER(LEN=100) :: int_char
            CHARACTER(LEN=100),INTENT(IN) :: path,filename


            ALLOCATE( par_id(1:num_par)  )
            unit_list = 200

            ! Read each particle's id
            OPEN(unit_list,FILE=TRIM(path)//TRIM(filename),STATUS='OLD')
            DO pn = 1,num_par
               READ(unit_list,*) par_id(pn) 
            END DO 
            CLOSE(unit_list)

            ! Make each particle dat file
            DO pn = 1,num_par
                unit_par = 300+myid+pn
                WRITE(int_char,'(I4.4)') pn
                OPEN(unit_par,FILE=TRIM(path)//'prt_traj_'//TRIM(int_char)      &
                                             //'.dat',STATUS='REPLACE')
            END DO 
            
          END SUBROUTINE MAKE_PAR_FILES

        END MODULE
