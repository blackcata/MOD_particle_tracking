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

          SAVE

        CONTAINS
          ! Reading the total number of the particles which want to track
          FUNCTION READ_PAR_NUM(path,filename)
            INTEGER(iwp) ::  READ_PAR_NUM, unit_par, io
            REAL(wp)     ::  tmp
            CHARACTER(LEN=100),INTENT(IN) :: path, filename

            unit_par = 200
            OPEN(unit_par,FILE=TRIM(path)//TRIM(filename),STATUS='OLD')

            READ_PAR_NUM = 0

            DO 
              READ(unit_par,*,IOSTAT=io) tmp
              IF (io < 0 ) EXIT
              READ_PAR_NUM = READ_PAR_NUM + 1
            END DO 
          END FUNCTION READ_PAR_NUM

        END MODULE
