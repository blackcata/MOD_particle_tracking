!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : main.f90                                                         !
!                                                                              !
!   PURPOSE : Module for tracking the particle's trajectories and attributes   !
!                                                                              !
!                                                             2019.01.29 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!


        PROGRAM main
          USE kinds
          USE particle_tracking

          IMPLICIT NONE
          CHARACTER(LEN=100) :: path, filename

          path= '/home/km109/PALM_60/JOBS/NP_test/OUTPUT/PARTICLE_DATA/'
          filename  = 'particle_list.dat' 
          
          path_par     = path
          filename_par = filename

          num_par = READ_PAR_NUM(path_par,filename_par)
          CALL MAKE_PAR_FILES

        END PROGRAM main
