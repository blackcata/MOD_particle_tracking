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
          USE particle_attributes,                                             &
            ONLY: grid_particles, number_of_particles, particles,              &
                  particle_advection_start, prt_count
          USE control_parameters,                                              &
            ONLY: simulated_time

          IMPLICIT NONE

          INTEGER(iwp) ::  unit_par, num_par
          INTEGER(wp),DIMENSION(:),ALLOCATABLE :: par_id
          CHARACTER(LEN=100) :: path_par,filename_par

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
!   PURPOSE : To make each tracking particles dat files                        !
!                                                             2019.01.29 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!

          SUBROUTINE MAKE_PAR_FILES
            IMPLICIT NONE
            INTEGER(iwp)       :: unit_list, pn
            CHARACTER(LEN=100) :: int_char

            ALLOCATE( par_id(1:num_par)  )
            unit_list = 200

            ! Read each particle's id
            OPEN(unit_list,FILE=TRIM(path_par)//TRIM(filename_par),STATUS='OLD')
            DO pn = 1,num_par
               READ(unit_list,*) par_id(pn) 
            END DO 
            CLOSE(unit_list)

            ! Make each particle dat file
            DO pn = 1,num_par
                unit_par = 300+myid+pn
                WRITE(int_char,'(I4.4)') pn
                OPEN(unit_par,FILE=TRIM(path_par)//'prt_traj_'//TRIM(int_char)  &
                                             //'.dat',STATUS='REPLACE')
            END DO 
            
          END SUBROUTINE MAKE_PAR_FILES

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : PAR_TRAJ_WRITE                                                !
!                                                                              !
!   PURPOSE : To write each tracking particles dat files                       !
!                                                             2019.01.29 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!

          SUBROUTINE PAR_TRAJ_WRITE(kp,jp,ip)
             IMPLICIT NONE
 
             INTEGER(iwp) :: ip  !< index of particle grid box, x-direction
             INTEGER(iwp) :: jp  !< index of particle grid box, x-direction
             INTEGER(iwp) :: kp  !< index of particle grid box, x-direction
             INTEGER(iwp) ::  n  !< particle index
             INTEGER(iwp) ::  nb !< index of sub-box particles are sorted in
             INTEGER(iwp) ::  pn !< the number of particles want to track

             INTEGER(iwp), DIMENSION(0:7)  ::  start_index !< start particle index for current sub-box
             INTEGER(iwp), DIMENSION(0:7)  ::  end_index   !< start particle index for current sub-box

             number_of_particles = prt_count(kp,jp,ip)
             particles => grid_particles(kp,jp,ip)%particles(1:number_of_particles)

             start_index = grid_particles(kp,jp,ip)%start_index
             end_index   = grid_particles(kp,jp,ip)%end_index

             DO  nb = 0, 7
                DO  n = start_index(nb), end_index(nb)
                  DO pn = 1,num_par
                      unit_par = 300+myid+pn
                      IF ( particles(n)%id == par_id(pn) ) THEN 
                          WRITE(unit_par,"(F15.7,2X,4(E15.7,2X))") simulated_time,       &
                                                                   particles(n)%x,       &
                                                                   particles(n)%y,       &
                                                                   particles(n)%z,       &
                                                                   particles(n)%radius
                      END IF

                  ENDDO 
                ENDDO
             ENDDO

          END SUBROUTINE PAR_TRAJ_WRITE
        END MODULE
