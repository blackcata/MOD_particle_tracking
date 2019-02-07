# Particle Tracking Module
This module is purposed to trace the each particles trajectories from the PALM code. 
This code is easily operated with the user codes in the PALM. 
Following the below process will make each particle's trajectories at the specific folder. 

### Setup 
1. Find the particle id which you want to trace. 
2. Make the id list file(particle_list.dat) with each particle's id number. 
3. Make the specific directories where the particles id list file (particle_list.dat).

### Install 
1. Write USE particle_tracking to user_init.f90 and user_lpm_advec.f90 file. 
2. Set the two path('path_par') and filename('filenmae_par') at the user_init.f90 file. 
3. Add two lines on the end of user_init.f90 

   - num_par = READ_PAR_NUM(path_par,filename_par)
   - CALL MAKE_PAR_FILES

4. Add one line on the end of user_init.f90 

   - CALL PAR_TRAJ_WRITE(ip,jp,kp)
    
5. Modifiy the Makefile 
  - Add mod_particle_tracking.f90 at the SOURCE
  - Add mod_particle_tracking.o to user_init.o & user_lpm_advec.o
  - Add mod_particle_tracking.o's related modules. 
  
    mod_particle_tracking.f90: \
         mod_kinds.o \
         modules.o \
         mod_particle_attributes.o
         
6. Run the PALM model with specific cases.
    
If you want more details,look at the EXAMPLE folder and compare what is different. 
