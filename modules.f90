!> @file modules.f90
!------------------------------------------------------------------------------!
! This file is part of the PALM model system.
!
! PALM is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License as published by the Free Software
! Foundation, either version 3 of the License, or (at your option) any later
! version.
!
! PALM is distributed in the hope that it will be useful, but WITHOUT ANY
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
! A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along with
! PALM. If not, see <http://www.gnu.org/licenses/>.
!
! Copyright 1997-2019 Leibniz Universitaet Hannover
!------------------------------------------------------------------------------!
!
! Current revisions:
! ------------------
! 
! 
! Former revisions:
! -----------------
! $Id: modules.f90 3668 2019-01-14 12:49:24Z maronga $
! Removed most_method
! 
! 3648 2019-01-02 16:35:46Z suehring
! -surface_data_output +surface_output
! 
! 3636 2018-12-19 13:48:34Z raasch
! nopointer option removed
! 
! 3597 2018-12-04 08:40:18Z maronga
! Added flag parameter do_output_at_2m for automatic output of 2m-temperature
! 
! 3589 2018-11-30 15:09:51Z suehring
! Move the control parameter "salsa" from salsa_mod to control_parameters 
! (M. Kurppa)
! 
! 3582 2018-11-29 19:16:36Z suehring
! dom_dwd_user, Schrempf:
! -uv_exposure flag, UV model is now part of biometeorology_mod
! 
! 3543 2018-11-20 17:06:15Z suehring
! +type_x_byte, type_y_byte
! 
! 3542 2018-11-20 17:04:13Z suehring
! +run_zone
! 
! 3473 2018-10-30 20:50:15Z suehring
! +virtual_measurement
! 
! 3472 2018-10-30 20:43:50Z suehring
! Add indoor model (kanani, srissman, tlang)
! 
! 3467 2018-10-30 19:05:21Z suehring
! Add biometeorology
! 
! 3435 2018-10-26 18:25:44Z gronemeier
! +mask_k_over_surface, mask_surface
! 
! 3422 2018-10-24 19:01:57Z gronemeier
! bugfix: increase number of blanks in output string
! 
! 3421 2018-10-24 18:39:32Z gronemeier
! Renamed output variables
! +surface_data_output
! 
! 3355 2018-10-16 14:03:34Z knoop
! (from branch resler)
! Increase dimension of uv_heights etc.
! 
! 3302 2018-10-03 02:39:40Z raasch
! +u/v_stokes_zu, u/v_stokes_zw
! 
! 3298 2018-10-02 12:21:11Z kanani
! Minor formatting/clean-up (kanani)
! Added some variables for time treatment (Russo)
!
! 3294 2018-10-01 02:37:10Z raasch
! ocean renamed ocean_mode
! 
! 3289 2018-09-28 10:23:58Z suehring
! +num_mean_inflow_profiles
! 
! 3288 2018-09-28 10:23:08Z suehring
! Modularization of all bulk cloud physics code components
! 
! 3240 2018-09-12 12:04:40Z Giersch
! max_pr_user_tmp has been defined as a control variable because it is not 
! local anymore
! 
! 3235 2018-09-07 14:06:15Z sward
! Added global variable dim_size_agtnum to own module. Necessary to avoid
! circular dependency in agent output.
! 
! 3232 2018-09-07 12:21:44Z raasch
! references to mrun replaced by palmrun, and updated
! 
! 3198 2018-08-15 09:23:10Z sward
! Added multi_agent_system_end and multi_agent_system_start
! 
! 3183 2018-07-27 14:25:55Z suehring
! Rename offline nesting variables:
! -inflow_l, inflow_n, inflow_r, inflow_s, 
!  nest_bound_l, nest_bound_n, nest_bound_r, nest_bound_s, nest_domain, forcing, 
!  force_bound_l, force_bound_n, force_bound_r, force_bound_s, outflow_l, 
!  outflow_n, outflow_r, outflow_s
! +bc_dirichlet_l, bc_dirichlet_n, bc_dirichlet_n, bc_dirichlet_r, 
!  bc_radiation_l, bc_radiation_n, bc_radiation_n, bc_radiation_r, child_domain 
!  nesting_offline
! 
! 3182 2018-07-27 13:36:03Z suehring
! Default value of dz_max has changed to a more uncommon value of 999 (value 
! of dz_max can not be part of dz values when using new stretching procedure)
! 
! 3159 2018-07-20 11:20:01Z sward
! Added multi agent system
! 
! 3157 2018-07-19 21:08:49Z maronga
! added use_free_convection_scaling
! 
! 3129 2018-07-16 07:45:13Z gronemeier
! add target attribute to km and kh, necessary for output in tcm_data_output_3d
! 
! 3120 2018-07-11 18:30:57Z gronemeier
! +les_dynamic
! 
! 3083 2018-06-19 14:03:12Z gronemeier
! set dt_3d = 0.01
! 
! 3065 2018-06-12 07:03:02Z Giersch
! Variables concerning stretching introduced or revised
! 
! 3045 2018-05-28 07:55:41Z Giersch
! z_max_do2d removed
! 
! 3026 2018-05-22 10:30:53Z schwenkel
! Changed the name specific humidity to mixing ratio, since we are computing
! mixing ratios.
! 
! 3014 2018-05-09 08:42:38Z maronga
! Added default values of u_max, v_max, and w_max to avoid floating invalid
! during spinup
! 
! 3004 2018-04-27 12:33:25Z Giersch
! precipitation_rate removed
! 
! 3003 2018-04-23 10:22:58Z Giersch
! The inversion height is defined as a global variable now which belongs to the 
! module statistics 
! 
! 2968 2018-04-13 11:52:24Z suehring
! +topo_min_level
! 
! 2964 2018-04-12 16:04:03Z raasch
! *_time_count variables are all initialized with zero now
! 
! 2918 2018-03-21 15:52:14Z gronemeier
! -l_grid, -l_wall
! 
! 2906 2018-03-19 08:56:40Z Giersch
! Module control_parameters has been extended with ENVIRONMENT variables 
! read/write_svf 
! 
! 2894 2018-03-15 09:17:58Z Giersch
! _prerun flags were removed, Control paramters restart_string and length have
! been added
! 
! 2881 2018-03-13 16:24:40Z suehring
! Added flag for switching on/off calculation of soil moisture
! 
! 2797 2018-02-08 13:24:35Z suehring
! +ghf_av
! 
! 2776 2018-01-31 10:44:42Z Giersch
! Variable synthetic_turbulence_generator has been abbreviated and _prerun flags
! for skipping module related restart data has beed introduced
! 
! 2765 2018-01-22 11:34:58Z maronga
! Set initial value for time_since_reference_point
! 
! 2746 2018-01-15 12:06:04Z suehring
! +plant_canopy
! 
! 2742 2018-01-12 14:59:47Z suehring
! +tsurf_av
! 
! 2735 2018-01-11 12:01:27Z suehring
! +r_a_av
! 
! 2718 2018-01-02 08:49:38Z maronga
! Corrected "Former revisions" section
! 
! 2696 2017-12-14 17:12:51Z kanani
! Change in file header (GPL part)
! Implementation of uv exposure model (FK)
! + turbulence closure variables (control_parameters)
! + arrays for prognostic equation of disspiation (arrays_3d)
! + km_av, kh_av (TG)
! Implementation of chemistry module (FK)
! -lod
! +topo_distinct (MS)
! 
! 2669 2017-12-06 16:03:27Z raasch
! CONTIGUOUS-attribut added to 3d pointer arrays,
! coupling_char extended to LEN=8
! 
! 2575 2017-10-24 09:57:58Z maronga
! Renamed phi -> latitude, moved longitude from radiation model to modules
! 
! 2563 2017-10-19 15:36:10Z Giersch
! Variable wind_turbine was added to control_parameters
! 
! 2550 2017-10-16 17:12:01Z boeske
! complex_terrain namelist parameter added
! 
! 2508 2017-10-02 08:57:09Z suehring
! Change default value for pt/q/s/sa_vertical_gradient_level
! 
! 2499 2017-09-22 16:47:58Z kanani
! Default changed to fft_method = 'temperton-algorithm'
! 
! 2408 2017-09-05 15:47:53Z gronemeier
! Changed default value of mg_cycles from -1 to 4.
! 
! 2375 2017-08-29 14:10:28Z schwenkel
! Moved mass_of_solute, molecular_weight_of_solute, molecular_weight_of_water,
! vanthoff back from particle attributes because they can now also be used in
! bulk microphysics. 
! Added aerosol_bulk, aerosol_nacl, aerosol_c3h4o4, aerosol_nh4no3
! 
! 2372 2017-08-25 12:37:32Z sward
! y_shift namelist parameter added
! 
! 2339 2017-08-07 13:55:26Z gronemeier
! corrected timestamp in header
! 
! 2338 2017-08-07 12:15:38Z gronemeier
! moved 1d-model varaibles to own module model_1d_mod
! 
! 2337 2017-08-07 08:59:53Z gronemeier
! -old_dt_1d
! +l1d_diss
! 
! 2326 2017-08-01 07:23:24Z gronemeier
! Updated variable descriptions
! 
! 2320 2017-07-21 12:47:43Z suehring
! -ptnudge, qnudge, tnudge, td_lsa_lpt, td_lsa_q, td_sub_lpt, td_sub_q, ug_vert,
!  vg_vert, unudge, vnudge, wsubs_vert, shf_surf, p_surf, pt_surf, q_surt,
!  qsws_surf, tmp_tnudge, timenudge, time_surf, time_vert
! 
! 2300 2017-06-29 13:31:14Z raasch
! default value for host changed to '????', default value for loop_optimization
! changed to 'cache', default value for termination_time_needed set to 35.0
! 
! 2298 2017-06-29 09:28:18Z raasch
! missing variable descriptions have been added,
! type of write_binary changed from CHARACTER to LOGICAL
! -plot_precision, plot_3d_precision, return_addres, return_username,
! avs_data_file, exchange_mg, sendrecvcound_yxd, sendrecv_in_background,
! port_name, profile_number, cross_ts_numbers, cross_ts_number_count,
! dots_crossindex, dots_index, cross_ts_uymax, cross_ts_uymax_computed,
! cross_ts_uymin, cross_ts_uymin_computed
! 
! 2296 2017-06-28 07:53:56Z maronga
! Added parameters for model spinup
! 
! 2292 2017-06-20 09:51:42Z schwenkel
! Implementation of new microphysic scheme: cloud_scheme = 'morrison' 
! includes two more prognostic equations for cloud drop concentration (nc)  
! and cloud water content (qc). 
! 
! 2277 2017-06-12 10:47:51Z kanani
! Added doxygen comments for variables/parameters,
! removed unused variables dissipation_control, do2d_xy_n, do2d_xz_n, do2d_yz_n, 
! do3d_avs_n, lptnudge, lqnudge, lunudge, lvnudge, lwnudge, skip_do_avs, 
! sums_up_fraction_l.
! 
! 2259 2017-06-08 09:09:11Z gronemeier
! Implemented synthetic turbulence generator
!
! 2256 2017-06-07 13:58:08Z suehring
! Change default value of zeta_min to -20
! Increase dimension for wall_heatflux, etc. 
! 
! 2233 2017-05-30 18:08:54Z suehring
!
! 2232 2017-05-30 17:47:52Z suehring
! Renamed wall_flags_0 and wall_flags_00 into advc_flags_1 and advc_flags_2, 
! respectively. Moreover, introduced further flag array wall_flags_0. 
!
! Adjustments for new topography concept:
!   -fwxm, fwxp, fwym, fwyp, fxm, fxp, fym, fyp, rif_wall, wall_e_x, wall_e_y, 
!   -wall_v, wall_u, wall_w_x, wall_w_y, wall_qflux, wall_sflux, wall_nrflux, 
!   -wall_qrflux
!
! Adjustments for new surface concept:
!   +land_surface
!   -z0, z0h, z0q, us, ts, qs, qsws, nrs, nrsws, qrs, qrsws, ssws, ss, saswsb 
!   -nzb_diff_u, nzb_diff_v, nzt_diff
!   -uswst, vswst, tswst, sswst, saswst, qswst, qrswst, nrswst, qswst_remote
!
! Generic tunnel setup:
!   +tunnel_height, tunnel_length, tunnel_width_x, tunnel_width_y, 
!   +tunnel_wall_depth
!
! Topography input via netcdf
!   +lod
! 
! 2200 2017-04-11 11:37:51Z suehring
! -monotonic_adjustment
! 
! 2174 2017-03-13 08:18:57Z maronga
! Changed default values for most_method to 'newton'
! 
! 2118 2017-01-17 16:38:49Z raasch
! -acc_rank, background_communication, i_left, i_right, j_south, j_north,
!  num_acc_per_node, on_device
! 
! 2107 2017-01-09 12:21:49Z kanani
! Preparation for doxygen comments (Giersch)
! 
! 2050 2016-11-08 15:00:55Z gronemeier
! Implement turbulent outflow condition
! 
! 2037 2016-10-26 11:15:40Z knoop
! Anelastic approximation implemented
! 
! 2031 2016-10-21 15:11:58Z knoop
! renamed variable rho to rho_ocean and rho_av to rho_ocean_av
! 
! 2011 2016-09-19 17:29:57Z kanani
! +urban_surface, +lsf_exception, +varnamelength
! 
! 2007 2016-08-24 15:47:17Z kanani
! Increased DIMENSION of data_output, data_output_user, do2d, do3d
! 
! 2000 2016-08-20 18:09:15Z knoop
! Forced header and separation lines into 80 columns
! 
! 1992 2016-08-12 15:14:59Z suehring
! +constant_top_scalarflux, top_scalarflux
! default of bc_s_t adjusted
! 
! 1968 2016-07-18 12:01:49Z suehring
! Changed dimension for MPI-datatypes type_x_int and type_y_int
! 
! 1960 2016-07-12 16:34:24Z suehring
! Separate humidity and passive scalar
! +bc_s_t_val, diss_s_s, diss_l_s, flux_s_s, flux_l_s, s, sp, s1, s2, s3, ssws_av, 
!  s_init, s_surf, sums_wsss_ws_l, ss, ssws, sswst, ts_m, wall_sflux
! +constant_scalarflux, ibc_s_b, ibc_s_t, s_vertical_gradient_level_ind
!
! Unused variables removed
! -gamma_x, gamma_y, gamma_z, var_x, var_y, var_z
!
! Change initial values (in order to unify gradient calculation): 
! pt_vertical_gradient_level, sa_vertical_gradient_level
!
! 1957 2016-07-07 10:43:48Z suehring
! +fl_max, num_leg, num_var_fl, num_var_fl_user, var_fl_max, virtual_flight
! 
! 1918 2016-05-27 14:35:57Z raasch
! default timestep switched from -1.0 to +1.0 in order to avoid wrong sign of
! initially calculated divergence
!
! 1906 2016-05-24 14:38:08Z suehring
! default value of mg_switch_to_pe0_level changed to -1
!
! 1849 2016-04-08 11:33:18Z hoffmann
! bfactor, mass_of_solute, molecular_weight_of_solute, molecular_weight_of_water,
! vanthoff moved to mod_particle_attributes.
! dt_micro and several cloud_parameters moved to microphysics_mod.
! 1d-microphysics profiles moved to microphysics_mod.
!
! 1845 2016-04-08 08:29:13Z raasch
! -nzb_2d
!
! 1833 2016-04-07 14:23:03Z raasch
! spectra parameter moved to spectra module
!
! 1831 2016-04-07 13:15:51Z hoffmann
! curvature_solution_effects removed
! turbulence renamed collision_turbulence, drizzle renamed 
! cloud_water_sedimentation
!
! 1822 2016-04-07 07:49:42Z hoffmann
! icloud_scheme removed. microphysics_sat_adjust, microphysics_kessler,
! microphysics_seifert added.
!
! 1815 2016-04-06 13:49:59Z raasch
! cpp-directive for decalpha removed
!
! 1808 2016-04-05 19:44:00Z raasch
! MPI module used by default on all machines
!
! 1804 2016-04-05 16:30:18Z maronga
! Removed code for parameter file check (__check)
! 
! 1788 2016-03-10 11:01:04Z maronga
! Added roughness length for moisture (z0q)
!
! 1786 2016-03-08 05:49:27Z raasch
! module spectrum moved to new separate module
!
! 1783 2016-03-06 18:36:17Z raasch
! netcdf variables moved to the netcdf-interface module
!
! 1779 2016-03-03 08:01:28Z raasch
! coupling_char extended to LEN=3
!
! 1764 2016-02-28 12:45:19Z raasch
! some reformatting
!
! 1762 2016-02-25 12:31:13Z hellstea
! +nest_* variables, size of volume_flow arrays increased by one element
!
! 1738 2015-12-18 13:56:05Z raasch
! +mean_surface_level_height
!
! 1695 2015-10-27 10:03:11Z maronga
! Removed rif (forgotten in last revision)
! 
! 1693 2015-10-27 08:35:45Z maronga
! Renamed zp -> z_mo
! 
! 1691 2015-10-26 16:17:44Z maronga
! Renamed Obukhov length. Added ol, removed rif. Increased number of profiles 
! (pr_palm). Changed default values for dissipation_1d to 'detering' and 
! (mixing_length_1d to 'blackadar'. Added most_method. rif_min and rif_max
! renamed to zeta_min and zeta_max and new values assigned.
! 
! 1682 2015-10-07 23:56:08Z knoop
! Code annotations made doxygen readable 
! 
! 1677 2015-10-02 13:25:23Z boeske
! +ngp_yz_int, type_xz_int, type_yz_int
! 
! 1666 2015-09-23 07:31:10Z raasch
! +user_interface_current_revision, user_interface_required_revision in
! control_parameters
!
! 1639 2015-08-31 14:46:48Z knoop
! Bugfix: string 'unknown' extended to match LEN=13 
!
! 1575 2015-03-27 09:56:27Z raasch
! +ngp_xz
!
! 1560 2015-03-06 10:48:54Z keck
! +recycling_yshift
! 
! 1557 2015-03-05 16:43:04Z suehring
! +monotonic_adjustment
! 
! 1551 2015-03-03 14:18:16Z maronga
! Increased pr_palm to 120. Increased length of dots_unit and dots_label to 13
! digits. Increased length of domask, do2d, and do3d to 20 digits.
! 
! 1496 2014-12-02 17:25:50Z maronga
! Renamed "radiation" -> "cloud_top_radiation"
! 
! 1484 2014-10-21 10:53:05Z kanani
! Changes due to new module structure of the plant canopy model:
!   canopy-model related parameters/variables moved to module 
!   plant_canopy_model_mod
! 
! 1468 2014-09-24 14:06:57Z maronga
! Adapted for use on up to 6-digit processor cores.
! Increased identifier string length for user-defined quantities to 20.
! 
! 1450 2014-08-21 07:31:51Z heinze
! ntnudge from 100 to 1000 increased to allow longer simulations
! 
! 1431 2014-07-15 14:47:17Z suehring
! +var_d
! 
! 1429 2014-07-15 12:53:45Z knoop
! +ensemble_member_nr to prepare the random_generator for ensemble runs
!
! 1382 2014-04-30 12:15:41Z boeske
! Renamed variables which store large scale forcing tendencies
! pt_lsa -> td_lsa_lpt, pt_subs -> td_sub_lpt, 
! q_lsa  -> td_lsa_q,   q_subs  -> td_sub_q
! 
! 1365 2014-04-22 15:03:56Z boeske
! Usage of large scale forcing enabled:
! increase pr_palm from 90 to 100 to allow for more standard profiles
! + ngp_sums_ls, pt_lsa, pt_subs, q_lsa, q_subs, tmp_tnudge, sums_ls_l, 
! use_subsidence_tendencies
! 
! 1361 2014-04-16 15:17:48Z hoffmann
! tend_* removed
! call_microphysics_at_all_substeps added
! default of drizzle set to true
! 
! 1359 2014-04-11 17:15:14Z hoffmann
! particle_attributes moved to mod_particle_attributes.f90
! 
! 1353 2014-04-08 15:21:23Z heinze
! REAL constants provided with KIND-attribute
! 
! 1327 2014-03-21 11:00:16Z raasch
! REAL constants defined as wp-kind
! -avs_output, data_output_format, do3d_compress, iso2d_output, netcdf_output
!
! 1320 2014-03-20 08:40:49Z raasch
! ONLY-attribute added to USE-statements,
! kind-parameters added to all INTEGER and REAL declaration statements,
! kinds are defined in new module kinds,
! old module precision_kind is removed,
! revision history before 2012 removed,
! comment fields (!:) to be used for variable explanations added to
! all variable declaration statements
!
! 1318 2014-03-17 13:35:16Z raasch
! module cpulog moved to new separate module-file
! interface for cpu_log removed
!
! 1314 2014-03-14 18:25:17Z suehring
! + log_z_z0, number_of_sublayers, z0_av_global 
! 1308 2014-03-13 14:58:42Z fricke
! +ntdim_2d_xy, ntdim_2d_xz, ntdim_2d_yz, ntdim_3d
!
! 1257 2013-11-08 15:18:40Z raasch
! set default values for grid indices of maximum velocity components
! u|v|w_max_ijk
!
! 1241 2013-10-30 11:36:58Z heinze
! Usage of nudging enabled 
! +nudging, ntnudge, ptnudge, qnudge, tnudge, unudge, vnudge, wnudge
! increase pr_palm from 80 to 90 to allow for more standard profiles
!
! Enable prescribed time depenend surface fluxes and geostrophic wind read in
! from external file LSF_DATA
! +large_scale_forcing, lsf_surf, lsf_vert, nlsf, time_surf, shf_surf, qsws_surf,
!  pt_surf, q_surf, p_surf, time_vert, ug_vert, vg_vert, wsubs_vert
!
! 1221 2013-09-10 08:59:13Z raasch
! wall_flags_0 changed to 32bit int, +wall_flags_00,
! +rflags_s_inner, rflags_invers
!
! 1216 2013-08-26 09:31:42Z raasch
! +transpose_compute_overlap,
! several variables are now defined in the serial (non-parallel) case also
!
! 1212 2013-08-15 08:46:27Z raasch
! +tri
!
! 1179 2013-06-14 05:57:58Z raasch
! +reference_state, ref_state, use_initial_profile_as_reference, vpt_reference,
! use_reference renamed use_single_reference_value
!
! 1159 2013-05-21 11:58:22Z fricke
! -bc_lr_dirneu, bc_lr_neudir, bc_ns_dirneu, bc_ns_neudir
! +use_cmax
!
! 1128 2013-04-12 06:19:32Z raasch
! +background_communication, i_left, i_right, j_north, j_south, req, req_count,
! send_receive, sendrecv_in_background, wait_stat
!
! 1115 2013-03-26 18:16:16Z hoffmann
! unused variables removed
!
! 1113 2013-03-10 02:48:14Z raasch
! +on_device
!
! 1111 2013-03-08 23:54:10Z raasch
! +tric, nr_timesteps_this_run
!
! 1106 2013-03-04 05:31:38Z raasch
! array_kind renamed precision_kind, pdims defined in serial code
! bugfix: default value assigned to coupling_start_time
!
! 1095 2013-02-03 02:21:01Z raasch
! FORTRAN error in r1092 removed
!
! 1092 2013-02-02 11:24:22Z raasch
! character length in some derived type changed for better alignment
!
! 1065 2012-11-22 17:42:36Z hoffmann
! + c_sedimentation, limiter_sedimentation, turbulence, a_1, a_2, a_3, b_1, b_2, 
! + b_3, c_1, c_2, c_3, beta_cc
!
! bottom boundary condition of qr, nr changed from Dirichlet to Neumann
!
! 1053 2012-11-13 17:11:03Z hoffmann
! necessary expansions according to the two new prognostic equations (nr, qr) 
! of the two-moment cloud physics scheme:
! +*_init, flux_l_*, diss_l_*, flux_s_*, diss_s_*, *sws, *swst, tend_*, *, *_p
! +t*_m, *_1, *_2, *_3, *_av, bc_*_b, bc_*_t, ibc_*_b, ibc_*_t, bc_*_t_val, 
! +*_vertical_gradient, *_surface_initial_change, *_vertical_gradient_level, 
! +*_vertical_gradient_level_ind, *_surface, constant_waterflux_*,  
! +cloud_scheme, icloud_scheme, surface_waterflux_*, sums_ws*s_ws_l, wall_*flux
!
! constants for the two-moment scheme:
! +a_vent, a_term, b_vent, b_term, c_evap, c_term, cof, eps_sb, k_cc, k_cr, k_rr, 
! +k_br, kappa_rr, kin_vis_air, mu_constant_value, nc, pirho_l, dpirho_l, rho_1,
! +schmidt, schmidt_p_1d3, stp, x0, xmin, xmax, dt_precipitation, w_precipitation
!
! steering parameters for the two_moment scheme:
! +mu_constant, ventilation_effect
!
! 1036 2012-10-22 13:43:42Z raasch
! code put under GPL (PALM 3.9)
!
! 1031 2012-10-19 14:35:30Z raasch
! +output_format_netcdf
!
! 1015 2012-09-27 09:23:24Z raasch
! +acc_rank, num_acc_per_node,
! -adjust_mixing_length
!
! 1010 2012-09-20 07:59:54Z raasch
! pointer free version can be generated with cpp switch __nopointer
!
! 1003 2012-09-14 14:35:53Z raasch
! -grid_matching, nxa, nya, etc., nnx_pe, nny_pe, spl_*
!
! 1001 2012-09-13 14:08:46Z raasch
! -asselin_filter_factor, cut_spline_overshoot, dt_changed, last_dt_change,
! last_dt_change_1d, long_filter_factor, overshoot_limit_*, ups_limit_*
! several pointer/target arrays converted to normal ones
!
! 996 2012-09-07 10:41:47Z raasch
! -use_prior_plot1d_parameters
!
! 978 2012-08-09 08:28:32Z fricke
! +c_u_m, c_u_m_l, c_v_m, c_v_m_l, c_w_m, c_w_m_l,
! +bc_lr_dirneu, bc_lr_neudir, bc_ns_dirneu, bc_ns_neudir
! -km_damp_x, km_damp_y, km_damp_max, outflow_damping_width
! +z0h, z0h_av, z0h_factor, z0h1d
! +ptdf_x, ptdf_y, pt_damping_width, pt_damping_factor 
!
! 964 2012-07-26 09:14:24Z raasch
! -cross_linecolors, cross_linestyles, cross_normalized_x, cross_normx_factor,
! cross_normalized_y, cross_normy_factor, cross_pnc_local,
! cross_profile_numbers, cross_profile_number_counter, cross_uxmax,
! cross_uxmax_computed, cross_uxmax_normalized,
! cross_uxmax_normalized_computed, cross_uxmin, cross_uxmin_computed,
! cross_uxmin_normalized, cross_uxmin_normalized_computed, cross_uymax,
! cross_uymin, cross_xtext, dopr_crossindex, dopr_label, linecolors, linestyles,
! nz_do1d, profil_output, z_max_do1d, z_max_do1d_normalized
!
! 951 2012-07-19 14:22:52Z hoffmann
! changing profile_columns and profile_rows
!
! 940 2012-07-09 14:31:00Z raasch
! +neutral
!
! 927 2012-06-06 19:15:04Z raasch
! +masking_method
!
! 880 2012-04-13 06:28:59Z raasch
! gathered_size, subdomain_size moved to control_parameters
!
! 866 2012-03-28 06:44:41Z raasch
! interface for global_min_max changed
!
! 861 2012-03-26 14:18:34Z suehring
! +wall_flags_0
! -boundary_flags
! +nzb_max
! +adv_sca_1, +adv_mom_1
!
! 849 2012-03-15 10:35:09Z raasch
! +deleted_particles, deleted_tails, tr.._count_sum, tr.._count_recv_sum in
! particle_attributes,
! +de_dx, de_dy, de_dz in arrays_3d,
! first_call_advec_particles renamed first_call_lpm
!
! 828 2012-02-21 12:00:36Z raasch
! +dissipation_classes, radius_classes, use_kernel_tables,
! particle feature color renamed class
!
! 825 2012-02-19 03:03:44Z raasch
! +bfactor, curvature_solution_effects, eps_ros, molecular_weight_of_water,
! vanthoff, -b_cond in cloud_parameters,
! dopts_num increased to 29, particle attributes speed_x|y|z_sgs renamed
! rvar1|2|3
! wang_collision_kernel and turbulence_effects_on_collision replaced by
! collision_kernel, hall_kernel, palm_kernel, wang_kernel
!
! 809 2012-01-30 13:32:58Z marongas
! Bugfix: replaced .AND. and .NOT. with && and ! in the preprocessor directives
!
! 807 2012-01-25 11:53:51Z maronga
! New cpp directive "__check" implemented which is used by check_namelist_files.
! New parameter check_restart has been defined which is needed by 
! check_namelist_files only.
!
! 805 2012-01-17 15:53:28Z franke
! Bugfix collective_wait must be out of parallel branch for runs in serial mode
!
! 801 2012-01-10 17:30:36Z suehring
! Dimesion of sums_wsus_ws_l, ! sums_wsvs_ws_l, sums_us2_ws_l, sums_vs2_ws_l,
! sums_ws2_ws_l, sums_wspts_ws_l, sums_wsqs_ws_l, sums_wssas_ws_l increased.
! for thread-safe summation in advec_ws.
!
! RCS Log replace by Id keyword, revision history cleaned up
!
! Revision 1.95  2007/02/11 13:18:30  raasch
! version 3.1b (last under RCS control)
!
! Revision 1.1  1997/07/24 11:21:26  raasch
! Initial revision
!
!
!------------------------------------------------------------------------------!
! Description:
! ------------
!> Definition of global variables
!------------------------------------------------------------------------------!
!------------------------------------------------------------------------------!
! Description:
! ------------
!> Definition of variables which define processor topology and the exchange of
!> ghost point layers. This module must be placed in all routines containing
!> MPI-calls.
!------------------------------------------------------------------------------!
 MODULE pegrid

    USE kinds
    CHARACTER(LEN=2) ::  send_receive = 'al'     !<
    CHARACTER(LEN=7) ::  myid_char = ''          !< character string containing processor id number
    
    INTEGER(iwp) ::  comm1dx                     !< communicator for domain decomposition along x
    INTEGER(iwp) ::  comm1dy                     !< communicator for domain decomposition along y
    INTEGER(iwp) ::  comm2d                      !< standard 2d (xy) communicator used in PALM for the process group the PE belongs to
    INTEGER(iwp) ::  comm_inter                  !< intercommunicator that connects atmosphere/ocean process groups
    INTEGER(iwp) ::  comm_palm                   !< internal communicator used during the MPI setup at the beginning of a run
    INTEGER(iwp) ::  id_inflow = 0               !< myidx of procs at inflow (turbulent inflow method)
    INTEGER(iwp) ::  id_outflow = 0              !< myidx of procs at outflow (turbulent outflow method)
    INTEGER(iwp) ::  id_outflow_source = 0       !< myidx of procs including ouflow source plane (turbulent outflow method)
    INTEGER(iwp) ::  id_recycling = 0            !< myidx of procs containing the recycling plane (turbulence recycling method)
    INTEGER(iwp) ::  ierr                        !< standard error parameter in MPI calls
    INTEGER(iwp) ::  myid = 0                    !< id number of processor element
    INTEGER(iwp) ::  myidx = 0                   !< id number of processor elements with same position along x-direction
    INTEGER(iwp) ::  myidy = 0                   !< id number of processor elements with same position along y-direction
    INTEGER(iwp) ::  ndim = 2                    !< dimension of the virtual PE grid
    INTEGER(iwp) ::  ngp_a                       !< used in atmosphere/ocean coupling: total number of horizontal grid points (atmosphere)
    INTEGER(iwp) ::  ngp_o                       !< used in atmosphere/ocean coupling: total number of horizontal grid points (ocean)
    INTEGER(iwp) ::  ngp_xy                      !< used in atmosphere/ocean coupling: number of grid points of the subdomain
    INTEGER(iwp) ::  ngp_y                       !< number of subdomain grid points along y including ghost points
    INTEGER(iwp) ::  npex = -1                   !< number of processor elements in x-direction
    INTEGER(iwp) ::  npey = -1                   !< number of processor elements in y-direction
    INTEGER(iwp) ::  numprocs = 1                !< total number of appointed processor elements
    INTEGER(iwp) ::  numprocs_previous_run = -1  !< total number of appointed processor elements in previous run (job chain)
    INTEGER(iwp) ::  pleft                       !< MPI-address of the processor left of the current one
    INTEGER(iwp) ::  pnorth                      !< MPI-address of the processor north of the current one
    INTEGER(iwp) ::  pright                      !< MPI-address of the processor right of the current one
    INTEGER(iwp) ::  psouth                      !< MPI-address of the processor south of the current one
    INTEGER(iwp) ::  req_count = 0               !< MPI return variable - checks if Send-Receive operation is already finished
    INTEGER(iwp) ::  sendrecvcount_xy            !< number of subdomain gridpoints to be exchanged in direct transpositions (y --> x, or x --> y) or second (2d) transposition x --> y
    INTEGER(iwp) ::  sendrecvcount_yz            !< number of subdomain gridpoints to be exchanged in third (2d) transposition y --> z
    INTEGER(iwp) ::  sendrecvcount_zx            !< number of subdomain gridpoints to be exchanged in first (2d) transposition z --> x
    INTEGER(iwp) ::  sendrecvcount_zyd           !< number of subdomain gridpoints to be exchanged in direct transpositions z --> y (used for calculating spectra)
    INTEGER(iwp) ::  target_id                   !< in atmosphere/ocean coupling: id of the ocean/atmosphere counterpart PE with whom the atmosphere/ocean PE exchanges data
    INTEGER(iwp) ::  tasks_per_node = -9999      !< MPI tasks per compute node
    INTEGER(iwp) ::  threads_per_task = 1        !< number of OPENMP threads per MPI task
    INTEGER(iwp) ::  type_x                      !< derived MPI datatype for 2-D ghost-point exchange - north / south 
    INTEGER(iwp) ::  type_xy                     !< derived MPI datatype for 2-D ghost-point exchange - north / south 
    INTEGER(iwp) ::  type_y                      !< derived MPI datatype for 2-D exchange in atmosphere-ocean coupler 

    INTEGER(iwp) ::  pdims(2) = 1  !< number of processors along x-y dimension
    INTEGER(iwp) ::  req(100)      !< MPI return variable indicating if send-receive operation is finished

    INTEGER(iwp), DIMENSION(:,:), ALLOCATABLE ::  hor_index_bounds               !< horizontal index bounds 
    INTEGER(iwp), DIMENSION(:,:), ALLOCATABLE ::  hor_index_bounds_previous_run  !< horizontal index bounds of previous run

    LOGICAL ::  collective_wait = .FALSE.          !< switch to set an explicit MPI barrier in front of all collective MPI calls

    SAVE

 END MODULE pegrid
