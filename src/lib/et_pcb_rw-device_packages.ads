------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   PCB READ AND WRITE FOR DEVICE PACKAGES                 --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                -- 
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

--   do do:

with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.packages;	use et_device_placeholders.packages;
with et_package_appearance;				use et_package_appearance;
with et_packages;						use et_packages;
with et_fill_zones;						use et_fill_zones;
with et_route_restrict;					use et_route_restrict;
with et_via_restrict;					use et_via_restrict;
with et_pcb_rw.restrict;				use et_pcb_rw.restrict;
with et_package_names;					use et_package_names;


package et_pcb_rw.device_packages is

	section_pad_contours_smt	: constant string	:= "[PAD_CONTOURS_SMT";
	section_pad_contours_tht	: constant string	:= "[PAD_CONTOURS_THT";	
	
	section_stencil_contours	: constant string	:= "[STENCIL_CONTOURS";

	section_stop_mask_contours_tht	: constant string	:= "[STOP_MASK_CONTOURS_THT";
	section_stop_mask_contours_smt	: constant string	:= "[STOP_MASK_CONTOURS_SMT";
	
	section_pad_millings		: constant string	:= "[MILLINGS";

	section_terminals			: constant string	:= "[TERMINALS";
	section_terminal			: constant string	:= "[TERMINAL";

	
	
	type type_section is (
		SEC_CONDUCTOR,
		SEC_CONTOURS, -- of fill and cutout zones
		SEC_CUTOUT_ZONE,
		SEC_INIT,
		SEC_TOP,
		SEC_BOTTOM,
		SEC_HOLE,
		SEC_LINE,
		SEC_ARC,
		SEC_CIRCLE,
		SEC_SILK_SCREEN,
		SEC_ASSEMBLY_DOCUMENTATION,
		SEC_KEEPOUT,
		SEC_STOP_MASK,
		SEC_STENCIL,
		SEC_ROUTE_RESTRICT,
		SEC_VIA_RESTRICT,
		SEC_PCB_CONTOURS_NON_PLATED,
		SEC_TERMINALS,
		SEC_TERMINAL,
		SEC_PAD_CONTOURS_SMT,
		SEC_PAD_CONTOURS_THT,
		SEC_STENCIL_CONTOURS,
		SEC_STOP_MASK_CONTOURS_SMT,
		SEC_STOP_MASK_CONTOURS_THT,
		SEC_MILLINGS,
		SEC_TEXT,
		SEC_PLACEHOLDER,
		SEC_ZONE,
		SEC_PACKAGE_3D_CONTOURS
		);

	-- Creates a package and stores the package in container et_packages.packages.								 
	procedure create_package (
		package_name 	: in pac_package_model_file_name.bounded_string; -- libraries/packages/S_SO14.pac
		appearance		: in type_package_appearance;
		log_threshold	: in type_log_level);

	
	-- Saves the given package model in a file specified by file_name.							   
	procedure save_package (
		file_name 		: in pac_package_model_file_name.bounded_string; -- libraries/packages/S_SO14.pac
		packge			: in type_package_model; -- the actual device model
		log_threshold	: in type_log_level);

	
	-- Opens the package file and stores the package in container et_packages.packages.
	-- If check_layers.check is YES, then a check will be done that tests
	-- whether all conductor layers are are in 
	-- range type_signal_layer'first .. deepest conductor layer.
	procedure read_package (
		file_name 		: in pac_package_model_file_name.bounded_string; -- libraries/packages/S_SO14.pac
		check_layers	: in et_pcb_stack.type_layer_check := (check => et_pcb_stack.NO);
		log_threshold	: in type_log_level);

	
end et_pcb_rw.device_packages;
