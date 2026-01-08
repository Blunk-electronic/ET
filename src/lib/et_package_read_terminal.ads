------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        PACKAGE READ / TERMINAL                           --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                -- 
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--
-- DESCRIPTION:
-- 
-- This is about contours in general.
--
--
--
--   do do:
--
--

with et_board_geometry;					use et_board_geometry;
with et_string_processing;				use et_string_processing;
with et_assembly_technology;			use et_assembly_technology;
with et_stencil_mask_status;			use et_stencil_mask_status;
with et_stopmask_status;				use et_stopmask_status;
with et_terminal_stopmask;				use et_terminal_stopmask;
with et_terminal_stencil;				use et_terminal_stencil;
with et_terminal_hole;					use et_terminal_hole;
with et_terminals;						use et_terminals;
with et_design_rules_board;				use et_design_rules_board;
with et_drills;							use et_drills;
with et_pcb_sides;						use et_pcb_sides;
with et_package_model;					use et_package_model;
with et_logging;						use et_logging;


package et_package_read_terminal is

	use pac_geometry_2;
	use pac_contours;



	terminal_position		: type_position := origin_zero_rotation;

	tht_stop_mask_status			: type_stop_mask_status := stop_mask_status_default;
	tht_stop_mask_shape_top			: type_stopmask_expand_mode := stopmask_expand_mode_default;
	tht_stop_mask_shape_bottom		: type_stopmask_expand_mode := stopmask_expand_mode_default;		
	tht_stop_mask_contours_top		: type_stopmask_contour;
	tht_stop_mask_contours_bottom	: type_stopmask_contour;		

	tht_width_inner_layers	: type_track_width := type_track_width'first;
	tht_hole				: type_terminal_tht_hole := terminal_tht_hole_default;
	tht_drill_size			: type_drill_size_tht := type_drill_size_tht'first;
	tht_millings			: type_contour;

	terminal_name			: pac_terminal_name.bounded_string;
	terminal_technology		: type_assembly_technology := assembly_technology_default;
	tht_pad_shape			: type_pad_outline_tht;		
	smt_pad_shape			: type_contour;

	smt_pad_face			: type_face := face_default;

	smt_stop_mask_status	: type_stop_mask_status := stop_mask_status_default;
	smt_stop_mask_shape		: type_stopmask_expand_mode := stopmask_expand_mode_default;
	smt_stop_mask_contours	: type_stopmask_contour;		

	-- NOTE: Solder paste is applied to SMT pads only.
	smt_solder_paste_status	: type_solder_paste_status := solder_paste_status_default;
	smt_stencil_shape		: type_stencil_shrink_mode := stencil_modification_default;
	smt_stencil_contours	: type_stencil_contours;
	--smt_stencil_shrink		: type_stencil_shrink := stencil_shrink_mode_default;
	smt_stencil_shrink		: type_distance_positive := stencil_shrink_mode_default;


	
	
	
	
	procedure read_terminal (
		line : in type_fields_of_line);
	
	
	

-- THT:

	procedure assign_contour_conductor_tht (
		face : in type_face);

	
	procedure assign_contour_stopmask_tht (
		face : in type_face);
	

	procedure assign_plated_millings;


	
	

-- SMT:
	
	procedure assign_contour_conductor_smt;

	procedure assign_contour_stopmask_smt;
	
	procedure assign_contour_stencil_smt;
	
	
	
	-- Assembles the elements of a terminal and appends the final terminal to the
	-- list of terminals of the package.
	procedure build_terminal ( -- CS rename to insert_terminal
		packge			: in type_package_model_access;
		log_threshold	: in type_log_level);
	
end et_package_read_terminal;
