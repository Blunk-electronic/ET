------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            PACKAGE READ                                  --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                -- 
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

--   do do:

with et_design_rules_board;				use et_design_rules_board;
with et_string_processing;				use et_string_processing;
with et_device_placeholders;			use et_device_placeholders;
with et_device_placeholders.packages;	use et_device_placeholders.packages;
with et_package_bom_relevance;			use et_package_bom_relevance;
with et_package_library;				use et_package_library;
with et_package_model_name;				use et_package_model_name;
with et_fill_zones;						use et_fill_zones;
with et_route_restrict;					use et_route_restrict;
with et_via_restrict;					use et_via_restrict;
with et_board_geometry;					use et_board_geometry;
with et_board_coordinates;				use et_board_coordinates;
with et_board_text;						use et_board_text;
with et_board_read;						use et_board_read;
with et_conductor_text;					use et_conductor_text;
with et_stopmask;						use et_stopmask;
with et_stencil;						use et_stencil;
with et_silkscreen;						use et_silkscreen;
with et_assy_doc;						use et_assy_doc;
with et_keepout;						use et_keepout;
with et_pcb_stack;						use et_pcb_stack;
with et_pcb_sides;						use et_pcb_sides;
with et_drills;							use et_drills;
with et_package_name;					use et_package_name;
with et_logging;						use et_logging;



package et_package_read is

	use pac_geometry_2;
	use pac_contours;
	use pac_text_board;


	
	-- Opens the package file and stores the package in container et_packages.packages.
	-- If check_layers.check is YES, then a check will be done that tests
	-- whether all conductor layers are are in 
	-- range type_signal_layer'first .. deepest conductor layer.
	procedure read_package (
		file_name 		: in pac_package_model_file_name.bounded_string; -- libraries/packages/S_SO14.pac
		check_layers	: in et_pcb_stack.type_layer_check := (check => et_pcb_stack.NO);
		log_threshold	: in type_log_level);
		-- CS error : out boolean;
		-- CS package_curosr : out ?.cursor;


	
	
end et_package_read;
