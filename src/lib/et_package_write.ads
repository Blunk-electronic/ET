------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            PACKAGE WRITE                                 --
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
with et_package_bom_relevance;			use et_package_bom_relevance;
with et_package_model;					use et_package_model;
with et_package_library;				use et_package_library;
with et_fill_zones;						use et_fill_zones;
with et_board_geometry;					use et_board_geometry;
with et_board_coordinates;				use et_board_coordinates;
with et_board_text;						use et_board_text;
with et_board_write;					use et_board_write;
with et_pcb_stack;						use et_pcb_stack;
with et_pcb_sides;						use et_pcb_sides;
with et_drills;							use et_drills;
with et_package_name;					use et_package_name;
with et_package_model_name;				use et_package_model_name;
with et_logging;						use et_logging;



package et_package_write is

	use pac_geometry_2;
	use pac_contours;
	use pac_text_board;


	
	-- Saves the given package model in a file specified by file_name.							   
	procedure write_package (
		file_name 		: in pac_package_model_file.bounded_string; -- libraries/packages/S_SO14.pac
		packge			: in type_package_model; -- the actual device model
		log_threshold	: in type_log_level);


	
	
end et_package_write;
