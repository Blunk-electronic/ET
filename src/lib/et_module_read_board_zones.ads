------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE READ / BOARD ZONES NON-ELECTRICAL              --
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
--                                                                          --
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
--
-- DESCRIPTION:
-- 1. This package is about fill zones of conductor and non-conductor
--    layers. With other words: All zones which do not conduct
--    electrical current.
-- 2. Zones in conductor layers are not connected with a net here.
--
--
-- ToDo:
-- - clean up
--
--
--

with et_generic_module;				use et_generic_module;
with et_board_layer_category;		use et_board_layer_category;
with et_pcb_stack;					use et_pcb_stack;
with et_pcb_sides;					use et_pcb_sides;
with et_string_processing;			use et_string_processing;
with et_logging;					use et_logging;



package et_module_read_board_zones is

	
	
	procedure read_fill_zone_keepout (
		line : in type_fields_of_line);
	

	procedure read_keepout_cutout (
		line : in type_fields_of_line);
	
	

	procedure read_cutout_non_conductor (
		line : in type_fields_of_line);	

	
	-- Reads cutout zone in conductor layer.
	-- NOTE: This is about floating conductor zones. Has nothing to
	-- do with nets and routes.
	procedure read_cutout_conductor_non_electric (
		line : in type_fields_of_line);

		
		
	procedure read_fill_zone_non_conductor (
		line : in type_fields_of_line);
		
	
	
	procedure read_fill_zone_conductor_non_electric (
		line : in type_fields_of_line);
	
	
	procedure read_fill_zone_restrict (
		line	: in type_fields_of_line;
		check	: in type_layer_check);


	procedure insert_polygon_route_restrict ( -- CS rename to insert_zone_route_restrict
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);
	

	procedure insert_zone_via_restrict (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);



	procedure insert_polygon_conductor ( -- CS rename to insert_zone_conductor
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);

		
	procedure insert_polygon ( -- CS rename to insert_zone ?
		module_cursor	: in pac_generic_modules.cursor;
		layer_cat		: in type_layer_category;
		face			: in type_face;
		log_threshold	: in type_log_level);


		

-- CUTOUT:
	
	procedure read_cutout_restrict (
		line	: in type_fields_of_line;
		check	: in type_layer_check);

	
	procedure insert_cutout_via_restrict (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);
		

	procedure insert_cutout_route_restrict (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);


	procedure insert_cutout (
		module_cursor	: in pac_generic_modules.cursor;
		layer_cat		: in type_layer_category;
		face			: in type_face;
		log_threshold	: in type_log_level);


	-- This is about cutout zones to trim floating contours in 
	-- signal layers. No connection to any net.
	procedure insert_cutout_conductor (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level);
	
	
end et_module_read_board_zones;

	


	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
