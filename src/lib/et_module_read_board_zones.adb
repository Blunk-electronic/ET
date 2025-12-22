------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE READ / BOARD ZONES NON-ELECTRICAL              --
--                                                                          --
--                               B o d y                                    --
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
-- ToDo:
-- - clean up
--
--
--

with ada.text_io;					use ada.text_io;
with ada.characters;				use ada.characters;
with ada.strings;					use ada.strings;

with et_module_names;				use et_module_names;
with et_keywords;					use et_keywords;
with et_fill_zones;					use et_fill_zones;
with et_fill_zones.boards;			use et_fill_zones.boards;
with et_pcb;						use et_pcb;
with et_pcb_signal_layers;			use et_pcb_signal_layers;
with et_design_rules_board;			use et_design_rules_board;
with et_board_geometry;				use et_board_geometry;
with et_primitive_objects;			use et_primitive_objects;
with et_via_restrict.boards;

with et_general_rw;					use et_general_rw;



package body et_module_read_board_zones is

	use pac_generic_modules;
	use pac_geometry_2;
	use pac_contours;

	
	
	board_filled : type_filled := filled_default;
	-- CS rename to zone_filled
	
	fill_spacing : type_track_clearance := type_track_clearance'first;
	-- CS rename to zone_fill_spacing
	
	board_fill_style : type_fill_style := fill_style_default;	
	-- CS rename to zone_fill_style
	
	board_easing : type_easing;
	-- CS rename to zone_easing
	
	signal_layer : type_signal_layer;
	-- CS rename to zone_signal_layer
	
	contour_priority : type_priority := type_priority'first;
	-- CS rename to zone_priority
	
	polygon_width_min : type_track_width := type_track_width'first;
	-- CS rename to zone_width_min
	
	polygon_isolation : type_track_clearance := type_track_clearance'first; 
	-- CS rename to zone_isolation
	-- applies to conductor zones only
		
	signal_layers : pac_signal_layers.set;
	-- CS rename to zone_signal_layers
	
	contour : type_contour;
	


	
	
	procedure board_reset_contour is -- CS rename
	begin
		fill_spacing		:= type_track_clearance'first;
		board_filled		:= filled_default;
		board_fill_style	:= fill_style_default;
		--board_hatching		:= (others => <>);
		board_easing 		:= (others => <>);
		
		contour_priority		:= type_priority'first;  -- board relevant only
		polygon_isolation		:= type_track_clearance'first;
		polygon_width_min		:= type_track_width'first;

		signal_layer			:= type_signal_layer'first;  -- board relevant only

		contour := (others => <>);
	end;


	
	
	
	procedure read_fill_zone_keepout (
		line : in type_fields_of_line)
	is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_filled then -- filled yes/no
			expect_field_count (line, 2);													
			board_filled := to_filled (f (line, 2));

		else
			invalid_keyword (kw);
		end if;
	end read_fill_zone_keepout;


	
	
	

	procedure read_cutout_non_conductor (
		line : in type_fields_of_line)
	is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_easing_style then -- easing_style none/chamfer/fillet
			expect_field_count (line, 2);													
			board_easing.style := to_easing_style (f (line, 2));

		elsif kw = keyword_easing_radius then -- easing_radius 0.4
			expect_field_count (line, 2);													
			board_easing.radius := to_distance (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_cutout_non_conductor;


	
	
	
	
	
	procedure read_cutout_conductor_non_electric (
		line : in type_fields_of_line)
	is
		use et_pcb_stack;
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_easing_style then -- easing_style none/chamfer/fillet
			expect_field_count (line, 2);													
			board_easing.style := to_easing_style (f (line, 2));

		elsif kw = keyword_easing_radius then -- easing_radius 0.4
			expect_field_count (line, 2);													
			board_easing.radius := to_distance (f (line, 2));
			
		elsif kw = keyword_layer then -- layer 1
			expect_field_count (line, 2);
			signal_layer := to_signal_layer (f (line, 2));
			-- CS validate_signal_layer;

		else
			invalid_keyword (kw);
		end if;
	end read_cutout_conductor_non_electric;

		
	
	
	
	
			
	procedure read_fill_zone_non_conductor (
		line : in type_fields_of_line)
	is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_fill_style then -- fill_style solid/hatched
			expect_field_count (line, 2);													
			board_fill_style := to_fill_style (f (line, 2));
		
		else
			invalid_keyword (kw);
		end if;
	end read_fill_zone_non_conductor;
		
			
			
	
	
	
	
			
	procedure read_fill_zone_conductor_non_electric (
		line : in type_fields_of_line)
	is
		use et_pcb_stack;
		use et_fill_zones.boards;
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_fill_style then -- fill_style solid/hatched
			expect_field_count (line, 2);													
			board_fill_style := to_fill_style (f (line, 2));

		elsif kw = keyword_easing_style then -- easing_style none/chamfer/fillet
			expect_field_count (line, 2);													
			board_easing.style := to_easing_style (f (line, 2));

		elsif kw = keyword_easing_radius then -- easing_radius 0.4
			expect_field_count (line, 2);													
			board_easing.radius := to_distance (f (line, 2));
			
		elsif kw = keyword_spacing then -- spacing 0.3
			expect_field_count (line, 2);													
			fill_spacing := to_distance (f (line, 2));

		elsif kw = keyword_width then -- width 0.5
			expect_field_count (line, 2);
			polygon_width_min := to_distance (f (line, 2));
			
		elsif kw = keyword_layer then -- layer 1
			expect_field_count (line, 2);
			signal_layer := to_signal_layer (f (line, 2));
			-- CS validate_signal_layer;
			
		elsif kw = keyword_priority then -- priority 2
			expect_field_count (line, 2);
			contour_priority := to_priority (f (line, 2));

		elsif kw = keyword_isolation then -- isolation 0.5
			expect_field_count (line, 2);
			polygon_isolation := to_distance (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;
	end read_fill_zone_conductor_non_electric;



	
	
	procedure read_fill_zone_restrict (
		line	: in type_fields_of_line;
		check	: in type_layer_check)
	is
		use et_pcb_stack;
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_filled then -- filled yes/no
			expect_field_count (line, 2);													
			board_filled := to_filled (f (line, 2));

		elsif kw = keyword_layers then -- layers 1 14 3

			-- there must be at least two fields:
			expect_field_count (line => line, count_expected => 2, warn => false);
			signal_layers := to_layers (line, check);

		else
			invalid_keyword (kw);
		end if;
	end read_fill_zone_restrict;

	
	

	
	
	
	
	procedure read_cutout_restrict (
		line	: in type_fields_of_line;
		check	: in type_layer_check)
	is
		kw : constant string := f (line, 1);
	begin
		-- CS: In the following: set a corresponding parameter-found-flag
		if kw = keyword_layers then -- layers 1 14 3

			-- there must be at least two fields:
			expect_field_count (line => line, count_expected => 2, warn => false);
			signal_layers := to_layers (line, check);

		else
			invalid_keyword (kw);
		end if;
	end read_cutout_restrict;





	

	procedure insert_cutout_via_restrict (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		use et_via_restrict.boards;
		use et_pcb_stack;
		use pac_signal_layers;

		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			pac_via_restrict_cutouts.append (
				container	=> module.board.via_restrict.cutouts,
				new_item	=> (contour with
								layers	=> signal_layers));
		end do_it;

		
	begin
		-- CS log messages
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

		-- clean up for next board contour
		board_reset_contour;

		clear (signal_layers);
	end insert_cutout_via_restrict;

	
end et_module_read_board_zones;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
