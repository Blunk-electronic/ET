------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE READ / USER SETTINGS IN BOARD                  --
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
-- 
-- with et_board_ops;
with et_fill_zones;
with et_fill_zones.boards;
with et_thermal_relief;

with et_module;						use et_module;
with et_module_board;				use et_module_board;
with et_module_board_user_settings;	use et_module_board_user_settings;
-- with et_pcb_stack;					use et_pcb_stack;
-- with et_design_rules_board;			use et_design_rules_board;
with et_board_geometry;				use et_board_geometry;
with et_primitive_objects;
-- with et_vias;						use et_vias;
-- with et_drills;						use et_drills;

with et_general_rw;					use et_general_rw;



package body et_module_read_board_user_settings is

	use pac_generic_modules;
	use pac_geometry_2;

	
	user_settings_board : type_user_settings;

	
	
	procedure read_user_settings_vias (
		line : in type_fields_of_line)
	is
		kw : constant string := f (line, 1);
	begin
		-- via drill
		if kw = keyword_via_drill then
			expect_field_count (line, 2);
			
			if f (line, 2) = keyword_dru then -- drill dru
				user_settings_board.vias.drill.active := false;
			else -- drill 0.6
				user_settings_board.vias.drill.active := true;
				user_settings_board.vias.drill.size := to_distance (f (line, 2));

				-- CS validate against dru settings
			end if;

		-- inner restring
		elsif kw = keyword_restring_inner then
			expect_field_count (line, 2);

			if f (line, 2) = keyword_dru then -- restring_inner dru
				user_settings_board.vias.restring_inner.active := false;
			else -- restring_inner 0.22
				user_settings_board.vias.restring_inner.active := true;
				user_settings_board.vias.restring_inner.width := to_distance (f (line, 2));
				
				-- CS validate against dru settings
			end if;

		-- outer restring
		elsif kw = keyword_restring_outer then
			expect_field_count (line, 2);

			if f (line, 2) = keyword_dru then -- restring_outer dru
				user_settings_board.vias.restring_outer.active := false;
			else -- restring_outer 0.2
				user_settings_board.vias.restring_outer.active := true;
				user_settings_board.vias.restring_outer.width := to_distance (f (line, 2));

				-- CS validate against dru settings
			end if;
			
		else
			invalid_keyword (kw);
		end if;
	end read_user_settings_vias;



	
	procedure read_user_settings_fill_zones_conductor (
		line : in type_fields_of_line)
	is
		use et_fill_zones;
		use et_fill_zones.boards;		
		use et_thermal_relief;
		use et_primitive_objects;
		kw : constant string := f (line, 1);
	begin
		if kw = keyword_fill_style then -- fill_style solid/hatched
			expect_field_count (line, 2);
			user_settings_board.polygons_conductor.fill_style := to_fill_style (f (line, 2));

		elsif kw = keyword_linewidth then -- linewidth 0.3
			expect_field_count (line, 2);
			user_settings_board.polygons_conductor.linewidth := to_distance (f (line, 2));

		elsif kw = keyword_priority then -- priority 2
			expect_field_count (line, 2);
			user_settings_board.polygons_conductor.priority_level := to_priority (f (line, 2));

		elsif kw = keyword_isolation then -- isolation 0.4
			expect_field_count (line, 2);
			user_settings_board.polygons_conductor.isolation := to_distance (f (line, 2));

		elsif kw = keyword_spacing then -- spacing 0.5
			expect_field_count (line, 2);
			user_settings_board.polygons_conductor.spacing := to_distance (f (line, 2));

		elsif kw = keyword_connection then -- connection thermal/solid
			expect_field_count (line, 2);
			user_settings_board.polygons_conductor.connection := to_pad_connection (f (line, 2));

		elsif kw = keyword_pad_technology then -- pad_technology smt_and_tht
			expect_field_count (line, 2);
			user_settings_board.polygons_conductor.thermal.technology := to_pad_technology (f (line, 2));

		elsif kw = keyword_relief_width_min then -- relief_width_min 0.25
			expect_field_count (line, 2);
			user_settings_board.polygons_conductor.thermal.width_min := to_distance (f (line, 2));

		elsif kw = keyword_relief_gap_max then -- relief_gap_max 0.25
			expect_field_count (line, 2);
			user_settings_board.polygons_conductor.thermal.gap_max := to_distance (f (line, 2));

		elsif kw = keyword_easing_style then -- easing_style none/chamfer/fillet
			expect_field_count (line, 2);
			user_settings_board.polygons_conductor.easing.style := to_easing_style (f (line, 2));

		elsif kw = keyword_easing_radius then -- easing_radius 1.0
			expect_field_count (line, 2);
			user_settings_board.polygons_conductor.easing.radius := to_distance (f (line, 2));
			
		else
			invalid_keyword (kw);
		end if;

		-- CS plausibility check ?
	end read_user_settings_fill_zones_conductor;

	
	
	
	
	
	procedure assign_user_settings_board (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			module.board.user_settings := user_settings_board;
		end do_it;
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " assign user settings board",
			level => log_threshold);
			
		log_indentation_up;
		
		update_element (generic_modules, module_cursor, do_it'access);
		
		log_indentation_down;		
	end assign_user_settings_board;


				
end et_module_read_board_user_settings;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
