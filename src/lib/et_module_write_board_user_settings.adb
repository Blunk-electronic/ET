------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                  MODULE WRITE / USER SETTINGS IN BOARD                   --
--                                                                          --
--                               B o d y                                    --
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
with ada.strings;					use ada.strings;

with et_module_names;				use et_module_names;
with et_keywords;					use et_keywords;
with et_section_headers;			use et_section_headers;

with et_fill_zones;
with et_fill_zones.boards;
with et_thermal_relief;

with et_module;						use et_module;
with et_module_board;				use et_module_board;
with et_module_board_user_settings;	use et_module_board_user_settings;
-- with et_pcb_stack;					use et_pcb_stack;

with et_board_geometry;				use et_board_geometry;
with et_primitive_objects;
-- with et_vias;						use et_vias;
-- with et_drills;						use et_drills;

with et_general_rw;					use et_general_rw;
with et_board_write;				use et_board_write;


package body et_module_write_board_user_settings is

	use pac_geometry_2;

	

	procedure write_board_user_settings (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is 
			us : type_user_settings renames module.board.user_settings;

			
			procedure vias is begin
				section_mark (section_vias, HEADER);

				-- via drill
				if us.vias.drill.active then
					write ( -- drill 0.3
						keyword		=> keyword_via_drill, 
						parameters	=> to_string (us.vias.drill.size));
				else
					write ( -- drill dru
						keyword		=> keyword_via_drill, 
						parameters	=> keyword_dru);
				end if;

				-- inner restring
				if us.vias.restring_inner.active then
					write ( -- restring_inner 0.3
						keyword		=> keyword_restring_inner, 
						parameters	=> to_string (us.vias.restring_inner.width));
				else
					write ( -- restring_inner dru
						keyword		=> keyword_restring_inner, 
						parameters	=> keyword_dru);
				end if;

				-- outer restring
				if us.vias.restring_outer.active then
					write ( -- restring_outer 0.3
						keyword		=> keyword_restring_outer, 
						parameters	=> to_string (us.vias.restring_outer.width));
				else
					write ( -- restring_inner dru
						keyword		=> keyword_restring_outer, 
						parameters	=> keyword_dru);
				end if;
				
				section_mark (section_vias, FOOTER);
			end vias;

			
			
			procedure polygons is 
				use et_thermal_relief;
				use et_fill_zones.boards;
			begin
				section_mark (section_fill_zones_conductor, HEADER);

				write_fill_style (us.polygons_conductor.fill_style);
				write_fill_linewidth (us.polygons_conductor.linewidth);

				write (keyword => keyword_priority , 
					parameters => to_string (us.polygons_conductor.priority_level));
								
				write (keyword => keyword_isolation, 
					   parameters => to_string (us.polygons_conductor.isolation));
				
				write_spacing (us.polygons_conductor.spacing);
				
				write (keyword => keyword_connection, 
					   parameters => to_string (us.polygons_conductor.connection));
				
				write_thermal (us.polygons_conductor.thermal);

				write_easing (us.polygons_conductor.easing);
				
				section_mark (section_fill_zones_conductor, FOOTER);
			end polygons;

			
		begin
			vias;
			polygons; -- CS rename to zones
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " write board user settings",
			 level => log_threshold);

		log_indentation_up;
		
		section_mark (section_user_settings, HEADER);
		query_element (module_cursor, query_module'access);		
		section_mark (section_user_settings, FOOTER);

		log_indentation_down;
	end write_board_user_settings;
		
	
				
end et_module_write_board_user_settings;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
