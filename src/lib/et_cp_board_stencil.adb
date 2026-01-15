------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                 COMMAND PROCESSOR / BOARD / STENCIL                      --
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
-- To Do:
-- - rework
--
--

with ada.text_io;						use ada.text_io;
with ada.characters.handling;			use ada.characters.handling;
with ada.strings; 						use ada.strings;

with et_directions;						use et_directions;
with et_primitive_objects;				use et_primitive_objects;
with et_pcb_sides;						use et_pcb_sides;

with et_keywords;						use et_keywords;

with et_board_geometry;					use et_board_geometry;
with et_board_ops.stencil;				use et_board_ops.stencil;
with et_keywords;



package body et_cp_board_stencil is

	use pac_generic_modules;
	use pac_geometry_2;
	use pac_contours;

	

	
	procedure draw_stencil (
		module			: in pac_generic_modules.cursor;
		cmd 			: in out type_single_cmd;
		log_threshold	: in type_log_level)
	is
		-- Contains the number of fields given by the caller of this procedure:
		cmd_field_count : constant type_field_count := get_field_count (cmd);


		-- Extract from the given command the zone arguments (everything after "zone"):
		-- example command: board demo draw stencil top zone line 0 0 line 50 0 line 50 50 line 0 50
		procedure build_zone is
			arguments : constant type_fields_of_line := 
				remove_field (get_fields (cmd), 1, 6);
			
			-- Build the basic contour from zone:
			c : constant type_contour := type_contour (to_contour (arguments));

			face : type_face;
		begin
			face := to_face (get_field (cmd, 5));
			
			draw_zone (
				module_cursor	=> module,
				zone			=> (c with null record),
				face			=> face,
				log_threshold	=> log_threshold + 1);

		end build_zone;

		
		shape : type_shape;

		
		-- Draws a line, arc or circle:
		procedure draw_shape is 
			arc_tmp		: type_arc;
			circle_tmp	: type_circle;
			line_tmp	: type_line;
			width_tmp	: type_distance_positive;
		begin
			case shape is
				when LINE =>
					case cmd_field_count is
						when 11 =>
							width_tmp := to_distance (get_field (cmd, 7));
							
							line_tmp := type_line (to_line (
								A => to_vector_model (get_field (cmd, 8), get_field (cmd, 9)),
								B => to_vector_model (get_field (cmd, 10), get_field (cmd, 11))));
							
							add_line (
								module_name 	=> key (module),
								face			=> to_face (get_field (cmd, 5)),
								line			=> (line_tmp with width_tmp),
								log_threshold	=> log_threshold + 1
								);

						when 12 .. type_field_count'last =>
							command_too_long (cmd, cmd_field_count - 1);
							
						when others =>
							command_incomplete (cmd);
					end case;

					
				when ARC =>
					case cmd_field_count is
						when 14 =>
							width_tmp := to_distance (get_field (cmd, 7));
							
							arc_tmp := type_arc (to_arc (
								center		=> to_vector_model (get_field (cmd, 8), get_field (cmd, 9)),
								A			=> to_vector_model (get_field (cmd, 10), get_field (cmd, 11)),
								B			=> to_vector_model (get_field (cmd, 12), get_field (cmd, 13)),
								direction	=> to_direction (get_field (cmd, 14))));
							
							add_arc (
								module_name 	=> key (module),
								face			=> to_face (get_field (cmd, 5)),
								arc				=> (arc_tmp with width_tmp),
								log_threshold	=> log_threshold + 1);

						when 15 .. type_field_count'last =>
							command_too_long (cmd, cmd_field_count - 1);
							
						when others =>
							command_incomplete (cmd);
					end case;

					
				when CIRCLE =>
					case cmd_field_count is						
						when 10 =>
							width_tmp := to_distance (get_field (cmd, 7));
							
							circle_tmp := type_circle (to_circle (
								center		=> to_vector_model (get_field (cmd, 8), get_field (cmd, 9)),
								radius		=> to_radius (get_field (cmd, 10))));
													
							add_circle (
								module_name 	=> key (module),
								face			=> to_face (get_field (cmd, 5)),
								circle			=> (circle_tmp with width_tmp),
								log_threshold	=> log_threshold + 1);
							
						when 11 .. type_field_count'last =>
							command_too_long (cmd, cmd_field_count - 1);
							
						when others =>
							command_incomplete (cmd);
					end case;
							
				when others => null;
			end case;
		end draw_shape;


	begin
		-- CS log message

		if get_field (cmd, 6) = keyword_zone then
			build_zone;
		else
			shape := to_shape (get_field (cmd, 6));
			draw_shape;
		end if;	
	end draw_stencil;



	
end et_cp_board_stencil;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
