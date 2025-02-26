------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   BOARD OPERATIONS / STENCIL                             --
--                                                                          --
--                               B o d y                                    --
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


package body et_board_ops.stencil is

	use pac_generic_modules;

	procedure draw_stencil_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in type_stencil_line;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_stencil_lines;
		begin
			case face is
				when TOP =>
					append (
						container	=> module.board.stencil.top.lines,
						new_item	=> line);
					
				when BOTTOM =>
					append (
						container	=> module.board.stencil.bottom.lines,
						new_item	=> line);
			end case;
		end;
							   
	begin -- draw_stencil_line
		log (text => "module " & to_string (module_name) &
			" drawing stencil line" &
			" face" & to_string (face) &
			to_string (line),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end draw_stencil_line;




	
	procedure draw_stencil_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_stencil_arc;		
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_stencil_arcs;
		begin
			case face is
				when TOP =>
					append (
						container	=> module.board.stencil.top.arcs,
						new_item	=> arc);

				when BOTTOM =>
					append (
						container	=> module.board.stencil.bottom.arcs,
						new_item	=> arc);
			end case;
		end;
							   
	begin -- draw_stencil_arc
		log (text => "module " & to_string (module_name) &
			" drawing stencil arc" &
			" face" & to_string (face) &
			to_string (arc) &
			" width" & to_string (arc.width),

			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end draw_stencil_arc;




	
	procedure draw_stencil_circle (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		circle			: in type_stencil_circle;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_stencil_circles;
		begin
			case face is
				when TOP =>
					append (
						container	=> module.board.stencil.top.circles,
						new_item	=> circle);

				when BOTTOM =>
					append (
						container	=> module.board.stencil.bottom.circles,
						new_item	=> circle);

			end case;
		end;

		
	begin -- draw_stencil_circle
		log (text => "module " & to_string (module_name) &
			" drawing stencil circle" &
			" face" & to_string (face) &
			to_string (circle),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end draw_stencil_circle;
	

	


	procedure draw_zone (
		module_cursor	: in pac_generic_modules.cursor;
		zone			: in type_stencil_zone;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is
		-- When searching among already existing zones then
		-- this flag is used to abort the iteration prematurely:
		proceed : boolean := true;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is 
			use pac_stencil_zones;
			c : pac_stencil_zones.cursor;

			-- This procedure tests whether the candidate
			-- zone z is open. If z is open, then it tries
			-- to merge the given zone into z. If the merge operation
			-- succeedes then no more zones are iterated (flag proceed):
			procedure query_zone (z : in out type_stencil_zone) is
				use et_board_shapes_and_text;
				use pac_contours;
				mr : type_merge_result;
			begin
				-- put_line ("query_zone");
				if is_open (zone) then
					--put_line (" is open");
					merge_contours (z, zone, mr);
					if mr.successful then
						--put_line ("  successful");
						-- No more searching needed -> abort iterator
						proceed := false;
					end if;
				end if;
			end query_zone;

			
		begin
			case face is
				when TOP =>
					-- Iterate through the already existing zones:
					c := module.board.stencil.top.contours.first;

					while c /= pac_stencil_zones.no_element and proceed loop
						module.board.stencil.top.contours.update_element (c, query_zone'access);
						next (c);
					end loop;

					-- If no open zone found, then add the given zone
					-- as a new zone:
					if proceed then
						-- put_line ("added as new zone");
						log (text => "added as new zone", level => log_threshold + 1);
						module.board.stencil.top.contours.append (zone);
					end if;

					
				when BOTTOM =>
					-- Iterate through the already existing zones:
					c := module.board.stencil.bottom.contours.first;

					while c /= pac_stencil_zones.no_element and proceed loop
						module.board.stencil.bottom.contours.update_element (c, query_zone'access);
						next (c);
					end loop;

					-- If no open zone found, then add the given zone
					-- as a new zone:
					if proceed then
						log (text => "added as new zone", level => log_threshold + 1);
						module.board.stencil.bottom.contours.append (zone);
					end if;

			end case;
		end query_module;


	begin
		log (text => "module " & to_string (module_cursor) 
			 & " drawing stencil zone" 
			 & to_string (face)
			 & " " & to_string (contour => zone, full => true),
			level => log_threshold);

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end draw_zone;

	


	
	procedure delete_stencil (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		point			: in type_vector_model; -- x/y
		accuracy		: in type_accuracy;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure delete (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_stencil_lines;
			use pac_stencil_arcs;
			use pac_stencil_circles;
			line_cursor   : pac_stencil_lines.cursor;
			arc_cursor    : pac_stencil_arcs.cursor;
			circle_cursor : pac_stencil_circles.cursor;

			deleted : boolean := false; -- goes true if at least one segment has been deleted
		begin
			if face = TOP then
				line_cursor   	:= module.board.stencil.top.lines.first;
				arc_cursor    	:= module.board.stencil.top.arcs.first;
				circle_cursor	:= module.board.stencil.top.circles.first;
			else
				line_cursor   	:= module.board.stencil.bottom.lines.first;
				arc_cursor    	:= module.board.stencil.bottom.arcs.first;
				circle_cursor	:= module.board.stencil.bottom.circles.first;
			end if;
			
			-- first search for a matching segment among the lines
			while line_cursor /= pac_stencil_lines.no_element loop
				if element (line_cursor).on_line (point) then
					-- CS use get_shortest_distance (point, element)
					-- and compare distance with accuracy	
					if face = TOP then
						delete (module.board.stencil.top.lines, line_cursor);
					else
						delete (module.board.stencil.bottom.lines, line_cursor);
					end if;
					deleted := true;
					exit;
				end if;
				next (line_cursor);
			end loop;

			-- if no line found, search among arcs
			if not deleted then
				while arc_cursor /= pac_stencil_arcs.no_element loop
					if element (arc_cursor).on_arc (point) then
						-- CS use get_shortest_distance (point, element)
						-- and compare distance with accuracy	
						if face = TOP then
							delete (module.board.stencil.top.arcs, arc_cursor);
						else
							delete (module.board.stencil.bottom.arcs, arc_cursor);
						end if;
						deleted := true;
						exit;
					end if;
					next (arc_cursor);
				end loop;
			end if;

			-- if no arc found, search among circles
			if not deleted then
				while circle_cursor /= pac_stencil_circles.no_element loop
					
					if element (circle_cursor).on_circle (point) then
						-- CS use get_shortest_distance (point, element)
						-- and compare distance with accuracy	
						if face = TOP then
							delete (module.board.stencil.top.circles, circle_cursor);
						else
							delete (module.board.stencil.bottom.circles, circle_cursor);
						end if;
						deleted := true;
						exit;
					end if;
					next (circle_cursor);
				end loop;
			end if;

			if not deleted then
				nothing_found (point, accuracy);
			end if;
			
		end delete;
		
	begin -- delete_stencil
		log (text => "module " & to_string (module_name) &
			" deleting stencil segment face" & to_string (face) &
			" at" & to_string (point) &
			" accuracy" & accuracy_to_string (accuracy),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> delete'access);
		
	end delete_stencil;
	
	
end et_board_ops.stencil;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
