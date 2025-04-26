------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   BOARD OPERATIONS / ROUTE RESTRICT                      --
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


package body et_board_ops.route_restrict is

	
	
	procedure draw_route_restrict_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		line			: in type_route_restrict_line;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		use pac_route_restrict_lines;

		procedure draw (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) is
		begin
			append (
				container	=> module.board.route_restrict.lines,
				new_item	=> line);
		end;
		
	begin 
		log (text => "module " & to_string (module_name) &
			" drawing route restrict line in layer(s) " & to_string (line.layers) &
			to_string (line),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- make sure the desired layers are available according to current layer stack:
		test_layers (module_cursor, line.layers);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> draw'access);
		
	end draw_route_restrict_line;

	
	procedure draw_route_restrict_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		arc				: in type_route_restrict_arc;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		use pac_route_restrict_arcs;

		procedure draw (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) is
		begin
			append (
				container	=> module.board.route_restrict.arcs,
				new_item	=> arc);
		end;
		
	begin 
		log (text => "module " & to_string (module_name) &
			" drawing route restrict arc in layer(s) " & to_string (arc.layers) &
			to_string (arc),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- make sure the desired layers are available according to current layer stack:
		test_layers (module_cursor, arc.layers);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> draw'access);
		
	end draw_route_restrict_arc;

	
	procedure draw_route_restrict_circle (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		circle			: in type_route_restrict_circle;
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		use pac_route_restrict_circles;

		procedure draw (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) is
		begin
			append (
				container	=> module.board.route_restrict.circles,
				new_item	=> circle);
		end;
		
	begin 
		log (text => "module " & to_string (module_name) &
			" drawing route restrict circle in layer(s) " & to_string (circle.layers) &
			to_string (circle),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- make sure the desired layers are available according to current layer stack:
		test_layers (module_cursor, circle.layers);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> draw'access);
		
	end draw_route_restrict_circle;


	
	
	procedure delete_route_restrict (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure delete (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			use pac_route_restrict_lines;
			use pac_route_restrict_arcs;
			use pac_route_restrict_circles;
			line_cursor   : pac_route_restrict_lines.cursor  := module.board.route_restrict.lines.first;
			arc_cursor    : pac_route_restrict_arcs.cursor   := module.board.route_restrict.arcs.first;
			circle_cursor : pac_route_restrict_circles.cursor := module.board.route_restrict.circles.first;

			deleted : boolean := false; -- goes true if at least one segment has been deleted
		begin
			-- first search for a matching segment among the lines
			while line_cursor /= pac_route_restrict_lines.no_element loop
				if in_catch_zone (catch_zone, element (line_cursor)) then
					delete (module.board.route_restrict.lines, line_cursor);
					deleted := true;
					exit;
				end if;
				next (line_cursor);
			end loop;

			-- if no line found, search among arcs
			if not deleted then
				while arc_cursor /= pac_route_restrict_arcs.no_element loop					
					if in_catch_zone (catch_zone, element (arc_cursor)) then
						delete (module.board.route_restrict.arcs, arc_cursor);
						deleted := true;
						exit;
					end if;
					next (arc_cursor);
				end loop;
			end if;

			-- if no arc found, search among circles
			if not deleted then
				while circle_cursor /= pac_route_restrict_circles.no_element loop
					if in_catch_zone (catch_zone, element (circle_cursor)) then
						delete (module.board.route_restrict.circles, circle_cursor);
						deleted := true;
						exit;
					end if;
					next (circle_cursor);
				end loop;
			end if;

			if not deleted then
				nothing_found (catch_zone);
			end if;
			
		end delete;

		
	begin
		log (text => "module " & to_string (module_name) &
			" deleting route restrict segment" &
			" in" & to_string (catch_zone),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> delete'access);
		
	end delete_route_restrict;

	


	procedure draw_zone (
		module_cursor	: in pac_generic_modules.cursor;
		zone			: in type_route_restrict_contour;
		log_threshold	: in type_log_level)
	is 
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.route_restrict.contours.append (zone);
		end;

	begin
		log (text => "module " & to_string (module_cursor) 
			& "drawing route restrict zone",
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

	end draw_zone;


	
end et_board_ops.route_restrict;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
