------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   BOARD OPERATIONS / VIA RESTRICT                        --
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


package body et_board_ops.via_restrict is

	use pac_generic_modules;
	
	
-- 	procedure delete_via_restrict (
-- 		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
-- 		point			: in type_vector_model; -- x/y
-- 		accuracy		: in type_accuracy;
-- 		log_threshold	: in type_log_level)
-- 	is
-- 		module_cursor : pac_generic_modules.cursor; -- points to the module being modified
-- 
-- 		procedure delete (
-- 			module_name	: in pac_module_name.bounded_string;
-- 			module		: in out type_generic_module) 
-- 		is
-- 			use pac_via_restrict_lines;
-- 			use pac_via_restrict_arcs;
-- 			use pac_via_restrict_circles;
-- 			line_cursor   : pac_via_restrict_lines.cursor  := module.board.via_restrict.lines.first;
-- 			arc_cursor    : pac_via_restrict_arcs.cursor   := module.board.via_restrict.arcs.first;
-- 			circle_cursor : pac_via_restrict_circles.cursor := module.board.via_restrict.circles.first;
-- 
-- 			deleted : boolean := false; -- goes true if at least one segment has been deleted
-- 		begin
-- 			-- first search for a matching segment among the lines
-- 			while line_cursor /= pac_via_restrict_lines.no_element loop
-- 				if element (line_cursor).on_line (point) then
-- 				-- CS use get_shortest_distance (point, element (line_cursor)
-- 				-- and compare distance with accuracy	
-- 					delete (module.board.via_restrict.lines, line_cursor);
-- 					deleted := true;
-- 					exit;
-- 				end if;
-- 				next (line_cursor);
-- 			end loop;
-- 
-- 			-- if no line found, search among arcs
-- 			if not deleted then
-- 				while arc_cursor /= pac_via_restrict_arcs.no_element loop
-- 					
-- 					if element (arc_cursor).on_arc (point) then
-- 						-- CS use get_shortest_distance (point, element (arc_cursor)
-- 						-- and compare distance with accuracy	
-- 
-- 						delete (module.board.via_restrict.arcs, arc_cursor);
-- 						deleted := true;
-- 						exit;
-- 					end if;
-- 					
-- 					next (arc_cursor);
-- 				end loop;
-- 			end if;
-- 
-- 			-- if no arc found, search among circles
-- 			if not deleted then
-- 				while circle_cursor /= pac_via_restrict_circles.no_element loop
-- 					
-- 					if element (circle_cursor).on_circle (point) then
-- 						-- CS use get_shortest_distance (point, element)
-- 						-- and compare distance with accuracy	
-- 						delete (module.board.via_restrict.circles, circle_cursor);
-- 						deleted := true;
-- 						exit;
-- 					end if;
-- 					
-- 					next (circle_cursor);
-- 				end loop;
-- 			end if;
-- 
-- 			if not deleted then
-- 				nothing_found (point, accuracy);
-- 			end if;
-- 			
-- 		end delete;
-- 		
-- 	begin -- delete_via_restrict
-- 		log (text => "module " & to_string (module_name) &
-- 			" deleting via restrict segment" &
-- 			" at" & to_string (point) &
-- 			" accuracy" & accuracy_to_string (accuracy),
-- 			level => log_threshold);
-- 
-- 		-- locate module
-- 		module_cursor := locate_module (module_name);
-- 
-- 		update_element (
-- 			container	=> generic_modules,
-- 			position	=> module_cursor,
-- 			process		=> delete'access);
-- 		
-- 	end delete_via_restrict;


	procedure dummy is begin null; end;

	
	procedure draw_zone (
		module_cursor	: in pac_generic_modules.cursor;
		zone			: in type_via_restrict_contour;
		log_threshold	: in type_log_level)
	is 
	begin
		null;
	end draw_zone;
	
	
end et_board_ops.via_restrict;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
