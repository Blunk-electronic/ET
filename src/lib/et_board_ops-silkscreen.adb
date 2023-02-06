------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                BOARD OPERATIONS / SILKSCREEN                             --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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


package body et_board_ops.silkscreen is

	use pac_generic_modules;
	
	procedure draw_silk_screen_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in type_silk_line;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is			
			use pac_silk_lines;
		begin
			case face is
				when TOP =>
					append (
						container	=> module.board.silk_screen.top.lines,
						new_item	=> line);
					
				when BOTTOM =>
					append (
						container	=> module.board.silk_screen.bottom.lines,
						new_item	=> line);
			end case;
		end;
							   
	begin -- draw_silk_scree_line
		log (text => "module " & to_string (module_name) &
			" drawing silk screen line" &
			" face" & to_string (face) &
			to_string (line),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end draw_silk_screen_line;

	
	procedure draw_silk_screen_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_silk_arc;		
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_silk_arcs;
		begin
			case face is
				when TOP =>
					append (
						container	=> module.board.silk_screen.top.arcs,
						new_item	=> arc);

				when BOTTOM =>
					append (
						container	=> module.board.silk_screen.bottom.arcs,
						new_item	=> arc);
			end case;
		end;
							   
	begin -- draw_silk_screen_arc
		log (text => "module " & to_string (module_name) &
			" drawing silk screen arc" &
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

	end draw_silk_screen_arc;

	
	procedure draw_silk_screen_circle (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		circle			: in type_silk_circle;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_silk_circles;
		begin
			case face is
				when TOP =>
					append (
						container	=> module.board.silk_screen.top.circles,
						new_item	=> circle);

				when BOTTOM =>
					append (
						container	=> module.board.silk_screen.bottom.circles,
						new_item	=> circle);

			end case;
		end;
							   
	begin -- draw_silk_screen_circle
		log (text => "module " & to_string (module_name) &
			" drawing silk screen circle" &
			" face" & to_string (face) &
			to_string (circle),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end draw_silk_screen_circle;

	
	procedure delete_silk_screen (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		point			: in type_point; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure delete (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_silk_lines;
			use pac_silk_arcs;
			use pac_silk_circles;
			line_cursor   : pac_silk_lines.cursor;
			arc_cursor    : pac_silk_arcs.cursor;
			circle_cursor : pac_silk_circles.cursor;

			deleted : boolean := false; -- goes true if at least one segment has been deleted
		begin
			if face = TOP then
				line_cursor   	:= module.board.silk_screen.top.lines.first;
				arc_cursor    	:= module.board.silk_screen.top.arcs.first;
				circle_cursor	:= module.board.silk_screen.top.circles.first;
			else
				line_cursor   	:= module.board.silk_screen.bottom.lines.first;
				arc_cursor    	:= module.board.silk_screen.bottom.arcs.first;
				circle_cursor	:= module.board.silk_screen.bottom.circles.first;
			end if;
			
			-- first search for a matching segment among the lines
			while line_cursor /= pac_silk_lines.no_element loop
				if element (line_cursor).on_line (point) then
				-- CS use get_shortest_distance (point, element)
				-- and compare distance with accuracy	

					if face = TOP then
						delete (module.board.silk_screen.top.lines, line_cursor);
					else
						delete (module.board.silk_screen.bottom.lines, line_cursor);
					end if;
					deleted := true;
					exit;
				end if;
				next (line_cursor);
			end loop;

			-- if no line found, search among arcs
			if not deleted then
				while arc_cursor /= pac_silk_arcs.no_element loop
					if element (arc_cursor).on_arc (point) then
						-- CS use get_shortest_distance (point, element)
						-- and compare distance with accuracy	
						if face = TOP then
							delete (module.board.silk_screen.top.arcs, arc_cursor);
						else
							delete (module.board.silk_screen.bottom.arcs, arc_cursor);
						end if;
						deleted := true;
						exit;
					end if;
					next (arc_cursor);
				end loop;
			end if;

			-- if no arc found, search among circles
			if not deleted then
				while circle_cursor /= pac_silk_circles.no_element loop
					
					if element (circle_cursor).on_circle (point) then
						-- CS use get_shortest_distance (point, element)
						-- and compare distance with accuracy	
						if face = TOP then
							delete (module.board.silk_screen.top.circles, circle_cursor);
						else
							delete (module.board.silk_screen.bottom.circles, circle_cursor);
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

		
	begin -- delete_silk_screen
		log (text => "module " & to_string (module_name) &
			" deleting silk screen segment face" & to_string (face) &
			" at" & to_string (point) &
			" accuracy" & catch_zone_to_string (accuracy),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> delete'access);
		
	end delete_silk_screen;

	
	function get_texts (
		module_cursor	: in pac_generic_modules.cursor;
		point			: in type_point;
		catch_zone		: in type_catch_zone; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_silk_texts.list
	is
		use et_text;
		use pac_silk_texts;
		result : pac_silk_texts.list;

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) 
		is
			procedure query_text (c : in pac_silk_texts.cursor) is
				text : type_silk_text renames element (c);
			begin
				if in_catch_zone (
					point_1		=> point,
					catch_zone	=> catch_zone,
					point_2		=> text.position.place)
				then
					log (text => to_string (text.position.place) 
						& " content " & enclose_in_quotes (to_string (text.content)),
						level => log_threshold + 2);
						
					result.append (text);
				end if;
			end query_text;
			
		begin
			module.board.silk_screen.top.texts.iterate (query_text'access);
		end query_module;

		
	begin
		log (text => "looking up texts at" & to_string (point) 
			 & " catch zone" & catch_zone_to_string (catch_zone),
			 level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log (text => "found" & count_type'image (result.length),
			 level => log_threshold + 1);
		
		log_indentation_down;
		return result;
	end get_texts;


	
end et_board_ops.silkscreen;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
