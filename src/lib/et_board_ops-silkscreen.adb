------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                BOARD OPERATIONS / SILKSCREEN                             --
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


package body et_board_ops.silkscreen is

	use pac_generic_modules;
	
	use pac_silk_lines;
	use pac_silk_arcs;
	use pac_silk_circles;

	
	procedure draw_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in type_silk_line;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
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

	end draw_line;


	function get_lines (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		point			: in type_vector_model;
		zone			: in type_accuracy; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_silk_lines.list
	is
		result : pac_silk_lines.list;

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is

			procedure query_line (c : in pac_silk_lines.cursor) is
				line : type_silk_line renames element (c);
			begin
				if within_accuracy (
					line	=> line,
					width	=> line.width,
					point	=> point,
					zone	=> zone)
				then
					result.append (line);
				end if;
			end query_line;
			
		begin
			case face is
				when TOP =>
					module.board.silk_screen.top.lines.iterate (query_line'access);

				when BOTTOM =>
					module.board.silk_screen.bottom.lines.iterate (query_line'access);
			end case;
		end query_module;
			
	begin
		log (text => "looking up lines at" & to_string (point) 
			 & " zone" & accuracy_to_string (zone),
			 level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log (text => "found" & count_type'image (result.length),
			 level => log_threshold + 1);
		
		log_indentation_down;		

		return result;
	end get_lines;

	
	
	procedure move_line (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		line			: in type_silk_line;
		point_of_attack	: in type_vector_model;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			line_cursor : pac_silk_lines.cursor;

			procedure query_line (line : in out type_silk_line) is
			begin
				-- case coordinates is
					-- when ABSOLUTE =>
						move_line_to (line, point_of_attack, destination);
						-- null;
					-- when RELATIVE =>
						-- null;
						-- CS
				-- end case;
			end query_line;
			
		begin
			case face is
				when TOP =>
					line_cursor := module.board.silk_screen.top.lines.find (line);
					module.board.silk_screen.top.lines.update_element (line_cursor, query_line'access);
					
				when BOTTOM =>
					line_cursor := module.board.silk_screen.bottom.lines.find (line);
					module.board.silk_screen.bottom.lines.update_element (line_cursor, query_line'access);
			end case;
		end query_module;

	begin
		log (text => "module " 
			& enclose_in_quotes (to_string (key (module_cursor)))
			& " face" & to_string (face) 
			& " moving assy doc " & to_string (line)
			& " point of attack " & to_string (point_of_attack)
			& " to" & to_string (destination),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (						
			position	=> module_cursor,
			process		=> query_module'access);

		
		log_indentation_down;
	end move_line;



	
	procedure draw_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_silk_arc;		
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
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

	end draw_arc;

	
	procedure draw_circle (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		circle			: in type_silk_circle;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
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

	end draw_circle;


	
	procedure delete (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		point			: in type_vector_model; -- x/y
		zone			: in type_accuracy;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		
		procedure delete (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
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
				nothing_found (point, zone);
			end if;
			
		end delete;

		
	begin -- delete_silk_screen
		log (text => "module " & to_string (module_name) &
			" deleting silkscreen segment face" & to_string (face) &
			" at" & to_string (point) &
			" zone" & accuracy_to_string (zone),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> delete'access);
		
	end delete;


	

	procedure delete (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		line			: in type_silk_line;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_silk_lines;
			line_cursor : pac_silk_lines.cursor;
		begin
			case face is
				when TOP =>
					-- Locate the given line in the top silkscreen layer:
					line_cursor := module.board.silk_screen.top.lines.find (line);

					-- Delete the line if it exists:
					if line_cursor /= pac_silk_lines.no_element then
						module.board.silk_screen.top.lines.delete (line_cursor); 
					else
						null; -- CS message
					end if;

				when BOTTOM =>
					-- Locate the given line in the bottom silkscreen layer:
					line_cursor := module.board.silk_screen.bottom.lines.find (line);

					-- Delete the line if it exists:
					if line_cursor /= pac_silk_lines.no_element then
						module.board.silk_screen.bottom.lines.delete (line_cursor); 
					else
						null; -- CS message
					end if;
			end case;
		end query_module;


	begin
		log (text => "module " 
			& enclose_in_quotes (to_string (key (module_cursor)))
			& " face" & to_string (face) 
			& " deleting in silkscreen" & to_string (line),
			level => log_threshold);
		
		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end delete;

	
	
	function get_texts (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		point			: in type_vector_model;
		zone			: in type_accuracy; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_silk_texts.list
	is
		use et_text;
		use pac_silk_texts;
		result : pac_silk_texts.list;

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			procedure query_text (c : in pac_silk_texts.cursor) is
				text : type_silk_text renames element (c);
			begin
				if within_accuracy (
					point_1	=> point,
					zone	=> zone,
					point_2	=> text.position.place)
				then
					log (text => to_string (text.position.place) 
						& " content " & enclose_in_quotes (to_string (text.content)),
						level => log_threshold + 2);
						
					result.append (text);
				end if;
			end query_text;
			
		begin
			case face is
				when TOP =>
					module.board.silk_screen.top.texts.iterate (query_text'access);

				when BOTTOM =>
					module.board.silk_screen.bottom.texts.iterate (query_text'access);
			end case;
		end query_module;

		
	begin
		log (text => "module " 
			& enclose_in_quotes (to_string (key (module_cursor)))
			& " face" & to_string (face) 
			& " looking up silkscreen texts at" & to_string (point) 
			& " zone" & accuracy_to_string (zone),
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



	procedure move_text (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		text			: in type_silk_text;
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model;
		log_threshold	: in type_log_level)
	is
		old_position : constant type_vector_model := get_place (text);
		new_position : type_vector_model;
		offset : type_distance_relative;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			text_cursor : pac_silk_texts.cursor;

			procedure query_text (text : in out type_silk_text) is begin
				move_text (text, offset);
				move_vector_text (text.vectors, offset);
			end query_text;
			
		begin
			case face is
				when TOP =>
					text_cursor := module.board.silk_screen.top.texts.find (text);
					module.board.silk_screen.top.texts.update_element (text_cursor, query_text'access);

				when BOTTOM =>
					text_cursor := module.board.silk_screen.bottom.texts.find (text);
					module.board.silk_screen.bottom.texts.update_element (text_cursor, query_text'access);
			end case;
		end query_module;

		
	begin
		case coordinates is
			when ABSOLUTE =>
				new_position := point;
				offset := get_distance_relative (old_position, new_position);

			when RELATIVE =>
				new_position := point;
				offset := to_distance_relative (point);
				move_by (new_position, offset);
		end case;
		
		log (text => "module " 
			& enclose_in_quotes (to_string (key (module_cursor)))
			& " face" & to_string (face) 
			& " moving silkscreen text from" & to_string (old_position)
			& " to" & to_string (new_position), -- CS by offset
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

	end move_text;


	
end et_board_ops.silkscreen;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
