------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                BOARD OPERATIONS / SILKSCREEN                             --
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


package body et_board_ops.silkscreen is

	use pac_generic_modules;
	
	use pac_silk_lines;
	use pac_silk_arcs;
	use pac_silk_circles;
	use pac_silk_texts;
	
	
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
						container	=> module.board.silkscreen.top.lines,
						new_item	=> line);
					
				when BOTTOM =>
					append (
						container	=> module.board.silkscreen.bottom.lines,
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
					module.board.silkscreen.top.lines.iterate (query_line'access);

				when BOTTOM =>
					module.board.silkscreen.bottom.lines.iterate (query_line'access);
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

	


	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		line_cursor		: in pac_silk_lines.cursor;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			top 	: pac_silk_lines.list renames module.board.silkscreen.top.lines;
			bottom	: pac_silk_lines.list renames module.board.silkscreen.bottom.lines;

			
			procedure query_line (
				line	: in out type_silk_line)
			is begin
				modify_status (line, operation);
			end query_line;

			
			lc : pac_silk_lines.cursor;
			
			procedure query_top is begin
				if not top.is_empty then
					lc := top.first;
					while lc /= pac_silk_lines.no_element loop
						if lc = line_cursor then
							top.update_element (lc, query_line'access);
							exit;					
						end if;
						next (lc);
					end loop;
				end if;
			end query_top;

			procedure query_bottom is begin
				if not bottom.is_empty then
					lc := bottom.first;
					while lc /= pac_silk_lines.no_element loop
						if lc = line_cursor then
							bottom.update_element (lc, query_line'access);
							exit;					
						end if;
						next (lc);
					end loop;
				end if;
			end query_bottom;

			
		begin
			query_top;

			if lc = pac_silk_lines.no_element then
				query_bottom;
			end if;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of "
			& to_string (element (line_cursor))
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;


	


	procedure propose_lines (
		module_cursor	: in pac_generic_modules.cursor;
		point			: in type_vector_model; -- x/y
		face			: in type_face;
		zone			: in type_accuracy; -- the circular area around the place
		count			: in out natural; -- the number of affected lines
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			lc : pac_silk_lines.cursor;

			procedure query_line (
				line	: in out type_silk_line)
			is 
				use et_object_status;
			begin
				if within_accuracy (
					line	=> line,
					width	=> line.width,
					point	=> point,
					zone	=> zone)
				then
					set_proposed (line);
					count := count + 1;
					log (text => to_string (line), level => log_threshold + 1);
				end if;
			end query_line;

			
			procedure query_top is 
				top : pac_silk_lines.list renames module.board.silkscreen.top.lines;
			begin
				if not top.is_empty then
					lc := top.first;
					while lc /= pac_silk_lines.no_element loop
						top.update_element (lc, query_line'access);
						next (lc);
					end loop;
				end if;
			end query_top;

			
			procedure query_bottom is 
				bottom : pac_silk_lines.list renames module.board.silkscreen.bottom.lines;
			begin
				if not bottom.is_empty then
					lc := bottom.first;
					while lc /= pac_silk_lines.no_element loop
						bottom.update_element (lc, query_line'access);
						next (lc);
					end loop;
				end if;
			end query_bottom;

			
		begin
			case face is
				when TOP	=> query_top;
				when BOTTOM	=> query_bottom;
			end case;
		end query_module;
		
		
	begin
		log (text => "proposing lines at " & to_string (point)
			 & " face " & to_string (face)
			 & " zone " & accuracy_to_string (zone),
			 level => log_threshold);

		log_indentation_up;

		count := 0;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end propose_lines;



	

	procedure reset_proposed_lines (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			top 	: pac_silk_lines.list renames module.board.silkscreen.top.lines;
			bottom	: pac_silk_lines.list renames module.board.silkscreen.bottom.lines;

			
			procedure query_line (
				line	: in out type_silk_line)
			is 
				use et_object_status;
			begin
				reset_status (line);
			end query_line;

			
			lc : pac_silk_lines.cursor;
			
			procedure query_top is begin
				if not top.is_empty then
					lc := top.first;
					while lc /= pac_silk_lines.no_element loop
						top.update_element (lc, query_line'access);
						next (lc);
					end loop;
				end if;
			end query_top;

			
			procedure query_bottom is begin
				if not bottom.is_empty then
					lc := bottom.first;
					while lc /= pac_silk_lines.no_element loop
						bottom.update_element (lc, query_line'access);
						next (lc);
					end loop;
				end if;
			end query_bottom;

			
		begin
			query_top;

			if lc = pac_silk_lines.no_element then
				query_bottom;
			end if;
		end query_module;


		
	begin
		log (text => "resetting proposed lines",
			 level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_proposed_lines;


	
	

	function get_first_line (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_line_segment
	is
		result : type_line_segment;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			proceed : aliased boolean := true;

			top_items 		: pac_silk_lines.list renames module.board.silkscreen.top.lines;
			bottom_items	: pac_silk_lines.list renames module.board.silkscreen.bottom.lines;

			
			procedure query_line (c : in pac_silk_lines.cursor) is
				line : type_silk_line renames element (c);
				use et_object_status;
			begin
				case flag is
					when PROPOSED =>
						if is_proposed (line) then
							result.cursor := c;
							proceed := false;
						end if;

					when SELECTED =>
						if is_selected (line) then
							result.cursor := c;
							proceed := false;
						end if;

					when others =>
						null; -- CS
				end case;
			end query_line;
			

			
		begin
			-- Query the objects in the top layer first:
			iterate (top_items, query_line'access, proceed'access);
			result.face := top;

			-- If nothing found, then query the bottom layer:
			if proceed then
				iterate (bottom_items, query_line'access, proceed'access);
				result.face := bottom;
			end if;

			-- If still nothing found, error:
			if proceed then
				raise constraint_error; -- CS
			end if;
		end query_module;

			
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first line / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;

		return result;
	end get_first_line;



	

	procedure next_proposed_line (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in out type_line_segment;
		-- CS last_item		: in out boolean;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is
			top_items 		: pac_silk_lines.list renames module.board.silkscreen.top.lines;
			bottom_items	: pac_silk_lines.list renames module.board.silkscreen.bottom.lines;
			
			proceed : boolean := true;
		
			
			procedure query_items (
				items			: in pac_silk_lines.list;
				start_at_first	: in boolean := false) 
			is 
				c : pac_silk_lines.cursor;
				do_iterate : boolean := false;
			begin
				-- If there are no items, then nothing to do.
				if not items.is_empty then
					
					-- Preset the cursor:
					if start_at_first then
						c := items.first; -- begin of list
						do_iterate := true;
					else
						c := line.cursor; -- forward to the position given by caller
						
						-- Advance to the next item after the given item:
						if c /= items.last then
							next (c);
							do_iterate := true;
						end if;
					end if;


					if do_iterate then
						while c /= pac_silk_lines.no_element loop
							if is_proposed (c) then
								line.cursor := c;
								proceed := false;
								exit; -- no further probing required
							end if;
							
							next (c);						
						end loop;
					end if;
				end if;
			end query_items;

			
		begin
			case line.face is
				when TOP =>
					query_items (top_items);

					-- If nothing found, start searching the bottom items:
					if proceed then
						query_items (bottom_items, start_at_first => true);
					end if;

					-- If still nothing found, search the top items from the begining:
					if proceed then
						query_items (top_items, start_at_first => true);
					end if;

					
				when BOTTOM =>
					query_items (bottom_items);

					-- If nothing found, start searching the top items:
					if proceed then
						query_items (top_items, start_at_first => true);
					end if;

					-- If still nothing found, search the bottom items from the begining:
					if proceed then
						query_items (bottom_items, start_at_first => true);
					end if;
				
			end case;


			-- If still nothing found, error:
			if proceed then
				raise constraint_error; -- CS
			end if;
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " advancing to next proposed line",
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end next_proposed_line;


	

	
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
					line_cursor := module.board.silkscreen.top.lines.find (line);
					module.board.silkscreen.top.lines.update_element (line_cursor, query_line'access);
					
				when BOTTOM =>
					line_cursor := module.board.silkscreen.bottom.lines.find (line);
					module.board.silkscreen.bottom.lines.update_element (line_cursor, query_line'access);
			end case;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " face" & to_string (face) 
			& " moving silkscreen " & to_string (line)
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
						container	=> module.board.silkscreen.top.arcs,
						new_item	=> arc);

				when BOTTOM =>
					append (
						container	=> module.board.silkscreen.bottom.arcs,
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
						container	=> module.board.silkscreen.top.circles,
						new_item	=> circle);

				when BOTTOM =>
					append (
						container	=> module.board.silkscreen.bottom.circles,
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






	procedure draw_zone (
		module_cursor	: in pac_generic_modules.cursor;
		zone			: in type_silk_contour;
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
			use pac_silk_contours;
			c : pac_silk_contours.cursor;

			-- This procedure tests whether the candidate
			-- zone z is open. If z is open, then it tries
			-- to merge the given zone into z. If the merge operation
			-- succeedes then no more zones are iterated (flag proceed):
			procedure query_zone (z : in out type_silk_contour) is
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
					c := module.board.silkscreen.top.zones.first;

					while c /= pac_silk_contours.no_element and proceed loop
						module.board.silkscreen.top.zones.update_element (c, query_zone'access);
						next (c);
					end loop;

					-- If no open zone found, then add the given zone
					-- as a new zone:
					if proceed then
						-- put_line ("added as new zone");
						log (text => "added as new zone", level => log_threshold + 1);
						module.board.silkscreen.top.zones.append (zone);
					end if;

					
				when BOTTOM =>
					-- Iterate through the already existing zones:
					c := module.board.silkscreen.bottom.zones.first;

					while c /= pac_silk_contours.no_element and proceed loop
						module.board.silkscreen.bottom.zones.update_element (c, query_zone'access);
						next (c);
					end loop;

					-- If no open zone found, then add the given zone
					-- as a new zone:
					if proceed then
						log (text => "added as new zone", level => log_threshold + 1);
						module.board.silkscreen.bottom.zones.append (zone);
					end if;
			end case;
		end query_module;


	begin
		log (text => "module " & to_string (module_cursor) 
			 & "drawing silkscreen zone"			 
			 & to_string (face)
			 & " " & to_string (contour => zone, full => true),
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

	end draw_zone;

	
	

	
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
				line_cursor   	:= module.board.silkscreen.top.lines.first;
				arc_cursor    	:= module.board.silkscreen.top.arcs.first;
				circle_cursor	:= module.board.silkscreen.top.circles.first;
			else
				line_cursor   	:= module.board.silkscreen.bottom.lines.first;
				arc_cursor    	:= module.board.silkscreen.bottom.arcs.first;
				circle_cursor	:= module.board.silkscreen.bottom.circles.first;
			end if;
			
			-- first search for a matching segment among the lines
			while line_cursor /= pac_silk_lines.no_element loop
				if element (line_cursor).on_line (point) then
				-- CS use get_shortest_distance (point, element)
				-- and compare distance with accuracy	

					if face = TOP then
						delete (module.board.silkscreen.top.lines, line_cursor);
					else
						delete (module.board.silkscreen.bottom.lines, line_cursor);
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
							delete (module.board.silkscreen.top.arcs, arc_cursor);
						else
							delete (module.board.silkscreen.bottom.arcs, arc_cursor);
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
							delete (module.board.silkscreen.top.circles, circle_cursor);
						else
							delete (module.board.silkscreen.bottom.circles, circle_cursor);
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
					line_cursor := module.board.silkscreen.top.lines.find (line);

					-- Delete the line if it exists:
					if line_cursor /= pac_silk_lines.no_element then
						module.board.silkscreen.top.lines.delete (line_cursor); 
					else
						null; -- CS message
					end if;

				when BOTTOM =>
					-- Locate the given line in the bottom silkscreen layer:
					line_cursor := module.board.silkscreen.bottom.lines.find (line);

					-- Delete the line if it exists:
					if line_cursor /= pac_silk_lines.no_element then
						module.board.silkscreen.bottom.lines.delete (line_cursor); 
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







	

	procedure add_text (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		text			: in type_text_fab_with_content;
		log_threshold	: in type_log_level)
	is 
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin			
			case face is
				when TOP =>
					append (module.board.silkscreen.top.texts, (text with null record));

				when BOTTOM =>
					append (module.board.silkscreen.bottom.texts, (text with null record));
			end case;
		end query_module;

	begin
		log (text => "module " & to_string (module_cursor)
			& " placing text in silkscreen at"
			& to_string (text.position)
			& " face" & to_string (face),
			level => log_threshold);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

	end add_text;





	
	
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
					module.board.silkscreen.top.texts.iterate (query_text'access);

				when BOTTOM =>
					module.board.silkscreen.bottom.texts.iterate (query_text'access);
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
			end query_text;
			
		begin
			case face is
				when TOP =>
					text_cursor := module.board.silkscreen.top.texts.find (text);
					module.board.silkscreen.top.texts.update_element (text_cursor, query_text'access);

				when BOTTOM =>
					text_cursor := module.board.silkscreen.bottom.texts.find (text);
					module.board.silkscreen.bottom.texts.update_element (text_cursor, query_text'access);
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





	procedure add_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		placeholder		: in type_text_placeholder;
		face			: in type_face;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			use pac_text_placeholders;						
		begin
			case face is
				when TOP =>
					module.board.silkscreen.top.placeholders.append (placeholder);

				when BOTTOM =>
					module.board.silkscreen.bottom.placeholders.append (placeholder);
			end case;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " adding text placeholder in silkscreen "
			& to_string (placeholder)
			& " face " & to_string (face),
			level => log_threshold);

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end add_placeholder;

	

	
	
end et_board_ops.silkscreen;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
