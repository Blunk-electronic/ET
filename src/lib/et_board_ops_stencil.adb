------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   BOARD OPERATIONS / STENCIL                             --
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

with et_module;							use et_module;


package body et_board_ops_stencil is

	use pac_stencil_lines;
	use pac_stencil_arcs;
	use pac_stencil_circles;
	

	procedure add_line (
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
							   
	begin
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

	end add_line;





	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in type_object_line;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_line (
				line	: in out type_stencil_line)
			is begin
				modify_status (line, operation);
			end query_line;

			
			procedure query_top is 
				top : pac_stencil_lines.list renames module.board.stencil.top.lines;
			begin
				top.update_element (line.cursor, query_line'access);
			end query_top;

			
			procedure query_bottom is 
				bottom	: pac_stencil_lines.list renames module.board.stencil.bottom.lines;
			begin
				bottom.update_element (line.cursor, query_line'access);
			end query_bottom;

			
		begin
			case line.face is
				when TOP =>
					query_top;

				when BOTTOM =>
					query_bottom;
			end case;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of "
			& to_string (element (line.cursor)) -- CS: log top/bottom			
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
		face			: in type_face;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			lc : pac_stencil_lines.cursor;

			procedure query_line (
				line	: in out type_stencil_line)
			is begin
				if in_catch_zone (
					zone	=> catch_zone,
					line	=> line,
					width	=> line.width)
				then
					set_proposed (line);
					count := count + 1;
					log (text => to_string (line), level => log_threshold + 1);
				end if;
			end query_line;

			
			procedure query_top is 
				top : pac_stencil_lines.list renames module.board.stencil.top.lines;
			begin
				if not top.is_empty then
					lc := top.first;
					while lc /= pac_stencil_lines.no_element loop
						top.update_element (lc, query_line'access);
						next (lc);
					end loop;
				end if;
			end query_top;

			
			procedure query_bottom is 
				bottom : pac_stencil_lines.list renames module.board.stencil.bottom.lines;
			begin
				if not bottom.is_empty then
					lc := bottom.first;
					while lc /= pac_stencil_lines.no_element loop
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
		log (text => "module " & to_string (module_cursor)
			 & " proposing lines in" & to_string (catch_zone)
			 & " face " & to_string (face),
			 level => log_threshold);

		log_indentation_up;

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
			top 	: pac_stencil_lines.list renames module.board.stencil.top.lines;
			bottom	: pac_stencil_lines.list renames module.board.stencil.bottom.lines;

			
			procedure query_line (
				line	: in out type_stencil_line)
			is begin
				reset_status (line);
			end query_line;

			
			lc : pac_stencil_lines.cursor;
			
			procedure query_top is begin
				if not top.is_empty then
					lc := top.first;
					while lc /= pac_stencil_lines.no_element loop
						top.update_element (lc, query_line'access);
						next (lc);
					end loop;
				end if;
			end query_top;

			
			procedure query_bottom is begin
				if not bottom.is_empty then
					lc := bottom.first;
					while lc /= pac_stencil_lines.no_element loop
						bottom.update_element (lc, query_line'access);
						next (lc);
					end loop;
				end if;
			end query_bottom;

			
		begin
			query_top;
			query_bottom;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " resetting proposed lines",
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
		return type_object_line
	is
		result : type_object_line;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			proceed : aliased boolean := true;

			top_items 		: pac_stencil_lines.list renames module.board.stencil.top.lines;
			bottom_items	: pac_stencil_lines.list renames module.board.stencil.bottom.lines;

			
			procedure query_line (c : in pac_stencil_lines.cursor) is begin
				case flag is
					when PROPOSED =>
						if is_proposed (c) then
							result.cursor := c;
							proceed := false;
						end if;

					when SELECTED =>
						if is_selected (c) then
							result.cursor := c;
							proceed := false;
						end if;

					when others =>
						null; -- CS
				end case;
			end query_line;
			

			
		begin
			-- Query the lines in the top layer first:
			iterate (top_items, query_line'access, proceed'access);
			result.face := top;

			-- If nothing found, then query the bottom layer:
			if proceed then
				iterate (bottom_items, query_line'access, proceed'access);
				result.face := bottom;
			end if;

			-- If still nothing found, return TOP and no_element:
			if proceed then
				result := (others => <>);	
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



	

	
	procedure move_line (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		line			: in type_stencil_line;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			line_cursor : pac_stencil_lines.cursor;

			
			procedure query_line (line : in out type_stencil_line) is
			begin
				-- case coordinates is
					-- when ABSOLUTE =>
						attack (line, point_of_attack, destination);
						-- null;
					-- when RELATIVE =>
						-- null;
						-- CS
				-- end case;
			end query_line;

			
		begin
			case face is
				when TOP =>
					line_cursor := module.board.stencil.top.lines.find (line);
					module.board.stencil.top.lines.update_element (line_cursor, query_line'access);
					
				when BOTTOM =>
					line_cursor := module.board.stencil.bottom.lines.find (line);
					module.board.stencil.bottom.lines.update_element (line_cursor, query_line'access);
			end case;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " face" & to_string (face) 
			& " moving stencil " & to_string (line)
			& " point of attack " & to_string (point_of_attack)
			& " to" & to_string (destination),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (						
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end move_line;






	procedure delete_line (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		line			: in type_stencil_line;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			line_cursor : pac_stencil_lines.cursor;
		begin
			case face is
				when TOP =>
					-- Locate the given line in the top stencil layer:
					line_cursor := module.board.stencil.top.lines.find (line);

					-- Delete the line if it exists:
					if line_cursor /= pac_stencil_lines.no_element then
						module.board.stencil.top.lines.delete (line_cursor); 
					else
						null; -- CS message
					end if;

				when BOTTOM =>
					-- Locate the given line in the bottom stencil layer:
					line_cursor := module.board.stencil.bottom.lines.find (line);

					-- Delete the line if it exists:
					if line_cursor /= pac_stencil_lines.no_element then
						module.board.stencil.bottom.lines.delete (line_cursor); 
					else
						null; -- CS message
					end if;
			end case;
		end query_module;


	begin
		log (text => "module " & to_string (module_cursor)
			& " face" & to_string (face) 
			& " deleting in stencil" & to_string (line),
			level => log_threshold);
		
		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end delete_line;





-- ARCS:
	
	
	procedure add_arc (
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
							   
	begin
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

	end add_arc;






	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		arc				: in type_object_arc;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_arc (
				arc	: in out type_stencil_arc)
			is begin
				modify_status (arc, operation);
			end query_arc;

			
			procedure query_top is 
				top : pac_stencil_arcs.list renames module.board.stencil.top.arcs;
			begin
				top.update_element (arc.cursor, query_arc'access);
			end query_top;

			
			procedure query_bottom is 
				bottom	: pac_stencil_arcs.list renames module.board.stencil.bottom.arcs;
			begin
				bottom.update_element (arc.cursor, query_arc'access);
			end query_bottom;

			
		begin
			case arc.face is
				when TOP =>
					query_top;

				when BOTTOM =>
					query_bottom;
			end case;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of "
			& to_string (element (arc.cursor)) -- CS: log top/bottom			
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;



	




	procedure propose_arcs (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			lc : pac_stencil_arcs.cursor;

			procedure query_arc (
				arc	: in out type_stencil_arc)
			is begin
				if in_catch_zone (
					zone	=> catch_zone,
					arc	=> arc,
					width	=> arc.width)
				then
					set_proposed (arc);
					count := count + 1;
					log (text => to_string (arc), level => log_threshold + 1);
				end if;
			end query_arc;

			
			procedure query_top is 
				top : pac_stencil_arcs.list renames module.board.stencil.top.arcs;
			begin
				if not top.is_empty then
					lc := top.first;
					while lc /= pac_stencil_arcs.no_element loop
						top.update_element (lc, query_arc'access);
						next (lc);
					end loop;
				end if;
			end query_top;

			
			procedure query_bottom is 
				bottom : pac_stencil_arcs.list renames module.board.stencil.bottom.arcs;
			begin
				if not bottom.is_empty then
					lc := bottom.first;
					while lc /= pac_stencil_arcs.no_element loop
						bottom.update_element (lc, query_arc'access);
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
		log (text => "module " & to_string (module_cursor)
			 & " proposing arcs in " & to_string (catch_zone)
			 & " face " & to_string (face),
			 level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end propose_arcs;


	
	


	procedure reset_proposed_arcs (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			top 	: pac_stencil_arcs.list renames module.board.stencil.top.arcs;
			bottom	: pac_stencil_arcs.list renames module.board.stencil.bottom.arcs;

			
			procedure query_arc (
				arc	: in out type_stencil_arc)
			is begin
				reset_status (arc);
			end query_arc;

			
			lc : pac_stencil_arcs.cursor;
			
			procedure query_top is begin
				if not top.is_empty then
					lc := top.first;
					while lc /= pac_stencil_arcs.no_element loop
						top.update_element (lc, query_arc'access);
						next (lc);
					end loop;
				end if;
			end query_top;

			
			procedure query_bottom is begin
				if not bottom.is_empty then
					lc := bottom.first;
					while lc /= pac_stencil_arcs.no_element loop
						bottom.update_element (lc, query_arc'access);
						next (lc);
					end loop;
				end if;
			end query_bottom;

			
		begin
			query_top;
			query_bottom;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " resetting proposed arcs",
			 level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_proposed_arcs;


	



	function get_first_arc (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object_arc
	is
		result : type_object_arc;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			proceed : aliased boolean := true;

			top_items 		: pac_stencil_arcs.list renames module.board.stencil.top.arcs;
			bottom_items	: pac_stencil_arcs.list renames module.board.stencil.bottom.arcs;

			
			procedure query_arc (c : in pac_stencil_arcs.cursor) is begin
				case flag is
					when PROPOSED =>
						if is_proposed (c) then
							result.cursor := c;
							proceed := false;
						end if;

					when SELECTED =>
						if is_selected (c) then
							result.cursor := c;
							proceed := false;
						end if;

					when others =>
						null; -- CS
				end case;
			end query_arc;
			

			
		begin
			-- Query the arcs in the top layer first:
			iterate (top_items, query_arc'access, proceed'access);
			result.face := top;

			-- If nothing found, then query the bottom layer:
			if proceed then
				iterate (bottom_items, query_arc'access, proceed'access);
				result.face := bottom;
			end if;

			-- If still nothing found, return TOP and no_element:
			if proceed then
				result := (others => <>);	
			end if;
		end query_module;

			
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first arc / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;

		return result;
	end get_first_arc;


	




	
	procedure move_arc (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		arc				: in type_stencil_arc;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			arc_cursor : pac_stencil_arcs.cursor;

			
			procedure query_arc (arc : in out type_stencil_arc) is
			begin
				-- case coordinates is
					-- when ABSOLUTE =>
						attack (arc, point_of_attack, destination);
						-- null;
					-- when RELATIVE =>
						-- null;
						-- CS
				-- end case;
			end query_arc;

			
		begin
			case face is
				when TOP =>
					arc_cursor := module.board.stencil.top.arcs.find (arc);
					module.board.stencil.top.arcs.update_element (arc_cursor, query_arc'access);
					
				when BOTTOM =>
					arc_cursor := module.board.stencil.bottom.arcs.find (arc);
					module.board.stencil.bottom.arcs.update_element (arc_cursor, query_arc'access);
			end case;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " face" & to_string (face) 
			& " moving stencil " & to_string (arc)
			& " point of attack " & to_string (point_of_attack)
			& " to " & to_string (destination),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (						
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end move_arc;



	



	procedure delete_arc (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		arc				: in type_stencil_arc;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_stencil_arcs;
			arc_cursor : pac_stencil_arcs.cursor;
		begin
			case face is
				when TOP =>
					-- Locate the given arc in the top documentation layer:
					arc_cursor := module.board.stencil.top.arcs.find (arc);

					-- Delete the arc if it exists:
					if arc_cursor /= pac_stencil_arcs.no_element then
						module.board.stencil.top.arcs.delete (arc_cursor); 
					else
						null; -- CS message
					end if;

				when BOTTOM =>
					-- Locate the given arc in the bottom documentation layer:
					arc_cursor := module.board.stencil.bottom.arcs.find (arc);

					-- Delete the arc if it exists:
					if arc_cursor /= pac_stencil_arcs.no_element then
						module.board.stencil.bottom.arcs.delete (arc_cursor); 
					else
						null; -- CS message
					end if;
			end case;
		end query_module;


	begin
		log (text => "module " & to_string (module_cursor)
			& " face" & to_string (face) 
			& " deleting stencil arc " & to_string (arc),
			level => log_threshold);
		
		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end delete_arc;

	


	
	
-- CIRLCES:
	
	procedure add_circle (
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

		
	begin
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

	end add_circle;
	

	


-- ZONES:
	

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
					c := module.board.stencil.top.zones.first;

					while c /= pac_stencil_zones.no_element and proceed loop
						module.board.stencil.top.zones.update_element (c, query_zone'access);
						next (c);
					end loop;

					-- If no open zone found, then add the given zone
					-- as a new zone:
					if proceed then
						-- put_line ("added as new zone");
						log (text => "added as new zone", level => log_threshold + 1);
						module.board.stencil.top.zones.append (zone);
					end if;

					
				when BOTTOM =>
					-- Iterate through the already existing zones:
					c := module.board.stencil.bottom.zones.first;

					while c /= pac_stencil_zones.no_element and proceed loop
						module.board.stencil.bottom.zones.update_element (c, query_zone'access);
						next (c);
					end loop;

					-- If no open zone found, then add the given zone
					-- as a new zone:
					if proceed then
						log (text => "added as new zone", level => log_threshold + 1);
						module.board.stencil.bottom.zones.append (zone);
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

	


	
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is
		use pac_contours;
		use pac_segments;
		use pac_stencil_zones;
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_segment (
				segment	: in out type_segment)
			is begin
				modify_status (segment, operation);
			end query_segment;

			
			procedure query_zone (
				zone : in out type_stencil_zone)
			is begin
				if is_circular (zone) then
					null; -- CS
				else
					-- Locate the given segment in the
					-- candidate zone:
					update_element (
						container	=> zone.contour.segments,
						position	=> segment.segment,
						process		=> query_segment'access);

				end if;
			end query_zone;
	
			
		begin
			-- Search the given segment according to its
			-- zone and face:
			case segment.face is
				when TOP =>
					update_element (
						container	=> module.board.stencil.top.zones, 
						position	=> segment.zone, 
						process		=> query_zone'access);

				when BOTTOM =>
					update_element (
						container	=> module.board.stencil.bottom.zones, 
						position	=> segment.zone, 
						process		=> query_zone'access);

			end case;
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of "
			& to_string (segment.segment)
			& " face " & to_string (segment.face)
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;


	


	procedure propose_segments (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		face			: in type_face;
		count			: in out natural;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_stencil_zones;
			zc : pac_stencil_zones.cursor;

			use pac_contours;
			use pac_segments;

			
			procedure query_segment (
				segment	: in out type_segment)
			is begin
				if in_catch_zone (catch_zone, segment) then
					set_proposed (segment);
					count := count + 1;
					log (text => to_string (segment), level => log_threshold + 1);
				end if;
			end query_segment;


			
			procedure query_zone (
				zone : in out type_stencil_zone)
			is
				use pac_contours;
				use pac_segments;
				c : pac_segments.cursor;
				
			begin
				if is_circular (zone) then
					null; -- CS
				else
					c := zone.contour.segments.first;

					while c /= pac_segments.no_element loop
						update_element (
							container	=> zone.contour.segments,
							position	=> c,
							process		=> query_segment'access);

						next (c);
					end loop;
				end if;
			end query_zone;
			
			
		begin
			case face is
				when TOP =>
					zc := module.board.stencil.top.zones.first;

					while zc /= pac_stencil_zones.no_element loop
						update_element (
							container	=> module.board.stencil.top.zones,
							position	=> zc,
							process		=> query_zone'access);
						
						next (zc);
					end loop;

					
				when BOTTOM =>
					zc := module.board.stencil.bottom.zones.first;

					while zc /= pac_stencil_zones.no_element loop
						update_element (
							container	=> module.board.stencil.bottom.zones,
							position	=> zc,
							process		=> query_zone'access);
						
						next (zc);
					end loop;
			end case;	
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " proposing segments in" & to_string (catch_zone)
			 & " face " & to_string (face),
			 level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end propose_segments;


	


	
	
	procedure reset_proposed_segments (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_stencil_zones;
			zc : pac_stencil_zones.cursor;

			use pac_contours;
			use pac_segments;

			
			procedure query_segment (
				segment	: in out type_segment)
			is begin
				reset_status (segment);
			end query_segment;


			
			procedure query_zone (
				zone : in out type_stencil_zone)
			is
				use pac_contours;
				use pac_segments;
				c : pac_segments.cursor;
				
			begin
				if is_circular (zone) then
					null; -- CS
				else
					c := zone.contour.segments.first;

					while c /= pac_segments.no_element loop
						update_element (
							container	=> zone.contour.segments,
							position	=> c,
							process		=> query_segment'access);

						next (c);
					end loop;
				end if;
			end query_zone;
			
			
		begin
			zc := module.board.stencil.top.zones.first;

			while zc /= pac_stencil_zones.no_element loop
				update_element (
					container	=> module.board.stencil.top.zones,
					position	=> zc,
					process		=> query_zone'access);
				
				next (zc);
			end loop;

					
			zc := module.board.stencil.bottom.zones.first;

			while zc /= pac_stencil_zones.no_element loop
				update_element (
					container	=> module.board.stencil.bottom.zones,
					position	=> zc,
					process		=> query_zone'access);
				
				next (zc);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " resetting proposed segments of zones in stencil",
			 level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_proposed_segments;


	

	

	function get_first_segment (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object_segment
	is
		use pac_contours;
		result : type_object_segment;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use pac_contours;
			use pac_segments;
			use pac_stencil_zones;
			
			proceed : aliased boolean := true;

			face : type_face := TOP;
			
			
			procedure query_zone (z : in pac_stencil_zones.cursor) is 

				procedure query_segment (
					c : in pac_segments.cursor) 
				is begin
					case flag is
						when PROPOSED =>
							if is_proposed (c) then
								result.segment := c;
								result.zone := z;
								result.face := face;
								proceed := false;

								log (text => to_string (c), level => log_threshold + 1);
							end if;

						when SELECTED =>
							if is_selected (c) then
								result.segment := c;
								result.zone := z;
								result.face := face;
								proceed := false;

								log (text => to_string (c), level => log_threshold + 1);
							end if;

						when others =>
							null; -- CS
					end case;
				end query_segment;
				
				
				procedure query_segments (z : in type_stencil_zone) is begin
					iterate (
						segments	=> z.contour.segments,
						process		=> query_segment'access,
						proceed		=> proceed'access);				
				end query_segments;

				
			begin
				if element (z).contour.circular then
					null; -- CS
				else
					query_element (z, query_segments'access);
				end if;
			end query_zone;

			
		begin
			-- Iterate the zones in top layer:
			iterate (
				zones	=> module.board.stencil.top.zones,
				process	=> query_zone'access, 
				proceed	=> proceed'access);

			
			-- If nothing found, iterate the bottom layer:
			if proceed then
				face := BOTTOM;
				
				iterate (
					zones	=> module.board.stencil.bottom.zones,
					process	=> query_zone'access, 
					proceed	=> proceed'access);

			end if;
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first segment / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		-- put_line ("found " & to_string (result));
		
		log_indentation_down;

		return result;
	end get_first_segment;






	procedure move_segment (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is
		use pac_contours;
		use pac_segments;
		use pac_stencil_zones;
				
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			-- Moves the candidate segment:
			procedure do_it (s : in out type_segment) is begin
				move_segment (s, point_of_attack, destination);
			end do_it;

			
			procedure query_zone (
				zone : in out type_stencil_zone)
			is 
				c : pac_segments.cursor;
			begin
				if is_circular (zone) then
					null; -- CS
				else
					-- Locate the given segment in 
					-- the candidate zone:
					update_element (
						container	=> zone.contour.segments,
						position	=> segment.segment,
						process		=> do_it'access);

				end if;
			end query_zone;
	
			
		begin
			-- Search for the given segment according to the 
			-- given zone and face:
			case segment.face is
				when TOP =>
					update_element (
						container	=> module.board.stencil.top.zones, 
						position	=> segment.zone, 
						process		=> query_zone'access);

				when BOTTOM =>
					update_element (
						container	=> module.board.stencil.bottom.zones, 
						position	=> segment.zone, 
						process		=> query_zone'access);

			end case;
		end query_module;
		
				
	begin
		log (text => "module " & to_string (module_cursor)
			& " moving stencil zone segment " & to_string (segment.segment)
			& " point of attack " & to_string (point_of_attack)
			& " to" & to_string (destination),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (						
			position	=> module_cursor,
			process		=> query_module'access);

		-- log (text => "new outline:" & to_string (get_outline (module_cursor), true),
		-- 	 level => log_threshold + 1);
		
		log_indentation_down;
	end move_segment;


	



	procedure delete_segment (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment;
		log_threshold	: in type_log_level)
	is
		use pac_contours;
		use pac_segments;
		use pac_stencil_zones;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_zone (
				zone : in out type_stencil_zone)
			is 
				c : pac_segments.cursor;
			begin
				if is_circular (zone) then
					null; -- CS
				else
					-- Delete the given segment:
					c := segment.segment;					
					zone.contour.segments.delete (c);
				end if;
			end query_zone;
	
			
		begin
			-- Search for the given segment according to the 
			-- given zone and face:
			case segment.face is
				when TOP =>
					update_element (
						container	=> module.board.stencil.top.zones, 
						position	=> segment.zone, 
						process		=> query_zone'access);

				when BOTTOM =>
					update_element (
						container	=> module.board.stencil.bottom.zones, 
						position	=> segment.zone, 
						process		=> query_zone'access);

			end case;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " deleting stencil zone segment " 
			& to_string (segment.segment),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (						
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end delete_segment;
	



-- OBJECTS:
	


	function get_count (
		objects : in pac_objects.list)
		return natural
	is begin
		return natural (objects.length);
	end get_count;
	
	
	
	

	function get_first_object (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object
	is
		result_category 	: type_object_category := CAT_VOID;
		result_segment  	: type_object_segment;
		result_line			: type_object_line;
		result_arc			: type_object_arc;
		-- result_circle		: type_object_circle;

		use pac_contours;
		use pac_segments;

		use pac_stencil_lines;
		use pac_stencil_arcs;
		use pac_stencil_circles;
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first object / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;

		
		-- SEARCH FOR A LINE:
		
		-- If a line has been found, then go to the end of this procedure:
		result_line := get_first_line (module_cursor, flag, log_threshold + 1);

		if result_line.cursor /= pac_stencil_lines.no_element then
			-- A line has been found.
			log (text => to_string (element (result_line.cursor))
				 & " face " & to_string (result_line.face),
				 level => log_threshold + 1);
			
			result_category := CAT_LINE;
		end if;

		if result_category /= CAT_VOID then
			goto end_of_search;
		end if;


		
		-- SEARCH FOR AN ARC:
		
		-- If an arc has been found, then go to the end of this procedure:
		result_arc := get_first_arc (module_cursor, flag, log_threshold + 1);

		if result_arc.cursor /= pac_stencil_arcs.no_element then
			-- An arc has been found.
			log (text => to_string (element (result_arc.cursor))
				 & " face " & to_string (result_arc.face),
				 level => log_threshold + 1);
			
			result_category := CAT_ARC;
		end if;

		if result_category /= CAT_VOID then
			goto end_of_search;
		end if;


		-- Now we search for an circle.
		-- If there is one, then go to the end of this procedure:
		-- CS


		-- SEARCH FOR A SEGMENT OF A ZONE:
		
		-- If there is one, then go to the end  of this procedure:
		result_segment := get_first_segment (module_cursor, flag, log_threshold + 1);

		if result_segment.segment /= pac_segments.no_element then
			-- A segment has been found.
			log (text => to_string (result_segment.segment)
					& " face " & to_string (result_segment.face),
					level => log_threshold + 1);
			
			result_category := CAT_ZONE_SEGMENT;
		end if;

		
		-- If still nothing has been found then the category is CAT_VOID.
		

	<<end_of_search>>
		
		log_indentation_down;

		case result_category is
			when CAT_VOID =>
				return (cat => CAT_VOID);

			when CAT_LINE =>
				return (CAT_LINE, result_line);

			when CAT_ARC =>
				return (CAT_ARC, result_arc);

			when CAT_ZONE_SEGMENT =>
				return (CAT_ZONE_SEGMENT, result_segment);
				
		end case;
	end get_first_object;



	
	
	function get_objects (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return pac_objects.list
	is
		use pac_objects;
		result : pac_objects.list;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_stencil_zones;
			zone_cursor : pac_stencil_zones.cursor;
			face : type_face := TOP;
			
			use pac_stencil_lines;
			line_cursor : pac_stencil_lines.cursor;

			use pac_stencil_arcs;
			arc_cursor : pac_stencil_arcs.cursor;

			-- CS circles
			
			
			procedure query_zone (zone : in type_stencil_zone) is
				use pac_contours;
				use pac_segments;
				-- CS test circular flag !!
				segment_cursor : pac_segments.cursor := zone.contour.segments.first;
				
				procedure query_segment (segment : in type_segment) is 

					procedure collect is begin
						result.append ((
							cat		=> CAT_ZONE_SEGMENT,
							segment	=> (face, zone_cursor, segment_cursor)));

						log (text => to_string (segment), level => log_threshold + 2);
					end collect;

				begin
					case flag is
						when PROPOSED =>
							if is_proposed (segment) then
								collect;
							end if;

						when SELECTED =>
							if is_selected (segment) then
								collect;
							end if;
							
						when others => null; -- CS
					end case;
				end query_segment;
				
			begin
				while segment_cursor /= pac_segments.no_element loop
					query_element (segment_cursor, query_segment'access);
					next (segment_cursor);
				end loop;
			end query_zone;
			

			
			procedure query_line (line : in type_stencil_line) is 

				procedure collect is begin
					result.append ((
						cat		=> CAT_LINE,
						line	=> (face, line_cursor)));

					log (text => to_string (line), level => log_threshold + 2);
				end collect;
				
			begin
				case flag is
					when PROPOSED =>
						if is_proposed (line) then
							collect;
						end if;

					when SELECTED =>
						if is_selected (line) then
							collect;
						end if;

					when others => null; -- CS
				end case;
			end query_line;
				



	
			procedure query_arc (arc : in type_stencil_arc) is 

				procedure collect is begin
					result.append ((
						cat	=> CAT_ARC,
						arc	=> (face, arc_cursor)));

					log (text => to_string (arc), level => log_threshold + 2);
				end collect;
				
			begin
				case flag is
					when PROPOSED =>
						if is_proposed (arc) then
							collect;
						end if;

					when SELECTED =>
						if is_selected (arc) then
							collect;
						end if;

					when others => null; -- CS
				end case;
			end query_arc;

			
			
		begin
			log (text => "top zones", level => log_threshold + 1);
			log_indentation_up;
			
			zone_cursor := module.board.stencil.top.zones.first;
			while zone_cursor /= pac_stencil_zones.no_element loop
				query_element (zone_cursor, query_zone'access);
				next (zone_cursor);
			end loop;

			log_indentation_down;

			
			log (text => "top lines", level => log_threshold + 1);
			log_indentation_up;
			
			line_cursor := module.board.stencil.top.lines.first;
			while line_cursor /= pac_stencil_lines.no_element loop
				query_element (line_cursor, query_line'access);
				next (line_cursor);
			end loop;

			log_indentation_down;



			log (text => "top arcs", level => log_threshold + 1);
			log_indentation_up;
			
			arc_cursor := module.board.stencil.top.arcs.first;
			while arc_cursor /= pac_stencil_arcs.no_element loop
				query_element (arc_cursor, query_arc'access);
				next (arc_cursor);
			end loop;

			log_indentation_down;



			
			-- CS circles


			face := BOTTOM;

			log (text => "bottom zones", level => log_threshold + 1);
			log_indentation_up;
			
			zone_cursor := module.board.stencil.bottom.zones.first;
			while zone_cursor /= pac_stencil_zones.no_element loop
				query_element (zone_cursor, query_zone'access);
				next (zone_cursor);
			end loop;

			log_indentation_down;

			
			log (text => "bottom lines", level => log_threshold + 1);
			log_indentation_up;
			
			line_cursor := module.board.stencil.bottom.lines.first;
			while line_cursor /= pac_stencil_lines.no_element loop
				query_element (line_cursor, query_line'access);
				next (line_cursor);
			end loop;

			log_indentation_down;


			log (text => "bottom arcs", level => log_threshold + 1);
			log_indentation_up;
			
			arc_cursor := module.board.stencil.bottom.arcs.first;
			while arc_cursor /= pac_stencil_arcs.no_element loop
				query_element (arc_cursor, query_arc'access);
				next (arc_cursor);
			end loop;

			log_indentation_down;

			-- CS circles
			
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up objects / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element ( -- CS query_module is sufficient
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;

		return result;
	end get_objects;
	




	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of object"
			& type_object_category'image (object.cat)
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		case object.cat is
			when CAT_LINE =>
				modify_status (module_cursor, object.line, operation, log_threshold + 1);

			when CAT_ARC =>
				modify_status (module_cursor, object.arc, operation, log_threshold + 1);

			when CAT_ZONE_SEGMENT =>
				modify_status (module_cursor, object.segment, operation, log_threshold + 1);
				
			when CAT_VOID =>
				null; -- CS
		end case;

		log_indentation_down;
	end modify_status;

	

	

	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object_cursor	: in pac_objects.cursor;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is 
		use pac_objects;
		object : constant type_object := element (object_cursor);
	begin
		modify_status (module_cursor, object, operation, log_threshold);
	end modify_status;



	

	procedure move_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " moving stencil object " 
			-- CS & to_string (object)
			& " point of attack " & to_string (point_of_attack)
			& " to" & to_string (destination),
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_LINE =>
				move_line (module_cursor, object.line.face, 
					element (object.line.cursor),
					point_of_attack, destination,
					log_threshold + 1);

			when CAT_ARC =>
				move_arc (module_cursor, object.arc.face, 
					element (object.arc.cursor),
					point_of_attack, destination,
					log_threshold + 1);
				
			when CAT_ZONE_SEGMENT =>
				move_segment (module_cursor,
					object.segment,
					point_of_attack, destination,
					log_threshold + 1);
							
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end move_object;
	


	

	procedure reset_proposed_objects (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor) &
			" resetting proposed objects",
			level => log_threshold);

		log_indentation_up;

		reset_proposed_lines (module_cursor, log_threshold + 1);
		reset_proposed_arcs (module_cursor, log_threshold + 1);
		-- CS circles

		reset_proposed_segments (module_cursor, log_threshold + 1);

		log_indentation_down;
	end reset_proposed_objects;


	
	


	procedure delete_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " deleting stencil object",
			-- CS & to_string (object)
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_LINE =>
				delete_line (
					module_cursor	=> module_cursor, 
					face			=> object.line.face,
					line			=> element (object.line.cursor),
					log_threshold	=> log_threshold + 1);					

			when CAT_ARC =>
				delete_arc (
					module_cursor	=> module_cursor, 
					face			=> object.arc.face,
					arc				=> element (object.arc.cursor),
					log_threshold	=> log_threshold + 1);					
				
			-- CS circles
				
			when CAT_ZONE_SEGMENT =>
				delete_segment (
					module_cursor	=> module_cursor, 
					segment			=> object.segment,
					log_threshold	=> log_threshold + 1);
				
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end delete_object;
	



	

	
	procedure delete_object (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		catch_zone		: in type_catch_zone;
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
				if in_catch_zone (
					zone	=> catch_zone,
					line	=> element (line_cursor))
				then
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
					if in_catch_zone (
						zone	=> catch_zone,
						arc		=> element (arc_cursor))
					then
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
					if in_catch_zone (
						zone	=> catch_zone,
						circle	=> element (circle_cursor))
					then					
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
				nothing_found (catch_zone);
			end if;
			
		end delete;
		
	begin
		log (text => "module " & to_string (module_name) &
			" deleting stencil object face" & to_string (face) &
			" in" & to_string (catch_zone),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> delete'access);
		
	end delete_object;
	
	
end et_board_ops_stencil;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
