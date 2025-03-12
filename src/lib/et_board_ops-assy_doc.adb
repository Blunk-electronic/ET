------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                BOARD OPERATIONS / ASSEMBLY DOCUMENTATION                 --
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

with ada.containers;
with ada.containers.doubly_linked_lists;


package body et_board_ops.assy_doc is

	use pac_generic_modules;
	
	use pac_doc_lines;
	use pac_doc_arcs;
	use pac_doc_circles;
	use pac_doc_texts;

	
	procedure add_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in type_doc_line;
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
						container	=> module.board.assy_doc.top.lines,
						new_item	=> line);
					
				when BOTTOM =>
					append (
						container	=> module.board.assy_doc.bottom.lines,
						new_item	=> line);
			end case;
		end;
							   
	begin
		log (text => "module " & to_string (module_name) &
			" drawing assembly documentation line" &
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

	


	function get_lines (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_doc_lines.list
	is
		result : pac_doc_lines.list;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is

			procedure query_line (c : in pac_doc_lines.cursor) is
				line : type_doc_line renames element (c);
			begin
				if in_catch_zone (
					zone	=> catch_zone,
					line	=> line,
					width	=> line.width)
				then
					result.append (line);
				end if;
			end query_line;

			
		begin
			case face is
				when TOP =>
					module.board.assy_doc.top.lines.iterate (query_line'access);

				when BOTTOM =>
					module.board.assy_doc.bottom.lines.iterate (query_line'access);
			end case;
		end query_module;

		
	begin
		log (text => "looking up lines in" & to_string (catch_zone),
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
		line			: in type_object_line;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_line (
				line	: in out type_doc_line)
			is begin
				modify_status (line, operation);
			end query_line;

			
			procedure query_top is 
				top : pac_doc_lines.list renames module.board.assy_doc.top.lines;
			begin
				top.update_element (line.cursor, query_line'access);
			end query_top;

			
			procedure query_bottom is 
				bottom	: pac_doc_lines.list renames module.board.assy_doc.bottom.lines;
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
			lc : pac_doc_lines.cursor;

			procedure query_line (
				line	: in out type_doc_line)
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
				top : pac_doc_lines.list renames module.board.assy_doc.top.lines;
			begin
				if not top.is_empty then
					lc := top.first;
					while lc /= pac_doc_lines.no_element loop
						top.update_element (lc, query_line'access);
						next (lc);
					end loop;
				end if;
			end query_top;

			
			procedure query_bottom is 
				bottom : pac_doc_lines.list renames module.board.assy_doc.bottom.lines;
			begin
				if not bottom.is_empty then
					lc := bottom.first;
					while lc /= pac_doc_lines.no_element loop
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
			 & " proposing lines in " & to_string (catch_zone)
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
			top 	: pac_doc_lines.list renames module.board.assy_doc.top.lines;
			bottom	: pac_doc_lines.list renames module.board.assy_doc.bottom.lines;

			
			procedure query_line (
				line	: in out type_doc_line)
			is begin
				reset_status (line);
			end query_line;

			
			lc : pac_doc_lines.cursor;
			
			procedure query_top is begin
				if not top.is_empty then
					lc := top.first;
					while lc /= pac_doc_lines.no_element loop
						top.update_element (lc, query_line'access);
						next (lc);
					end loop;
				end if;
			end query_top;

			
			procedure query_bottom is begin
				if not bottom.is_empty then
					lc := bottom.first;
					while lc /= pac_doc_lines.no_element loop
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

			top_items 		: pac_doc_lines.list renames module.board.assy_doc.top.lines;
			bottom_items	: pac_doc_lines.list renames module.board.assy_doc.bottom.lines;

			
			procedure query_line (c : in pac_doc_lines.cursor) is begin
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




	
	procedure next_proposed_line (
		module_cursor	: in pac_generic_modules.cursor;
		line			: in out type_object_line;
		-- CS last_item		: in out boolean;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is
			top_items 		: pac_doc_lines.list renames module.board.assy_doc.top.lines;
			bottom_items	: pac_doc_lines.list renames module.board.assy_doc.bottom.lines;
			
			proceed : boolean := true;
		
			
			procedure query_items (
				items			: in pac_doc_lines.list;
				start_at_first	: in boolean := false) 
			is 
				c : pac_doc_lines.cursor;
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
						while c /= pac_doc_lines.no_element loop
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
		line			: in type_doc_line;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			line_cursor : pac_doc_lines.cursor;

			
			procedure query_line (line : in out type_doc_line) is
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
					line_cursor := module.board.assy_doc.top.lines.find (line);
					module.board.assy_doc.top.lines.update_element (line_cursor, query_line'access);
					
				when BOTTOM =>
					line_cursor := module.board.assy_doc.bottom.lines.find (line);
					module.board.assy_doc.bottom.lines.update_element (line_cursor, query_line'access);
			end case;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
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



	

	procedure delete_line (
		module_cursor	: in pac_generic_modules.cursor;
		face			: in type_face;
		line			: in type_doc_line;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_doc_lines;
			line_cursor : pac_doc_lines.cursor;
		begin
			case face is
				when TOP =>
					-- Locate the given line in the top documentation layer:
					line_cursor := module.board.assy_doc.top.lines.find (line);

					-- Delete the line if it exists:
					if line_cursor /= pac_doc_lines.no_element then
						module.board.assy_doc.top.lines.delete (line_cursor); 
					else
						null; -- CS message
					end if;

				when BOTTOM =>
					-- Locate the given line in the bottom documentation layer:
					line_cursor := module.board.assy_doc.bottom.lines.find (line);

					-- Delete the line if it exists:
					if line_cursor /= pac_doc_lines.no_element then
						module.board.assy_doc.bottom.lines.delete (line_cursor); 
					else
						null; -- CS message
					end if;
			end case;
		end query_module;


	begin
		log (text => "module " & to_string (module_cursor)
			& " face" & to_string (face) 
			& " deleting line in assy doc." & to_string (line),
			level => log_threshold);
		
		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end delete_line;



	


	
	
	procedure add_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_doc_arc;		
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
						container	=> module.board.assy_doc.top.arcs,
						new_item	=> arc);

				when BOTTOM =>
					append (
						container	=> module.board.assy_doc.bottom.arcs,
						new_item	=> arc);
			end case;
		end;
		
	begin
		log (text => "module " & to_string (module_name) &
			" drawing assembly documentation arc" &
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



	
	procedure add_circle (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		circle			: in type_doc_circle;
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
						container	=> module.board.assy_doc.top.circles,
						new_item	=> circle);

				when BOTTOM =>
					append (
						container	=> module.board.assy_doc.bottom.circles,
						new_item	=> circle);

			end case;
		end;
							   
	begin
		log (text => "module " & to_string (module_name) &
			" drawing assembly documentation circle" &
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





	procedure add_zone (
		module_cursor	: in pac_generic_modules.cursor;
		zone			: in type_doc_zone;
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
			use pac_doc_zones;
			c : pac_doc_zones.cursor;

			-- This procedure tests whether the candidate
			-- zone z is open. If z is open, then it tries
			-- to merge the given zone into z. If the merge operation
			-- succeedes then no more zones are iterated (flag proceed):
			procedure query_zone (z : in out type_doc_zone) is
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
					c := module.board.assy_doc.top.zones.first;

					while c /= pac_doc_zones.no_element and proceed loop
						module.board.assy_doc.top.zones.update_element (c, query_zone'access);
						next (c);
					end loop;

					-- If no open zone found, then add the given zone
					-- as a new zone:
					if proceed then
						-- put_line ("added as new zone");
						log (text => "added as new zone", level => log_threshold + 1);
						module.board.assy_doc.top.zones.append (zone);
					end if;

					
				when BOTTOM =>
					-- Iterate through the already existing zones:
					c := module.board.assy_doc.bottom.zones.first;

					while c /= pac_doc_zones.no_element and proceed loop
						module.board.assy_doc.bottom.zones.update_element (c, query_zone'access);
						next (c);
					end loop;

					-- If no open zone found, then add the given zone
					-- as a new zone:
					if proceed then
						log (text => "added as new zone", level => log_threshold + 1);
						module.board.assy_doc.bottom.zones.append (zone);
					end if;
			end case;
		end query_module;


	begin
		log (text => "module " & to_string (module_cursor) 
			 & "drawing assembly documentation zone"			 
			 & to_string (face)
			 & " " & to_string (contour => zone, full => true),
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

	end add_zone;

	

	
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is
		use pac_contours;
		use pac_segments;
		use pac_doc_zones;
		
		
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
				zone : in out type_doc_zone)
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
						container	=> module.board.assy_doc.top.zones, 
						position	=> segment.zone, 
						process		=> query_zone'access);

				when BOTTOM =>
					update_element (
						container	=> module.board.assy_doc.bottom.zones, 
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
			use pac_doc_zones;
			zc : pac_doc_zones.cursor;

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
				zone : in out type_doc_zone)
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
					zc := module.board.assy_doc.top.zones.first;

					while zc /= pac_doc_zones.no_element loop
						update_element (
							container	=> module.board.assy_doc.top.zones,
							position	=> zc,
							process		=> query_zone'access);
						
						next (zc);
					end loop;

					
				when BOTTOM =>
					zc := module.board.assy_doc.bottom.zones.first;

					while zc /= pac_doc_zones.no_element loop
						update_element (
							container	=> module.board.assy_doc.bottom.zones,
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
			use pac_doc_zones;
			zc : pac_doc_zones.cursor;

			use pac_contours;
			use pac_segments;

			
			procedure query_segment (
				segment	: in out type_segment)
			is begin
				reset_status (segment);
			end query_segment;


			
			procedure query_zone (
				zone : in out type_doc_zone)
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
			zc := module.board.assy_doc.top.zones.first;

			while zc /= pac_doc_zones.no_element loop
				update_element (
					container	=> module.board.assy_doc.top.zones,
					position	=> zc,
					process		=> query_zone'access);
				
				next (zc);
			end loop;

					
			zc := module.board.assy_doc.bottom.zones.first;

			while zc /= pac_doc_zones.no_element loop
				update_element (
					container	=> module.board.assy_doc.bottom.zones,
					position	=> zc,
					process		=> query_zone'access);
				
				next (zc);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " resetting proposed segments of zones in assembly documentation",
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
			use pac_doc_zones;
			
			proceed : aliased boolean := true;

			face : type_face := TOP;
			
			
			procedure query_zone (z : in pac_doc_zones.cursor) is 

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
				
				
				procedure query_segments (z : in type_doc_zone) is begin
					iterate (
						segments	=> z.contour.segments,
						process		=> query_segment'access,
						proceed		=> proceed'access);				
				end query_segments;

				
			begin
				if is_circular (z) then
					null; -- CS
				else
					query_element (z, query_segments'access);
				end if;
			end query_zone;

			
		begin
			-- Iterate the zones in top layer:
			iterate (
				zones	=> module.board.assy_doc.top.zones,
				process	=> query_zone'access, 
				proceed	=> proceed'access);

			
			-- If nothing found, iterate the bottom layer:
			if proceed then
				face := BOTTOM;
				
				iterate (
					zones	=> module.board.assy_doc.bottom.zones,
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





	procedure next_proposed_segment (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in out type_object_segment;
		-- CS last_item		: in out boolean;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is
			use pac_contours;
			use pac_segments;

			package pac_proposed_segments is new doubly_linked_lists (type_object_segment);
			use pac_proposed_segments;
			
			-- Here we store the proposed segments of all zones:
			proposed_segments : pac_proposed_segments.list;

			-- This cursor points to a proposed segment:
			ps : pac_proposed_segments.cursor;

			
			use pac_doc_zones;

			-- 1. In the course of this procedure we iterate through all
			--    zones and all segments and collect the proposed segments
			--    in list proposed_segments.
			-- 2. After that we advance inside list proposed_segments 
			--    from the given segment to the next.
			
			-- Whenever we encounter a proposed segment, then
			-- the cursor of the zone, the segment itself and the face
			-- is stored in list proposed_segments:
			zone_cursor		: pac_doc_zones.cursor;
			segment_cursor	: pac_segments.cursor;
			face			: type_face := TOP;
			
			
			procedure query_zone (
				zone : in type_doc_zone)
			is 

				procedure query_segment (
					segment : in type_segment)
				is begin
					if is_proposed (segment) then
						proposed_segments.append ((face, zone_cursor, segment_cursor));
					end if;
				end query_segment;
							

			begin
				if is_circular (zone) then
					null; -- CS
				else
					-- Iterate the segments of the candidate zone:
					segment_cursor := zone.contour.segments.first;

					while segment_cursor /= pac_segments.no_element loop
						query_element (segment_cursor, query_segment'access);
						next (segment_cursor);
					end loop;					
				end if;
			end query_zone;


			-- For the number of proposed segments found:
			ct : count_type;

		begin
			-- Step 1:
			-- Get the proposed segments of top and bottom zones:
			zone_cursor := module.board.assy_doc.top.zones.first;
			while zone_cursor /= pac_doc_zones.no_element loop
				query_element (zone_cursor, query_zone'access);
				next (zone_cursor);
			end loop;

			face := BOTTOM;

			zone_cursor := module.board.assy_doc.bottom.zones.first;
			while zone_cursor /= pac_doc_zones.no_element loop
				query_element (zone_cursor, query_zone'access);
				next (zone_cursor);
			end loop;

			
			-- Get the number of segments that have been found
			-- during step 1:
			ct := proposed_segments.length;
			
			log (text => "proposed segments total: " & count_type'image (ct),
				 level => log_threshold + 1);

			
			-- Step 2:
			case ct is
				when 0 =>
					-- If no segments are proposed then
					-- return no_element:
					segment.segment := pac_segments.no_element;

				when 1 =>
					-- If only one proposed segment found, then
					-- the given segment remains as it is:
					null;
					
					
				when others =>
					-- More than one proposed segment found.

					-- Start the search with the given segment.
					-- Locate the given segment among the proposed segments:
					ps := proposed_segments.find (segment);

					-- log (text => "proposed segment A: " & to_string (segment.segment),
					-- 	level => log_threshold + 1);

					-- Advance to the next proposed segment:
					next (ps);
					
					-- If the end of the list has been reached,
					-- then move to the begin of the list:
					if ps = pac_proposed_segments.no_element then
						ps := proposed_segments.first;
					end if;
					
					-- Overwrite the given segment with the
					-- segment that has just been found:
					segment := element (ps);

					-- log (text => "proposed segment B: " & to_string (segment.segment),
					-- 	level => log_threshold + 1);

			end case;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " advancing to next proposed segment",
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end next_proposed_segment;

	


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
		use pac_doc_zones;
				
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			-- Moves the candidate segment:
			procedure do_it (s : in out type_segment) is begin
				move_segment (s, point_of_attack, destination);
			end do_it;

			
			procedure query_zone (
				zone : in out type_doc_zone)
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
						container	=> module.board.assy_doc.top.zones, 
						position	=> segment.zone, 
						process		=> query_zone'access);

				when BOTTOM =>
					update_element (
						container	=> module.board.assy_doc.bottom.zones, 
						position	=> segment.zone, 
						process		=> query_zone'access);

			end case;
		end query_module;
		
				
	begin
		log (text => "module " & to_string (module_cursor)
			& " moving assy documentation zone segment " & to_string (segment.segment)
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
		use pac_doc_zones;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_zone (
				zone : in out type_doc_zone)
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
						container	=> module.board.assy_doc.top.zones, 
						position	=> segment.zone, 
						process		=> query_zone'access);

				when BOTTOM =>
					update_element (
						container	=> module.board.assy_doc.bottom.zones, 
						position	=> segment.zone, 
						process		=> query_zone'access);

			end case;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " deleting assy documentation zone segment " 
			& to_string (segment.segment),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (						
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end delete_segment;
	



	

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
					append (module.board.assy_doc.top.texts, (text with null record));

				when BOTTOM =>
					append (module.board.assy_doc.bottom.texts, (text with null record));
			end case;
		end query_module;

	begin
		log (text => "module " & to_string (module_cursor)
			& " placing text in assembly documentation at"
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
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_doc_texts.list
	is
		use et_text;
		result : pac_doc_texts.list;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			procedure query_text (c : in pac_doc_texts.cursor) is
				text : type_doc_text renames element (c);
			begin
				if in_catch_zone (
					zone	=> catch_zone,
					point	=> text.position.place)
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
					module.board.assy_doc.top.texts.iterate (query_text'access);

				when BOTTOM =>
					module.board.assy_doc.bottom.texts.iterate (query_text'access);
			end case;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " face" & to_string (face) 
			& " looking up assembly documentation texts in" & to_string (catch_zone),
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
		text			: in type_doc_text;
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
			text_cursor : pac_doc_texts.cursor;

			procedure query_text (text : in out type_doc_text) is begin
				move_text (text, offset);
			end query_text;
			
		begin
			case face is
				when TOP =>
					text_cursor := module.board.assy_doc.top.texts.find (text);
					module.board.assy_doc.top.texts.update_element (text_cursor, query_text'access);

				when BOTTOM =>
					text_cursor := module.board.assy_doc.bottom.texts.find (text);
					module.board.assy_doc.bottom.texts.update_element (text_cursor, query_text'access);
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
		
		log (text => "module " & to_string (module_cursor)
			& " face" & to_string (face) 
			& " moving assembly documentation text from" & to_string (old_position)
			& " to" & to_string (new_position), -- CS by offset
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

	end move_text;


	


	

	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		text			: in type_object_text;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is

			procedure query_text (text : in out type_doc_text) is begin
				modify_status (text, operation);
			end query_text;
			
		begin
			case text.face is
				when TOP =>
					module.board.assy_doc.top.texts.update_element (
						text.cursor, query_text'access);

				when BOTTOM =>
					module.board.assy_doc.bottom.texts.update_element (
						text.cursor, query_text'access);
			end case;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of text" -- CS log position and content ?
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end modify_status;

	


	

	procedure propose_texts (
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
			c : pac_doc_texts.cursor;

			procedure query_text (
				text	: in out type_doc_text)
			is begin
				if in_catch_zone (
					zone	=> catch_zone,
					point	=> get_place (text))
				then
					set_proposed (text);
					count := count + 1;
					log (text => to_string (text), level => log_threshold + 1);
				end if;
			end query_text;

			
			procedure query_top is 
				top : pac_doc_texts.list renames module.board.assy_doc.top.texts;
			begin
				if not top.is_empty then
					c := top.first;
					while c /= pac_doc_texts.no_element loop
						top.update_element (c, query_text'access);
						next (c);
					end loop;
				end if;
			end query_top;

			
			procedure query_bottom is 
				bottom : pac_doc_texts.list renames module.board.assy_doc.bottom.texts;
			begin
				if not bottom.is_empty then
					c := bottom.first;
					while c /= pac_doc_texts.no_element loop
						bottom.update_element (c, query_text'access);
						next (c);
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
			 & " proposing texts in" & to_string (catch_zone)
			 & " face " & to_string (face),
			 level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end propose_texts;



	
	

	procedure move_text (
		module_cursor	: in pac_generic_modules.cursor;
		text			: in type_object_text;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is

			procedure query_text (text : in out type_doc_text) is begin
				move_text (text, destination);
			end query_text;
			
		begin
			case text.face is
				when TOP =>
					module.board.assy_doc.top.texts.update_element (
						text.cursor, query_text'access);

				when BOTTOM =>
					module.board.assy_doc.bottom.texts.update_element (
						text.cursor, query_text'access);
			end case;
		end query_module;
		

	begin
		log (text => "module " & to_string (module_cursor)
			& " face" & to_string (text.face) 
			& " moving assembly documentation text to "
			& to_string (destination),
			level => log_threshold);

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end move_text;


	


	procedure delete_text (
		module_cursor	: in pac_generic_modules.cursor;
		text			: in type_object_text;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			c : pac_doc_texts.cursor := text.cursor;			
		begin
			case text.face is
				when TOP =>
					module.board.assy_doc.top.texts.delete (c);

				when BOTTOM =>
					module.board.assy_doc.bottom.texts.delete (c);
			end case;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " face" & to_string (text.face) 
			& " deleting assembly documentation text",
			level => log_threshold);

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end delete_text;



	

	function get_first_text (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object_text
	is
		result : type_object_text;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use pac_doc_texts;
			
			proceed : aliased boolean := true;

			top_items 		: pac_doc_texts.list renames module.board.assy_doc.top.texts;
			bottom_items	: pac_doc_texts.list renames module.board.assy_doc.bottom.texts;

			
			procedure query_text (c : in pac_doc_texts.cursor) is begin
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
			end query_text;
	
			
		begin
			-- Query the texts in the top layer first:
			iterate (top_items, query_text'access, proceed'access);
			result.face := top;

			-- If nothing found, then query the bottom layer:
			if proceed then
				iterate (bottom_items, query_text'access, proceed'access);
				result.face := bottom;
			end if;

			-- If still nothing found, return TOP and no_element:
			if proceed then
				result := (others => <>);	
			end if;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first text / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		-- put_line ("found " & to_string (result));
		
		log_indentation_down;

		return result;
	end get_first_text;



	

	procedure reset_proposed_texts (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			top 	: pac_doc_texts.list renames module.board.assy_doc.top.texts;
			bottom	: pac_doc_texts.list renames module.board.assy_doc.bottom.texts;

			
			procedure query_text (
				text	: in out type_doc_text)
			is begin
				reset_status (text);
			end query_text;


			c : pac_doc_texts.cursor;
			
			procedure query_top is begin
				if not top.is_empty then
					c := top.first;
					while c /= pac_doc_texts.no_element loop
						top.update_element (c, query_text'access);
						next (c);
					end loop;
				end if;
			end query_top;

			
			procedure query_bottom is begin
				if not bottom.is_empty then
					c := bottom.first;
					while c /= pac_doc_texts.no_element loop
						bottom.update_element (c, query_text'access);
						next (c);
					end loop;
				end if;
			end query_bottom;

			
		begin
			query_top;
			query_bottom;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " resetting proposed texts",
			 level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_proposed_texts;





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
					module.board.assy_doc.top.placeholders.append (placeholder);

				when BOTTOM =>
					module.board.assy_doc.bottom.placeholders.append (placeholder);
			end case;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " adding text placeholder in assembly documentation "
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





	
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		placeholder		: in type_object_placeholder;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			use pac_text_placeholders;
			
			procedure query_placeholder (
				ph : in out type_text_placeholder) 
			is begin
				modify_status (ph, operation);
			end query_placeholder;
			
		begin
			case placeholder.face is
				when TOP =>
					module.board.assy_doc.top.placeholders.update_element (
						placeholder.cursor, query_placeholder'access);

				when BOTTOM =>
					module.board.assy_doc.bottom.placeholders.update_element (
						placeholder.cursor, query_placeholder'access);
			end case;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of text placeholder" -- CS log position and content ?
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end modify_status;





	procedure propose_placeholders (
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
			use pac_text_placeholders;
			c : pac_text_placeholders.cursor;

			procedure query_placeholder (
				ph : in out type_text_placeholder)
			is begin
				if in_catch_zone (
					zone	=> catch_zone,
					point	=> get_place (ph))
				then
					set_proposed (ph);
					count := count + 1;
					log (text => to_string (ph), level => log_threshold + 1);
				end if;
			end query_placeholder;
			
			
		begin
			case face is
				when TOP =>
					c := module.board.assy_doc.top.placeholders.first;
					
					while c /= pac_text_placeholders.no_element loop
						module.board.assy_doc.top.placeholders.update_element (c, query_placeholder'access);
						next (c);
					end loop;


				when BOTTOM =>
					c := module.board.assy_doc.bottom.placeholders.first;
					
					while c /= pac_text_placeholders.no_element loop
						module.board.assy_doc.bottom.placeholders.update_element (c, query_placeholder'access);
						next (c);
					end loop;
			end case;					
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " proposing text placeholders in" & to_string (catch_zone)
			 & " face " & to_string (face),
			 level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end propose_placeholders;
	


	

	procedure move_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		placeholder		: in type_object_placeholder;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			use pac_text_placeholders;
			
			procedure query_placeholder (
				ph : in out type_text_placeholder) 
			is begin
				move_text (ph, destination);
			end query_placeholder;
			
		begin
			case placeholder.face is
				when TOP =>
					module.board.assy_doc.top.placeholders.update_element (
						placeholder.cursor, query_placeholder'access);

				when BOTTOM =>
					module.board.assy_doc.bottom.placeholders.update_element (
						placeholder.cursor, query_placeholder'access);
			end case;
		end query_module;
		

	begin
		log (text => "module " & to_string (module_cursor)
			& " moving text placeholder " 
			& to_string (placeholder.cursor)
			& " " & to_string (destination),
			level => log_threshold);

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end move_placeholder;






	procedure delete_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		placeholder		: in type_object_placeholder;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			c : pac_text_placeholders.cursor := placeholder.cursor;			
		begin
			case placeholder.face is
				when TOP =>
					module.board.assy_doc.top.placeholders.delete (c);

				when BOTTOM =>
					module.board.assy_doc.bottom.placeholders.delete (c);
			end case;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " deleting text placeholder" & to_string (placeholder.cursor),
			level => log_threshold);

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end delete_placeholder;


	


	function get_first_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object_placeholder
	is
		result : type_object_placeholder;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use pac_text_placeholders;
			
			proceed : aliased boolean := true;

			top_items 		: pac_text_placeholders.list renames module.board.assy_doc.top.placeholders;
			bottom_items	: pac_text_placeholders.list renames module.board.assy_doc.bottom.placeholders;

			
			procedure query_placeholder (
				c : in pac_text_placeholders.cursor) 
			is begin
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
			end query_placeholder;
	
			
		begin
			-- Query the placeholders in the top layer first:
			iterate (top_items, query_placeholder'access, proceed'access);
			result.face := top;

			-- If nothing found, then query the bottom layer:
			if proceed then
				iterate (bottom_items, query_placeholder'access, proceed'access);
				result.face := bottom;
			end if;

			-- If still nothing found, return TOP and no_element:
			if proceed then
				result := (others => <>);	
			end if;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first text placeholder / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		-- put_line ("found " & to_string (result));
		
		log_indentation_down;

		return result;
	end get_first_placeholder;







	procedure reset_proposed_placeholders (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_placeholder (
				ph : in out type_text_placeholder)
			is begin
				reset_status (ph);
			end query_placeholder;

			use pac_text_placeholders;
			c : pac_text_placeholders.cursor := 
				module.board.assy_doc.top.placeholders.first;
		begin
			-- Iterate the placeholders at the top:
			while c /= pac_text_placeholders.no_element loop
				module.board.assy_doc.top.placeholders.update_element (
					c, query_placeholder'access);
				next (c);
			end loop;

			-- Iterate the placeholders at the bottom:
			c := module.board.assy_doc.bottom.placeholders.first;
			while c /= pac_text_placeholders.no_element loop
				module.board.assy_doc.bottom.placeholders.update_element (
					c, query_placeholder'access);
				next (c);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " resetting proposed text placeholders",
			 level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_proposed_placeholders;



	



	


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
		result_text			: type_object_text;
		result_placeholder	: type_object_placeholder;

		use pac_contours;
		use pac_segments;

		use pac_doc_lines;
		use pac_doc_texts;
		use pac_text_placeholders;
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first object / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;

		
		-- SEARCH FOR A LINE:
		
		-- If a line has been found, then go to the end of this procedure:
		result_line := get_first_line (module_cursor, flag, log_threshold + 1);

		if result_line.cursor /= pac_doc_lines.no_element then
			-- A line has been found.
			log (text => to_string (element (result_line.cursor))
				 & " face " & to_string (result_line.face),
				 level => log_threshold + 1);
			
			result_category := CAT_LINE;
		end if;

		if result_category /= CAT_VOID then
			goto end_of_search;
		end if;

		
		-- Now we search for an arc.
		-- If there is one, then go to the end of this procedure:
		-- CS


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

		if result_category /= CAT_VOID then
			goto end_of_search;
		end if;



		-- SEARCH FOR A TEXT:
		
		result_text := get_first_text (module_cursor, flag, log_threshold + 1);
		
		if result_text.cursor /= pac_doc_texts.no_element then
			-- A text has been found.
			log (text => to_string (result_text.cursor)
					& " face " & to_string (result_text.face),
					level => log_threshold + 1);
			
			result_category := CAT_TEXT;
		end if;


		
		-- SEARCH FOR A PLACEHOLDER:

		result_placeholder := get_first_placeholder (module_cursor, flag, log_threshold + 1);
		
		if result_placeholder.cursor /= pac_text_placeholders.no_element then
			-- A placeholder has been found.
			log (text => to_string (result_placeholder.cursor)
					& " face " & to_string (result_placeholder.face),
					level => log_threshold + 1);
			
			result_category := CAT_PLACEHOLDER;
		end if;


		
		-- If still nothing has been found then the category is CAT_VOID.
		

	<<end_of_search>>
		
		log_indentation_down;

		case result_category is
			when CAT_VOID =>
				return (cat => CAT_VOID);

			when CAT_LINE =>
				return (CAT_LINE, result_line);

			when CAT_ZONE_SEGMENT =>
				return (CAT_ZONE_SEGMENT, result_segment);

			when CAT_TEXT =>
				return (CAT_TEXT, result_text);

			when CAT_PLACEHOLDER =>
				return (CAT_PLACEHOLDER, result_placeholder);
				
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
			use pac_doc_zones;
			zone_cursor : pac_doc_zones.cursor;
			face : type_face := TOP;
			
			use pac_doc_lines;
			line_cursor : pac_doc_lines.cursor;

			-- CS arcs, circles
			
			use pac_doc_texts;
			text_cursor : pac_doc_texts.cursor;


			use pac_text_placeholders;
			placeholder_cursor : pac_text_placeholders.cursor;

			
			
			procedure query_zone (zone : in type_doc_zone) is
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
			

			procedure query_line (line : in type_doc_line) is 

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
				

			procedure query_text (text : in type_doc_text) is 

				procedure collect is begin
					result.append ((
						cat		=> CAT_TEXT,
						text	=> (face, text_cursor)));

					log (text => to_string (text), level => log_threshold + 2);
				end collect;
				
			begin
				case flag is
					when PROPOSED =>
						if is_proposed (text) then
							collect;
						end if;

					when SELECTED =>
						if is_selected (text) then
							collect;
						end if;

					when others => null; -- CS
				end case;
			end query_text;


			procedure query_placeholder (placeholder : in type_text_placeholder) is 

				procedure collect is begin
					result.append ((
						cat			=> CAT_PLACEHOLDER,
						placeholder	=> (face, placeholder_cursor)));

					log (text => to_string (placeholder), level => log_threshold + 2);
				end collect;
				
			begin
				case flag is
					when PROPOSED =>
						if is_proposed (placeholder) then
							collect;
						end if;

					when SELECTED =>
						if is_selected (placeholder) then
							collect;
						end if;

					when others => null;
				end case;
			end query_placeholder;

			
			
		begin
			log (text => "top zones", level => log_threshold + 1);
			log_indentation_up;
			
			zone_cursor := module.board.assy_doc.top.zones.first;
			while zone_cursor /= pac_doc_zones.no_element loop
				query_element (zone_cursor, query_zone'access);
				next (zone_cursor);
			end loop;

			log_indentation_down;

			
			log (text => "top lines", level => log_threshold + 1);
			log_indentation_up;
			
			line_cursor := module.board.assy_doc.top.lines.first;
			while line_cursor /= pac_doc_lines.no_element loop
				query_element (line_cursor, query_line'access);
				next (line_cursor);
			end loop;

			log_indentation_down;
			
			-- CS arcs, circles


			log (text => "top texts", level => log_threshold + 1);
			log_indentation_up;
			
			text_cursor := module.board.assy_doc.top.texts.first;
			while text_cursor /= pac_doc_texts.no_element loop
				query_element (text_cursor, query_text'access);
				next (text_cursor);
			end loop;

			log_indentation_down;

			

			log (text => "top text placeholders", level => log_threshold + 1);
			log_indentation_up;
			
			placeholder_cursor := module.board.assy_doc.top.placeholders.first;
			while placeholder_cursor /= pac_text_placeholders.no_element loop
				query_element (placeholder_cursor, query_placeholder'access);
				next (placeholder_cursor);
			end loop;

			log_indentation_down;


			
			face := BOTTOM;

			log (text => "bottom zones", level => log_threshold + 1);
			log_indentation_up;
			
			zone_cursor := module.board.assy_doc.bottom.zones.first;
			while zone_cursor /= pac_doc_zones.no_element loop
				query_element (zone_cursor, query_zone'access);
				next (zone_cursor);
			end loop;

			log_indentation_down;

			
			log (text => "bottom lines", level => log_threshold + 1);
			log_indentation_up;
			
			line_cursor := module.board.assy_doc.bottom.lines.first;
			while line_cursor /= pac_doc_lines.no_element loop
				query_element (line_cursor, query_line'access);
				next (line_cursor);
			end loop;

			log_indentation_down;
			-- CS arcs, circles

			
			log (text => "bottom texts", level => log_threshold + 1);
			log_indentation_up;
			
			text_cursor := module.board.assy_doc.bottom.texts.first;
			while text_cursor /= pac_doc_texts.no_element loop
				query_element (text_cursor, query_text'access);
				next (text_cursor);
			end loop;

			log_indentation_down;


			log (text => "bottom text placeholders", level => log_threshold + 1);
			log_indentation_up;
			
			placeholder_cursor := module.board.assy_doc.bottom.placeholders.first;
			while placeholder_cursor /= pac_text_placeholders.no_element loop
				query_element (placeholder_cursor, query_placeholder'access);
				next (placeholder_cursor);
			end loop;

			log_indentation_down;			
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
			-- & to_string (segment.segment) CS output object category ?
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		case object.cat is
			when CAT_LINE =>
				modify_status (module_cursor, object.line, operation, log_threshold + 1);

			when CAT_ZONE_SEGMENT =>
				modify_status (module_cursor, object.segment, operation, log_threshold + 1);

			when CAT_TEXT =>
				modify_status (module_cursor, object.text, operation, log_threshold + 1);

			when CAT_PLACEHOLDER =>
				modify_status (module_cursor, object.placeholder, operation, log_threshold + 1);
				
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
			& " moving assy documentation object " 
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

			when CAT_ZONE_SEGMENT =>
				move_segment (module_cursor,
					object.segment,
					point_of_attack, destination,
					log_threshold + 1);

			when CAT_TEXT =>
				move_text (module_cursor,
					object.text,
					destination,
					log_threshold + 1);

			when CAT_PLACEHOLDER =>
				move_placeholder (module_cursor,
					object.placeholder,
					destination,
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
		-- CS arcs, circles
		
		reset_proposed_texts (module_cursor, log_threshold + 1);
		reset_proposed_placeholders (module_cursor, log_threshold + 1);
		reset_proposed_segments (module_cursor, log_threshold + 1);

		log_indentation_down;
	end reset_proposed_objects;
	





	procedure delete_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " deleting assy documentation object",
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

			-- CS arcs, circles
				
			when CAT_ZONE_SEGMENT =>
				delete_segment (
					module_cursor	=> module_cursor, 
					segment			=> object.segment,
					log_threshold	=> log_threshold + 1);

				
			when CAT_TEXT =>
				delete_text (
					module_cursor	=> module_cursor, 
					text			=> object.text,
					log_threshold	=> log_threshold + 1);


			when CAT_PLACEHOLDER =>
				delete_placeholder (
					module_cursor	=> module_cursor, 
					placeholder		=> object.placeholder,
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
			line_cursor   : pac_doc_lines.cursor;
			arc_cursor    : pac_doc_arcs.cursor;
			circle_cursor : pac_doc_circles.cursor;
			-- CS zones, text

			deleted : boolean := false; -- goes true if at least one segment has been deleted
		begin
			if face = TOP then
				line_cursor   	:= module.board.assy_doc.top.lines.first;
				arc_cursor    	:= module.board.assy_doc.top.arcs.first;
				circle_cursor	:= module.board.assy_doc.top.circles.first;
			else
				line_cursor   	:= module.board.assy_doc.bottom.lines.first;
				arc_cursor    	:= module.board.assy_doc.bottom.arcs.first;
				circle_cursor	:= module.board.assy_doc.bottom.circles.first;
			end if;
			
			-- first search for a matching segment among the lines
			while line_cursor /= pac_doc_lines.no_element loop
				if in_catch_zone (
					zone	=> catch_zone,
					line	=> element (line_cursor),
					width	=> element (line_cursor).width)
				then
					if face = TOP then
						delete (module.board.assy_doc.top.lines, line_cursor);
					else
						delete (module.board.assy_doc.bottom.lines, line_cursor);
					end if;
					deleted := true;
					exit;
				end if;
				next (line_cursor);
			end loop;

			-- if no line found, search among arcs
			if not deleted then
				while arc_cursor /= pac_doc_arcs.no_element loop
					if in_catch_zone (
						zone	=> catch_zone,
						arc		=> element (arc_cursor),
						width	=> element (arc_cursor).width)
					then
						if face = TOP then
							delete (module.board.assy_doc.top.arcs, arc_cursor);
						else
							delete (module.board.assy_doc.bottom.arcs, arc_cursor);
						end if;
						deleted := true;
						exit;
					end if;
					next (arc_cursor);
				end loop;
			end if;

			-- if no arc found, search among circles
			if not deleted then
				while circle_cursor /= pac_doc_circles.no_element loop
					if in_catch_zone (
						zone	=> catch_zone,
						circle	=> element (circle_cursor),
						width	=> element (circle_cursor).width)
					then
						if face = TOP then
							delete (module.board.assy_doc.top.circles, circle_cursor);
						else
							delete (module.board.assy_doc.bottom.circles, circle_cursor);
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
			" deleting assembly documentation object. face" & to_string (face) &
			" in" & to_string (catch_zone),
			level => log_threshold);

		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> delete'access);
		
	end delete_object;


	
end et_board_ops.assy_doc;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
