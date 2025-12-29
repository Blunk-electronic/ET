------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   BOARD OPERATIONS / BOARD OUTLINE                       --
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


package body et_board_ops.outline is


	
	procedure set_outline (
		module_cursor	: in pac_generic_modules.cursor;
		outline			: in type_outer_contour;
		log_threshold	: in type_log_level)
	is

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.board_contour.outline := outline;
		end;
							   
	begin
		log (text => "module " & to_string (module_cursor) 
			 & " set outline " & to_string (outline),
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end set_outline;





	procedure add_outline (
		module_cursor	: in pac_generic_modules.cursor;
		outline			: in type_outer_contour;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is 
			mr : type_merge_result;
		begin
			if is_open (outline) then
				merge_contours (
					target	=> module.board.board_contour.outline,
					source	=> outline,
					status	=> mr);

				if not mr.successful then
					log_indentation_up;
					log (text => "outline rejected", level => log_threshold + 1);
					log_indentation_down;
				end if;
				
			end if;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			 & "drawing board outline "			 
			 & to_string (contour => outline, full => true),
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

	end add_outline;

	


	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_outer_contour_segment;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_segment (
				segment	: in out type_segment)
			is begin
				modify_status (segment, operation);
			end query_segment;

			
		begin
			pac_segments.update_element (
				container	=> module.board.board_contour.outline.contour.segments, 
				position	=> segment.segment, 
				process		=> query_segment'access);
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of "
			& to_string (segment.segment)
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;



	


	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		segment_cursor	: in pac_segments.cursor;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_segment (
				segment	: in out type_segment)
			is begin
				modify_status (segment, operation);
			end query_segment;

			
		begin
			pac_segments.update_element (
				container	=> module.board.board_contour.outline.contour.segments, 
				position	=> segment_cursor, 
				process		=> query_segment'access);
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of "
			& to_string (segment_cursor)
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;



	

	procedure propose_outer_contour_segments (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_segments;
			c : pac_segments.cursor;

			
			procedure query_segment (
				segment	: in out type_segment)
			is begin
				if in_catch_zone (catch_zone, segment) then
					set_proposed (segment);
					count := count + 1;
					log (text => to_string (segment), level => log_threshold + 1);
				end if;
			end query_segment;

			
		begin
			if is_circular (module.board.board_contour.outline) then
				null; -- CS
			else
				c := module.board.board_contour.outline.contour.segments.first;
				
				while c /= pac_segments.no_element loop
					pac_segments.update_element (
						container	=> module.board.board_contour.outline.contour.segments, 
						position	=> c, 
						process		=> query_segment'access);

					next (c);
				end loop;
			end if;
		end query_module;
		
		
	begin
		log (text => "proposing outer contour segments in" & to_string (catch_zone),
			 level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end propose_outer_contour_segments;

	



	procedure reset_proposed_outer_segments (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_segments;
			c : pac_segments.cursor;

			
			procedure query_segment (
				segment	: in out type_segment)
			is 
				use et_object_status;
			begin
				reset_status (segment);
			end query_segment;

			
		begin
			if is_circular (module.board.board_contour.outline) then
				null; -- CS
			else
				c := module.board.board_contour.outline.contour.segments.first;
				
				while c /= pac_segments.no_element loop
					pac_segments.update_element (
						container	=> module.board.board_contour.outline.contour.segments, 
						position	=> c, 
						process		=> query_segment'access);

					next (c);
				end loop;
			end if;
		end query_module;

		
	begin
		log (text => "resetting proposed outer contour segments of board",
			 level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_proposed_outer_segments;



	

	
	function get_first_segment (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return pac_segments.cursor
	is
		result : pac_segments.cursor;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use pac_segments;
			proceed : aliased boolean := true;

			
			procedure query_segment (
				c : in pac_segments.cursor) 
			is begin
				case flag is
					when PROPOSED =>
						if is_proposed (c) then
							result := c;
							proceed := false;
						end if;

					when SELECTED =>
						if is_selected (c) then
							result := c;
							proceed := false;
						end if;

					when others =>
						null; -- CS
				end case;
			end query_segment;

			
		begin
			iterate (
				segments	=> module.board.board_contour.outline.contour.segments,
				process		=> query_segment'access, 
				proceed		=> proceed'access);
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first segment / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;

		return result;
	end get_first_segment;
	




	function get_first_outer_segment (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object_outer_contour_segment
	is
		result : type_object_outer_contour_segment;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use pac_segments;
			proceed : aliased boolean := true;

			
			procedure query_segment (
				c : in pac_segments.cursor) 
			is begin
				case flag is
					when PROPOSED =>
						if is_proposed (c) then
							result.segment := c;
							proceed := false;
						end if;

					when SELECTED =>
						if is_selected (c) then
							result.segment := c;
							proceed := false;
						end if;

					when others =>
						null; -- CS
				end case;
			end query_segment;

			
		begin
			if is_circular (module.board.board_contour.outline) then
				null; -- CS
			else
				iterate (
					segments	=> module.board.board_contour.outline.contour.segments,
					process		=> query_segment'access, 
					proceed		=> proceed'access);
			end if;
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first outer contour segment / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;

		return result;
	end get_first_outer_segment;


	
	

	procedure next_proposed_segment (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in out pac_segments.cursor;
		-- CS last_item		: in out boolean;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is
			use pac_segments;
			c : pac_segments.cursor := next (segment);
		begin
			-- Start the search after the given segment:
			while c /= pac_segments.no_element loop
				if is_proposed (c) then
					segment := c;
					exit;
				end if;
				next (c);
			end loop;

			-- If nothing found, then restart the search
			-- at the begin of the list:
			if c = pac_segments.no_element then
				c := module.board.board_contour.outline.contour.segments.first;

				while c /= pac_segments.no_element loop
					if is_proposed (c) then
						segment := c;
						exit;
					end if;
					next (c);
				end loop;				
			end if;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& "advancing to next proposed segment",
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end next_proposed_segment;


	


	procedure move_segment (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in pac_segments.cursor;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_segments;

			procedure do_it (s : in out type_segment) is begin
				move_segment (s, point_of_attack, destination);
			end do_it;
			
		begin
			module.board.board_contour.outline.contour.segments.update_element (
				segment, do_it'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " moving outline segment " & to_string (segment)
			& " point of attack " & to_string (point_of_attack)
			& " to" & to_string (destination),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (						
			position	=> module_cursor,
			process		=> query_module'access);

		log (text => "new outline:" & to_string (get_outer_contour (module_cursor), true),
			 level => log_threshold + 1);
		
		log_indentation_down;
	end move_segment;



	

	procedure move_outer_segment (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_outer_contour_segment;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_segments;

			procedure do_it (s : in out type_segment) is begin
				move_segment (s, point_of_attack, destination);
			end do_it;
			
		begin
			if is_circular (module.board.board_contour.outline) then
				null; -- CS
			else
				module.board.board_contour.outline.contour.segments.update_element (
					segment.segment, do_it'access);
			end if;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " moving outer contour segment " & to_string (segment.segment)
			& " point of attack " & to_string (point_of_attack)
			& " to" & to_string (destination),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (						
			position	=> module_cursor,
			process		=> query_module'access);

		log (text => "new outer contour:" & to_string (get_outer_contour (module_cursor), true),
			 level => log_threshold + 1);
		
		log_indentation_down;
	end move_outer_segment;

	
	
	
	

	function get_outer_contour (
		module_cursor	: in pac_generic_modules.cursor)
		return type_outer_contour
	is begin
		return element (module_cursor).board.board_contour.outline;
	end get_outer_contour;




	
	
	function get_outer_contour (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
		return type_outer_contour
	is begin
		log (text => "module " & to_string (module_cursor)
			 & " getting outline",
			level => log_threshold);
		
		return get_outer_contour (module_cursor);
	end get_outer_contour;



	

	procedure delete_outer_segment (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
	is
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is			
			deleted : boolean := false; -- goes true if at least one segment has been deleted

			
			procedure delete_segment is 
				use pac_segments;
				c : pac_segments.cursor;
			begin
				c := module.board.board_contour.outline.contour.segments.first;
				
				while c /= pac_segments.no_element loop

					-- Delete the segment if it is the given catch zone:					
					if in_catch_zone (catch_zone, c) then
						delete (module.board.board_contour.outline.contour.segments, c);
						deleted := true;

						-- CS update start/end point of predecessor/successor segment
							
						exit; -- CS no exit if all segments are to be deleted
					end if;
					
					next (c);
				end loop;
			end delete_segment;

			
			procedure delete_circle is begin
				if in_catch_zone (
					zone	=> catch_zone,
					circle	=> module.board.board_contour.outline.contour.circle)
				then
					module.board.board_contour.outline.contour := (others => <>);					
					deleted := true;
				end if;
			end delete_circle;

			
		begin
			if is_circular (module.board.board_contour.outline) then
				delete_circle;				
			else
				delete_segment;
			end if;
			
			if not deleted then
				nothing_found (catch_zone);
			end if;			
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " deleting outer contour segment in" & to_string (catch_zone),
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
	end delete_outer_segment;



	

	procedure delete_segment (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in pac_segments.cursor;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is			
			c : pac_segments.cursor := segment;
		begin
			module.board.board_contour.outline.contour.segments.delete (c);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " deleting outline segment " & to_string (segment),
			level => log_threshold);

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		log (text => "new outline:" & to_string (get_outer_contour (module_cursor), true),
			 level => log_threshold + 1);
		
		log_indentation_down;
	end delete_segment;



	

	procedure delete_outer_segment (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_outer_contour_segment;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is			
			c : pac_segments.cursor := segment.segment;
		begin
			-- CS test circular flag ?
			module.board.board_contour.outline.contour.segments.delete (c);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " deleting outer contour segment " & to_string (segment.segment),
			level => log_threshold);

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		log (text => "new outer contour:" & to_string (get_outer_contour (module_cursor), true),
			 level => log_threshold + 1);
		
		log_indentation_down;
	end delete_outer_segment;


	




	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_hole_segment;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is
		use pac_contours;
		use pac_segments;
		use pac_holes;
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_segment (
				segment	: in out type_segment)
			is begin
				modify_status (segment, operation);
			end query_segment;

			
			procedure query_hole (
				hole : in out type_hole)
			is begin
				if is_circular (hole) then
					null; -- CS
				else
					-- Locate the given segment in the
					-- candidate hole:
					update_element (
						container	=> hole.contour.segments,
						position	=> segment.segment,
						process		=> query_segment'access);

				end if;
			end query_hole;
	
			
		begin
			-- Search the given segment according to the
			-- hole it belongs to:
			update_element (
				container	=> module.board.board_contour.holes, 
				position	=> segment.hole, 
				process		=> query_hole'access);

		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of "
			& to_string (segment.segment)
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;


	


	procedure propose_hole_segments (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_holes;
			hc : pac_holes.cursor;

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


			
			procedure query_hole (
				hole : in out type_hole)
			is
				use pac_contours;
				use pac_segments;
				c : pac_segments.cursor;
				
			begin
				if is_circular (hole) then
					null; -- CS
				else
					c := hole.contour.segments.first;

					while c /= pac_segments.no_element loop
						update_element (
							container	=> hole.contour.segments,
							position	=> c,
							process		=> query_segment'access);

						next (c);
					end loop;
				end if;
			end query_hole;
			
			
		begin
			hc := module.board.board_contour.holes.first;

			while hc /= pac_holes.no_element loop
				update_element (
					container	=> module.board.board_contour.holes,
					position	=> hc,
					process		=> query_hole'access);
				
				next (hc);
			end loop;
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " proposing hole segments in" & to_string (catch_zone),
			 level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end propose_hole_segments;

	




	procedure reset_proposed_hole_segments (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_holes;
			hc : pac_holes.cursor;

			use pac_contours;
			use pac_segments;

			
			procedure query_segment (
				segment	: in out type_segment)
			is begin
				reset_status (segment);
			end query_segment;


			
			procedure query_hole (
				hole : in out type_hole)
			is
				use pac_contours;
				use pac_segments;
				c : pac_segments.cursor;
				
			begin
				if is_circular (hole) then
					null; -- CS
				else
					c := hole.contour.segments.first;

					while c /= pac_segments.no_element loop
						update_element (
							container	=> hole.contour.segments,
							position	=> c,
							process		=> query_segment'access);

						next (c);
					end loop;
				end if;
			end query_hole;
			
			
		begin
			hc := module.board.board_contour.holes.first;

			while hc /= pac_holes.no_element loop
				update_element (
					container	=> module.board.board_contour.holes,
					position	=> hc,
					process		=> query_hole'access);
				
				next (hc);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " resetting proposed board contour hole segments",
			 level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_proposed_hole_segments;


	




	function get_first_hole_segment (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object_hole_segment
	is
		use pac_contours;
		result : type_object_hole_segment;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use pac_contours;
			use pac_segments;
			use pac_holes;
			
			proceed : aliased boolean := true;

			
			procedure query_hole (h : in pac_holes.cursor) is 

				procedure query_segment (
					c : in pac_segments.cursor) 
				is begin
					case flag is
						when PROPOSED =>
							if is_proposed (c) then
								result.segment := c;
								result.hole := h;
								proceed := false;

								log (text => to_string (c), level => log_threshold + 1);
							end if;

						when SELECTED =>
							if is_selected (c) then
								result.segment := c;
								result.hole := h;
								proceed := false;

								log (text => to_string (c), level => log_threshold + 1);
							end if;

						when others =>
							null; -- CS
					end case;
				end query_segment;
				
				
				procedure query_segments (hole : in type_hole) is begin
					iterate (
						segments	=> hole.contour.segments,
						process		=> query_segment'access,
						proceed		=> proceed'access);				
				end query_segments;

				
			begin
				if is_circular (element (h)) then
					null; -- CS
				else
					query_element (h, query_segments'access);
				end if;
			end query_hole;

			
		begin
			-- Iterate the holes:
			iterate (
				holes	=> module.board.board_contour.holes,
				process	=> query_hole'access, 
				proceed	=> proceed'access);

		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first hole segment / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		-- put_line ("found " & to_string (result));
		
		log_indentation_down;

		return result;
	end get_first_hole_segment;


	


	procedure move_hole_segment (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_hole_segment;
		point_of_attack	: in type_vector_model;
		-- coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is
		use pac_contours;
		use pac_segments;
		use pac_holes;
				
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			-- Moves the candidate segment:
			procedure do_it (s : in out type_segment) is begin
				move_segment (s, point_of_attack, destination);
			end do_it;

			
			procedure query_hole (
				hole : in out type_hole)
			is 
				c : pac_segments.cursor;
			begin
				if is_circular (hole) then
					null; -- CS
				else
					-- Locate the given segment in 
					-- the candidate zone:
					update_element (
						container	=> hole.contour.segments,
						position	=> segment.segment,
						process		=> do_it'access);

				end if;
			end query_hole;
	
			
		begin
			-- Search for the given segment according to hole
			-- it belongs to:
			update_element (
				container	=> module.board.board_contour.holes, 
				position	=> segment.hole, 
				process		=> query_hole'access);

		end query_module;
		
				
	begin
		log (text => "module " & to_string (module_cursor)
			& " moving hole segment " & to_string (segment.segment)
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
	end move_hole_segment;



	


	procedure delete_hole_segment (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_hole_segment;
		log_threshold	: in type_log_level)
	is
		use pac_contours;
		use pac_segments;
		use pac_holes;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			procedure query_hole (
				hole : in out type_hole)
			is 
				c : pac_segments.cursor;
			begin
				if is_circular (hole) then
					null; -- CS
				else
					-- Delete the given segment:
					c := segment.segment;					
					hole.contour.segments.delete (c);
				end if;
			end query_hole;
	
			
		begin
			-- Search for the given segment according to the 
			-- hole it belongs to:
			update_element (
				container	=> module.board.board_contour.holes, 
				position	=> segment.hole, 
				process		=> query_hole'access);

		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " deleting hole segment " 
			& to_string (segment.segment),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (						
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;
	end delete_hole_segment;
	

	


	
	

	procedure set_hole (
		module_cursor	: in pac_generic_modules.cursor;
		hole			: in type_hole;
		log_threshold	: in type_log_level)
	is

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is 
			use pac_holes;
		begin
			append (module.board.board_contour.holes, hole);
		end;
							   
	begin
		log (text => "module " & to_string (module_cursor) 
			 & " setting hole " & to_string (hole),
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end set_hole;

	
	


	procedure add_hole (
		module_cursor	: in pac_generic_modules.cursor;
		hole			: in type_hole;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is 
			use pac_holes;
			hc : pac_holes.cursor := module.board.board_contour.holes.first;

			proceed : boolean := true;
			
			procedure query_hole (
				h : in out type_hole)
			is 
				mr : type_merge_result;
			begin
				log (text => "query hole " & to_string (h), level => log_threshold + 1);
				
				if is_open (h) then
					merge_contours (
						target	=> h,
						source	=> hole,
						status	=> mr);

					if mr.successful then
						proceed := false;
					end if;
					
					-- if not mr.successful then
					-- 	log_indentation_up;
					-- 	log (text => "hole contour rejected", level => log_threshold + 1);
					-- 	log_indentation_down;
					-- end if;

				else -- closed candidate contour
					null;
					-- CS test whether given hole touches or crosses
					-- the candidate contour.
					-- When positive, reject given hole.
				end if;
			end query_hole;
			
		begin
			-- Iterate the holes of the board.
			-- Abort the iteration once the given hole contour has
			-- been successfully added to the holes:
			while has_element (hc) loop
				module.board.board_contour.holes.update_element (hc, query_hole'access);
				
				if not proceed then
					exit;
				end if;
				next (hc);
			end loop;

			if proceed then
				module.board.board_contour.holes.append (hole);
			end if;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			 & " adding hole " & to_string (hole),
			level => log_threshold);

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end add_hole;



	
	

	function get_holes (
		module_cursor	: in pac_generic_modules.cursor)
		return pac_holes.list
	is begin
		return element (module_cursor).board.board_contour.holes;
	end get_holes;
	


	
	procedure delete_hole_segment (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is 
			use pac_holes;
		begin
			null;
			-- CS
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " deleting hole segment in" & to_string (catch_zone),
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

	end delete_hole_segment;







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
		result_category			: type_object_category := CAT_VOID;
		result_outer_segment 	: type_object_outer_contour_segment;
		result_hole_segment		: type_object_hole_segment;

		use pac_contours;
		use pac_segments;

		use pac_holes;
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first object / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;

		
		-- SEARCH FOR A SEGMENT OF THE OUTER BOARD CONTOUR:
		
		-- If there is one, then go to the end of this procedure:
		result_outer_segment := get_first_outer_segment (module_cursor, flag, log_threshold + 1);

		if result_outer_segment.segment /= pac_segments.no_element then
			-- A segment has been found.
			log (text => to_string (result_outer_segment.segment),
				level => log_threshold + 1);
			
			result_category := CAT_OUTER_CONTOUR_SEGMENT;
		end if;

		if result_category /= CAT_VOID then
			goto end_of_search;
		end if;



		-- SEARCH FOR A SEGMENT OF A HOLE:
	
		-- If there is one, then go to the end of this procedure:
		result_hole_segment := get_first_hole_segment (module_cursor, flag, log_threshold + 1);

		if result_hole_segment.segment /= pac_segments.no_element then
			-- A segment has been found.
			log (text => to_string (result_hole_segment.segment),
				level => log_threshold + 1);
			
			result_category := CAT_HOLE_SEGMENT;
		end if;

		
		-- If still nothing has been found then the category is CAT_VOID.
		

	<<end_of_search>>
		
		log_indentation_down;

		case result_category is
			when CAT_VOID =>
				return (cat => CAT_VOID);

			when CAT_OUTER_CONTOUR_SEGMENT =>
				return (CAT_OUTER_CONTOUR_SEGMENT, result_outer_segment);

			when CAT_HOLE_SEGMENT =>
				return (CAT_HOLE_SEGMENT, result_hole_segment);
				
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
			use pac_contours;
			use pac_segments;

			outer_segment_cursor : pac_segments.cursor;

			
			-- This procedure queries a segment of the outer contour:
			procedure query_outer_segment (segment : in type_segment) is 

				procedure collect is begin
					result.append ((
						cat				=> CAT_OUTER_CONTOUR_SEGMENT,
						outer_segment	=> (segment => outer_segment_cursor)));

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
			end query_outer_segment;

			
			
			use pac_holes;
			hole_cursor : pac_holes.cursor;
			
			
			-- This procedure queries a hole and iterates its segments:
			procedure query_hole (hole : in type_hole) is
				segment_cursor : pac_segments.cursor;
				
				procedure query_segment (segment : in type_segment) is 

					procedure collect is begin
						result.append ((
							cat				=> CAT_HOLE_SEGMENT,
							hole_segment	=> (hole_cursor, segment_cursor)));

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
				if is_circular (hole) then
					null; -- CS
				else
					-- Iterate the segments of the hole candidate:
					segment_cursor := hole.contour.segments.first;
					
					while segment_cursor /= pac_segments.no_element loop
						query_element (segment_cursor, query_segment'access);
						next (segment_cursor);
					end loop;
				end if;
			end query_hole;
			

			
		begin
			-- OUTER CONTOUR:
			log (text => "outer contour", level => log_threshold + 1);
			log_indentation_up;

			if is_circular (module.board.board_contour.outline) then
				null; -- CS 
			else
				outer_segment_cursor := module.board.board_contour.outline.contour.segments.first;
				while outer_segment_cursor /= pac_segments.no_element loop
					query_element (outer_segment_cursor, query_outer_segment'access);
					next (outer_segment_cursor);
				end loop;
			end if;
			log_indentation_down;			

			-- HOLES:
			log (text => "holes", level => log_threshold + 1);
			log_indentation_up;
			
			hole_cursor := module.board.board_contour.holes.first;
			while hole_cursor /= pac_holes.no_element loop
				query_element (hole_cursor, query_hole'access);
				next (hole_cursor);
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
			when CAT_OUTER_CONTOUR_SEGMENT =>
				modify_status (module_cursor, object.outer_segment, operation, log_threshold + 1);

			when CAT_HOLE_SEGMENT =>
				modify_status (module_cursor, object.hole_segment, operation, log_threshold + 1);

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
			& " moving board contour object " 
			-- CS & to_string (object)
			& " point of attack " & to_string (point_of_attack)
			& " to" & to_string (destination),
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_OUTER_CONTOUR_SEGMENT =>
				move_outer_segment (module_cursor,
					object.outer_segment,
					point_of_attack, destination,
					log_threshold + 1);

			when CAT_HOLE_SEGMENT =>
				move_hole_segment (module_cursor,
					object.hole_segment,
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

		reset_proposed_outer_segments (module_cursor, log_threshold + 1);
		reset_proposed_hole_segments (module_cursor, log_threshold + 1);

		log_indentation_down;
	end reset_proposed_objects;


	
	


	procedure delete_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " deleting board contour object",
			-- CS & to_string (object)
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_OUTER_CONTOUR_SEGMENT =>
				delete_outer_segment (
					module_cursor	=> module_cursor, 
					segment			=> object.outer_segment,
					log_threshold	=> log_threshold + 1);

			when CAT_HOLE_SEGMENT =>
				delete_hole_segment (
					module_cursor	=> module_cursor, 
					segment			=> object.hole_segment,
					log_threshold	=> log_threshold + 1);
				
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end delete_object;
	

	
end et_board_ops.outline;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
