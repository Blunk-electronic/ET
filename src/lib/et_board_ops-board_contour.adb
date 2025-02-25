------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   BOARD OPERATIONS / BOARD CONTOURS                      --
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


package body et_board_ops.board_contour is

	use pac_generic_modules;



	
	procedure set_outline (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		outline			: in type_outer_contour;
		log_threshold	: in type_log_level)
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is begin
			module.board.contours.outline := outline;
		end;
							   
	begin
		log (text => "module " & enclose_in_quotes (to_string (module_name)) 
			 & " setting outline" & to_string (outline),
			level => log_threshold);

		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end set_outline;





	procedure draw_outline (
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
					target	=> module.board.contours.outline,
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

	end draw_outline;

	



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
				container	=> module.board.contours.outline.contour.segments, 
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



	

	procedure propose_segments (
		module_cursor	: in pac_generic_modules.cursor;
		point			: in type_vector_model;
		zone			: in type_accuracy;
		count			: in out natural;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_segments;
			lc : pac_segments.cursor := module.board.contours.outline.contour.segments.first;

			
			procedure query_segment (
				segment	: in out type_segment)
			is 
				use et_object_status;
			begin
				case segment.shape is
					when LINE =>
						if within_accuracy (
							line	=> segment.segment_line,
							width	=> zero,
							point	=> point,
							zone	=> zone)
						then
							set_proposed (segment);
							count := count + 1;
							log (text => to_string (segment), level => log_threshold + 1);
						end if;

					when ARC =>
						null; -- CS
				end case;
			end query_segment;

			
		begin
			while lc /= pac_segments.no_element loop
				pac_segments.update_element (
					container	=> module.board.contours.outline.contour.segments, 
					position	=> lc, 
					process		=> query_segment'access);

				next (lc);
			end loop;
		end query_module;
		
		
	begin
		log (text => "proposing segments at " & to_string (point)
			 & " zone " & accuracy_to_string (zone),
			 level => log_threshold);

		log_indentation_up;

		count := 0;
		
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
			use pac_segments;
			lc : pac_segments.cursor := module.board.contours.outline.contour.segments.first;

			
			procedure query_segment (
				segment	: in out type_segment)
			is 
				use et_object_status;
			begin
				reset_status (segment);
			end query_segment;

			
		begin
			while lc /= pac_segments.no_element loop
				pac_segments.update_element (
					container	=> module.board.contours.outline.contour.segments, 
					position	=> lc, 
					process		=> query_segment'access);

				next (lc);
			end loop;
		end query_module;

		
	begin
		log (text => "resetting proposed lines of board outline",
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
				segments	=> module.board.contours.outline.contour.segments,
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
				c := module.board.contours.outline.contour.segments.first;

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

			procedure do_it (s : in out type_segment) is
			begin
				case s.shape is
					when LINE =>
						move_line_to (s.segment_line, point_of_attack, destination);

					when ARC =>
						null;
						-- CS
				end case;
			end do_it;
			
		begin
			module.board.contours.outline.contour.segments.update_element (
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

		log (text => "new outline:" & to_string (get_outline (module_cursor), true),
			 level => log_threshold + 1);
		
		log_indentation_down;
	end move_segment;


	
	
	

	function get_outline (
		module_cursor	: in pac_generic_modules.cursor)
		return type_outer_contour
	is begin
		return element (module_cursor).board.contours.outline;
	end get_outline;




	
	
	function get_outline (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		log_threshold	: in type_log_level)
		return type_outer_contour
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being queried
	begin
		log (text => "module " & enclose_in_quotes (to_string (module_name)) 
			 & " getting outline",
			level => log_threshold);

		module_cursor := locate_module (module_name);
		
		return get_outline (module_cursor);
	end get_outline;



	

	procedure delete_outline (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		point			: in type_vector_model; -- x/y
		accuracy		: in type_accuracy;
		log_threshold	: in type_log_level)
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		
		procedure delete (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is			
			deleted : boolean := false; -- goes true if at least one segment has been deleted

			procedure delete_segment is 
				use pac_segments;
				c : pac_segments.cursor;
			begin
				c := module.board.contours.outline.contour.segments.first;
				
				while c /= pac_segments.no_element loop

					case element (c).shape is
						when LINE =>
							if element (c).segment_line.on_line (point) then
								-- CS use get_shortest_distance (point, element)
								-- and compare distance with accuracy	

								delete (module.board.contours.outline.contour.segments, c);
								deleted := true;

								-- CS update start/end point of predecessor/successor segment
								
								exit; -- CS no exit if all segments are to be deleted
							end if;

						when ARC =>
							if element (c).segment_arc.on_arc (point) then
								-- CS use get_shortest_distance (point, element)
								-- and compare distance with accuracy	

								delete (module.board.contours.outline.contour.segments, c);
								deleted := true;

								-- CS update start/end point of predecessor/successor segment
								
								exit; -- CS no exit if all segments are to be deleted
							end if;

					end case;
					
					next (c);
				end loop;
			end delete_segment;

			procedure delete_circle is begin
				if module.board.contours.outline.contour.circle.on_circle (point) then
					-- CS use get_shortest_distance (point, element)
					-- and compare distance with accuracy	

					module.board.contours.outline.contour := (others => <>);					
					deleted := true;
				end if;
			end delete_circle;

			
		begin -- delete
			if module.board.contours.outline.contour.circular then
				delete_circle;				
			else
				delete_segment;
			end if;
			
			if not deleted then
				nothing_found (point, accuracy);
			end if;			
		end delete;

		
	begin
		log (text => "module " & to_string (module_name)
			& " deleting outline segment at" & to_string (point) 
			& " accuracy" & accuracy_to_string (accuracy),
			level => log_threshold);

		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> delete'access);
		
	end delete_outline;



	

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
			module.board.contours.outline.contour.segments.delete (c);
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

		log (text => "new outline:" & to_string (get_outline (module_cursor), true),
			 level => log_threshold + 1);
		
		log_indentation_down;
	end delete_segment;



	

	procedure add_hole (
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
			append (module.board.contours.holes, hole);
		end;
							   
	begin
		log (text => "module " & enclose_in_quotes (to_string (key (module_cursor))) 
			 & " adding hole" & to_string (hole),
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end add_hole;

	
	
	procedure add_hole (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		hole			: in type_hole;
		log_threshold	: in type_log_level)
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is 
			use pac_holes;
		begin
			append (module.board.contours.holes, hole);
		end;
							   
	begin
		log (text => "module " & enclose_in_quotes (to_string (module_name)) 
			 & " adding hole" & to_string (hole),
			level => log_threshold);

		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end add_hole;


	function get_holes (
		module_cursor	: in pac_generic_modules.cursor)
		return pac_holes.list
	is begin
		return element (module_cursor).board.contours.holes;
	end get_holes;
	


	procedure delete_hole (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		point			: in type_vector_model; -- x/y
		accuracy		: in type_accuracy;
		log_threshold	: in type_log_level)
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

	begin -- delete_hole
		log (text => "module " & enclose_in_quotes (to_string (module_name)) 
			& " deleting hole segment at" & to_string (point) 
			& " accuracy" & accuracy_to_string (accuracy),
			level => log_threshold);

		module_cursor := locate_module (module_name);

		--update_element (
			--container	=> generic_modules,
			--position	=> module_cursor,
			--process		=> delete'access);

		-- CS
		
	end delete_hole;



	
end et_board_ops.board_contour;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
