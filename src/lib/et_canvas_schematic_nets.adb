------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        CANVAS SCHEMATIC NETS                             --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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
--                                                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.text_io;					use ada.text_io;

with et_canvas_schematic;			use et_canvas_schematic;

package body et_canvas_schematic_nets is

	use et_canvas_schematic.pac_canvas;
	
	procedure delete_net_segment (point : in type_point) is 
		use et_schematic_ops.nets;
		use pac_selected_segments;
		segment_cursor : pac_selected_segments.cursor;
	begin
		log (text => "deleting net segment ...", level => log_threshold);
		log_indentation_up;
		
		-- Collect all segments in the vicinity of the given point:
		selected_segments := collect_segments (
			module			=> current_active_module,
			place			=> to_position (point, current_active_sheet),
			catch_zone		=> catch_zone_default, -- CS should depend on current scale
			log_threshold	=> log_threshold + 1);

		-- evaluate the number of segments found here:
		case length (selected_segments) is
			when 0 =>
				reset_request_clarification;
				
			when 1 =>
				segment_cursor := selected_segments.first;
			
				delete_selected_segment (
					module_cursor	=> current_active_module,
					segment			=> element (segment_cursor),
					log_threshold	=> log_threshold + 1);

				reset_request_clarification;
				set_status (status_click_left & "delete net segment." & status_hint_for_abort);
				
			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;

				-- preselect the first segment
				selected_segment := selected_segments.first;
		end case;
		
		log_indentation_down;
	end delete_net_segment;

	
	procedure clarify_net_segment is
		use et_schematic;
		use et_schematic_ops.nets;
		use pac_selected_segments;
		s : type_net_segments.cursor;
	begin
		-- On every call of this procedure we must advance from one
		-- segment to the next in a circular manner. So if the end 
		-- of the list is reached, then the cursor selected_segment
		-- moves back to the start of the segment list.
		if next (selected_segment) /= pac_selected_segments.no_element then
			next (selected_segment);
		else
			selected_segment := selected_segments.first;
		end if;

		-- show the selected segment in the status bar
		s := element (selected_segment).segment;

		set_status (to_string (s));
	end clarify_net_segment;

	
	procedure delete_selected_net_segment is
		use et_schematic_ops.nets;
		use pac_selected_segments;
	begin
		log (text => "deleting net segment after clarification ...", level => log_threshold);
		log_indentation_up;

		delete_selected_segment (
			module_cursor	=> current_active_module,
			segment			=> element (selected_segment),
			log_threshold	=> log_threshold + 1);

		-- Update list of selected net segments:
		delete (selected_segments, selected_segment);
		
		reset_request_clarification;
		set_status (status_click_left & "delete net segment." & status_hint_for_abort);
		
		log_indentation_down;
	end delete_selected_net_segment;


	procedure reset_net_route is begin
		net_route := (
			bend_style	=> net_route.bend_style,
			others 		=> <>);
	end reset_net_route;


	procedure insert_net_segment (
		module			: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		segment			: in et_schematic.type_net_segment;
		log_threshold	: in type_log_level)
	is 
		start_point : et_coordinates.type_position := to_position (segment.start_point, sheet);
		end_point	: et_coordinates.type_position := to_position (segment.end_point, sheet);

		use et_schematic;
		use et_schematic_ops.nets;
		use pac_selected_segments;
		segments_at_start_point : pac_selected_segments.list;
		segments_at_end_point	: pac_selected_segments.list;

		use et_schematic.type_nets;
		net_cursor	: et_schematic.type_nets.cursor;
		net_name	: type_net_name.bounded_string; -- RESET, MOTOR_ON_OFF

		net_name_start, net_name_end : type_net_name.bounded_string;
		empty_name : constant type_net_name.bounded_string := to_net_name ("");

		function is_empty (n : in type_net_name.bounded_string) return boolean is 
			use type_net_name;
		begin
			if n = empty_name then return true;
			else return false;
			end if;
		end is_empty;
	
	begin -- insert_net_segment
		log (text => "adding net segment on sheet" & to_sheet (sheet) & to_string (segment), 
			 level => log_threshold);

		log_indentation_up;

		segments_at_start_point := collect_segments (
			module			=> module,
			place			=> start_point,
			catch_zone		=> zero,
			log_threshold	=> log_threshold + 2);

		net_name_start := first_net (segments_at_start_point);
		
		segments_at_end_point := collect_segments (
			module			=> module,
			place			=> end_point,
			catch_zone		=> zero,
			log_threshold	=> log_threshold + 2);

		net_name_end := first_net (segments_at_end_point);
		
		-- If no nets at BOTH start AND end point, create a new 
		-- anonymous net with a name like N$234:
		if is_empty (net_name_start) and is_empty (net_name_end) then

			net_name := lowest_available_anonymous_net (module); -- N$234
			
			log (text => "creating new anonymous net " & to_string (net_name), level => log_threshold + 1);
			log_indentation_up;

			-- Insert the new net with this single segment in the module:
			et_schematic_ops.nets.insert_segment (
				module, net_cursor, sheet, net_name, segment, log_threshold + 2);

			log_indentation_down;
		end if;

		-- If net at start point AND no net at end point then
		-- the net at the start point is extended by the new segment:
		if not is_empty (net_name_start) and is_empty (net_name_end) then
						
			log (text => "attaching start point of segment to net " & to_string (net_name_start),
				 level => log_threshold + 1);
			log_indentation_up;

			net_cursor := et_schematic_ops.nets.locate_net (module, net_name_start);
			
			-- Insert the new segment:
			et_schematic_ops.nets.insert_segment (
				module, net_cursor, sheet, net_name_start, segment, log_threshold + 2);

			log_indentation_down;
		end if;

		-- If net at end point AND no net at start_point point then
		-- the net at the end point is extended by the new segment:
		if not is_empty (net_name_end) and is_empty (net_name_start) then
						
			log (text => "attaching end point of segment to net " & to_string (net_name_end),
				 level => log_threshold + 1);
			log_indentation_up;

			net_cursor := et_schematic_ops.nets.locate_net (module, net_name_end);
			
			-- Insert the new segment:
			et_schematic_ops.nets.insert_segment (
				module, net_cursor, sheet, net_name_end, segment, log_threshold + 2);

			log_indentation_down;
		end if;

		status_clear;
		
		-- If net at start point AND at end point then
		-- the net at the end point is extended by the new segment:
		if not is_empty (net_name_end) and not is_empty (net_name_start) then
			
			log (WARNING, "Attempt to connect net " & to_string (net_name_start) &
				 " with net " & to_string (net_name_end) & " rejected !");

			set_status ("Attempt to connect net " & to_string (net_name_start) &
						" with net " & to_string (net_name_end) & " rejected !" &
					   " Solution: Rename one of them !");
		end if;
		
		log_indentation_down;

	end insert_net_segment;

	function valid_for_net_segment (
		point			: in type_point;
		log_threshold	: in type_log_level)
		return boolean 
	is
		result : boolean := false;
		
		use et_schematic_ops.nets;
		use pac_selected_segments;
		segments : pac_selected_segments.list;

		choose : constant string := "Choose another place for the junction !";
	begin
		segments := collect_segments (
			module			=> current_active_module,
			place			=> to_position (point, current_active_sheet),
			catch_zone		=> zero,
			log_threshold	=> log_threshold); 

		-- If there are no segments at given point, then the point is valid:
		if is_empty (segments) then
			result := true;
		else
			-- Test if all segments here belong to the same net then the 
			-- point is valid:
			if all_belong_to_same_net (segments) then 

				-- Test for sloping segments here:
				if between_start_and_end_point_of_sloping_segment (point, segments) then
					set_status ("Junction in sloping segment not allowed. " & choose);
					result := false;
				else
					-- no sloping segments here
					result := true;
				end if;

			else
				-- point invalid because more than one net found here:
				set_status ("More than one net here. " & choose);
				result := false;
			end if;
		end if;

		return result;
	end valid_for_net_segment;
	
end et_canvas_schematic_nets;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
