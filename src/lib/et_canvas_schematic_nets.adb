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
with ada.strings;					use ada.strings;
with et_geometry;					use et_geometry;
with et_canvas_schematic;			use et_canvas_schematic;

package body et_canvas_schematic_nets is

	use et_canvas_schematic.pac_canvas;

	procedure delete_selected_segment (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		segment			: in type_selected_segment; -- net/strand/segment
		log_threshold	: in type_log_level)
	is
		s : type_selected_segment := segment;
		
		procedure query_net (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is

			procedure query_strands (
			-- Searches the strands of the net for a segment that sits on given place.
				net_name	: in et_general.type_net_name.bounded_string;
				net			: in out et_schematic.type_net) is
				
				procedure query_segments (strand : in out type_strand) is
				begin
					log (text => "segment " & to_string (element (s.segment)), 
						 level => log_threshold + 1);
															  
					delete (strand.segments, s.segment);
				end query_segments;
				
			begin -- query_strands
				update_element (
					container	=> net.strands,
					position	=> s.strand,
					process		=> query_segments'access);

				-- In case no more segments are left in the strand,
				-- remove the now useless strand entirely.
				if is_empty (element (s.strand).segments) then
					delete (net.strands, s.strand);
				end if;
				
			end query_strands;
		
		begin -- query_net
			log (text => "net name is " & to_string (key (s.net)), level => log_threshold);
			log_indentation_up;
			
			update_element (
				container	=> module.nets,
				position	=> s.net,
				process		=> query_strands'access);

			-- If the net has no strands anymore, delete it entirely because a
			-- net without strands is useless.
			if is_empty (element (s.net).strands) then
				delete (module.nets, s.net);
			end if;

			log_indentation_down;
		end query_net;

	begin
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_net'access);

		clear_proposed_segments;

		reset_request_clarification;
		
		set_status (status_delete);
		
	end delete_selected_segment;

	
	procedure clear_proposed_segments is begin
		clear (proposed_segments);
		selected_segment := pac_proposed_segments.no_element;
	end clear_proposed_segments;
	
	function lowest_available_anonymous_net (
		module		: in pac_generic_modules.cursor)
		return type_net_name.bounded_string
	is
		net : type_net_name.bounded_string; -- like N$56
		cursor : et_schematic.type_nets.cursor;

		-- This flag goes true once a suitable net
		-- name has been found:
		candiate_found : boolean := false; 
	begin
		-- Propose net names like N$1, N$2, ... and locate them
		-- in the module. The search ends once a net like N$56 can not
		-- be located. This net name would be returned to the caller.
		for i in type_anonymous_net_index'first .. type_anonymous_net_index'last loop

			-- compose net name and locate it in module:
			net := to_anonymous_net_name (i); -- N$1, N$2, ...
			cursor := locate_net (module, net);

			if cursor = et_schematic.type_nets.no_element then -- not located
				candiate_found := true;
				exit;
			end if;
		end loop;

		if not candiate_found then
			raise constraint_error;
		end if;
		
		return net;
	end lowest_available_anonymous_net;
	
	function first_net (segments : in pac_proposed_segments.list) 
		return type_net_name.bounded_string -- RESET_N, MASTER_CLOCK
	is
		seg : type_selected_segment;
		c	: pac_proposed_segments.cursor;
		net : type_net_name.bounded_string; -- to be returned
	begin
		if is_empty (segments) then
			return net; -- empty string
		else
			-- Get the first segment of given list of segments.
			seg := element (segments.first);

			-- get the name of the net
			net := key (seg.net);
		end if;

		return net;
	end first_net;

	function more_than_one (segments : in pac_proposed_segments.list) return boolean is 
	begin
		if length (segments) > 1 then
			return true;
		else
			return false;
		end if;
	end more_than_one;

	
	function all_belong_to_same_net (
		segments	: in pac_proposed_segments.list)
		return boolean 
	is 
		result : boolean := true;
		
		net_name : type_net_name.bounded_string;
		net_names_differ : boolean := false;
		
		procedure query_segment (c : in pac_proposed_segments.cursor) is 
			use type_nets;
			use type_net_name;
			
			s : type_selected_segment := element (c);
		begin
			if c = segments.first then
				net_name := key (s.net);
				result := true;
			else
				if key (s.net) /= net_name then
					result := false;
				end if;
			end if;
		end query_segment;
		
	begin
		iterate (segments, query_segment'access);

		return result;
	end all_belong_to_same_net;

	function between_start_and_end_point_of_sloping_segment (
		point		: in type_point;
		segments	: in pac_proposed_segments.list)
		return boolean 
	is 
		result : boolean := false;
		
		procedure query_segment (c : in pac_proposed_segments.cursor) is 
			s : type_selected_segment := element (c);
		begin
			if between_start_and_end_point (point, s.segment) then

				if segment_orientation (s.segment) = SLOPING then
					result := true;
				end if;
				
			end if;
		end query_segment;
		
	begin
		iterate (segments, query_segment'access);

		return result;
	end between_start_and_end_point_of_sloping_segment;
	

	function collect_segments (
		module			: in pac_generic_modules.cursor;
		place			: in et_coordinates.type_position; -- sheet/x/y
		catch_zone		: in type_catch_zone; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_proposed_segments.list
	is
		result : pac_proposed_segments.list;

		procedure query_nets (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) 
		is
			net_cursor : type_nets.cursor := module.nets.first;

			procedure query_strands (
				net_name	: in type_net_name.bounded_string;
				net			: in type_net)
			is
				strand_cursor : type_strands.cursor := net.strands.first;

				procedure query_segments (strand : in type_strand) is
					segment_cursor : type_net_segments.cursor := strand.segments.first;
				begin
					log (text => "probing strand at" & to_string (strand.position),
						 level => log_threshold + 1);
					
					log_indentation_up;
					
					while segment_cursor /= type_net_segments.no_element loop
						log (text => "probing segment" & to_string (element (segment_cursor)),
							level => log_threshold + 1);
						
						-- If the segment is within the catch zone, append
						-- the current net, stand and segment cursor to the result:
						if on_line (
							point		=> type_point (place),
							line		=> element (segment_cursor),
							catch_zone	=> catch_zone) then

							log_indentation_up;
							log (text => "sits on segment", level => log_threshold + 1);
						
							result.append ((net_cursor, strand_cursor, segment_cursor));

							log_indentation_down;
						end if;

						next (segment_cursor);
					end loop;

					log_indentation_down;
				end query_segments;
				
			begin -- query_strands
				while strand_cursor /= type_strands.no_element loop

					-- We are interested in strands on the given sheet only:
					if sheet (element (strand_cursor).position) = sheet (place) then
						query_element (strand_cursor, query_segments'access);
					end if;

					next (strand_cursor);
				end loop;
			end query_strands;
			
		begin -- query_nets
			while net_cursor /= type_nets.no_element loop

				query_element (
					position	=> net_cursor,
					process		=> query_strands'access);

				next (net_cursor);
			end loop;
		end query_nets;

	begin -- collect_segments
		log (text => "looking up net segments at" & to_string (place) 
			 & " catch zone" & to_string (catch_zone), level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module,
			process		=> query_nets'access);

		log_indentation_down;
		
		return result;
		
	end collect_segments;

	
	procedure delete_net_segment (point : in type_point) is 
		use et_schematic_ops.nets;
		segment_cursor : pac_proposed_segments.cursor;
	begin
		log (text => "deleting net segment ...", level => log_threshold);
		log_indentation_up;
		
		-- Collect all segments in the vicinity of the given point:
		proposed_segments := collect_segments (
			module			=> current_active_module,
			place			=> to_position (point, current_active_sheet),
			catch_zone		=> catch_zone_default, -- CS should depend on current scale
			log_threshold	=> log_threshold + 1);

		-- evaluate the number of segments found here:
		case length (proposed_segments) is
			when 0 =>
				reset_request_clarification;
				
			when 1 =>
				segment_cursor := proposed_segments.first;
			
				delete_selected_segment (
					module_cursor	=> current_active_module,
					segment			=> element (segment_cursor),
					log_threshold	=> log_threshold + 1);
				
			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;

				-- preselect the first segment
				selected_segment := proposed_segments.first;
		end case;
		
		log_indentation_down;
	end delete_net_segment;

	
	procedure clarify_net_segment is
		use et_schematic;
		use et_schematic_ops.nets;
		s : type_net_segments.cursor;
		n : type_net_name.bounded_string;
	begin
		-- On every call of this procedure we must advance from one
		-- segment to the next in a circular manner. So if the end 
		-- of the list is reached, then the cursor selected_segment
		-- moves back to the start of the segment list.
		if next (selected_segment) /= pac_proposed_segments.no_element then
			next (selected_segment);
		else
			selected_segment := proposed_segments.first;
		end if;

		-- show the selected segment in the status bar
		s := element (selected_segment).segment;

		-- get the name of the selected net
		n := key (element (selected_segment).net);
		
		set_status ("net " & to_string (n) & space 
			& to_string (s) & ". " & status_next_object_clarification);
	end clarify_net_segment;

	
	procedure delete_selected_net_segment is
		use et_schematic_ops.nets;
	begin
		log (text => "deleting net segment after clarification ...", level => log_threshold);
		log_indentation_up;

		delete_selected_segment (
			module_cursor	=> current_active_module,
			segment			=> element (selected_segment),
			log_threshold	=> log_threshold + 1);
		
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
		segments_at_start_point : pac_proposed_segments.list;
		segments_at_end_point	: pac_proposed_segments.list;

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

			net_cursor := locate_net (module, net_name_start);
			
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

			net_cursor := locate_net (module, net_name_end);
			
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
		segments : pac_proposed_segments.list;

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


-- DRAG/MOVE NET SEGMENT

	procedure reset_segment is begin
		segment := (others => <>);
		clear_proposed_segments;
	end reset_segment;

	
	procedure finalize_drag (
		destination		: in type_point;
		log_threshold	: in type_log_level) is

		net_name : type_net_name.bounded_string;
		point_of_attack : et_coordinates.type_position := to_position (segment.point_of_attack, current_active_sheet);
	begin
		log (text => "finalizing drag ...", level => log_threshold);
		log_indentation_up;

		-- Finalize only if procedure et_canvas_schematic.draw_nets has
		-- granted permission:
		if segment.finalizing_granted then
	
			if selected_segment /= pac_proposed_segments.no_element then

				net_name := key (element (selected_segment).net);

				drag_segment (
					module_name		=> et_project.modules.pac_generic_modules.key (current_active_module),
					net_name		=> net_name,
					point_of_attack	=> point_of_attack,
					coordinates		=> et_geometry.ABSOLUTE,
					destination		=> destination,
					log_threshold	=> log_threshold + 1);

				-- CS Use a procedure drag_segment that takes a cursor to the module instead.

			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
		else
			log (text => "not granted", level => log_threshold);
		end if;
		
		log_indentation_down;
		
		set_status (status_move);
		
		reset_segment;

	end finalize_drag;

	
	procedure find_segments (point : in type_point) is 
		use et_schematic_ops.nets;
	begin
		log (text => "locating net segments ...", level => log_threshold);
		log_indentation_up;
		
		-- Collect all segments in the vicinity of the given point:
		proposed_segments := collect_segments (
			module			=> current_active_module,
			place			=> to_position (point, current_active_sheet),
			catch_zone		=> catch_zone_default, -- CS should depend on current scale
			log_threshold	=> log_threshold + 1);

		-- evaluate the number of segments found here:
		case length (proposed_segments) is
			when 0 =>
				reset_request_clarification;
				reset_segment;
				
			when 1 =>
				segment.being_moved := true;
				selected_segment := proposed_segments.first;

				reset_request_clarification;

				set_status (status_move);
				
			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;

				-- preselect the first segment
				selected_segment := proposed_segments.first;
		end case;
		
		log_indentation_down;
	end find_segments;


	
-- NET LABLES

	procedure clear_proposed_labels is begin
		null; -- CS
	end clear_proposed_labels;
		
	
	procedure reset_label is begin
		label := (others => <>);
		clear_proposed_labels;
		clear_proposed_segments;
	end reset_label;
	

	procedure place_label (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_selected_segment;
		destination		: in type_point; -- x/y
		appearance 		: in type_net_label_appearance; -- simple/tag
		log_threshold	: in type_log_level) is

		procedure query_nets (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is

			procedure query_strands (
				net_name	: in type_net_name.bounded_string;
				net			: in out type_net) is

				procedure query_segments (strand : in out type_strand) is

					procedure attach_label (segment : in out type_net_segment) is 
						use type_net_labels;
						label : type_net_label_base;
					begin
						-- The label position is absolute:
						label.position := destination;

						case appearance is
							when SIMPLE =>
								append (
									container	=> segment.labels,
									new_item	=> (label with
										appearance		=> SIMPLE,
										rotation_simple	=> pac_text.snap (zero_rotation)) -- CS
									   );
								
							when TAG =>
								-- A tag label can be attached to a stub only.
								null; -- CS
								
						end case;

					end attach_label;
					
				begin
					update_element (
						container	=> strand.segments,
						position	=> segment.segment,
						process		=> attach_label'access);

				end query_segments;
				
			begin -- query_strands
				update_element (
					container	=> net.strands,
					position	=> segment.strand,
					process		=> query_segments'access);

			end query_strands;
			
		begin -- query_nets
			update_element (
				container	=> module.nets,
				position	=> segment.net,
				process		=> query_strands'access);
			
		end query_nets;
		
	begin
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_nets'access);
	end place_label;
	
	procedure finalize_place_segment (
		destination		: in type_point;
		log_threshold	: in type_log_level) is
	begin
		log (text => "finalizing place net segment ...", level => log_threshold);
		log_indentation_up;

		-- Finalize only if procedure et_canvas_schematic.draw_nets has
		-- granted permission:
		if label.finalizing_granted then
			
			if selected_segment /= pac_proposed_segments.no_element then

				place_label (
					module_cursor		=> current_active_module,
					segment				=> element (selected_segment),
					destination			=> destination,
					appearance			=> label.appearance,
					log_threshold		=> log_threshold + 1);
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;

		else
			log (text => "not granted", level => log_threshold);
		end if;
		
		log_indentation_down;
		
		set_status (status_place_label);
		
		reset_label;

	end finalize_place_segment;

	
end et_canvas_schematic_nets;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
