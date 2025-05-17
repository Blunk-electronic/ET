------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        CANVAS SCHEMATIC NETS                             --
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
-- <http://www.gnu.org/licenses/>.   
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
with ada.characters.handling;
with ada.exceptions;				use ada.exceptions;

with gtk.window;
with gtk.box;
with gtk.label;
with gtk.gentry;

with et_canvas_schematic_2;			use et_canvas_schematic_2;
with et_module_names;				use et_module_names;
with et_modes.schematic;			use et_modes.schematic;
with et_netlists;
with et_net_class;
with et_board_ops.ratsnest;

with et_undo_redo;
with et_commit;
with et_object_status;				use et_object_status;
with et_canvas_schematic_preliminary_object;	use et_canvas_schematic_preliminary_object;


package body et_canvas_schematic_nets is

	use et_schematic_ops;
	use et_canvas_schematic_2.pac_canvas;

	
	procedure delete_selected_segment (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		segment			: in type_selected_segment; -- net/strand/segment
		log_threshold	: in type_log_level)
	is
		s : type_selected_segment := segment;
		
		procedure query_net (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			procedure query_strands (
			-- Searches the strands of the net for a segment that sits on given place.
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is				
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

		et_board_ops.ratsnest.update_ratsnest (module_cursor, log_threshold + 1);
		
		set_status (status_delete);
		
	end delete_selected_segment;


	

	function selected_net return pac_net_name.bounded_string is
		ss : constant type_selected_segment := element (selected_segment);
	begin
		return key (ss.net);
	end selected_net;



	
	function get_strand_position return type_object_position is
		ss : constant type_selected_segment := element (selected_segment);
	begin
		return element (ss.strand).position;
	end get_strand_position;



	
	procedure clear_proposed_segments is begin
		clear (proposed_segments);
		selected_segment := pac_proposed_segments.no_element;
	end clear_proposed_segments;



	
	function first_net (
		segments : in pac_proposed_segments.list) 
		return pac_net_name.bounded_string -- RESET_N, MASTER_CLOCK
	is
		seg : type_selected_segment;
		c	: pac_proposed_segments.cursor;
		net : pac_net_name.bounded_string; -- to be returned
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



	
	function more_than_one (segments : in pac_proposed_segments.list) 
		return boolean is 
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
		
		net_name : pac_net_name.bounded_string;
		net_names_differ : boolean := false;
		
		procedure query_segment (c : in pac_proposed_segments.cursor) is 
			use pac_nets;
			use pac_net_name;
			
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



	
	
	function between_A_and_B_of_sloping_segment (
		point		: in type_vector_model;
		segments	: in pac_proposed_segments.list)
		return boolean 
	is 
		result : boolean := false;
		
		procedure query_segment (c : in pac_proposed_segments.cursor) is 
			s : type_selected_segment := element (c);
		begin
			if between_A_and_B (set_catch_zone (point, 0.0), s.segment) then

				if get_segment_orientation (s.segment) = SLOPING then
					result := true;
				end if;
				
			end if;
		end query_segment;
		
	begin
		iterate (segments, query_segment'access);

		return result;
	end between_A_and_B_of_sloping_segment;
	



	
	function collect_segments (
		module			: in pac_generic_modules.cursor;
		place			: in type_object_position; -- sheet/x/y
		zone			: in type_zone_radius := type_zone_radius'first; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_proposed_segments.list
	is
		result : pac_proposed_segments.list;

		
		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			
			procedure query_strands (
				net_name	: in pac_net_name.bounded_string;
				net			: in type_net)
			is
				strand_cursor : pac_strands.cursor := net.strands.first;

				
				procedure query_segments (
					strand : in type_strand) 
				is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;
				begin
					log (text => "probing strand at" & to_string (strand.position),
						 level => log_threshold + 1);
					
					log_indentation_up;
					
					while segment_cursor /= pac_net_segments.no_element loop
						log (text => "probing segment " & to_string (element (segment_cursor)),
							level => log_threshold + 1);

						--put_line ("probing segment");

						-- If the segment is in the catch zone, append
						-- the current net, stand and segment cursor to the result:
						if in_catch_zone (
							zone	=> set_catch_zone (place.place, zone),
							line	=> element (segment_cursor),
							width	=> net_line_width)
						then
							log_indentation_up;
							log (text => "sits on segment", level => log_threshold + 1);
						
							result.append ((net_cursor, strand_cursor, segment_cursor));
							log_indentation_down;
						end if;

						next (segment_cursor);
					end loop;

					log_indentation_down;
				end query_segments;
				
				
			begin
				while strand_cursor /= pac_strands.no_element loop

					-- We are interested in strands on the given sheet only:
					if get_sheet (element (strand_cursor).position) = get_sheet (place) then
						query_element (strand_cursor, query_segments'access);
					end if;

					next (strand_cursor);
				end loop;
			end query_strands;
			
			
		begin
			while net_cursor /= pac_nets.no_element loop

				query_element (
					position	=> net_cursor,
					process		=> query_strands'access);

				next (net_cursor);
			end loop;
		end query_nets;

		
	begin -- collect_segments
		log (text => "looking up net segments in " 
			 & to_string (set_catch_zone (place.place, zone)),
			 level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module,
			process		=> query_nets'access);

		log_indentation_down;
		
		return result;		
	end collect_segments;


	
	
	procedure delete_net_segment (
		point : in type_vector_model) 
	is 
		use et_schematic_ops.nets;
		segment_cursor : pac_proposed_segments.cursor;

		use et_undo_redo;
		use et_commit;
	begin
		log (text => "deleting net segment ...", level => log_threshold);
		log_indentation_up;
		
		-- Collect all segments in the vicinity of the given point:
		proposed_segments := collect_segments (
			module			=> active_module,
			place			=> to_position (point, active_sheet),
			zone			=> get_catch_zone (catch_zone_radius_default),
			log_threshold	=> log_threshold + 1);

		
		-- evaluate the number of segments found here:
		case length (proposed_segments) is
			when 0 =>
				reset_request_clarification;
				
			when 1 =>
				segment_cursor := proposed_segments.first;

				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
		
				delete_selected_segment (
					module_cursor	=> active_module,
					segment			=> element (segment_cursor),
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);
				
			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;

				-- preselect the first segment
				selected_segment := proposed_segments.first;
		end case;
		
		log_indentation_down;
	end delete_net_segment;


	
	procedure clarify_net_segment is
		s : pac_net_segments.cursor;
		n : pac_net_name.bounded_string;
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
		use et_undo_redo;
		use et_commit;
	begin
		log (text => "deleting net segment after clarification ...", level => log_threshold);
		log_indentation_up;

		-- Commit the current state of the design:
		commit (PRE, verb, noun, log_threshold + 1);

		delete_selected_segment (
			module_cursor	=> active_module,
			segment			=> element (selected_segment),
			log_threshold	=> log_threshold + 1);

		-- Commit the new state of the design:
		commit (POST, verb, noun, log_threshold + 1);
		
		log_indentation_down;
	end delete_selected_net_segment;


	

	
	procedure make_path (
		tool	: in type_tool;
		point	: in type_vector_model)
	is begin
		-- Set the tool being used for this path so that procedure
		-- draw_path (in et_canvas_schematic-draw_nets)
		-- knows where to get the end point from.
		object_tool := tool;

		-- Initally the preliminary_segment is NOT ready. Nothing will be drawn.
		-- Upon the first calling of this procedure the start point of the
		-- path will be set.
		
		if not edit_process_running then
			-- Set start point:
			live_path.A := point;
					
			-- Before processing the start point further, it must be validated:
			if valid_for_net_segment (live_path.A, log_threshold + 3) then

				-- Allow drawing of the path:
				set_edit_process_running;
				
				set_status (status_A & to_string (live_path.A) & ". " &
					status_press_space & status_set_B & status_hint_for_abort);
			end if;
			

		else -- preliminary_segment IS ready

			-- Start a new path only if the given point differs from 
			-- the start point of the current path:
			if point /= live_path.A then
				
				-- Complete the path by setting its end point.
				-- The the current bend point (if there is one) into account:

				if live_path.bended = NO then				
					live_path.B := point;

					-- Before processing the end point further, it must be validated:
					if valid_for_net_segment (live_path.B, log_threshold + 3) then

						-- Insert a single net segment:
						insert_net_segment (
							module			=> active_module,
							sheet			=> active_sheet,
							net_name_given	=> object_net_name, -- RESET_N, or empty
							segment			=> to_net_segment (live_path.A, live_path.B),
							log_threshold	=> log_threshold + 1);

					end if;

				else
					-- The path is bended. The bend point has been computed
					-- interactively while moving the mouse or the cursor.
					-- See procedure draw_path in et_canvas_schematic-draw_nets.
					
					-- Before processing the BEND point further, it must be validated:
					if valid_for_net_segment (live_path.bend_point, log_threshold + 3) then

						-- Insert first segment of the path:
						insert_net_segment (
							module			=> active_module,
							sheet			=> active_sheet,
							net_name_given	=> object_net_name, -- RESET_N, or empty
							segment			=> to_net_segment (live_path.A, live_path.bend_point),
							log_threshold	=> log_threshold + 1);


						
						-- END POINT:
						live_path.B := point;
						
						-- Before processing the END point further, it must be validated:
						if valid_for_net_segment (live_path.B, log_threshold + 3) then

							-- Insert second segment of the path:
							insert_net_segment (
								module			=> active_module,
								sheet			=> active_sheet,
								net_name_given	=> object_net_name, -- RESET_N, or empty
								segment			=> to_net_segment (live_path.bend_point, live_path.B),
								log_threshold	=> log_threshold + 1);
						
						end if;
					end if;

				end if;

				-- Set start point of path so that a new
				-- path can be drawn:			
				live_path.A := point;

			else
				reset_preliminary_segment;
			end if;
		end if;
	end make_path;


	


	procedure insert_net_segment (
		module			: in pac_generic_modules.cursor;
		net_name_given	: in pac_net_name.bounded_string; -- RESET_N
		sheet			: in type_sheet;
		segment			: in type_net_segment;
		log_threshold	: in type_log_level)
	is 
		A : constant type_object_position := to_position (get_A (segment), sheet);
		B : constant type_object_position := to_position (get_B (segment), sheet);

		use et_schematic_ops.nets;
		segments_at_A : pac_proposed_segments.list;
		segments_at_B : pac_proposed_segments.list;

		use pac_nets;
		net_cursor	: pac_nets.cursor;

		net_name_auto_generated	: pac_net_name.bounded_string; -- N$234
		net_name_start, net_name_end : pac_net_name.bounded_string;

		-- Extends the given net (named after net_name) by the given segment.
		-- Outputs a message if an explicit net_name_given was provided stating
		-- that this net_name_given will be ignored.
		-- Calls et_schematic_ops.nets.insert_segment to do the actual insertion
		-- of the segment in the targeted net:
		procedure extend_net (net_name : in pac_net_name.bounded_string) is begin
			log (text => "attaching start point of new segment to net "
				& enclose_in_quotes (to_string (net_name)),
				level => log_threshold + 1);
			
			log_indentation_up;

			net_cursor := locate_net (module, net_name);
			
			-- Extend the existing net by the given segment:
			et_schematic_ops.nets.insert_segment (
				module, net_cursor, sheet, net_name, segment, log_threshold + 2);

			status_clear;
			
			log_indentation_down;

			-- If an explicit net name was given AND if it does
			-- NOT match the name of the net being extended,
			-- then output a message:
			if not is_empty (net_name_given) then -- explicit name given
				if net_name_given /= net_name then -- names do NOT match

					set_status ("WARNING ! Given net name " 
						& enclose_in_quotes (to_string (net_name_given))
						& " ignored while extending net " 
						& enclose_in_quotes (to_string (net_name)) & " !");
					
				end if;
			end if;
		end extend_net;


		use et_undo_redo;
		use et_commit;

		
	begin -- insert_net_segment
		log (text => "adding net segment on sheet " & to_string (sheet) & to_string (segment), 
			 level => log_threshold);

		log_indentation_up;

		-- Commit the current state of the design:
		commit (PRE, verb, noun, log_threshold + 1);
		

		-- Look for already existing nets at the start of the segment:
		segments_at_A := collect_segments (
			module			=> module,
			place			=> A,
			log_threshold	=> log_threshold + 2);

		-- We assume there are either no segments at all or 
		-- segments belonging to the same net at the start point:
		net_name_start := first_net (segments_at_A);
		
		-- Look for already existing nets at the end of the segment:
		segments_at_B := collect_segments (
			module			=> module,
			place			=> B,
			log_threshold	=> log_threshold + 2);

		-- We assume there are either no segments at all or 
		-- segments belonging to the same net at the end point:
		net_name_end := first_net (segments_at_B);
		
		-- If no nets at BOTH start AND end point, then
		-- an anonymous net or an explicit named net will be generated:
		if is_empty (net_name_start) and is_empty (net_name_end) then

			if is_empty (net_name_given) then -- no explicit net name provided

				-- Create a new anonymous net with a name like N$234:
				net_name_auto_generated := get_lowest_available_anonymous_net (module); -- N$234
				
				log (text => "creating new anonymous net " 
					 & to_string (net_name_auto_generated),
					 level => log_threshold + 1);
				
				log_indentation_up;

				-- Create the new net with this single segment in the module:
				et_schematic_ops.nets.insert_segment (
					module, net_cursor, sheet, net_name_auto_generated, segment, log_threshold + 2);

				status_clear;
				
				log_indentation_down;
				
			else -- explicit net name provided
				log (text => "explicit net name is " 
					& to_string (net_name_given),
					level => log_threshold + 1);
				
				log_indentation_up;

				net_cursor := locate_net (module, net_name_given);

				-- If net_cursor is no_element then a new explicit named net will be generated.
				-- If net_cursor points to an existing net, then the existing net will be
				-- extended by the segment:
				et_schematic_ops.nets.insert_segment (
					module, net_cursor, sheet, net_name_given, segment, log_threshold + 2);

				status_clear;
				
				log_indentation_down;
			end if;
		end if;

		-- If net at start point AND no net at end point then
		-- the net at the start point is extended by the new segment:
		if not is_empty (net_name_start) and is_empty (net_name_end) then
			extend_net (net_name_start);
		end if;

		-- If net at end point AND no net at start point then
		-- the net at the end point is extended by the new segment:
		if not is_empty (net_name_end) and is_empty (net_name_start) then
			extend_net (net_name_end);
		end if;
		
		-- If net at start point AND at end point then extend the
		-- net at the start point by the segment. We could extend the net
		-- at the end point as well. It does not matter.
		-- This results in connecting two strands with each other. Their
		-- net names must be equal.
		-- The verification that the net names match is done by
		-- et_schematic_ops.nets.insert_segment.
		if not is_empty (net_name_end) and not is_empty (net_name_start) then
			extend_net (net_name_start);
		end if;

		et_board_ops.ratsnest.update_ratsnest (module, log_threshold + 1);

		-- Commit the new state of the design:
		commit (POST, verb, noun, log_threshold + 1);
		
		log_indentation_down;
	end insert_net_segment;



	
	
	function valid_for_net_segment (
		point			: in type_vector_model;
		log_threshold	: in type_log_level)
		return boolean 
	is
		result : boolean := false;
		
		use et_schematic_ops.nets;
		segments : pac_proposed_segments.list;

		choose : constant string := "Choose another place for the junction !";
	begin
		segments := collect_segments (
			module			=> active_module,
			place			=> to_position (point, active_sheet),
			log_threshold	=> log_threshold); 

		-- If there are no segments at given point, then the point is valid:
		if is_empty (segments) then
			result := true;
		else
			-- Test if all segments here belong to the same net then the 
			-- point is valid:
			if all_belong_to_same_net (segments) then 

				-- Test for sloping segments here:
				if between_A_and_B_of_sloping_segment (point, segments) then
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



	

	procedure reset_preliminary_segment is 
	begin
		reset_edit_process_running;
		object_tool := MOUSE;
		-- live_path := (bend_style => live_path.bend_style, -- no change
					-- others => <>);

		object_net_name := no_name;
		object_point_of_attack := origin;
		reset_finalizing_granted;
		
		clear_proposed_segments;
	end reset_preliminary_segment;


	

	-- Called when the operator presses ENTER after typing a property in
	-- the properties window.
	-- The properties window will remain open until the operator enters 
	-- a correct property. The status bar of the window shows the error message:	
	procedure property_entered (self : access gtk.gentry.gtk_entry_record'class) is 

		procedure clean_up is begin
			properties_confirmed := true;
			window_properties.window.destroy;
			reset_request_clarification;
			--status_clear;
			clear_proposed_segments;
		-- CS redraw;
		end clean_up;

		position : type_object_position;
		
	begin -- property_entered
		case noun is
			when NOUN_NET =>
				case net_rename.scope is
					when STRAND =>
						position := get_strand_position;
							
					when SHEET =>
						position := to_position (
										point => origin, -- don't care
										sheet => active_sheet); -- sheet number

					when EVERYWHERE =>
						position := to_position (
										point => origin, -- don't care
										sheet => 1); -- don't care
				end case;

				rename_net (
					module_cursor	=> active_module,
					net_name_before	=> selected_net, -- RESET_N
					net_name_after	=> to_net_name (self.get_text), -- RST_N
					scope			=> net_rename.scope,
					place			=> position,
					log_threshold	=> log_threshold + 1);

				
			when others => raise constraint_error;
		end case;

		-- If everything was fine, close the window and clean up.
		-- If one of the operations above has raised an exception then
		-- nothing will be cleaned up and the window will remain until the
		-- operator enters a correct property.
		clean_up;
		
		-- Whatever goes wrong, output the message in the status bar
		-- of the properties window:
		exception when event: others =>
			set_status_properties (exception_message (event));
			
	end property_entered;
	

	
	procedure window_set_property is
		use gtk.window;
		use gtk.box;
		use gtk.label;
		use gtk.gentry;
		
		box : gtk_vbox;
		label : gtk_label;
		gentry : gtk_gentry;
		
		net_name : constant string := to_string (selected_net); -- RESET_N
	begin
		build_window_properties;

		window_properties.window.set_default_size (200, 100);
		window_properties.window.set_resizable (false);
		
		gtk_new_vbox (box);
		add (window_properties.window, box);

		-- Prepare displaying the old state of the property:
		gtk_new (entry_property_old);
		
		case noun is
			when NOUN_NET =>
				gtk_new (label, "Net name");
				set_property_before (net_name);
				
			when others => raise constraint_error;
		end case;				

		pack_start (box, label);

		-- show the old property:
		gtk_new (label_property_old, "old:");
		pack_start (box, label_property_old);
		pack_start (box, entry_property_old);

		-- show the new property (will be entered by the operator later):
		gtk_new (label_property_new, "new:"); -- CS some info or warning about the scope ?
		pack_start (box, label_property_new);
		
		gtk_new (gentry);
		pack_start (box, gentry);
		gentry.on_activate (property_entered'access);
		gentry.grab_focus;

		gtk_new (label_properties_status);
		pack_start (box, label_properties_status);
		
		window_properties.window.show_all;

	end window_set_property;

	

	
-- DRAG/MOVE NET SEGMENT

	
	procedure find_segments (point : in type_vector_model) is 
		use et_schematic_ops.nets;
		use et_modes.schematic;
	begin
		log (text => "locating net segments ...", level => log_threshold);
		log_indentation_up;
		
		-- Collect all segments in the vicinity of the given point:
		proposed_segments := collect_segments (
			module			=> active_module,
			place			=> to_position (point, active_sheet),
			zone			=> get_catch_zone (catch_zone_radius_default),
			log_threshold	=> log_threshold + 1);

		
		-- Since this procedure is called by several schematic operations,
		-- we must test the current verb and noun:

		case verb is
			when VERB_DRAG =>
				case noun is
					when NOUN_NET =>
						
						-- evaluate the number of segments found here:
						case length (proposed_segments) is
							when 0 =>
								reset_request_clarification;
								reset_preliminary_segment;
								
							when 1 =>
								set_edit_process_running;
								selected_segment := proposed_segments.first;

								reset_request_clarification;

								set_status (status_move);
								
							when others =>
								set_request_clarification;

								-- preselect the first segment
								selected_segment := proposed_segments.first;
						end case;

					when others => null;
				end case;

			when VERB_PLACE =>
				case noun is
					when NOUN_LABEL =>

						-- evaluate the number of segments found here:
						case length (proposed_segments) is
							when 0 =>
								reset_request_clarification;
								reset_label;
								
							when 1 =>
								label.ready := true;
								selected_segment := proposed_segments.first;

								reset_request_clarification;

								case label.appearance is
									when SIMPLE	=> set_status (status_place_label_simple);
									when TAG	=> set_status (status_place_label_tag);
								end case;
								
							when others =>
								set_request_clarification;

								-- preselect the first segment
								selected_segment := proposed_segments.first;
						end case;

					when others => null;
				end case;

			when VERB_RENAME =>
				case noun is
					when NOUN_NET => 

						-- evaluate the number of segments found here:
						case length (proposed_segments) is
							when 0 =>
								reset_request_clarification;
								reset_preliminary_segment;
								
							when 1 =>
								selected_segment := proposed_segments.first;

								reset_request_clarification;
								
								window_set_property;
								
							when others =>
								set_request_clarification;

								-- preselect the first segment
								selected_segment := proposed_segments.first;
						end case;
						
					when others => null;
				end case;

			when VERB_SHOW =>
				case noun is
					when NOUN_NET => 

						-- evaluate the number of segments found here:
						case length (proposed_segments) is
							when 0 =>
								reset_request_clarification;
								
							when 1 =>
								selected_segment := proposed_segments.first;
								show_properties_of_selected_net;
								
							when others =>
								set_request_clarification;

								-- preselect the first segment
								selected_segment := proposed_segments.first;
						end case;
						
					when others => null;
				end case;
				
			when others => null;
		end case;
		
		log_indentation_down;
	end find_segments;



	


	procedure show_selected_object (
		object		: in type_object)
	is 
		praeamble : constant string := "selected object: ";
	begin
		case object.cat is
			when CAT_SEGMENT =>
				set_status (praeamble & to_string (object.segment)
					& ". " & status_next_object_clarification);

			-- CS placeholders
				
			when CAT_VOID => null; -- CS
		end case;
	end show_selected_object;






	
	procedure clarify_object is 

		procedure do_it is
			use pac_objects;
			
			-- Gather all proposed objects:
			proposed_objects : constant pac_objects.list := 
				get_objects (active_module, PROPOSED, log_threshold + 1);

			proposed_object : pac_objects.cursor;

			-- We start with the first object that is currently selected:
			selected_object : type_object := 
				get_first_object (active_module, SELECTED, log_threshold + 1);

		begin
			log (text => "proposed objects total " 
				& natural'image (get_count (proposed_objects)),
				level => log_threshold + 2);

			
			-- Locate the selected object among the proposed objects:
			proposed_object := proposed_objects.find (selected_object);

			-- Deselect the the proposed object:
			modify_status (
				module_cursor	=> active_module, 
				operation		=> to_operation (CLEAR, SELECTED),
				object_cursor	=> proposed_object, 
				log_threshold	=> log_threshold + 1);

			-- Advance to the next proposed object:
			next (proposed_object);

			-- If end of list reached, then proceed at 
			-- the begin of the list:
			if proposed_object = pac_objects.no_element then
				proposed_object := proposed_objects.first;
			end if;
			
			-- Select the proposed object:
			modify_status (
				module_cursor	=> active_module, 
				operation		=> to_operation (SET, SELECTED),
				object_cursor	=> proposed_object, 
				log_threshold	=> log_threshold + 1);

			-- Display the object in the status bar:
			show_selected_object (element (proposed_object));
		end do_it;
		
		
	begin
		log (text => "clarify_object", level => log_threshold + 1);
		log_indentation_up;		
		do_it;		
		log_indentation_down;
	end clarify_object;



	

	


	-- This procedure searches for the first selected object
	-- and sets its status to "moving":
	procedure set_first_selected_object_moving is
		
		procedure do_it is
			-- Get the first selected object:
			selected_object : constant type_object := 
				get_first_object (active_module, SELECTED, log_threshold + 1);

			-- Gather all selected objects:
			objects : constant pac_objects.list :=
				get_objects (active_module, SELECTED, log_threshold + 1);

			c : pac_objects.cursor;
		begin
			-- Get a cursor to the candidate object
			-- among all selected objects:
			c := objects.find (selected_object);
			
			modify_status (active_module, c, to_operation (SET, MOVING), log_threshold + 1);
		end do_it;
		
		
	begin
		log (text => "set_first_selected_object_moving ...", level => log_threshold);
		log_indentation_up;
		do_it;
		log_indentation_down;
	end set_first_selected_object_moving;


	
	

	
	

	procedure find_objects (
		point : in type_vector_model)
	is 
		use et_modes.schematic;
		
		-- The total number of objects that have
		-- been proposed:
		count_total : natural := 0;


		-- This procedure searches for the first proposed
		-- object and marks it as "selected":
		procedure select_first_proposed is
			object : type_object := get_first_object (
						active_module, PROPOSED, log_threshold + 1);
		begin
			modify_status (
				active_module, object, to_operation (SET, SELECTED), log_threshold + 1);

			-- If only one object found, then show it in the status bar:
			if count_total = 1 then
				show_selected_object (object);
			end if;
		end select_first_proposed;

		
	begin
		log (text => "locating objects ...", level => log_threshold);
		log_indentation_up;

		-- CS propose objects according to
		-- current verb.
		
		-- Propose net segments in the vicinity of the given point:
		propose_segments (
			module_cursor	=> active_module,
			catch_zone		=> set_catch_zone (point, get_catch_zone (catch_zone_radius_default)),
			count			=> count_total,
			log_threshold	=> log_threshold + 1);


		-- CS net labels, junctions


		log (text => "proposed objects total" & natural'image (count_total),
			 level => log_threshold + 1);

		
		-- Evaluate the number of objects found here:
		case count_total is
			when 0 =>
				null; -- nothing to do
				
			when 1 =>
				set_edit_process_running;
				select_first_proposed;

				case verb is
					when VERB_MOVE =>
						set_first_selected_object_moving;

						
					when VERB_DRAG =>
						set_first_selected_object_moving;

						-- Set the net segments which are
						-- connected with the selected segment
						-- as "moving":
						set_segments_moving (active_module, log_threshold + 1);

					when others => null; -- CS
				end case;
				
				reset_request_clarification;
				
			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;
				select_first_proposed;
		end case;
		
		log_indentation_down;
	end find_objects;


	


	
-- 	procedure drag_segment (
-- 		tool		: in type_tool;
-- 		position	: in type_vector_model)
-- 	is 
-- 		use et_undo_redo;
-- 		use et_commit;
-- 
-- 		
-- 		-- Assigns the given destination after the drag to the selected segment:
-- 		procedure finalize_drag is
-- 			net_name : pac_net_name.bounded_string;
-- 			
-- 			point_of_attack : type_object_position := 
-- 				to_position (object_point_of_attack, active_sheet);
-- 		begin
-- 			log (text => "finalizing drag ...", level => log_threshold + 1);
-- 			log_indentation_up;
-- 
-- 			-- Finalize only if procedure et_canvas_schematic.draw_nets has
-- 			-- granted permission:
-- 			if finalizing_granted then
-- 		
-- 				if selected_segment /= pac_proposed_segments.no_element then
-- 
-- 					net_name := key (element (selected_segment).net);
-- 
-- 					drag_segment (
-- 						module_cursor	=> active_module,
-- 						net_name		=> net_name,
-- 						point_of_attack	=> point_of_attack,
-- 						coordinates		=> ABSOLUTE,
-- 						destination		=> position,
-- 						log_threshold	=> log_threshold + 2);
-- 
-- 				else
-- 					log (text => "nothing to do", level => log_threshold);
-- 				end if;
-- 					
-- 			else
-- 				log (text => "not granted", level => log_threshold);
-- 			end if;
-- 			
-- 			log_indentation_down;
-- 			
-- 			set_status (status_move);
-- 			
-- 			reset_preliminary_segment;
-- 		end finalize_drag;
-- 
-- 		
-- 	begin
-- 		if not edit_process_running then
-- 			
-- 			-- Set the tool being used for dragging the net segment:
-- 			object_tool := tool;
-- 			
-- 			if not clarification_pending then
-- 				find_segments (position);
-- 				object_point_of_attack := position;
-- 			else
-- 				set_edit_process_running;
-- 				reset_request_clarification;
-- 			end if;
-- 
-- 		else
-- 			-- Commit the current state of the design:
-- 			commit (PRE, verb, noun, log_threshold + 1);
-- 			
-- 			-- Finally assign the cursor position to the
-- 			-- currently selected segment:
-- 			finalize_drag;
-- 
-- 			-- Commit the new state of the design:
-- 			commit (POST, verb, noun, log_threshold + 1);
-- 		end if;		
-- 	end drag_segment;
-- 






	procedure drag_object (
		tool	: in type_tool;
		point	: in type_vector_model)
	is 

		-- Drags the selected object:
		procedure finalize is
			use et_modes.schematic;
			use et_undo_redo;
			use et_commit;

			object : constant type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalizing drag ...", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				drag_object (
					module_cursor	=> active_module, 
					object			=> object, 
					point_of_attack	=> object_point_of_attack,
					destination		=> point,
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

				-- -- If a segment has been dragged, then the board
				-- -- must be redrawn:
				-- if object.cat = CAT_SEGMENT then
				-- 	redraw_board;
				-- end if;
				-- CS really required ? Redraw the schematic instead ?
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			
			set_status (status_drag);
			
			reset_proposed_objects (active_module, log_threshold + 1);

			reset_editing_process; -- prepare for a new editing process
		end finalize;

		
	begin
		-- Initially the editing process is not running:
		if not edit_process_running then
			
			-- Set the tool being used:
			object_tool := tool;

			object_point_of_attack := point;
			
			
			if not clarification_pending then
				-- Locate all objects in the vicinity of the given point:
				find_objects (point);

				-- NOTE: If many objects have been found, then
				-- clarification is now pending.

				-- If find_objects has found only one object,				
				-- then the flag edit_process_running is set true.
			else
				-- Here the clarification procedure ends.
				-- An object has been selected via procedure clarify_object.
				-- By setting the status of the selected object
				-- as "moving", the selected object
				-- will be drawn according to the given point and 
				-- the tool position.
				set_first_selected_object_moving;
				
				-- Set the net segments which are
				-- connected with the selected segment as "moving":
				set_segments_moving (active_module, log_threshold + 1);

				-- Furtheron, on the next call of this procedure
				-- the selected object will be assigned its final position.

				set_edit_process_running;
				reset_request_clarification;
			end if;

		else
			finalize;
		end if;
	end drag_object;

	



	procedure move_object (
		tool	: in type_tool;
		point	: in type_vector_model)
	is 

		-- Moves the selected object:
		procedure finalize is
			use et_modes.schematic;
			use et_undo_redo;
			use et_commit;

			object : constant type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalizing move ...", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				move_object (
					module_cursor	=> active_module, 
					object			=> object, 
					point_of_attack	=> object_point_of_attack,
					destination		=> point,
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

				-- -- If a segment has been dragged, then the board
				-- -- must be redrawn:
				-- if object.cat = CAT_SEGMENT then
				-- 	redraw_board;
				-- end if;
				-- CS really required ? Redraw the schematic instead ?
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			
			set_status (status_move);
			
			reset_proposed_objects (active_module, log_threshold + 1);

			reset_editing_process; -- prepare for a new editing process
		end finalize;

		
	begin
		-- Initially the editing process is not running:
		if not edit_process_running then
			
			-- Set the tool being used:
			object_tool := tool;

			object_point_of_attack := point;
			
			
			if not clarification_pending then
				-- Locate all objects in the vicinity of the given point:
				find_objects (point);

				-- NOTE: If many objects have been found, then
				-- clarification is now pending.

				-- If find_objects has found only one object,				
				-- then the flag edit_process_running is set true.
			else
				-- Here the clarification procedure ends.
				-- An object has been selected via procedure clarify_object.
				-- By setting the status of the selected object
				-- as "moving", the selected object
				-- will be drawn according to the given point and 
				-- the tool position.
				set_first_selected_object_moving;

				-- Furtheron, on the next call of this procedure
				-- the selected object will be assigned its final position.

				set_edit_process_running;
				reset_request_clarification;
			end if;

		else
			finalize;
		end if;
	end move_object;






	procedure delete_object (
		point	: in type_vector_model)
	is 

		-- Deletes the selected object:
		procedure finalize is
			use et_modes.schematic;
			use et_undo_redo;
			use et_commit;

			object : constant type_object := get_first_object (
					active_module, SELECTED, log_threshold + 1);
		begin
			log (text => "finalizing delete ...", level => log_threshold);
			log_indentation_up;

			-- If a selected object has been found, then
			-- we do the actual finalizing:
			if object.cat /= CAT_VOID then
				
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
				
				delete_object (
					module_cursor	=> active_module, 
					object			=> object, 
					log_threshold	=> log_threshold + 1);

				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);

				-- -- If a segment has been dragged, then the board
				-- -- must be redrawn:
				-- if object.cat = CAT_SEGMENT then
				-- 	redraw_board;
				-- end if;
				-- CS really required ? Redraw the schematic instead ?
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			
			set_status (status_delete);
			
			reset_proposed_objects (active_module, log_threshold + 1);

			reset_editing_process; -- prepare for a new editing process
		end finalize;

		
	begin
		if not clarification_pending then
			-- Locate all objects in the vicinity of the given point:
			find_objects (point);
			
			-- NOTE: If many objects have been found, then
			-- clarification is now pending.

			-- If find_objects has found only one object
			-- then the flag edit_process_running is set true.

			if edit_process_running then
				finalize;
			end if;
		else
			-- Here the clarification procedure ends.
			-- An object has been selected
			-- via procedure clarify_object.

			finalize;
		end if;
	end delete_object;



	
	
	
-- NET LABLES

	--procedure clear_proposed_labels is begin
	--proposed_labels.clear;
	--end clear_proposed_labels;
		
	
	procedure reset_label is begin
		label := (others => <>);
		--clear_proposed_labels;

		clear_proposed_segments;
		proposed_labels.clear;
		selected_label := pac_proposed_labels.no_element;
	end reset_label;

	
	-- This function collects net labels of a certain category
	-- inside the catch zone around a place.
	function collect_labels (
		module			: in pac_generic_modules.cursor;
		place			: in type_object_position; -- sheet/x/y
		zone			: in type_zone_radius; -- the circular area around the place
		category		: in type_label_category := BOTH; -- default is: collect all kinds of labels
		log_threshold	: in type_log_level)
		return pac_proposed_labels.list
	is
		result : pac_proposed_labels.list;

		
		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			
			procedure query_strands (
				net_name	: in pac_net_name.bounded_string;
				net			: in type_net)
			is
				strand_cursor : pac_strands.cursor := net.strands.first;

				procedure query_segments (strand : in type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					procedure query_labels (segment : in type_net_segment) is
						use pac_net_labels;
						label_cursor : pac_net_labels.cursor := segment.labels.first;

						-- Test distance between label and given place.
						-- Appends the label if distance less or equal catch zone:
						procedure test_distance is 
							d : constant type_float_model := get_distance_absolute (
								point_one	=> element (label_cursor).position,
								point_two	=> place.place);
						begin
							-- If the label position is within the catch zone, append
							-- the current net, stand, segment and label cursor to the result:
							if d <= zone then

								result.append ((net_cursor, strand_cursor, segment_cursor, label_cursor));
								
								log (text => space 
									& to_string (element (label_cursor).appearance) 
									& " label found at " 
									& to_string (element (label_cursor).position),
									level => log_threshold + 2);
							end if;
						end test_distance;

						
					begin -- query_labels
						while label_cursor /= pac_net_labels.no_element loop

							case category is
								when BOTH => 
									-- Label category does not matter. Test all kinds of labels:
									test_distance;

								when SIMPLE =>
									-- Test only simple labels:
									if element (label_cursor).appearance = SIMPLE then
										test_distance;
									end if;

								when TAG =>
									-- Test only tag labels:
									if element (label_cursor).appearance = TAG then
										test_distance;
									end if;
									
							end case;
							
							next (label_cursor);
						end loop;
					end query_labels;
					
				begin -- query_segments
					log (text => "probing strand at " & to_string (strand.position),
						 level => log_threshold + 1);
					
					log_indentation_up;
					
					while segment_cursor /= pac_net_segments.no_element loop
						log (text => "probing segment" & to_string (element (segment_cursor)),
							level => log_threshold + 1);

						query_element (segment_cursor, query_labels'access);

						next (segment_cursor);
					end loop;

					log_indentation_down;
				end query_segments;
				
			begin -- query_strands
				while strand_cursor /= pac_strands.no_element loop

					-- We are interested in strands on the given sheet only:
					if get_sheet (element (strand_cursor).position) = get_sheet (place) then
						query_element (strand_cursor, query_segments'access);
					end if;

					next (strand_cursor);
				end loop;
			end query_strands;
			
		begin -- query_nets
			while net_cursor /= pac_nets.no_element loop

				query_element (
					position	=> net_cursor,
					process		=> query_strands'access);

				next (net_cursor);
			end loop;
		end query_nets;

		
	begin -- collect_labels
		log (text => "looking up net labels" 
			 & to_string (set_catch_zone (place.place, zone)),
			 level => log_threshold);
		-- CS output category of label
		
		log_indentation_up;
		
		query_element (
			position	=> module,
			process		=> query_nets'access);

		log_indentation_down;
		
		return result;		
	end collect_labels;

	
	

	
	procedure delete_selected_label (
		module_cursor	: in pac_generic_modules.cursor;
		label			: in type_selected_label;
		log_threshold	: in type_log_level)
	is 
		use pac_net_labels;
		label_position_sheet : constant type_sheet := get_sheet (element (label.strand).position);
		
		-- This flag goes true once the first net label that equals the given label
		-- has been found t. All iterations are cancelled as soon as it goes true.
		label_found : boolean := false;
		
		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) is

			net_cursor : pac_nets.cursor := module.nets.first;

			procedure query_strands (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) is

				strand_cursor : pac_strands.cursor := net.strands.first;
				
				procedure query_segments (strand : in out type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					procedure query_labels (segment : in out type_net_segment) is 
						c : pac_net_labels.cursor := segment.labels.first;
					begin
						while c /= pac_net_labels.no_element loop

							-- If label found, delete it from the label list
							-- of that segment:
							if element (c) = element (label.label) then
								delete (segment.labels, c);
								
								label_found := true; -- aborts all iteration
								exit; -- no further search required
							end if;

							next (c);
						end loop;
					end query_labels;
					
				begin -- query_segments
					while not label_found and segment_cursor /= pac_net_segments.no_element loop

						update_element (
							container	=> strand.segments,
							position	=> segment_cursor,
							process		=> query_labels'access);
						
						next (segment_cursor);
					end loop;
				end query_segments;
				
			begin -- query_strands
				while not label_found and strand_cursor /= pac_strands.no_element loop
					
					-- We pick out only the strands on the targeted sheet:
					if get_sheet (element (strand_cursor).position) = label_position_sheet then

						update_element (
							container	=> net.strands,
							position	=> strand_cursor,
							process		=> query_segments'access);
					
					end if;
					
					next (strand_cursor);
				end loop;
			end query_strands;
			
		begin -- query_nets
			while not label_found and net_cursor /= pac_nets.no_element loop

				update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);

				next (net_cursor);
			end loop;
		end query_nets;

	begin -- delete_selected_label

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_nets'access);

		reset_label;

		reset_request_clarification;

		set_status (status_delete_label);
		
	end delete_selected_label;


	
	
	function to_string (cat : in type_label_category) return string is 
		use ada.characters.handling;
	begin
		return to_lower (type_label_category'image (cat));
	end to_string;


	
	
	procedure delete_selected_label is begin
		log (text => "deleting net label after clarification ...", level => log_threshold);
		log_indentation_up;

		delete_selected_label (
			module_cursor	=> active_module,
			label			=> element (selected_label),
			log_threshold	=> log_threshold + 1);
		
		log_indentation_down;
	end delete_selected_label;



	
	
	procedure delete_label (point : in type_vector_model) is begin
		log (text => "deleting net label ...", level => log_threshold);
		log_indentation_up;
		
		-- Collect all net labels in the vicinity of the given point:
		proposed_labels := collect_labels (
			module			=> active_module,
			place			=> to_position (point, active_sheet),
			zone			=> get_catch_zone (catch_zone_radius_default),
			log_threshold	=> log_threshold + 1);

		
		-- evaluate the number of lables found here:
		case length (proposed_labels) is
			when 0 =>
				reset_request_clarification;
				
			when 1 =>
				delete_selected_label (
					module_cursor	=> active_module,
					label			=> element (proposed_labels.first),
					log_threshold	=> log_threshold + 1);
					
			when others =>
				set_request_clarification;

				-- preselect the first label
				selected_label := proposed_labels.first;
		end case;
		
		log_indentation_down;
	end delete_label;

	

	procedure clarify_label is
		use pac_net_labels;

		s : pac_net_segments.cursor;
		n : pac_net_name.bounded_string;

		function info (c : in pac_net_labels.cursor) return string is 
			l : type_net_label := element (c);
		begin
			return "label at" & to_string (l.position); -- CS other properties like appearance, direction ?
		end info;
		
	begin
		-- On every call of this procedure we must advance from one
		-- label to the next in a circular manner. So if the end 
		-- of the list is reached, then the cursor selected_label
		-- moves back to the start of the label list.
		if next (selected_label) /= pac_proposed_labels.no_element then
			next (selected_label);
		else
			selected_label := proposed_labels.first;
		end if;

		-- get the segment of the net
		s := element (selected_label).segment;
		
		-- get the name of the net
		n := key (element (selected_label).net);
		
		set_status ("net " & to_string (n) & space 
			--& to_string (s) 
			& info (element (selected_label).label)
			& ". " & status_next_object_clarification);
		
	end clarify_label;



	
	
	procedure place_label (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_selected_segment;
		destination		: in type_vector_model; -- x/y
		appearance 		: in type_net_label_appearance; -- simple/tag
		log_threshold	: in type_log_level) 
	is

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			procedure query_strands (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is

				procedure query_segments (strand : in out type_strand) is

					procedure attach_label (segment : in out type_net_segment) is 
						use pac_net_labels;
						l : type_net_label_base;
					begin
						-- The label position is absolute:
						l.position := destination;

						case appearance is
							when SIMPLE =>
								append (
									container	=> segment.labels,
									new_item	=> (l with
										appearance		=> SIMPLE,
										rotation_simple	=> label.rotation_simple)
									   );
								
							when TAG =>
								-- A tag label can be attached to a stub only.
								declare
									s : constant type_stub := query_stub (
										module_cursor	=> module_cursor,
										net_name		=> net_name,
										position		=> to_position (type_vector_model (destination), active_sheet),
										log_threshold	=> log_threshold + 1);

									-- CS use a function query_stub that take a module cursor and
									-- a net cursor instead.
								begin
									append (
										container	=> segment.labels,
										new_item	=> (l with 
											appearance		=> TAG,

											-- derive the label rotation from the stub direction:
											rotation_tag	=> to_label_rotation (s.direction),
														
											--direction		=> direction) -- the given signal direction
											others			=> <>)
										   );
										
								end;
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




	
	procedure finalize_place_label (
		destination		: in type_vector_model;
		log_threshold	: in type_log_level) is
	begin
		log (text => "finalizing place net label ...", level => log_threshold);
		log_indentation_up;

		-- A tag label can be placed once permission is granted by 
		-- procedure et_canvas_schematic.draw_nets.
		-- A simple label does not require permission and can be placed anywhere:
		if (label.appearance = TAG and boolean (label.finalizing_granted))
		or label.appearance = SIMPLE 
		then
			
			if selected_segment /= pac_proposed_segments.no_element then

				place_label (
					module_cursor		=> active_module,
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

	end finalize_place_label;



	
	procedure place_label (
		tool		: in type_tool;
		position	: in type_vector_model)
	is begin
		if not label.ready then
			
			-- Set the tool being used:
			label.tool := tool;
			
			if not clarification_pending then
				find_segments (position);
			else
				label.ready := true;
				reset_request_clarification;
			end if;
			
		else
			-- Finally place the label at the current
			-- cursor position:
			finalize_place_label (
				destination		=> position,
				log_threshold	=> log_threshold + 1);
		end if;
	end place_label;




	
	procedure find_labels (
		point		: in type_vector_model;
		category	: in type_label_category)
	is begin
		log (text => "locating net labels ...", level => log_threshold);
		-- CS output category if simple or tag.
		
		log_indentation_up;

		-- Collect all net labels in the vicinity of the given point:
		proposed_labels := collect_labels (
			module			=> active_module,
			place			=> to_position (point, active_sheet),
			zone			=> get_catch_zone (catch_zone_radius_default),
			category		=> category,
			log_threshold	=> log_threshold + 1);

		
		-- evaluate the number of lables found here:
		case length (proposed_labels) is
			when 0 =>
				reset_request_clarification;
				
			when 1 =>
				label.ready := true;
				selected_label := proposed_labels.first;
				
				set_status (status_move_label);
				
				reset_request_clarification;
				
			when others =>
				set_request_clarification;

				-- preselect the first label
				selected_label := proposed_labels.first;
		end case;
		
		log_indentation_down;
	end find_labels;



	
	
	procedure move_selected_label (
		module_cursor	: in pac_generic_modules.cursor;
		label			: in type_selected_label;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			procedure query_strands (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is
				procedure query_segments (strand : in out type_strand) is

					procedure query_labels (segment : in out type_net_segment) is
						use pac_net_labels;
						
						procedure move (label : in out type_net_label) is begin
							label.position := destination;
						end move;
						
					begin
						update_element (
							container	=> segment.labels,
							position	=> label.label,
							process		=> move'access);
						
					end query_labels;
					
				begin
					update_element (
						container	=> strand.segments,
						position	=> label.segment,
						process		=> query_labels'access);
					
				end query_segments;
				
			begin
				update_element (
					container	=> net.strands,
					position	=> label.strand,
					process		=> query_segments'access);
				
			end query_strands;
			
		begin
			update_element (
				container	=> module.nets,
				position	=> label.net,
				process		=> query_strands'access);

		end query_nets;
		
	begin -- move_selected_label
		log (text => "module " & enclose_in_quotes (to_string (key (module_cursor))) 
			 & " moving net label to"
			 & to_string (destination) & " ...",
			 level => log_threshold);

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_nets'access);
		
		log_indentation_down;				

	end move_selected_label;




	
	procedure finalize_move_label (
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is
		sl : type_selected_label;
	begin
		log (text => "finalizing move net label ...", level => log_threshold);
		log_indentation_up;

		if selected_label /= pac_proposed_labels.no_element then
			sl := element (selected_label);

			move_selected_label (active_module, sl, destination, log_threshold + 1);
								
		else
			log (text => "nothing to do", level => log_threshold);
		end if;
			
		log_indentation_down;

		set_status (status_move_label);
		
		reset_label;
	end finalize_move_label;




	
	procedure move_label (
		tool		: in type_tool;
		position	: in type_vector_model)
	is begin
		if not label.ready then

			-- Set the tool being used:
			label.tool := tool;

			if not clarification_pending then
				find_labels (position, SIMPLE);
			else
				label.ready := true;
				reset_request_clarification;
			end if;
			
		else
			-- Finally assign the cursor position to the
			-- currently selected net label:
			finalize_move_label (
				destination		=> position,
				log_threshold	=> log_threshold + 1);

		end if;
	end move_label;



	
	
	procedure show_properties_of_selected_net is
		ss	: constant type_selected_segment := element (selected_segment);
		use et_net_class;
		use et_netlists;
	begin
		reset_request_clarification;
		
		set_status ("Properties:"
			& " name " & to_string (key (ss.net))
			& ", class " & to_string (element (ss.net).class)
			& ", scope " & to_string (element (ss.net).scope)
			);
		
	end show_properties_of_selected_net;


	
end et_canvas_schematic_nets;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
