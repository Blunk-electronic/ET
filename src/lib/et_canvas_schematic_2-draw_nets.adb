------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         SCHEMATIC DRAW NETS                              --
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

with gnat;
with gnat.exception_traces;

with ada.exceptions;
with ada.containers;						use ada.containers;
with ada.containers.doubly_linked_lists;

with et_text;								use et_text;
with et_alignment;
with et_nets;								use et_nets;
with et_net_segment;						use et_net_segment;
with et_net_labels;							use et_net_labels;
with et_net_names;							use et_net_names;


with et_schematic_ops.nets;


separate (et_canvas_schematic_2)

procedure draw_nets is
	use et_canvas_schematic_nets;
	use pac_nets;
	use pac_strands;
	use pac_net_segments;
	use pac_net_labels;
	
	use pac_proposed_segments;
	use pac_proposed_labels;

	
	-- Draws the junctions of a segment:
	procedure draw_junctions (
		s : in pac_net_segments.cursor)
	is
		j : type_circle := junction_symbol;

		procedure draw is begin
			draw_circle (
				circle		=> type_circle (j),
				pos			=> origin_zero_rotation,
				filled		=> YES,
				width		=> zero,
				do_stroke	=> true);
		end draw;
		
	begin
		-- at start point of segment:
		if element (s).junctions.A then
			set_center (j, element (s).A);
			draw;
		end if;

		-- at end point of segment:
		if element (s).junctions.B then
			set_center (j, element (s).B);
			draw;
		end if;

	end draw_junctions;


	
	-- Draws the junctions of a segment:
	procedure draw_junctions (
		s : in type_net_segment)
	is
		j : type_circle := junction_symbol;

		procedure draw is begin
			draw_circle (
				circle		=> type_circle (j),
				pos			=> origin_zero_rotation,
				filled		=> YES,
				width		=> zero,
				do_stroke	=> true);
		end draw;
		
	begin
		-- at start point of segment:
		if s.junctions.A then
			set_center (j, s.A);
			draw;
		end if;

		-- at end point of segment:
		if s.junctions.B then
			set_center (j, s.B);
			draw;
		end if;

	end draw_junctions;



	-- This procedure draws a tag label:
	procedure draw_tag_label (
		net		: in pac_net_name.bounded_string;
		label	: in type_net_label)
	is
		use pac_net_name;
		use et_alignment;

		box : type_area;
		
		content : pac_text_content.bounded_string := to_content (to_string (net));
		-- CS append to content the position of the net on the next sheet (strand position)
		-- using the quadrant bars.
		
		
		-- The text rotation must be either 0 or 90 degree (documentational text) and is thus
		-- to be calculated according to the rotation of the label:
		text_rotation : type_rotation;

		-- The alignment is assigned as if the text were drawn at zero rotation.
		-- The vertical alignment is always CENTER. Horizontal alignment changes depending on 
		-- the rotation of the label:
		text_alignment : type_text_alignment := (vertical => ALIGN_CENTER, horizontal => <>);

		-- The text position is not the same as the label position, thus it must be 
		-- calculated according to the label rotation and tag_label_text_offset:
		text_position : type_vector_model;

		use pac_draw_text;
	begin
		-- Form a box that wraps around the net name:
		box := to_area (get_text_extents (content, label.size, net_label_font));

		-- Expand the box so that there is some empty space between
		-- text and border:
		box.height := box.height * tag_label_height_to_size_ratio;
		box.width  := box.width  * tag_label_height_to_size_ratio;
		
		if label.rotation_tag = zero_rotation then
	
			box.position := set (
				get_x (label.position), 
				get_y (label.position) - box.height * 0.5);

			text_rotation := zero_rotation;
			text_position := set (get_x (label.position) + tag_label_text_offset, get_y (label.position));
			text_alignment.horizontal := ALIGN_LEFT;
		end if;

		
		if label.rotation_tag = 90.0 then

			box.position := set (
				get_x (label.position) - box.height * 0.5,
				get_y (label.position));

			swap_edges (box);

			text_rotation := 90.0;
			text_position := set (get_x (label.position), get_y (label.position) + tag_label_text_offset);
			text_alignment.horizontal := ALIGN_LEFT;
		end if;

		
		if label.rotation_tag = 180.0 then

			box.position := set (
				get_x (label.position) - box.width,
				get_y (label.position) - box.height * 0.5);

			text_rotation := zero_rotation;
			text_position := set (get_x (label.position) - tag_label_text_offset, get_y (label.position));
			text_alignment.horizontal := ALIGN_RIGHT;
		end if;

		
		if label.rotation_tag = -90.0 then

			box.position := set (
				get_x (label.position) - box.height * 0.5,
				get_y (label.position) - box.width);
			
			swap_edges (box);

			text_rotation := 90.0;
			text_position := set (get_x (label.position), get_y (label.position) - tag_label_text_offset);
			text_alignment.horizontal := ALIGN_RIGHT;
		end if;


		-- Draw the box enshrouding the net name:
		draw_rectangle (
			rectangle	=> box, 
			width		=> tag_label_box_line_width);

		-- Draw the actual net name:
		draw_text (
			content		=> content,
			size		=> label.size,
			font		=> net_label_font,
			anchor		=> text_position,
			origin		=> false, -- no origin for net names required
			
			-- Text rotation about its anchor point. This is documentational text.
			-- It is readable from the front or the right.
			rotation	=> text_rotation,
			alignment	=> text_alignment);

	end draw_tag_label;

	

	-- Returns true if the given label is selected.
	-- Returns false if there are no proposed labels or
	-- if the given label is not selected.
	function is_selected (
		net		: in pac_nets.cursor;
		strand	: in pac_strands.cursor;
		segment	: in type_net_segment;
		label	: in pac_net_labels.cursor)
		return boolean 
	is
		use pac_net_name;
		sl : type_selected_label;
	begin
		if is_empty (proposed_labels) then
			return false;
		else
			if selected_label /= pac_proposed_labels.no_element then

				sl := element (selected_label);
				
				if key (net) = key (sl.net)
				and element (strand) = element (sl.strand)
				and segment = element (sl.segment)
				and element (label) = element (sl.label)
				then
					return true;
				else
					return false;
				end if;
			else
				return false;
			end if;
		end if;		
	end is_selected;


	
	-- Draws a single net label:
	procedure draw_label (
		net		: in pac_net_name.bounded_string;
		label	: in pac_net_labels.cursor)
	is 
		use pac_net_name;
		use pac_draw_text;
		use et_alignment;
	begin
		case element (label).appearance is
			when SIMPLE =>
				draw_text (
					content		=> to_content (to_string (net)),
					size		=> element (label).size,
					font		=> net_label_font,
					anchor		=> element (label).position,
					origin		=> true, -- CS must be false on export to image
					
					-- Text rotation about its anchor point.
					-- This is documentational text.
					-- It is readable from the front or the right.
					rotation	=> pac_text.to_rotation (element (label).rotation_simple),
					alignment	=> net_label_alignment);

			when TAG =>
				draw_tag_label (net, element (label));

		end case;
	end draw_label;


	
	-- Draws a single net label that is being moved:
	procedure draw_simple_label_being_moved (
		net		: in pac_net_name.bounded_string;
		label	: in type_net_label)
	is 
		use pac_net_name;
		use pac_draw_text;
	begin
		--case element (label).appearance is
			--when SIMPLE =>
				draw_text (
					content		=> to_content (to_string (net)),
					size		=> label.size,
					font		=> net_label_font,
					anchor		=> label.position,
					origin		=> true, -- CS must be false on export to image
					
					-- Text rotation about its anchor point.
					-- This is documentational text.
					-- It is readable from the front or the right.
					rotation	=> pac_text.to_rotation (label.rotation_simple),
					alignment	=> net_label_alignment);

			--when TAG =>
				--draw_tag_label (self, in_area, context, net, element (label));

		--end case;
	end draw_simple_label_being_moved;


	
	-- Draws labels that are NOT selected:
	procedure draw_labels (
		net		: in pac_nets.cursor;
		strand	: in pac_strands.cursor;
		segment	: in type_net_segment)
	is 
		procedure draw_fixed (label : in pac_net_labels.cursor) is begin
			if not is_selected (net, strand, segment, label) then
				draw_label (key (net), label);
			end if;
		end draw_fixed;
		
	begin
		iterate (segment.labels, draw_fixed'access);
	end draw_labels;

	
	
	-- Draws the net label being moved. If no net label
	-- is being moved, nothing happens here:
	procedure draw_label_being_moved is
		use et_modes.schematic;
		use pac_net_name;
		l : type_net_label (label.appearance);

		use pac_draw_text;
	begin
		case verb is
			when VERB_PLACE =>
				if label.ready then

					case label.tool is
						when KEYBOARD	=> l.position := get_cursor_position;
						when MOUSE		=> l.position := snap_to_grid (get_mouse_position);
					end case;
					
					case label.appearance is
						when SIMPLE =>

							--l.rotation_simple := label.rotation_simple;
							
							draw_text (
								content		=> to_content (to_string (selected_net)),
								size		=> l.size,
								font		=> net_label_font,
								anchor		=> l.position,
								origin		=> true, -- CS must be false on export to image
								
								-- Text rotation about its anchor point.
								-- This is documentational text.
								-- It is readable from the front or the right.
								rotation	=> pac_text.to_rotation (label.rotation_simple),
								alignment	=> net_label_alignment);
							

						when TAG =>

							-- The current position must be tested whether it qualifies
							-- for a tag label. If the position is suitable then the
							-- flag label.finalizing_granted is set so that 
							-- procedure finalize_place_label is allowed to do the final
							-- placement of the label:
							declare
								use et_schematic_ops.nets;
								s : constant type_stub := query_stub (
										module_cursor	=> active_module,
										net_name		=> selected_net,
										position		=> to_position (type_vector_model (l.position), active_sheet),
										log_threshold	=> log_threshold + 1);

									-- CS use a function query_stub that take a module cursor and
									-- a net cursor instead.

							begin
								if s.is_stub then
									-- Set the rotation of the label according to the 
									-- direction of the stub:
									l.rotation_tag := to_label_rotation (s.direction);

									label.finalizing_granted := true;
								else
									label.finalizing_granted := false;
								end if;

								draw_tag_label (selected_net, l);
							end;
							
					end case;
				end if;

			when others => null;
		end case;

	end draw_label_being_moved;


	
	
	-- Draws the net label as indicated by variable selected_label:
	procedure draw_selected_label (
		net		: in pac_nets.cursor;
		strand	: in pac_strands.cursor;
		segment	: in pac_net_segments.cursor)
	is
		procedure query_label (s : in type_net_segment) is
			label_cursor : pac_net_labels.cursor := s.labels.first;

			sl : type_net_label (SIMPLE);
		begin
			while label_cursor /= pac_net_labels.no_element loop

				if is_selected (net, strand, element (segment), label_cursor) then

					if label.ready then
						-- Draw a copy of the label. Assign position
						-- according to tool:
						sl := element (label_cursor);
						
						case label.tool is
							when KEYBOARD =>
								sl.position := get_cursor_position;
							when MOUSE =>
								sl.position := snap_to_grid (get_mouse_position);
						end case;

						-- draw the temporarily label
						draw_simple_label_being_moved (key (net), sl);
					else
						-- draw label as it is according to module database:
						draw_label (key (net), label_cursor);
					end if;
														  
					exit; -- there is only on selected label. no further search required
				end if;

				next (label_cursor);
			end loop;
		end query_label;

		
	begin
		if not is_empty (proposed_labels) then
			query_element (segment, query_label'access);
		end if;
	end draw_selected_label;

	
	
	
	-- Returns true if the given segment is selected.
	-- Returns false if there are no proposed segments or
	-- if the given segment is not selected.
	function is_selected (
		net		: in pac_nets.cursor;
		strand	: in pac_strands.cursor;
		segment	: in pac_net_segments.cursor)
		return boolean
	is
		use pac_net_name;
		ss : type_selected_segment;
	begin
		if is_empty (proposed_segments) then
			return false;
		else
			if selected_segment /= pac_proposed_segments.no_element then
				ss := element (selected_segment);

				if key (ss.net) = key (net)
				and element (ss.strand) = element (strand)
				and element (ss.segment) = element (segment) then
					return true;
				else
					return false;
				end if;
			else
				return false;
			end if;
		end if;		
	end is_selected;


	
	-- We need a list of segments that have been drawn already.
	-- Reason: While draggin/moving segments temporarily segments are drawn.
	--         During this time the original segments (as given in database)
	--         must not be drawn.
	package pac_already_drawn_segments is new doubly_linked_lists (type_net_segment);
	use pac_already_drawn_segments;
	-- This list keeps record of already drawn segments:
	already_drawn_segments : pac_already_drawn_segments.list;

	
	
	-- Draws the given net segment as it is according to module database
	-- if it has not already been drawn.
	-- Draws also possible junctions that may exist at start or end point
	-- of the segment.
	procedure draw_fixed_segment (
		s : in pac_net_segments.cursor) 
	is begin
		if not contains (already_drawn_segments, element (s)) then
			
			draw_line (
				line		=> element (s),
				pos			=> origin_zero_rotation,		  
				width		=> net_line_width,
				do_stroke	=> true);

			draw_junctions (s);
			
		end if;
	end draw_fixed_segment;


	
	-- Draws a net segment.
	-- Draws also possible junctions that may exist at start or end point
	-- of the segment.
	procedure draw_preliminary_segment (
		net		: in pac_nets.cursor;
		strand	: in pac_strands.cursor;
		segment : in type_net_segment) 
	is begin
		draw_line (
			line		=> segment,
			pos			=> origin_zero_rotation,		  
			width		=> net_line_width,
			do_stroke	=> true);

		draw_junctions (segment);

		draw_labels (net, strand, segment);
	end draw_preliminary_segment;

	
	
	-- Draws secondary nets which are attached to the primary net.
	procedure draw_secondary_segments (
		net_cursor			: in pac_nets.cursor;
		strand_cursor		: in pac_strands.cursor;

		-- The segment as it is according to database (before the drag)										  
		original_segment	: in pac_net_segments.cursor;

		-- The primary segment being drawn:
		primary_segment		: in type_net_segment;

		-- the zone being dragged:
		zone				: in type_line_zone)
	is
		
		procedure query_segment (c : in pac_net_segments.cursor) is
			secondary_segment : type_net_segment;

			-- Draw the secondary segment and mark it as drawn.
			-- It must be marked as drawn so that procedure query_nets
			-- does not draw it in its inital state according to the database.
			procedure draw_and_mark is begin
				draw_preliminary_segment (net_cursor, strand_cursor, secondary_segment);

				-- mark segment as already drawn
				already_drawn_segments.append (element (c));
			end draw_and_mark;

			
			procedure drag_at_start is begin
				if element (original_segment).A = secondary_segment.A then
				-- Start point of secondary segment is attached to the start point of the original segment.
					secondary_segment.A := primary_segment.A;

					draw_and_mark;
				end if;

				if element (original_segment).A = secondary_segment.B then
				-- end point of secondary net segment is attached to the start point of the original segment
					secondary_segment.B := primary_segment.A;

					draw_and_mark;
				end if;
			end drag_at_start;

			
			procedure drag_at_end is begin
				if element (original_segment).B = secondary_segment.A then
				-- Start point of secondary segment is attached to the end point of the original segment.
					secondary_segment.A := primary_segment.B;

					draw_and_mark;
				end if;

				if element (original_segment).B = secondary_segment.B then
				-- end point of secondary segment is attached to the end point of the original segment
					secondary_segment.B := primary_segment.B;

					draw_and_mark;
				end if;
			end drag_at_end;

			
		begin -- query_segment
			
			-- Skip original segment. It has been drawn already by caller:
			if element (c) /= element (original_segment) then

				-- Take a copy of the current segment.
				-- From now on we call it "secondary segment".
				-- Depending on the zone we are dragging at (in the original segment),
				-- the start or/and end point of the secondary segment will be overwritten.
				secondary_segment := element (c);
				
				case zone is -- the zone of the original segment we are dragging at
					when START_POINT => 
						drag_at_start;
						
					when END_POINT =>
						drag_at_end;
		
					when CENTER =>
						drag_at_start;
						drag_at_end;
				end case;
				
			end if;
		end query_segment;
		

	begin
		-- Iterate all segments of the given strand.
		-- Skip the original segment:
		pac_net_segments.iterate (
			container	=> element (strand_cursor).segments,
			process		=> query_segment'access);
		
	end draw_secondary_segments;


	
	-- Draws the net segment being moved or dragged.
	-- If we are dragging a segment, then other attached segments
	-- will be dragged along.
	-- If we are moving a single segment, then only the current segment
	-- will be moved:
	-- NOTE: The given original net segment (via cursor) is the segment
	-- as given by the module database.
	procedure draw_moving_segments (
		net_cursor			: in pac_nets.cursor;
		strand_cursor		: in pac_strands.cursor;
		original_segment	: in pac_net_segments.cursor) 
	is
		use et_modes.schematic;
		use et_schematic_ops.nets;


		-- Calculate the zone where the original segment is being attacked:
		zone : constant type_line_zone := get_zone (
				point	=> object_point_of_attack,
				line	=> element (original_segment));

		destination : type_vector_model;
		primary_segment : type_net_segment;

		
		-- Moves net labels of the primary segment.
		-- Draws the primary segment.
		-- Draws secondary segments.
		-- Signals that the finalizing is granted (see et_canvas_schematic_nets.finalize_drag).
		procedure move_labels_and_secondary_nets is begin
			move_net_labels (
				segment_before	=> element (original_segment),
				segment_after	=> primary_segment,
				zone			=> zone);
			
			-- Draw the primary segment in its temporarily state:
			draw_preliminary_segment (net_cursor, strand_cursor, primary_segment);

			-- Drawing attached secondary segments requires the original
			-- net segment (before the drag operation):
			draw_secondary_segments (
				net_cursor			=> net_cursor,
				strand_cursor		=> strand_cursor,
				original_segment	=> original_segment,
				primary_segment		=> primary_segment,
				zone				=> zone);
			
			set_finalizing_granted;
		end move_labels_and_secondary_nets;

		
	begin -- draw_moving_segments
		
		-- First test whether the original segment is movable at all.
		-- Ports connected with the segment may prohibit moving the segment
		-- as they belong to symbols which are not dragged along.
		-- If the segment is not movable then it will be drawn as given by
		-- the module database.
		if is_movable (
			module_cursor	=> active_module,
			segment			=> element (original_segment),
			zone			=> zone,
			point_of_attack	=> to_position (object_point_of_attack, active_sheet),
			log_threshold	=> log_threshold + 10) -- CS: avoids excessive log information. find a more elegant way.
		then
			-- segment is movable

			-- Take a copy of the original net segment.
			-- We call this copy from now on "primary segment" because other
			-- segments (secondary segments) could be connected with it.
			primary_segment := element (original_segment);

			-- calculate the destination point according to the current drawing tool:
			case object_tool is
				when MOUSE =>
					destination := snap_to_grid (get_mouse_position);

				when KEYBOARD =>
					destination := get_cursor_position;
			end case;

		
			case verb is
				when VERB_DRAG =>
					attack (primary_segment, object_point_of_attack, destination);
					move_labels_and_secondary_nets;

				when others => null;
			end case;
		else
			-- Not movable. Draw as given in database:
			draw_fixed_segment (original_segment);
			draw_labels (net_cursor, strand_cursor, element (original_segment));
		end if;
	end draw_moving_segments;


	

	-- Draws the segments attached to a unit being dragged.
	-- If the list segments_being_dragged is empty, nothing happens.
	procedure draw_segment_being_dragged_along_with_unit (
		s : in pac_net_segments.cursor) -- the original segment as given in database
	is
		tool_position : type_vector_model;
		displacement : type_distance_relative;

		use et_canvas_schematic_units;
		use pac_segments_being_dragged;

		-- Tests if the segment being dragged is the same as the given segment.
		-- If the segments are identical, then a copy of the original segment is
		-- taken. According to the drag zone, the start or end point of this copy
		-- is moved by the calculated displacement.
		-- Finally the segment is marked as drawn so that is won't be drawn anew by
		-- the callers.
		procedure query_segment (g : in pac_segments_being_dragged.cursor) is
			copy_of_original_segment : type_net_segment;
		begin
			if element (g).segment = element (s) then
				--log (text => "segment" & to_string (element (g).segment), console => true);

				copy_of_original_segment := element (s);

				case element (g).zone is
					when START_POINT =>
						move_by (copy_of_original_segment.A, displacement);
					
					when END_POINT =>
						move_by (copy_of_original_segment.B, displacement);
				end case;

				draw_line (
					line		=> copy_of_original_segment,
					pos			=> origin_zero_rotation,		  
					width		=> net_line_width,
					do_stroke	=> true);

				-- mark segment as already drawn
				already_drawn_segments.append (element (s));
				
			end if;
		end query_segment;

		
	begin -- draw_segment_being_dragged_along_with_unit
		if not is_empty (segments_being_dragged) then

			-- Calculate the displacement of segments according to the
			-- current drawing tool and the current displacement of the unit:
			case object_tool is
				when MOUSE =>
					tool_position := snap_to_grid (get_mouse_position);

				when KEYBOARD =>
					tool_position := get_cursor_position;
			end case;

			-- This is the displacement of the attached segments:
			displacement := get_distance_relative (object_original_position, tool_position);

-- 			log (text => "original    " & to_string (unit.original_position), console => true);
-- 			log (text => "displacement" & to_string (displacement), console => true);
-- 			log (text => "count       " & count_type'image (length (segments_being_dragged)), console => true);
			
			segments_being_dragged.iterate (query_segment'access);
		end if;
	end draw_segment_being_dragged_along_with_unit;


	
	procedure query_nets (
		module_name	: in pac_module_name.bounded_string;
		module		: in type_generic_module) 
	is
		use et_colors;
		use et_colors.schematic;

		use et_modes.schematic;

		-- This cursor points to the current net being drawn:
		net_cursor : pac_nets.cursor := module.nets.first;

		
		-- Draws the strands of the given net in "normal" mode.
		-- "Normal" mode means, the whole net is not to be drawn highlighted.
		-- This is the case when the verb VERB_SHOW is not active.
		procedure query_strands_normal (
			net_name	: in pac_net_name.bounded_string;
			net			: in type_net) 
		is
			strand_cursor : pac_strands.cursor := net.strands.first;

			
			procedure query_segments (strand : in type_strand) is
				segment_cursor : pac_net_segments.cursor := strand.segments.first;
			begin
				-- draw nets of the active sheet only:
				if get_sheet (strand.position) = active_sheet then

					-- First we draw selected segments or those being moved/dragged:
					set_color_nets (BRIGHT);
					
					while segment_cursor /= pac_net_segments.no_element loop

						-- CS test verb and noun ?
						draw_segment_being_dragged_along_with_unit (segment_cursor);
						-- (If segments_being_dragged is empty, then nothing happens.)
						
						-- CS test verb and noun ?						
						if is_selected (net_cursor, strand_cursor, segment_cursor) then
						
							if edit_process_running then
								-- Draw the net segments being moved or dragged.
								-- If we are dragging a segment, then other attached segments
								-- will be dragged along.
								-- If we are moving a single segment, then only the current segment
								-- will be moved.
								draw_moving_segments (net_cursor, strand_cursor, segment_cursor);
							else
								-- Draw the net segment as it is according to module database:
								draw_fixed_segment (segment_cursor);

								draw_labels (net_cursor, strand_cursor, element (segment_cursor));

								-- Draw the net label being moved. If no net label
								-- is being moved, nothing happens here:
								draw_label_being_moved;
							end if;
						end if;

						-- Draw selected label. If no label selected, nothing happens here:
						draw_selected_label (net_cursor, strand_cursor, segment_cursor);
						
						next (segment_cursor);
					end loop;

					
					-- Now we draw the remaining segments:
					set_color_nets (NORMAL);
					segment_cursor := strand.segments.first;
					
					while segment_cursor /= pac_net_segments.no_element loop

						if not is_selected (net_cursor, strand_cursor, segment_cursor) then

							-- Draw the net segment as it is according to module database:
							draw_fixed_segment (segment_cursor);

							draw_labels (net_cursor, strand_cursor, element (segment_cursor));
						end if;

						next (segment_cursor);
					end loop;

				end if;
			end query_segments;
			
			
		begin -- query_strands_normal
			while strand_cursor /= pac_strands.no_element loop

				query_element (
					position	=> strand_cursor,
					process		=> query_segments'access);
				
				next (strand_cursor);
			end loop;
		end query_strands_normal;

		
		-- Draws the whole net inclusive net labesl highlighted. 
		-- This is the case when the verb VERB_SHOW is active.
		procedure query_strands_show (
			net_name	: in pac_net_name.bounded_string;
			net			: in type_net)
		is
			strand_cursor : pac_strands.cursor := net.strands.first;

			procedure query_segments (strand : in type_strand) is
				segment_cursor : pac_net_segments.cursor := strand.segments.first;

				use et_colors;
				use et_colors.schematic;
			begin
				-- draw nets of the active sheet only:
				if get_sheet (strand.position) = active_sheet then

					set_color_nets (BRIGHT);
					
					while segment_cursor /= pac_net_segments.no_element loop

						-- Draw the net segment as it is according to module database:
						draw_fixed_segment (segment_cursor);

						draw_labels (net_cursor, strand_cursor, element (segment_cursor));
						
						next (segment_cursor);
					end loop;

				end if;
			end query_segments;

			
		begin -- query_strands_show
			while strand_cursor /= pac_strands.no_element loop

				query_element (
					position	=> strand_cursor,
					process		=> query_segments'access);
				
				next (strand_cursor);
			end loop;
		end query_strands_show;

		
		-- A net can be drawn in "normal" mode or highlighted. This flag indicates
		-- that a net has alredy been drawn highlighted so that it won't be drawn
		-- again in "normal" mode.
		net_already_drawn : boolean := false;

		
		-- This procedure calls query_strands_show if the current net (indicated by net_cursor)
		-- is selected for highlighting. It sets the flag net_already_drawn in that case.
		procedure highlight_net is
			use pac_net_name;
			ss : type_selected_segment;
		begin
			-- The net selected for highlighting is to be found in the list proposed_segments.
			-- The last element of the proposed_segments points to the net that is to 
			-- be drawn highlighted.
			-- So the list of proposed_segments must contain something:
			if not is_empty (proposed_segments) then

				-- There must be a selected segment (indicated by cursor selected_segment):
				if selected_segment /= pac_proposed_segments.no_element then
					ss := element (selected_segment);

					-- The selected_segment must provide a cursor to a net:
					if ss.net /= pac_nets.no_element then
						
						-- The net name of the selected segment must match the name of the
						-- current net (indicated by net_cursor):
						if key (ss.net) = key (net_cursor) then

							-- Draw the whole net on the current sheet highlighted:
							pac_nets.query_element (
								position	=> net_cursor,
								process		=> query_strands_show'access);

							-- The net (indicated by net_cursor) must not be drawn again:
							net_already_drawn := true;
						end if;
					end if;
				end if;
			end if;
		end highlight_net;

		
	begin -- query_nets
		set_color_nets;

		-- iterate nets
		while net_cursor /= pac_nets.no_element loop

			net_already_drawn := false;

			-- If the show mode is active AND the net is selected for highlighting,
			-- then highlight the net and set the flag net_already_drawn:
			case verb is
				when VERB_SHOW => 
					case noun is
						when NOUN_NET => highlight_net;

						when others => null;
					end case;
		
				when others => null;
			end case;

			-- If the net (indicated by net_cursor) has been drawn already,
			-- don't draw it again. Otherwise draw it in "normal" mode:
			if not net_already_drawn then

				-- Draw the net in "normal" mode:
				pac_nets.query_element (
					position	=> net_cursor,
					process		=> query_strands_normal'access);

			end if;
			
			next (net_cursor); -- advance to next net
		end loop;

	end query_nets;



	
	procedure draw_path is
		use pac_path_and_bend;
		use et_colors.schematic;
		use et_modes.schematic;
		
		line : pac_geometry_2.type_line;

		
		procedure compute_route (s, e : in type_vector_model) is 

			-- Do the actual route calculation.
			r : type_path := to_path (s, e, live_path.bend_style);

			procedure draw is begin
				-- draw the net segment:
				draw_line (
					line		=> line,
					pos			=> origin_zero_rotation,		  
					width		=> net_line_width,
					do_stroke	=> true);
			end draw;

			
		begin -- compute_route

			-- The calculated route may required a bend point.
			-- Set/clear the "bended" flag of the net_segment being drawn.
			live_path.bended := r.bended;

			-- set color and line width for net segments:
			set_color_nets;

			-- If the route does not require a bend point, draw a single line
			-- from start to end point:
			if r.bended = NO then
				
				line.A := r.A;
				line.B := r.B;

				draw;

			-- If the route DOES require a bend point, then draw first a line
			-- from start point to bend point. Then draw a second line from
			-- bend point end point:
			else
				live_path.bend_point := r.bend_point;

				line.A := r.A;
				line.B := r.bend_point;
				
				draw;

				line.A := r.bend_point;
				line.B := r.B;
				
				draw;
				
			end if;
		end compute_route;

		
	begin -- draw_path
		if verb = VERB_DRAW and noun = NOUN_NET and edit_process_running = true then

			-- The route start point has been set eariler by procedures
			-- key_pressed or button_pressed.
			-- For drawing here, the route end point is to be taken from
			-- either the mouse pointer or the cursor position:

			case object_tool is				
				when MOUSE => 
					compute_route (
						s	=> live_path.A,	-- start of route
						e	=> snap_to_grid (get_mouse_position));	-- end of route
					
				when KEYBOARD =>
					compute_route (
						s	=> live_path.A,	-- start of route
						e	=> get_cursor_position);	-- end of route
					
			end case;			
		end if;
	end draw_path;

	
	
begin
-- 	put_line ("draw nets ...");
-- 	put_line (to_string (in_area));
	
	-- draw the nets
	pac_generic_modules.query_element (
		position	=> active_module,
		process		=> query_nets'access);


	-- Draw a net that is being drawn. If no net is being drawn,
	-- then nothing happens here:
	draw_path;

	
end draw_nets;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
