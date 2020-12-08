------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         SCHEMATIC DRAW NETS                              --
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

with gnat;
with gnat.exception_traces;

with ada.exceptions;
with ada.containers;						use ada.containers;
with ada.containers.doubly_linked_lists;

separate (et_canvas_schematic)

procedure draw_nets (
	self    : not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context) is

	use et_schematic;
	use et_schematic.type_nets;
	use et_schematic.pac_strands;
	use et_schematic.pac_net_segments;
	use et_schematic.pac_net_labels;
	use et_schematic.pac_shapes;
	
	use pac_draw_misc;

	use pac_proposed_segments;
	use pac_proposed_labels;
	
	-- Draws the junctions of a segment:
	procedure draw_junctions (
		s : in pac_net_segments.cursor)
	is
		j : type_junction_symbol := junction_symbol;

		procedure draw is begin
			draw_circle (
				area		=> in_area,
				context		=> context,
				circle		=> j,
				filled		=> YES,
				height		=> self.frame_height);
		end draw;
		
	begin
		-- at start point of segment:
		if element (s).junctions.start_point then
			j.center := element (s).start_point;
			draw;
		end if;

		-- at end point of segment:
		if element (s).junctions.end_point then
			j.center := element (s).end_point;
			draw;
		end if;

	end draw_junctions;

	-- Draws the junctions of a segment:
	procedure draw_junctions (
		s : in type_net_segment)
	is
		j : type_junction_symbol := junction_symbol;

		procedure draw is begin
			draw_circle (
				area		=> in_area,
				context		=> context,
				circle		=> j,
				filled		=> YES,
				height		=> self.frame_height);
		end draw;
		
	begin
		-- at start point of segment:
		if s.junctions.start_point then
			j.center := s.start_point;
			draw;
		end if;

		-- at end point of segment:
		if s.junctions.end_point then
			j.center := s.end_point;
			draw;
		end if;

	end draw_junctions;


	-- Returns true if the given label is selected.
	-- Returns false if there are no proposed labels or
	-- if the given label is not selected.
	function is_selected (
		net		: in type_nets.cursor;
		strand	: in pac_strands.cursor;
		segment	: in type_net_segment;
		label	: in pac_net_labels.cursor)
		return boolean is

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
		use et_text;
		use pac_text;
	begin
		case element (label).appearance is
			when SIMPLE =>
				draw_text (
					area		=> in_area,
					context		=> context,
					content		=> to_content (to_string (net)),
					size		=> element (label).size,
					font		=> net_label_font,
					position	=> element (label).position,
					origin		=> true, -- CS must be false on export to image
					
					-- Text rotation about its anchor point.
					-- This is documentational text.
					-- It is readable from the front or the right.
					rotation	=> to_rotation (element (label).rotation_simple),
					alignment	=> net_label_alignment,
					height		=> self.frame_height
					);

			when TAG =>
				draw_tag_label (self, in_area, context, net, element (label));

		end case;
	end draw_label;

	-- Draws a single net label that is being moved:
	procedure draw_simple_label_being_moved (
		net		: in pac_net_name.bounded_string;
		label	: in type_net_label)
	is
		use et_text;
		use pac_text;
	begin
		--case element (label).appearance is
			--when SIMPLE =>
				draw_text (
					area		=> in_area,
					context		=> context,
					content		=> to_content (to_string (net)),
					size		=> label.size,
					font		=> net_label_font,
					position	=> label.position,
					origin		=> true, -- CS must be false on export to image
					
					-- Text rotation about its anchor point.
					-- This is documentational text.
					-- It is readable from the front or the right.
					rotation	=> to_rotation (label.rotation_simple),
					alignment	=> net_label_alignment,
					height		=> self.frame_height
					);

			--when TAG =>
				--draw_tag_label (self, in_area, context, net, element (label));

		--end case;
	end draw_simple_label_being_moved;
	
	-- Draws labels that are NOT selected:
	procedure draw_labels (
		net		: in type_nets.cursor;
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
		use et_text;
		use pac_text;
		
		l : type_net_label (label.appearance);
	begin
		case verb is
			when VERB_PLACE =>
				if label.being_moved then

					case label.tool is
						when KEYBOARD	=> l.position := cursor_main.position;
						when MOUSE		=> l.position := self.snap_to_grid (self.mouse_position);
					end case;
					
					case label.appearance is
						when SIMPLE =>

							--l.rotation_simple := label.rotation_simple;
							
							draw_text (
								area		=> in_area,
								context		=> context,
								content		=> to_content (to_string (selected_net)),
								size		=> l.size,
								font		=> net_label_font,
								position	=> l.position,
								origin		=> true, -- CS must be false on export to image
								
								-- Text rotation about its anchor point.
								-- This is documentational text.
								-- It is readable from the front or the right.
								rotation	=> to_rotation (label.rotation_simple),
								alignment	=> net_label_alignment,
								height		=> self.frame_height);
							

						when TAG =>

							-- The current position must be tested whether it qualifies
							-- for a tag label. If the position is suitable then the
							-- flag label.finalizing_granted is set so that 
							-- procedure finalize_place_label is allowed to do the final
							-- placement of the label:
							declare
								use et_schematic_ops.nets;
								s : constant type_stub := query_stub (
										module_name		=> key (current_active_module),
										net_name		=> selected_net,
										position		=> to_position (type_point (l.position), current_active_sheet),
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

								draw_tag_label (self, in_area, context, selected_net, l);
							end;
							
					end case;
				end if;

			when others => null;
		end case;

	end draw_label_being_moved;

	-- Draws the net label as indicated by variable selected_label:
	procedure draw_selected_label (
		net		: in type_nets.cursor;
		strand	: in pac_strands.cursor;
		segment	: in pac_net_segments.cursor)
	is
		procedure query_label (s : in type_net_segment) is
			label_cursor : pac_net_labels.cursor := s.labels.first;

			sl : type_net_label (SIMPLE);
		begin
			while label_cursor /= pac_net_labels.no_element loop

				if is_selected (net, strand, element (segment), label_cursor) then

					if label.being_moved then
						-- Draw a copy of the label. Assign position
						-- according to tool:
						sl := element (label_cursor);
						
						case label.tool is
							when KEYBOARD =>
								sl.position := cursor_main.position;
							when MOUSE =>
								sl.position := self.snap_to_grid (self.mouse_position);
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
		net		: in type_nets.cursor;
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
				area		=> in_area,
				context		=> context,
				line		=> element (s),
				height		=> self.frame_height);

			draw_junctions (s);
			
		end if;
	end draw_fixed_segment;

	-- Draws a net segment.
	-- Draws also possible junctions that may exist at start or end point
	-- of the segment.
	procedure draw_preliminary_segment (
		net		: in type_nets.cursor;
		strand	: in pac_strands.cursor;
		segment : in type_net_segment) 
	is begin
		draw_line (
			area		=> in_area,
			context		=> context,
			line		=> segment,
			height		=> self.frame_height);

		draw_junctions (segment);

		draw_labels (net, strand, segment);
	end draw_preliminary_segment;

	-- Draws secondary nets which are attached to the primary net.
	procedure draw_secondary_segments (
		net_cursor			: in type_nets.cursor;
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
				if element (original_segment).start_point = secondary_segment.start_point then
				-- Start point of secondary segment is attached to the start point of the original segment.
					secondary_segment.start_point := primary_segment.start_point;

					draw_and_mark;
				end if;

				if element (original_segment).start_point = secondary_segment.end_point then
				-- end point of secondary net segment is attached to the start point of the original segment
					secondary_segment.end_point := primary_segment.start_point;

					draw_and_mark;
				end if;
			end drag_at_start;

			procedure drag_at_end is begin
				if element (original_segment).end_point = secondary_segment.start_point then
				-- Start point of secondary segment is attached to the end point of the original segment.
					secondary_segment.start_point := primary_segment.end_point;

					draw_and_mark;
				end if;

				if element (original_segment).end_point = secondary_segment.end_point then
				-- end point of secondary segment is attached to the end point of the original segment
					secondary_segment.end_point := primary_segment.end_point;

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
		net_cursor			: in type_nets.cursor;
		strand_cursor		: in pac_strands.cursor;
		original_segment	: in pac_net_segments.cursor) 
	is
		use et_schematic_ops.nets;

		-- Calculate the zone where the original segment is being attacked:
		zone : constant type_line_zone := which_zone (
				point	=> segment.point_of_attack,
				line	=> element (original_segment));

		dx, dy : type_distance;

		destination : type_point;
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
			
			segment.finalizing_granted := true;
		end move_labels_and_secondary_nets;
		
	begin -- draw_moving_segments
		
		-- First test whether the original segment is movable at all.
		-- Ports connected with the segment may prohibit moving the segment
		-- as they belong to symbols which are not dragged along.
		-- If the segment is not movable then it will be drawn as given by
		-- the module database.
		if movable (
			module_name		=> key (current_active_module),
			segment			=> element (original_segment),
			zone			=> zone,
			point_of_attack	=> to_position (segment.point_of_attack, current_active_sheet),
			log_threshold	=> log_threshold + 10) -- CS: avoids excessive log information. find a more elegant way.
		then
			-- segment is movable

			-- Take a copy of the original net segment.
			-- We call this copy from now on "primary segment" because other
			-- segments (secondary segments) could be connected with it.
			primary_segment := element (original_segment);

			-- calculate the destination point according to the current drawing tool:
			case segment.tool is
				when MOUSE =>
					destination := self.snap_to_grid (self.mouse_position);

				when KEYBOARD =>
					destination := cursor_main.position;
			end case;

			-- calculate the distance in x and y from point of attack to destination:
			dx := distance (segment.point_of_attack, destination, X);
			dy := distance (segment.point_of_attack, destination, Y);
			
			case verb is
				when VERB_DRAG =>

					-- Depending on the zone being dragged, we move the
					-- start or/and end point of the primary segment:
					case zone is
						when START_POINT =>
							if dx = zero or dy = zero then

								move_by (
									point	=> primary_segment.start_point,
									offset	=> set (dx, dy));
							
							else
								primary_segment.start_point := destination;
							end if;

							move_labels_and_secondary_nets;
										
						when END_POINT =>
							if dx = zero or dy = zero then

								move_by (
									point	=> primary_segment.end_point,
									offset	=> set (dx, dy));
							
							else
								primary_segment.end_point := destination;
							end if;

							move_labels_and_secondary_nets;
							
						when CENTER =>
							move_by (
								point	=> primary_segment.start_point,
								offset	=> set (dx, dy));

							move_by (
								point	=> primary_segment.end_point,
								offset	=> set (dx, dy));

							move_labels_and_secondary_nets;

					end case;
					
	-- 			when VERB_MOVE =>
	-- 				null;

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
		tool_position : type_point;
		displacement : type_point;
		
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
						move_by (copy_of_original_segment.start_point, displacement);
					
					when END_POINT =>
						move_by (copy_of_original_segment.end_point, displacement);
				end case;

				draw_line (
					area		=> in_area,
					context		=> context,
					line		=> copy_of_original_segment,
					height		=> self.frame_height);

				-- mark segment as already drawn
				already_drawn_segments.append (element (s));
				
			end if;
		end query_segment;
		
	begin -- draw_segment_being_dragged_along_with_unit
		if not is_empty (segments_being_dragged) then

			-- Calculate the displacement of segments according to the
			-- current drawing tool and the current displacement of the unit:
			case unit_move.tool is
				when MOUSE =>
					tool_position := self.snap_to_grid (self.mouse_position);

				when KEYBOARD =>
					tool_position := cursor_main.position;
			end case;

			-- This is the displacement of the attached segments:
			displacement := type_point (distance_relative (unit_move.original_position, tool_position));

-- 			log (text => "original    " & to_string (unit.original_position), console => true);
-- 			log (text => "displacement" & to_string (displacement), console => true);
-- 			log (text => "count       " & count_type'image (length (segments_being_dragged)), console => true);
			
			segments_being_dragged.iterate (query_segment'access);
		end if;
	end draw_segment_being_dragged_along_with_unit;
	
	procedure query_nets (
		module_name	: in pac_module_name.bounded_string;
		module		: in type_module) is

		-- This cursor points to the current net being drawn:
		net_cursor : type_nets.cursor := module.nets.first;

		-- Draws the strands of the given net in "normal" mode.
		-- "Normal" mode means, the whole net is not to be drawn highlighted.
		-- This is the case when the verb VERB_SHOW is not active.
		procedure query_strands_normal (
			net_name	: in pac_net_name.bounded_string;
			net			: in type_net) is
			strand_cursor : pac_strands.cursor := net.strands.first;

			procedure query_segments (strand : in type_strand) is
				segment_cursor : pac_net_segments.cursor := strand.segments.first;
			begin
				-- draw nets of the active sheet only:
				if strand.position.sheet = current_active_sheet then

					-- set line width for net segments:
					set_line_width (context.cr, type_view_coordinate (et_schematic.net_line_width));

					-- First we draw selected segments or those being moved/dragged:
					set_color_nets (context.cr, BRIGHT);
					
					while segment_cursor /= pac_net_segments.no_element loop

						-- CS test verb and noun ?
						draw_segment_being_dragged_along_with_unit (segment_cursor);
						-- (If segments_being_dragged is empty, then nothing happens.)
						
						-- CS test verb and noun ?						
						if is_selected (net_cursor, strand_cursor, segment_cursor) then
						
							if segment.being_moved then
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
					set_color_nets (context.cr, NORMAL);
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
			net			: in type_net) is
			strand_cursor : pac_strands.cursor := net.strands.first;

			procedure query_segments (strand : in type_strand) is
				segment_cursor : pac_net_segments.cursor := strand.segments.first;
			begin
				-- draw nets of the active sheet only:
				if strand.position.sheet = current_active_sheet then

					-- set line width for net segments:
					set_line_width (context.cr, type_view_coordinate (et_schematic.net_line_width));

					set_color_nets (context.cr, BRIGHT);
					
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
					if ss.net /= type_nets.no_element then
						
						-- The net name of the selected segment must match the name of the
						-- current net (indicated by net_cursor):
						if key (ss.net) = key (net_cursor) then

							-- Draw the whole net on the current sheet highlighted:
							type_nets.query_element (
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
		set_color_nets (context.cr);

		-- iterate nets
		while net_cursor /= type_nets.no_element loop

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
				type_nets.query_element (
					position	=> net_cursor,
					process		=> query_strands_normal'access);

			end if;
			
			next (net_cursor); -- advance to next net
		end loop;

	end query_nets;
	
begin
-- 	put_line ("draw nets ...");
-- 	put_line (to_string (in_area));
	
	-- draw the nets
	pac_generic_modules.query_element (
		position	=> current_active_module,
		process		=> query_nets'access);
	
end draw_nets;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
