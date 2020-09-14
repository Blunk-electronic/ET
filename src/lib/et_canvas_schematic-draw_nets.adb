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
	use et_schematic.type_strands;
	use et_schematic.type_net_segments;
	use et_schematic.pac_shapes;
	
	use pac_draw_misc;

	-- Draws the junctions of a segment:
	procedure draw_junctions (
		s : in type_net_segments.cursor)
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

	procedure draw_labels (
		n : in type_nets.cursor;
		s : in type_net_segment)
	is 
		use type_net_labels;
		use et_text;
		use pac_text;

		procedure draw (l : in type_net_labels.cursor) is begin
			case element (l).appearance is
				when SIMPLE =>
					draw_text (
						area		=> in_area,
						context		=> context,
						content		=> to_content (to_string (key (n))),
						size		=> element (l).size,
						font		=> net_label_font,
						position	=> element (l).position,
						origin		=> true, -- CS must be false on export to image
						
						-- Text rotation around its anchor point.
						-- This is documentational text.
						-- It is readable from the front or the right.
						rotation	=> to_rotation (element (l).rotation_simple),
						alignment	=> (LEFT, BOTTOM),
						height		=> self.frame_height
						);

				when TAG =>
					draw_tag_label (self, in_area, context, key (n), element (l));

			end case;
		end draw;
		
	begin
		iterate (s.labels, draw'access);
	end draw_labels;
	
	-- Returns true if the given segment is selected.
	-- Returns false if there are no proposed segments or
	-- if the given segment is not selected.
	function is_selected (
		s : in type_net_segments.cursor)
		return boolean is
		use pac_proposed_segments;
	begin
		if is_empty (proposed_segments) then
			return false;
		else
			if selected_segment /= pac_proposed_segments.no_element then
				if s = element (selected_segment).segment then
					return true;
				else
					return false;
				end if;
			else
				return false;
			end if;
		end if;
		
	end is_selected;

	-- We need a list segments that have been drawn already.
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
		s : in type_net_segments.cursor) 
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
		n : in type_nets.cursor;
		s : in type_net_segment) 
	is begin
		draw_line (
			area		=> in_area,
			context		=> context,
			line		=> s,
			height		=> self.frame_height);

		draw_junctions (s);

		draw_labels (n, s);
	end draw_preliminary_segment;

	-- Draws secondary nets which are attached to the primary net.
	procedure draw_secondary_segments (
		net_cursor			: in type_nets.cursor;
		strand_cursor		: in type_strands.cursor;

		-- The segment as it is according to database (before the drag)										  
		original_segment	: in type_net_segments.cursor;

		-- The primary segment being drawn:
		primary_segment		: in type_net_segment;

		-- the zone being dragged:
		zone				: in type_line_zone)
	is 		
		
		procedure query_segment (c : in type_net_segments.cursor) is
			secondary_segment : type_net_segment;

			-- Draw the secondary segment and mark it as drawn.
			-- It must be marked as drawn so that procedure query_nets
			-- does not draw it in its inital state according to the database.
			procedure draw_and_mark is begin
				draw_preliminary_segment (net_cursor, secondary_segment);

				-- mark segment as already drawn
				already_drawn_segments.append (element (c));
			end draw_and_mark;
			
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
						if element (original_segment).start_point = secondary_segment.start_point then
						-- Start point of secondary segment is attached to the start point of the original segment.
							secondary_segment.start_point := primary_segment.start_point;

							draw_and_mark;
						end if;

						if element (original_segment).start_point = secondary_segment.end_point then
						-- end point of secondary net segment to the start point of the original segment
							secondary_segment.end_point := primary_segment.start_point;

							draw_and_mark;
						end if;
						
					when END_POINT =>
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
		
					when CENTER => null; -- CS
					
				end case;
				
			end if;
		end query_segment;
		
	begin
		-- Iterate all segments of the given strand.
		-- Skip the original segment:
		type_net_segments.iterate (
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
		strand_cursor		: in type_strands.cursor;
		original_segment	: in type_net_segments.cursor) 
	is
		use et_schematic_ops.nets;

		-- Calculate the zone where the original segement is being attacked:
		zone : constant type_line_zone := which_zone (
				point	=> segment.point_of_attack,
				line	=> element (original_segment));

		dx, dy : type_distance;

		destination : type_point;
		primary_segment : type_net_segment;
		
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
			log_threshold	=> log_threshold + 10) -- CS: avoids excessive log information
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

							move_net_labels (
								segment_before	=> element (original_segment),
								segment_after	=> primary_segment,
								zone			=> zone);
							
							-- Draw the primary segment in its temporarily state:
							draw_preliminary_segment (net_cursor, primary_segment);

							-- Drawing attached secondary segments requires the original
							-- net segment (before the drag operation):
							draw_secondary_segments (
								net_cursor			=> net_cursor,
								strand_cursor		=> strand_cursor,
								original_segment	=> original_segment,
								primary_segment		=> primary_segment,
								zone				=> zone);
							
							segment.finalizing_granted := true;  -- CS
							
						when END_POINT =>
							if dx = zero or dy = zero then

								move_by (
									point	=> primary_segment.end_point,
									offset	=> set (dx, dy));
							
							else
								primary_segment.end_point := destination;
							end if;

							move_net_labels (
								segment_before	=> element (original_segment),
								segment_after	=> primary_segment,
								zone			=> zone);
							
							-- Draw the primary segment in its temporarily state:
							draw_preliminary_segment (net_cursor, primary_segment);

							-- Drawing attached secondary segments requires the original
							-- net segment (before the drag operation):
							draw_secondary_segments (
								net_cursor			=> net_cursor,
								strand_cursor		=> strand_cursor,
								original_segment	=> original_segment,
								primary_segment		=> primary_segment,
								zone				=> zone);
							
							segment.finalizing_granted := true;  -- CS
							
						when CENTER =>
							-- CS currently dragging at center not possible
							-- so we draw the selected_segment as it is:
							draw_fixed_segment (original_segment);
							draw_labels (net_cursor, element (original_segment));
							
						when others => null;
					end case;
					
	-- 			when VERB_MOVE =>
	-- 				null;

				when others => null;
			end case;
		else
			-- Not movable. Draw as given in database:
			draw_fixed_segment (original_segment);
			draw_labels (net_cursor, element (original_segment));
		end if;
	end draw_moving_segments;
	
	procedure query_nets (
		module_name	: in type_module_name.bounded_string;
		module		: in type_module) is

		net_cursor : type_nets.cursor := module.nets.first;

		procedure query_strands (
			net_name	: in et_general.type_net_name.bounded_string;
			net			: in type_net) is
			strand_cursor : type_strands.cursor := net.strands.first;

			procedure query_segments (strand : in type_strand) is
				segment_cursor : type_net_segments.cursor := strand.segments.first;
			begin
				-- draw nets of the active sheet only:
				if strand.position.sheet = current_active_sheet then

					-- set line width for net segments:
					set_line_width (context.cr, type_view_coordinate (et_schematic.net_line_width));

					-- First we draw selected segments or those being moved/dragged:
					set_color_nets (context.cr, BRIGHT);
					
					while segment_cursor /= type_net_segments.no_element loop
						
						if is_selected (segment_cursor) then
						
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

								draw_labels (net_cursor, element (segment_cursor));
							end if;
						end if;
						
						next (segment_cursor);
					end loop;

					
					-- Now we draw the remaining segments:
					set_color_nets (context.cr, NORMAL);
					segment_cursor := strand.segments.first;
					
					while segment_cursor /= type_net_segments.no_element loop

						if not is_selected (segment_cursor) then

							-- Draw the net segment as it is according to module database:
							draw_fixed_segment (segment_cursor);

							draw_labels (net_cursor, element (segment_cursor));
						end if;

						next (segment_cursor);
					end loop;

				end if;
			end query_segments;
			
		begin -- query_strands
			while strand_cursor /= type_strands.no_element loop

				query_element (
					position	=> strand_cursor,
					process		=> query_segments'access);
				
				next (strand_cursor);
			end loop;
		end query_strands;
		
	begin -- query_nets
		set_color_nets (context.cr);

		-- iterate nets
		while net_cursor /= type_nets.no_element loop

			type_nets.query_element (
				position	=> net_cursor,
				process		=> query_strands'access);

			next (net_cursor);
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
