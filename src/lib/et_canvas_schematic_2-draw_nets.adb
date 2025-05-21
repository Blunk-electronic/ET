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

-- with gnat;
-- with gnat.exception_traces;

with ada.exceptions;
-- with ada.containers;						use ada.containers;
-- with ada.containers.doubly_linked_lists;

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
	
	-- use pac_proposed_segments;
	-- use pac_proposed_labels;

	
-- 	-- Draws the junctions of a segment:
-- 	procedure draw_junctions (
-- 		s : in pac_net_segments.cursor)
-- 	is
-- 		j : type_circle := junction_symbol;
-- 
-- 		procedure draw is begin
-- 			draw_circle (
-- 				circle		=> type_circle (j),
-- 				pos			=> origin_zero_rotation,
-- 				filled		=> YES,
-- 				width		=> zero,
-- 				do_stroke	=> true);
-- 		end draw;
-- 		
-- 	begin
-- 		-- at start point of segment:
-- 		if element (s).junctions.A then
-- 			set_center (j, get_A (s));
-- 			draw;
-- 		end if;
-- 
-- 		-- at end point of segment:
-- 		if element (s).junctions.B then
-- 			set_center (j, get_B (s));
-- 			draw;
-- 		end if;
-- 
-- 	end draw_junctions;
-- 
-- 
-- 	
-- 	-- Draws the junctions of a segment:
-- 	procedure draw_junctions (
-- 		s : in type_net_segment)
-- 	is
-- 		j : type_circle := junction_symbol;
-- 
-- 		procedure draw is begin
-- 			draw_circle (
-- 				circle		=> type_circle (j),
-- 				pos			=> origin_zero_rotation,
-- 				filled		=> YES,
-- 				width		=> zero,
-- 				do_stroke	=> true);
-- 		end draw;
-- 		
-- 	begin
-- 		-- at start point of segment:
-- 		if s.junctions.A then
-- 			set_center (j, get_A (s));
-- 			draw;
-- 		end if;
-- 
-- 		-- at end point of segment:
-- 		if s.junctions.B then
-- 			set_center (j, get_B (s));
-- 			draw;
-- 		end if;
-- 
-- 	end draw_junctions;



-- 	-- This procedure draws a tag label:
-- 	procedure draw_tag_label (
-- 		net		: in pac_net_name.bounded_string;
-- 		label	: in type_net_label)
-- 	is
-- 		use pac_net_name;
-- 		use et_alignment;
-- 
-- 		box : type_area;
-- 		
-- 		content : pac_text_content.bounded_string := to_content (to_string (net));
-- 		-- CS append to content the position of the net on the next sheet (strand position)
-- 		-- using the quadrant bars.
-- 		
-- 		
-- 		-- The text rotation must be either 0 or 90 degree (documentational text) and is thus
-- 		-- to be calculated according to the rotation of the label:
-- 		text_rotation : type_rotation;
-- 
-- 		-- The alignment is assigned as if the text were drawn at zero rotation.
-- 		-- The vertical alignment is always CENTER. Horizontal alignment changes depending on 
-- 		-- the rotation of the label:
-- 		text_alignment : type_text_alignment := (vertical => ALIGN_CENTER, horizontal => <>);
-- 
-- 		-- The text position is not the same as the label position, thus it must be 
-- 		-- calculated according to the label rotation and tag_label_text_offset:
-- 		text_position : type_vector_model;
-- 
-- 		use pac_draw_text;
-- 	begin
-- 		-- Form a box that wraps around the net name:
-- 		box := to_area (get_text_extents (content, label.size, net_label_font));
-- 
-- 		-- Expand the box so that there is some empty space between
-- 		-- text and border:
-- 		box.height := box.height * tag_label_height_to_size_ratio;
-- 		box.width  := box.width  * tag_label_height_to_size_ratio;
-- 		
-- 		if label.rotation_tag = zero_rotation then
-- 	
-- 			box.position := set (
-- 				get_x (label.position), 
-- 				get_y (label.position) - box.height * 0.5);
-- 
-- 			text_rotation := zero_rotation;
-- 			text_position := set (get_x (label.position) + tag_label_text_offset, get_y (label.position));
-- 			text_alignment.horizontal := ALIGN_LEFT;
-- 		end if;
-- 
-- 		
-- 		if label.rotation_tag = 90.0 then
-- 
-- 			box.position := set (
-- 				get_x (label.position) - box.height * 0.5,
-- 				get_y (label.position));
-- 
-- 			swap_edges (box);
-- 
-- 			text_rotation := 90.0;
-- 			text_position := set (get_x (label.position), get_y (label.position) + tag_label_text_offset);
-- 			text_alignment.horizontal := ALIGN_LEFT;
-- 		end if;
-- 
-- 		
-- 		if label.rotation_tag = 180.0 then
-- 
-- 			box.position := set (
-- 				get_x (label.position) - box.width,
-- 				get_y (label.position) - box.height * 0.5);
-- 
-- 			text_rotation := zero_rotation;
-- 			text_position := set (get_x (label.position) - tag_label_text_offset, get_y (label.position));
-- 			text_alignment.horizontal := ALIGN_RIGHT;
-- 		end if;
-- 
-- 		
-- 		if label.rotation_tag = -90.0 then
-- 
-- 			box.position := set (
-- 				get_x (label.position) - box.height * 0.5,
-- 				get_y (label.position) - box.width);
-- 			
-- 			swap_edges (box);
-- 
-- 			text_rotation := 90.0;
-- 			text_position := set (get_x (label.position), get_y (label.position) - tag_label_text_offset);
-- 			text_alignment.horizontal := ALIGN_RIGHT;
-- 		end if;
-- 
-- 
-- 		-- Draw the box enshrouding the net name:
-- 		draw_rectangle (
-- 			rectangle	=> box, 
-- 			width		=> tag_label_box_line_width);
-- 
-- 		-- Draw the actual net name:
-- 		draw_text (
-- 			content		=> content,
-- 			size		=> label.size,
-- 			font		=> net_label_font,
-- 			anchor		=> text_position,
-- 			origin		=> false, -- no origin for net names required
-- 			
-- 			-- Text rotation about its anchor point. This is documentational text.
-- 			-- It is readable from the front or the right.
-- 			rotation	=> text_rotation,
-- 			alignment	=> text_alignment);
-- 
-- 	end draw_tag_label;
-- 
	

-- 	-- Returns true if the given label is selected.
-- 	-- Returns false if there are no proposed labels or
-- 	-- if the given label is not selected.
-- 	function is_selected (
-- 		net		: in pac_nets.cursor;
-- 		strand	: in pac_strands.cursor;
-- 		segment	: in type_net_segment;
-- 		label	: in pac_net_labels.cursor)
-- 		return boolean 
-- 	is
-- 		use pac_net_name;
-- 		sl : type_selected_label;
-- 	begin
-- 		if is_empty (proposed_labels) then
-- 			return false;
-- 		else
-- 			if selected_label /= pac_proposed_labels.no_element then
-- 
-- 				sl := element (selected_label);
-- 				
-- 				if key (net) = key (sl.net)
-- 				and element (strand) = element (sl.strand)
-- 				and segment = element (sl.segment)
-- 				and element (label) = element (sl.label)
-- 				then
-- 					return true;
-- 				else
-- 					return false;
-- 				end if;
-- 			else
-- 				return false;
-- 			end if;
-- 		end if;		
-- 	end is_selected;


	
-- 	-- Draws a single net label:
-- 	procedure draw_label (
-- 		net		: in pac_net_name.bounded_string;
-- 		label	: in pac_net_labels.cursor)
-- 	is 
-- 		use pac_net_name;
-- 		use pac_draw_text;
-- 		use et_alignment;
-- 	begin
-- 		case element (label).appearance is
-- 			when SIMPLE =>
-- 				draw_text (
-- 					content		=> to_content (to_string (net)),
-- 					size		=> element (label).size,
-- 					font		=> net_label_font,
-- 					anchor		=> element (label).position,
-- 					origin		=> true, -- CS must be false on export to image
-- 					
-- 					-- Text rotation about its anchor point.
-- 					-- This is documentational text.
-- 					-- It is readable from the front or the right.
-- 					rotation	=> pac_text.to_rotation (element (label).rotation_simple),
-- 					alignment	=> net_label_alignment);
-- 
-- 			when TAG =>
-- 				draw_tag_label (net, element (label));
-- 
-- 		end case;
-- 	end draw_label;


	
	-- Draws a single net label that is being moved:
-- 	procedure draw_simple_label_being_moved (
-- 		net		: in pac_net_name.bounded_string;
-- 		label	: in type_net_label)
-- 	is 
-- 		use pac_net_name;
-- 		use pac_draw_text;
-- 	begin
-- 		--case element (label).appearance is
-- 			--when SIMPLE =>
-- 				draw_text (
-- 					content		=> to_content (to_string (net)),
-- 					size		=> label.size,
-- 					font		=> net_label_font,
-- 					anchor		=> label.position,
-- 					origin		=> true, -- CS must be false on export to image
-- 					
-- 					-- Text rotation about its anchor point.
-- 					-- This is documentational text.
-- 					-- It is readable from the front or the right.
-- 					rotation	=> pac_text.to_rotation (label.rotation_simple),
-- 					alignment	=> net_label_alignment);
-- 
-- 			--when TAG =>
-- 				--draw_tag_label (self, in_area, context, net, element (label));
-- 
-- 		--end case;
-- 	end draw_simple_label_being_moved;
-- 
-- 
-- 	
-- 	-- Draws labels that are NOT selected:
-- 	procedure draw_labels (
-- 		net		: in pac_nets.cursor;
-- 		strand	: in pac_strands.cursor;
-- 		segment	: in type_net_segment)
-- 	is 
-- 		procedure draw_fixed (label : in pac_net_labels.cursor) is begin
-- 			if not is_selected (net, strand, segment, label) then
-- 				draw_label (key (net), label);
-- 			end if;
-- 		end draw_fixed;
-- 		
-- 	begin
-- 		iterate (segment.labels, draw_fixed'access);
-- 	end draw_labels;

	
	
-- 	-- Draws the net label being moved. If no net label
-- 	-- is being moved, nothing happens here:
-- 	procedure draw_label_being_moved is
-- 		use et_modes.schematic;
-- 		use pac_net_name;
-- 		l : type_net_label (label.appearance);
-- 
-- 		use pac_draw_text;
-- 	begin
-- 		case verb is
-- 			when VERB_PLACE =>
-- 				if label.ready then
-- 
-- 					case label.tool is
-- 						when KEYBOARD	=> l.position := get_cursor_position;
-- 						when MOUSE		=> l.position := snap_to_grid (get_mouse_position);
-- 					end case;
-- 					
-- 					case label.appearance is
-- 						when SIMPLE =>
-- 
-- 							--l.rotation_simple := label.rotation_simple;
-- 							
-- 							draw_text (
-- 								content		=> to_content (to_string (selected_net)),
-- 								size		=> l.size,
-- 								font		=> net_label_font,
-- 								anchor		=> l.position,
-- 								origin		=> true, -- CS must be false on export to image
-- 								
-- 								-- Text rotation about its anchor point.
-- 								-- This is documentational text.
-- 								-- It is readable from the front or the right.
-- 								rotation	=> pac_text.to_rotation (label.rotation_simple),
-- 								alignment	=> net_label_alignment);
-- 							
-- 
-- 						when TAG =>
-- 
-- 							-- The current position must be tested whether it qualifies
-- 							-- for a tag label. If the position is suitable then the
-- 							-- flag label.finalizing_granted is set so that 
-- 							-- procedure finalize_place_label is allowed to do the final
-- 							-- placement of the label:
-- 							declare
-- 								use et_schematic_ops.nets;
-- 								s : constant type_stub := query_stub (
-- 										module_cursor	=> active_module,
-- 										net_name		=> selected_net,
-- 										position		=> to_position (type_vector_model (l.position), active_sheet),
-- 										log_threshold	=> log_threshold + 1);
-- 
-- 									-- CS use a function query_stub that take a module cursor and
-- 									-- a net cursor instead.
-- 
-- 							begin
-- 								if s.is_stub then
-- 									-- Set the rotation of the label according to the 
-- 									-- direction of the stub:
-- 									l.rotation_tag := to_label_rotation (s.direction);
-- 
-- 									label.finalizing_granted := true;
-- 								else
-- 									label.finalizing_granted := false;
-- 								end if;
-- 
-- 								draw_tag_label (selected_net, l);
-- 							end;
-- 							
-- 					end case;
-- 				end if;
-- 
-- 			when others => null;
-- 		end case;
-- 
-- 	end draw_label_being_moved;
-- 

	
	
	-- Draws the net label as indicated by variable selected_label:
-- 	procedure draw_selected_label (
-- 		net		: in pac_nets.cursor;
-- 		strand	: in pac_strands.cursor;
-- 		segment	: in pac_net_segments.cursor)
-- 	is
-- 		procedure query_label (s : in type_net_segment) is
-- 			label_cursor : pac_net_labels.cursor := s.labels.first;
-- 
-- 			sl : type_net_label (SIMPLE);
-- 		begin
-- 			while label_cursor /= pac_net_labels.no_element loop
-- 
-- 				if is_selected (net, strand, element (segment), label_cursor) then
-- 
-- 					if label.ready then
-- 						-- Draw a copy of the label. Assign position
-- 						-- according to tool:
-- 						sl := element (label_cursor);
-- 						
-- 						case label.tool is
-- 							when KEYBOARD =>
-- 								sl.position := get_cursor_position;
-- 							when MOUSE =>
-- 								sl.position := snap_to_grid (get_mouse_position);
-- 						end case;
-- 
-- 						-- draw the temporarily label
-- 						draw_simple_label_being_moved (key (net), sl);
-- 					else
-- 						-- draw label as it is according to module database:
-- 						draw_label (key (net), label_cursor);
-- 					end if;
-- 														  
-- 					exit; -- there is only on selected label. no further search required
-- 				end if;
-- 
-- 				next (label_cursor);
-- 			end loop;
-- 		end query_label;
-- 
-- 		
-- 	begin
-- 		if not is_empty (proposed_labels) then
-- 			query_element (segment, query_label'access);
-- 		end if;
-- 	end draw_selected_label;

	


	
	procedure query_module (
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
		procedure query_net (
			net_name	: in pac_net_name.bounded_string;
			net			: in type_net) 
		is
			strand_cursor : pac_strands.cursor := net.strands.first;


			procedure query_segment (segment : in type_net_segment) is 
				label_cursor : pac_net_labels.cursor := segment.labels.first;


				-- This procedure draws a single net segment:
				procedure draw_segment is begin
					draw_line (
						line		=> segment,
						width		=> net_line_width,
						do_stroke	=> true);
				end draw_segment;


				-- This procedure queries a net label and draws it
				-- according to its appearance (SIMPLE or TAG label):
				procedure query_label (label : in type_net_label) is
					use pac_net_name;
					use pac_draw_text;


					-- This procedure draws a simple label:
					procedure draw_simple is begin

						-- CS overwrite position by get_object_tool_position
						-- if the label is moving.
						
						draw_text (
							content		=> to_content (to_string (net_name)),
							size		=> label.size,
							font		=> net_label_font,
							anchor		=> label.position,
							origin		=> true,
							
							-- Text rotation about its anchor point.
							-- This is documentational text.
							-- It is readable from the front or the right.
							rotation	=> pac_text.to_rotation (label.rotation_simple),
							alignment	=> net_label_alignment);					
					end draw_simple;


					-- This procedure draws a tag label:
					procedure draw_tag is
						use et_alignment;

						box : type_area;
						
						content : pac_text_content.bounded_string := 
							to_content (to_string (net_name));
						-- CS: append to content the position of the net
						-- on the next sheet (strand position) using the quadrant bars.
												
						-- The text rotation must be either 0 or 90 degree
						-- (documentational text !) and is thus
						-- to be calculated according to the rotation of the label:
						text_rotation : type_rotation;

						-- The alignment is assigned as if the text 
						-- were drawn at zero rotation.
						-- The vertical alignment is always CENTER. Horizontal alignment 
						-- changes depending on the rotation of the label:
						text_alignment : type_text_alignment := 
							(vertical => ALIGN_CENTER, horizontal => <>);

						-- The text position is not the same as the 
						-- label position, thus it must be calculated according to 
						-- the label rotation and tag_label_text_offset:
						text_position : type_vector_model;

					begin
						-- Form a box that wraps around the net name:
						box := to_area (get_text_extents (content, label.size, net_label_font));

						-- Expand the box so that there is some empty space between
						-- text and border:
						box.height := box.height * tag_label_height_to_size_ratio;
						box.width  := box.width  * tag_label_height_to_size_ratio;
						
						-- CS overwrite position if the label is moving.

						
						if label.rotation_tag = zero_rotation then
					
							box.position := set (
								get_x (label.position), 
								get_y (label.position) - box.height * 0.5);

							text_rotation := zero_rotation;
							text_position := set (get_x (label.position) + tag_label_text_offset, 
												  get_y (label.position));
							
							text_alignment.horizontal := ALIGN_LEFT;
						end if;

						
						if label.rotation_tag = 90.0 then

							box.position := set (
								get_x (label.position) - box.height * 0.5,
								get_y (label.position));

							swap_edges (box);

							text_rotation := 90.0;
							text_position := set (get_x (label.position), 
												  get_y (label.position) + tag_label_text_offset);
							
							text_alignment.horizontal := ALIGN_LEFT;
						end if;

						
						if label.rotation_tag = 180.0 then

							box.position := set (
								get_x (label.position) - box.width,
								get_y (label.position) - box.height * 0.5);

							text_rotation := zero_rotation;
							text_position := set (get_x (label.position) - tag_label_text_offset, 
												  get_y (label.position));
							
							text_alignment.horizontal := ALIGN_RIGHT;
						end if;

						
						if label.rotation_tag = -90.0 then

							box.position := set (
								get_x (label.position) - box.height * 0.5,
								get_y (label.position) - box.width);
							
							swap_edges (box);

							text_rotation := 90.0;
							text_position := set (get_x (label.position), 
												  get_y (label.position) - tag_label_text_offset);
							
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

					end draw_tag;

					
				begin
					case label.appearance is
						when SIMPLE	=> draw_simple;
						when TAG	=> draw_tag;
					end case;
				end query_label;


				
				-- This procedure draws the junctions of the segment. 
				-- Note: A junction can be at the start or the end (A/B)
				-- of the segment, but never somewhere between.
				procedure draw_junctions is
					j : type_circle := junction_symbol;

					procedure draw is begin
						draw_circle (
							circle		=> type_circle (j),
							filled		=> YES,
							width		=> zero,
							do_stroke	=> true);
					end draw;
					
				begin
					-- Probe start point of segment:
					if segment.junctions.A then
						set_center (j, get_A (segment));
						draw;
					end if;

					-- Probe end point of segment:
					if segment.junctions.B then
						set_center (j, get_B (segment));
						draw;
					end if;
				end draw_junctions;

				
			begin			
				-- Increase brightness if segment is selected:
				if is_selected (segment) then
					set_color_nets (BRIGHT);
				end if;				

				draw_segment;				
				
				-- Iterate through the net labels:
				while has_element (label_cursor) loop
					query_element (label_cursor, query_label'access);
					next (label_cursor);
				end loop;
				
				draw_junctions;

				set_color_nets (NORMAL);
			end query_segment;

			
			
			procedure query_strand (strand : in type_strand) is
				segment_cursor : pac_net_segments.cursor := strand.segments.first;
			begin
				-- draw nets of the active sheet only:
				if get_sheet (strand.position) = active_sheet then

					-- CS increase brightness if strand is selected

					-- Iterate through the segments of the candidate strand:
					while segment_cursor /= pac_net_segments.no_element loop
						query_element (segment_cursor, query_segment'access);
						next (segment_cursor);
					end loop;

				end if;
			end query_strand;
			
			
		begin
			-- CS increase brightness if net is selected
			
			-- Iterate through the strands of the candidate net:
			while strand_cursor /= pac_strands.no_element loop
			
				query_element (
					position	=> strand_cursor,
					process		=> query_strand'access);
				
				next (strand_cursor);
			end loop;
		end query_net;

		
-- 		-- Draws the whole net inclusive net labesl highlighted. 
-- 		-- This is the case when the verb VERB_SHOW is active.
-- 		procedure query_strands_show (
-- 			net_name	: in pac_net_name.bounded_string;
-- 			net			: in type_net)
-- 		is
-- 			strand_cursor : pac_strands.cursor := net.strands.first;
-- 
-- 			procedure query_segments (strand : in type_strand) is
-- 				segment_cursor : pac_net_segments.cursor := strand.segments.first;
-- 
-- 				use et_colors;
-- 				use et_colors.schematic;
-- 			begin
-- 				-- draw nets of the active sheet only:
-- 				if get_sheet (strand.position) = active_sheet then
-- 
-- 					set_color_nets (BRIGHT);
-- 					
-- 					while segment_cursor /= pac_net_segments.no_element loop
-- 
-- 						-- Draw the net segment as it is according to module database:
-- 						draw_fixed_segment (segment_cursor);
-- 
-- 						draw_labels (net_cursor, strand_cursor, element (segment_cursor));
-- 						
-- 						next (segment_cursor);
-- 					end loop;
-- 
-- 				end if;
-- 			end query_segments;
-- 
-- 			
-- 		begin -- query_strands_show
-- 			while strand_cursor /= pac_strands.no_element loop
-- 
-- 				query_element (
-- 					position	=> strand_cursor,
-- 					process		=> query_segments'access);
-- 				
-- 				next (strand_cursor);
-- 			end loop;
-- 		end query_strands_show;
-- 
		
		-- A net can be drawn in "normal" mode or highlighted. This flag indicates
		-- that a net has alredy been drawn highlighted so that it won't be drawn
		-- again in "normal" mode.
		-- net_already_drawn : boolean := false;

		
-- 		-- This procedure calls query_strands_show if the current net (indicated by net_cursor)
-- 		-- is selected for highlighting. It sets the flag net_already_drawn in that case.
-- 		procedure highlight_net is
-- 			use pac_net_name;
-- 			ss : type_selected_segment;
-- 		begin
-- 			-- The net selected for highlighting is to be found in the list proposed_segments.
-- 			-- The last element of the proposed_segments points to the net that is to 
-- 			-- be drawn highlighted.
-- 			-- So the list of proposed_segments must contain something:
-- 			if not is_empty (proposed_segments) then
-- 
-- 				-- There must be a selected segment (indicated by cursor selected_segment):
-- 				if selected_segment /= pac_proposed_segments.no_element then
-- 					ss := element (selected_segment);
-- 
-- 					-- The selected_segment must provide a cursor to a net:
-- 					if ss.net /= pac_nets.no_element then
-- 						
-- 						-- The net name of the selected segment must match the name of the
-- 						-- current net (indicated by net_cursor):
-- 						if key (ss.net) = key (net_cursor) then
-- 
-- 							-- Draw the whole net on the current sheet highlighted:
-- 							pac_nets.query_element (
-- 								position	=> net_cursor,
-- 								process		=> query_strands_show'access);
-- 
-- 							-- The net (indicated by net_cursor) must not be drawn again:
-- 							net_already_drawn := true;
-- 						end if;
-- 					end if;
-- 				end if;
-- 			end if;
-- 		end highlight_net;

		
	begin -- query_module
		set_color_nets;

		-- Iterate through the nets:
		while net_cursor /= pac_nets.no_element loop

			
			-- net_already_drawn := false;

			-- If the show mode is active AND the net is selected for highlighting,
			-- then highlight the net and set the flag net_already_drawn:
-- 			case verb is
-- 				when VERB_SHOW => 
-- 					case noun is
-- 						when NOUN_NET => highlight_net;
-- 
-- 						when others => null;
-- 					end case;
-- 		
-- 				when others => null;
-- 			end case;

			-- If the net (indicated by net_cursor) has been drawn already,
			-- don't draw it again. Otherwise draw it in "normal" mode:
			-- if not net_already_drawn then

				-- Draw the net in "normal" mode:
				pac_nets.query_element (
					position	=> net_cursor,
					process		=> query_net'access);

			-- end if;
			
			next (net_cursor); -- advance to next net
		end loop;

	end query_module;



	
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
				
				set_A (line, r.A);
				set_B (line, r.B);

				draw;

			-- If the route DOES require a bend point, then draw first a line
			-- from start point to bend point. Then draw a second line from
			-- bend point end point:
			else
				live_path.bend_point := r.bend_point;

				set_A (line, r.A);
				set_B (line, r.bend_point);
				
				draw;

				set_A (line, r.bend_point);
				set_B (line, r.B);
				
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
		process		=> query_module'access);


	-- Draw a net that is being drawn. If no net is being drawn,
	-- then nothing happens here:
	draw_path;

	
end draw_nets;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
