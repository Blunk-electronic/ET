------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         SCHEMATIC DRAW NETS                              --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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

-- with ada.exceptions;

with et_primitive_objects;			use et_primitive_objects;
with et_text_content;				use et_text_content;
with et_alignment;
with et_nets;						use et_nets;
with et_net_junction;				use et_net_junction;
with et_net_segment;				use et_net_segment;
with et_net_strands;				use et_net_strands;
with et_net_labels;					use et_net_labels;
with et_net_connectors;				use et_net_connectors;
with et_net_names;					use et_net_names;


-- with et_schematic_ops_nets;


separate (et_canvas_schematic)

procedure draw_nets is
	use et_canvas_schematic_nets;
	use pac_nets;
	use pac_strands;
	use pac_net_segments;
	use pac_net_labels;

	
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
-- 								use et_schematic_ops_nets;
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


				-- This procedure queries a net label and draws it:
				procedure query_label (label : in type_net_label) is
					use pac_net_name;
					use pac_draw_text;


					-- This procedure draws a net label:
					procedure draw_label is
						-- Temporarily we store here the position
						-- of the label. In case it is moving, then it
						-- will be overwritten by the tool position:
						position : type_vector_model := get_position (label);
					begin
						-- Overwrite position by get_object_tool_position
						-- if the label is moving.
						if is_moving (label) then
							position := get_object_tool_position;
							--put_line ("label pos" & to_string (position));
						end if;
						
						draw_text (
							content		=> to_content (to_string (net_name)),
							size		=> label.size,
							font		=> net_label_font,
							anchor		=> position,
							origin		=> true,
							
							-- Text rotation about its anchor point.
							-- This is documentational text.
							-- It is readable from the front or the right.
							rotation	=> pac_text.to_rotation (label.rotation),
							alignment	=> net_label_alignment);					
					end draw_label;

					
				begin
					-- If the candidate label is selected, then
					-- draw it highlighted:
					if is_selected (label) then
						set_color_nets (BRIGHT);
					end if;
					
					draw_label;

					if is_selected (label) then
						set_color_nets (NORMAL);
					end if;
				end query_label;



				
				procedure draw_net_connectors is 
					-- The place at which the label is to be drawn.
					-- It will be taken from the A or B end of the segment,
					-- depending on which end has a connector:
					position : type_vector_model;
					
					-- The rotation of the connector must be deduced
					-- from the orientation and affected A/B end
					-- of the net segment:
					rotation : type_rotation := 0.0;
					
					
					-- This procedure draws a net connector:
					procedure draw_connector (label : in type_net_connector) is
						use pac_draw_text;
						use et_alignment;
						use pac_net_name;

						box : type_area;
						
						content : pac_text_content.bounded_string := 
							to_content (to_string (net_name));
						-- CS: append to content the position of the net
						-- on the next sheet (strand position) using the quadrant bars.

						
						procedure make_box is begin
							-- Form a box that wraps around the net name:
							box := to_area (get_text_extents (content, label.size, net_label_font));

							-- Expand the box so that there is some empty space between
							-- text and border:
							box.height := box.height * net_connector_height_to_size_ratio;
							box.width  := box.width  * net_connector_height_to_size_ratio;
						end make_box;
						
						
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
						-- the label rotation and net_connector_text_offset:
						text_position : type_vector_model;

						
					begin
						make_box;

						-- Depending on the rotation the box
						-- is drawn either vertically or horizontally.
						-- Moreover the position, alignment and rotation of the text
						-- inside the box depends on the rotation:
						
						if rotation = zero_rotation then
					
							box.position := set (
								get_x (position), 
								get_y (position) - box.height * 0.5);

							text_rotation := zero_rotation;
							text_position := set (get_x (position) + net_connector_text_offset, 
												  get_y (position));
							
							text_alignment.horizontal := ALIGN_LEFT;
						end if;

						
						if rotation = 90.0 then

							box.position := set (
								get_x (position) - box.height * 0.5,
								get_y (position));

							swap_edges (box);

							text_rotation := 90.0;
							text_position := set (get_x (position), 
												  get_y (position) + net_connector_text_offset);
							
							text_alignment.horizontal := ALIGN_LEFT;
						end if;

						
						if rotation = 180.0 then

							box.position := set (
								get_x (position) - box.width,
								get_y (position) - box.height * 0.5);

							text_rotation := zero_rotation;
							text_position := set (get_x (position) - net_connector_text_offset, 
												  get_y (position));
							
							text_alignment.horizontal := ALIGN_RIGHT;
						end if;

						
						if rotation = 270.0 then

							box.position := set (
								get_x (position) - box.height * 0.5,
								get_y (position) - box.width);
							
							swap_edges (box);

							text_rotation := 90.0;
							text_position := set (get_x (position), 
												  get_y (position) - net_connector_text_offset);
							
							text_alignment.horizontal := ALIGN_RIGHT;
						end if;


						-- If the label is selected then draw it highlighted:
						if is_selected (label) then
							set_color_nets (BRIGHT);
						end if;				

						
						-- Draw the box enshrouding the net name:
						draw_rectangle (
							rectangle	=> box, 
							width		=> net_connector_box_linewidth);

						-- Draw the actual net name:
						draw_text (
							content		=> content,
							size		=> label.size,
							font		=> net_connector_font,
							anchor		=> text_position,
							origin		=> false, -- no origin for net names required
							
							-- Text rotation about its anchor point. This is documentational text.
							-- It is readable from the front or the right.
							rotation	=> text_rotation,
							alignment	=> text_alignment);

						
						if is_selected (label) then
							set_color_nets (NORMAL);
						end if;
					end draw_connector;

					
				begin
					-- put_line ("draw net connectors");
 					
					-- Draw the label on the A end (if it is active):
					if is_active (segment.connectors.A) then
						--put_line ("A is active");
						position := get_A (segment);

						-- Deduce the rotation of the label from
						-- the segment and the affected A end:
						rotation := to_rotation (segment, A);
						
						-- If the parent segment is moving
						-- with its A end, then move the label accordingly
						-- by the current object_displacement:
						if is_A_moving (segment) then
							move_by (position, object_displacement);
						end if;
						
						draw_connector (segment.connectors.A);
					end if;

					
					-- Draw the label on the B end (if it is active):
					if is_active (segment.connectors.B) then
						--put_line ("B is active");
						position := get_B (segment);

						-- Deduce the rotation of the label from
						-- the segment and the affected B end:
						rotation := to_rotation (segment, B);

						-- If the parent segment is moving
						-- with its B end, then move the label accordingly
						-- by the current object_displacement:
						if is_B_moving (segment) then
							move_by (position, object_displacement);
						end if;

						draw_connector (segment.connectors.B);
					end if;					
				end draw_net_connectors;
				
				
				
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
				draw_net_connectors;

				if is_selected (segment) then
					set_color_nets (NORMAL);
				end if;
			end query_segment;

			
			
			procedure query_strand (strand : in type_strand) is
				segment_cursor : pac_net_segments.cursor := strand.segments.first;
			begin
				-- draw nets of the active sheet only:
				if get_sheet (strand.position) = active_sheet then

					-- Increase brightness if strand is selected
					if is_selected (strand) then
						set_color_nets (BRIGHT);
					end if;

					-- Iterate through the segments of the candidate strand:
					while segment_cursor /= pac_net_segments.no_element loop
						query_element (segment_cursor, query_segment'access);
						next (segment_cursor);
					end loop;

					if is_selected (strand) then
						set_color_nets (NORMAL);
					end if;					
				end if;
			end query_strand;
			
			
		begin
			-- Increase brightness if net is selected:
			if is_selected (net) then
				set_color_nets (BRIGHT);
			end if;
				
			-- Iterate through the strands of the candidate net:
			while strand_cursor /= pac_strands.no_element loop
			
				query_element (
					position	=> strand_cursor,
					process		=> query_strand'access);
				
				next (strand_cursor);
			end loop;

			if is_selected (net) then
				set_color_nets (NORMAL);
			end if;
		end query_net;


		
	begin -- query_module
		set_color_nets;

		-- Iterate through the nets:
		while net_cursor /= pac_nets.no_element loop

			pac_nets.query_element (
				position	=> net_cursor,
				process		=> query_net'access);
			
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
