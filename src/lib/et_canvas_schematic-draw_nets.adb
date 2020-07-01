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

with ada.text_io;				use ada.text_io;

with et_schematic;				use et_schematic;

use et_schematic.type_nets;
use et_schematic.type_strands;
use et_schematic.type_net_segments;

separate (et_canvas_schematic)

procedure draw_nets (
	self    : not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context) is

	procedure draw_tag_label (
		net		: in et_general.type_net_name.bounded_string;
		label	: in type_net_label) is
		use et_text;
		use pac_draw_misc;

		-- type type_line is new pac_shapes.type_line with null record;
		-- line_left : type_line;

		content : type_text_content.bounded_string := to_content (to_string (net));
		-- CS append to content the position of the net on the next sheet (strand position)
		
		-- The position, width and height of the enshrouding box (lower left corner)
		-- as if the box was drawn for a label in zero rotation:
		box_position : type_point;
		box_width	: type_distance_positive;
		box_height	: constant type_distance_positive := type_distance_positive (label.size) * tag_label_height_to_size_ratio;

		-- The text rotation must be either 0 or 90 degree (documentational text) and is thus
		-- to be calculated according to the rotation of the label:
		text_rotation : type_rotation;

		-- The alignment is assigned as if the text were drawn at zero rotation.
		-- The vertical alignment is always CENTER. Horizontal alignment changes depending on 
		-- the rotation of the label:
		text_alignment : type_text_alignment := (vertical => CENTER, horizontal => <>);

		-- The text position is not the same as the label position, thus it must be 
		-- calculated according to the label rotation and tag_label_text_offset (see et_schematic specs):
		text_position : type_point;
	begin
		set_line_width (context.cr, type_view_coordinate (tag_label_box_line_width));
		
		-- CS paint box outline depending on label signal direction

		-- Calculate the box width according to text content, size and font:
		box_width := type_distance_positive (get_text_extents (context, content, label.size, net_label_font).width)
					 + 2.0 * tag_label_text_offset;

		if label.rotation_tag = zero_rotation then
			box_position := type_point (set (x (label.position), y (label.position) - box_height * 0.5));
			draw_rectangle (in_area, context, box_position, box_width, box_height, self.frame_height);

			text_rotation := zero_rotation;
			text_position := type_point (set (x (label.position) + tag_label_text_offset, y (label.position)));
			text_alignment.horizontal := LEFT;
		end if;

		if label.rotation_tag = 90.0 then
			box_position := type_point (set (x (label.position) - box_height * 0.5, y (label.position)));
			draw_rectangle (in_area, context, box_position, box_height, box_width, self.frame_height);

			text_rotation := 90.0;
			text_position := type_point (set (x (label.position), y (label.position) + tag_label_text_offset));
			text_alignment.horizontal := LEFT;
		end if;

		if label.rotation_tag = 180.0 then
			box_position := type_point (set (x (label.position) - box_width, y (label.position) - box_height * 0.5));
			draw_rectangle (in_area, context, box_position, box_width, box_height, self.frame_height);

			text_rotation := zero_rotation;
			text_position := type_point (set (x (label.position) - tag_label_text_offset, y (label.position)));
			text_alignment.horizontal := RIGHT;
		end if;

		if label.rotation_tag = -90.0 then
			box_position := type_point (set (x (label.position) - box_height * 0.5, y (label.position) - box_width));
			draw_rectangle (in_area, context, box_position, box_height, box_width, self.frame_height);

			text_rotation := 90.0;
			text_position := type_point (set (x (label.position), y (label.position) - tag_label_text_offset));
			text_alignment.horizontal := RIGHT;
		end if;
			
		pac_draw_misc.draw_text (
			area		=> in_area,
			context		=> context,
			content		=> content,
			size		=> label.size,
			font		=> net_label_font,
			position	=> text_position,
			origin		=> false, -- no origin for net names required
			
			-- Text rotation about its anchor point. This is documentational text.
			-- It is readable from the front or the right.
			rotation	=> text_rotation,

			alignment	=> text_alignment,
			height		=> self.frame_height
			);


		cairo.stroke (context.cr);
	end draw_tag_label;
	
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
				junction : type_junction_symbol := junction_symbol;

				procedure draw_junction is begin
					pac_draw_misc.draw_circle (
						area		=> in_area,
						context		=> context,
						circle		=> junction,
						filled		=> YES,
						height		=> self.frame_height);
				end draw_junction;

				procedure query_label (c : in type_net_labels.cursor) is 
					use type_net_labels;
					use et_text;
					use pac_text;
				begin
					--put_line ("label at" & to_string (element (c).position));

					case element (c).appearance is
						when SIMPLE =>
							pac_draw_misc.draw_text (
								area		=> in_area,
								context		=> context,
								content		=> to_content (to_string (key (net_cursor))),
								size		=> element (c).size,
								font		=> net_label_font,
								position	=> element (c).position,
								origin		=> true, -- CS must be false on export to image
								
								-- Text rotation around its anchor point.
								-- This is documentational text.
								-- It is readable from the front or the right.
								rotation	=> to_rotation (element (c).rotation_simple),
								alignment	=> (LEFT, BOTTOM),
								height		=> self.frame_height
								);

						when TAG =>
							draw_tag_label (net_name, element (c));

					end case;
				end query_label; 
					
			begin -- query_segments
				-- draw nets of the active sheet only:
				if strand.position.sheet = current_active_sheet then
					
					while segment_cursor /= type_net_segments.no_element loop

						-- set line width for net segments:
						cairo.set_line_width (context.cr, type_view_coordinate (et_schematic.net_line_width));

						-- draw the net segment:
						pac_draw_misc.draw_line (
							area		=> in_area,
							context		=> context,
							line		=> element (segment_cursor),
							height		=> self.frame_height
							);

						cairo.stroke (context.cr);
						
						-- draw junctions:
						
						-- at start point of segment:
						if element (segment_cursor).junctions.start_point then
							junction.center := element (segment_cursor).start_point;
							draw_junction;
						end if;

						-- at end point of segment:
						if element (segment_cursor).junctions.end_point then
							junction.center := element (segment_cursor).end_point;
							draw_junction;
						end if;

						-- draw labels
						type_net_labels.iterate (element (segment_cursor).labels, query_label'access);
						
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

		cairo.stroke (context.cr);
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
