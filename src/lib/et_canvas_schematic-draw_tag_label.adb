------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          DRAW NET TAG LABEL                              --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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

separate (et_canvas_schematic)

procedure draw_tag_label (
	self	: not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context;
	net		: in pac_net_name.bounded_string;
	label	: in et_schematic.type_net_label)
is
	use et_schematic;
	
	content : pac_text_content.bounded_string := to_content (to_string (net));
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
		box_position := type_point (set (get_x (label.position), get_y (label.position) - box_height * 0.5));
		draw_rectangle (in_area, context, box_position, box_width, box_height, self.frame_height);

		text_rotation := zero_rotation;
		text_position := type_point (set (get_x (label.position) + tag_label_text_offset, get_y (label.position)));
		text_alignment.horizontal := LEFT;
	end if;

	if label.rotation_tag = 90.0 then
		box_position := type_point (set (get_x (label.position) - box_height * 0.5, get_y (label.position)));
		draw_rectangle (in_area, context, box_position, box_height, box_width, self.frame_height);

		text_rotation := 90.0;
		text_position := type_point (set (get_x (label.position), get_y (label.position) + tag_label_text_offset));
		text_alignment.horizontal := LEFT;
	end if;

	if label.rotation_tag = 180.0 then
		box_position := type_point (set (get_x (label.position) - box_width, get_y (label.position) - box_height * 0.5));
		draw_rectangle (in_area, context, box_position, box_width, box_height, self.frame_height);

		text_rotation := zero_rotation;
		text_position := type_point (set (get_x (label.position) - tag_label_text_offset, get_y (label.position)));
		text_alignment.horizontal := RIGHT;
	end if;

	if label.rotation_tag = -90.0 then
		box_position := type_point (set (get_x (label.position) - box_height * 0.5, get_y (label.position) - box_width));
		draw_rectangle (in_area, context, box_position, box_height, box_width, self.frame_height);

		text_rotation := 90.0;
		text_position := type_point (set (get_x (label.position), get_y (label.position) - tag_label_text_offset));
		text_alignment.horizontal := RIGHT;
	end if;
		
	draw_text (
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

end draw_tag_label;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
