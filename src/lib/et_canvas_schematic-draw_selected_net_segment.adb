------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                       DRAW SELECTED NET SEGMENT                          --
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

separate (et_canvas_schematic)

procedure draw_selected_net_segment (
	self	: not null access type_view;
	in_area	: in type_rectangle := no_rectangle;
	context : in type_draw_context;
	segment	: in type_selected_segment) is

	use pac_draw_misc;
	use et_schematic_ops.nets;
	use pac_selected_segments;

	use et_colors;
	use et_schematic;
	use type_nets;
	use type_net_segments;
	use type_net_labels;

	net : type_net_name.bounded_string := key (segment.net);
	
	procedure query_label (c : in type_net_labels.cursor) is 
		use pac_text;
	begin
		case element (c).appearance is
			
			when TAG => 
				draw_tag_label (self, in_area, context, net, element (c));

			when SIMPLE =>	
				draw_text (
					area		=> in_area,
					context		=> context,
					content		=> to_content (to_string (net)),
					size		=> element (c).size,
					font		=> net_label_font,
					position	=> element (c).position,
					origin		=> true,
					
					-- Text rotation around its anchor point.
					-- This is documentational text.
					-- It is readable from the front or the right.
					rotation	=> to_rotation (element (c).rotation_simple),
					alignment	=> (LEFT, BOTTOM),
					height		=> self.frame_height
					);
		end case;
	end query_label;
	
begin
	set_color_nets (context.cr, BRIGHT);

	-- set line width for net segments:
	set_line_width (context.cr, type_view_coordinate (net_line_width));

	-- draw the net segment:
	draw_line (
		area		=> in_area,
		context		=> context,
		line		=> element (segment.segment),
		height		=> self.frame_height
		);

	-- draw all labels attached to the segment:
	iterate (element (segment.segment).labels, query_label'access);
	
end draw_selected_net_segment;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
