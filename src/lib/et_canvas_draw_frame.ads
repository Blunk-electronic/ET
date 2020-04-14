------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            CANVAS GENERAL                                --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
--                                                                          --
--         Bases on the package gtkada.canvas_view written by               --
--         E. Briot, J. Brobecker and A. Charlet, AdaCore                   --
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
-- DESCRIPTION:
--

-- with ada.text_io;
with gdk.rgba;
with cairo;						use cairo;
with et_geometry;
with et_frames;
with et_canvas_general;
with et_canvas_primitive_draw_ops;

package et_canvas_draw_frame is

generic

	with package draw_ops is new et_canvas_primitive_draw_ops.pac_draw (<>);
	in_area			: draw_ops.pac_shapes.geometry.type_rectangle;
	context			: draw_ops.pac_canvas.type_draw_context;
	frame_height	: draw_ops.pac_shapes.geometry.type_distance_positive; -- CS this is bounding box height
	frame_size		: et_frames.type_size;
	border_width	: et_frames.type_border_width;
	sectors			: et_frames.type_sectors;
	title_block_pos	: et_frames.type_position;
	
package pac_draw_frame is
	use draw_ops;
	use draw_ops.pac_shapes.geometry;
	use et_frames;
	use pac_lines;

	-- The sector delimiters are short lines between outer an inner border of the frame.
	-- Between the delimiters are the row and column indexes.
	procedure draw_sector_delimiters;

	-- Draws the outer an inder border of the frame:
	procedure draw_border;

	-- Draws a line of the title block. The line is offset by the position of the title block.
	procedure query_line (cursor : in pac_lines.cursor);


	
end pac_draw_frame;
	
end et_canvas_draw_frame;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
