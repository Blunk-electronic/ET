------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          CANVAS DRAW FRAME                               --
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
with cairo;							use cairo;
with et_geometry;
with et_text;						use et_text;
with et_frames;
with et_meta;
with et_canvas_general;
with et_canvas_primitive_draw_ops;

package et_canvas_draw_frame is

generic

	with package draw_ops is new et_canvas_primitive_draw_ops.pac_draw (<>);

	in_area			: draw_ops.pac_shapes.pac_geometry.type_rectangle;
	context			: draw_ops.pac_canvas.type_draw_context;
	frame_size		: et_frames.type_frame_size;
	border_width	: et_frames.type_border_width;
	sectors			: et_frames.type_sectors;
	title_block		: et_frames.type_title_block;  -- incl. common placeholders
	meta			: et_meta.type_basic;
	placeholders	: et_frames.type_placeholders_basic;
	
package pac_draw_frame is
	use draw_ops;
	use draw_ops.pac_shapes.pac_geometry;
	use et_frames;
	use pac_lines;
	
	-- The sector delimiters are short lines between outer an inner border of the frame.
	-- Between the delimiters are the row and column indexes.
	procedure draw_sector_delimiters;

	-- Draws the outer an inder border of the frame:
	procedure draw_border;

	-- Draws a line of the title block. 
	-- The line position is relative to the lower left corner of the title block.
	-- A cairo.strock (context.cr) must be called once all lines have been drawn.
	procedure query_line (cursor : in pac_lines.cursor);

	-- Draws a single text of the title block.
	-- The line position is relative to the lower left corner of the title block.	
	procedure draw_text (
		content	: in type_text_content.bounded_string;
		size	: in type_text_size;
		font	: in type_font;
		pos		: in et_frames.type_position);
	
	-- Draw common placeholders. They apply to both schematic and layout.
	-- Common placeholders are project name, module file name and assembly variant.
	-- Draws other texts such as "approved" or "edited". Such texts have no placeholders:
	procedure draw_texts;
	
end pac_draw_frame;
	
end et_canvas_draw_frame;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
