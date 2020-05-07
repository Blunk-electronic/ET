------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          PRIMITIVE DRAW OPS                              --
--                                                                          --
--                               S p e c                                    --
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
-- DESCRIPTION:
-- 
-- This package provides primite draw operations for the board.
-- It bases on an instance of the general canvas package. The general canvas
-- package has been instantiated for example in et_canvas_schematic or et_canvas_board.
-- The instantiated canvas package is required because the draw operations ALWAYS require
-- the drawing information provided in the type_view.drawing.
--
-- This package also bases on a instantiated shapes package because it is about
-- shapes here.
-- and extends the type_view by the type_drawing. The latter is the link
-- to the actual drawing. The type_drawing provides information on sheet size,
-- fame bounding box, paper size etc. These information is frequently used
-- by various draw operations.
--  Further-on the generic package for primitve draw operations (et_canvas_draw.pac_draw)
-- is instantiated here so that lots of draw operations can use pac_draw_package.

with glib;					use glib;
with cairo;					use cairo;

with et_geometry;			use et_geometry;
with et_text;
with et_canvas_general;

package et_canvas_primitive_draw_ops is

generic

	-- The instantiated canvas package:
	with package pac_canvas is new et_canvas_general.pac_canvas (<>);

	-- The instantiated shapes package:
	with package pac_shapes is new et_geometry.shapes_2d (<>);

	-- The instantiated text package:
	with package pac_text is new et_text.text (<>);
	
package pac_draw is
	use pac_canvas;
	use pac_shapes;
	use pac_shapes.geometry;

	-- This procedure draws the given line on the given context.
	-- The line is shifted in y to a plane of given height. This plane
	-- has y-axis going downwards.
	-- The line will be drawn if its bounding box intersects the given area.
	-- If area is no_rectangle then the line would be drawn in any case.
	procedure draw_line (
		area			: in type_rectangle;	
		context			: in type_draw_context;
		line			: in type_line'class;
		height			: in pac_shapes.geometry.type_distance);
		

	-- This procedure draws the given arc on the given context.
	-- The arc is shifted in y to a plane of given height. This plane
	-- has y-axis going downwards.
	-- The arc will be drawn if its bounding box intersects the given area.
	-- If area is no_rectangle then the arc would be drawn in any case.
	procedure draw_arc (
		area	: in type_rectangle;
		context	: in type_draw_context;
		arc		: in type_arc'class;
		height	: in pac_shapes.geometry.type_distance);

	-- This procedure draws the given circle on the given context.
	-- The circle is shifted in y to a plane of given height. This plane
	-- has y-axis going downwards.
	-- The circle will be drawn if its bounding box intersects the given area.
	-- If area is no_rectangle then the circle would be drawn in any case.
	-- If the circle is filled, the line width will be set to zero
	-- and left with this setting.
	procedure draw_circle (
		area	: in type_rectangle;
		context	: in type_draw_context;
		circle	: in type_circle'class;
		filled	: in type_filled;
		height	: in pac_shapes.geometry.type_distance);
		-- CS fill style ?

	procedure draw_polygon (
		area	: in type_rectangle;
		context	: in type_draw_context;
		polygon	: in type_polygon_base'class;
		filled	: in type_filled;
		-- CS fill style
		height	: in pac_shapes.geometry.type_distance);
	
	-- This procedure draws the a rectangle on the given context.
	-- The rectangle is shifted in y to a plane of given height. This plane
	-- has y-axis going downwards.
	-- The rectangle will be drawn if its bounding box intersects the given area.
	-- If area is no_rectangle then the rectangle would be drawn in any case.
	procedure draw_rectangle (
		area			: in type_rectangle;
		context			: in type_draw_context;
		position		: in type_point'class;	-- position of the rectangle (lower left corner)
		width			: in pac_shapes.geometry.type_distance;		-- widht of the rectangle
		height			: in pac_shapes.geometry.type_distance;		-- height of the rectangle
		frame_height	: in pac_shapes.geometry.type_distance;
		extend_boundaries	: in boolean := false;
		boundaries_to_add	: in type_boundaries := boundaries_default);
		-- CS fill style ?

	
-- TEXT
	use et_text;
	use pac_text;
	
	conversion_factor_mm_to_pt : constant gdouble := 1.53; -- CS use exact factor
	
	function to_points (size : in pac_text.type_text_size)
		return gdouble;

	-- Draws a text right in the view.
	-- Does not care about area and bounding box. It is assumed that the calling
	-- unit has decided whether the text is to be drawn or not. No area check here.
	procedure draw_text (
		context		: in type_draw_context;
		content		: in type_text_content.bounded_string;
		size		: in pac_text.type_text_size;
		font		: in et_text.type_font;
		x,y			: in gdouble; -- the anchor point in the view
		origin		: in boolean; -- when true, an origin is drawn at the anchor point
		rotation	: in pac_shapes.geometry.type_rotation;
		alignment	: in type_text_alignment);

	-- Computes for the given text content, size and font the extents.
	function get_text_extents (
		context		: in type_draw_context;
		content		: in type_text_content.bounded_string;
		size		: in pac_text.type_text_size;
		font		: in et_text.type_font)
		return cairo_text_extents;
	
	-- Draws a text in the drawing plane.
	-- Draws the text in case it is inside the given area or whether the
	-- text intersects the given area.
	-- If area is no_rectangle then the text would be drawn in any case.
	procedure draw_text (
		area		: in type_rectangle; -- in model plane
		context		: in type_draw_context;
		content		: in type_text_content.bounded_string;
		size		: in pac_text.type_text_size;
		font		: in et_text.type_font;
		position	: in type_point; -- the anchor point in the drawing, the origin
		origin		: in boolean; -- when true, an origin is drawn at the anchor point
		rotation	: in type_rotation;
		alignment	: in type_text_alignment;
		height		: in pac_shapes.geometry.type_distance); -- the height of the drawing frame
	
end pac_draw;

end et_canvas_primitive_draw_ops;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
