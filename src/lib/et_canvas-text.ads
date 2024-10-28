------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             CANVAS TEXT                                  --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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


with et_text;					use et_text;
with et_alignment;				use et_alignment;


generic

	
package et_canvas.text is


	-- Converts a given text size (in mm) to size in points
	-- according to the current zoom-factor:
	function to_points (size : in pac_text.type_text_size)
		return type_logical_pixels;


	-- In cairo all angles increase in clockwise direction
	-- and must be specified in radians. But our model-domain
	-- uses angles specified in degrees increasing in counter-clockwise
	-- direction. This function converts from degrees to cairo-angles:
	function to_cairo_angle (
		angle : in type_rotation)
		return glib.gdouble;


	
	
	type type_align_mode_vertical is (
		-- In this mode the text is aligned basing on
		-- the y-bearing and used size exlusively.
		-- There is no baseline. Uppercase and lowercase letters
		-- are not aligned with a baseline:
		MODE_ALIGN_BY_USED_SPACE,

		-- In this mode the text is aligned relative to
		-- the baseline and based to the real text size
		-- specified in the model domain:
		MODE_ALIGN_RELATIVE_TO_BASELINE);


	
	-- This function computes the extents of the 
	-- given text content, size and font
	-- according to the current zoom-factor:
	function get_text_extents (
		content		: in et_text.pac_text_content.bounded_string;
		size		: in pac_text.type_text_size;
		font		: in et_text.type_font)
		return cairo.cairo_text_extents;



	
	-- Converts cairo_text_extents to an area (in the model-domain)
	-- according to the current zoom-factor.
	-- NOTE: Since cairo_text_extents does not include the position
	-- of a text, the returned area has position (0;0).
	-- CS: unifiy this function with above function get_text_extents ?
	function to_area (
		extents : in cairo.cairo_text_extents)
		return type_area;


	
	-- This function computes the canvas point where
	-- a text of given extents and requested alignment
	-- is to be drawn:
	function get_text_start_point (
		extents		: in cairo.cairo_text_extents;
		alignment	: in type_text_alignment;
		anchor		: in type_logical_pixels_vector; -- the anchor point of the text
		mode_v		: in type_align_mode_vertical;
		size		: in pac_text.type_text_size) -- the size of the text
		return type_logical_pixels_vector;


	
	
	-- Draws a text on the canvas:
	procedure draw_text (
		content		: in et_text.pac_text_content.bounded_string;
		size		: in pac_text.type_text_size;
		font		: in et_text.type_font;
		anchor		: in type_vector_model; -- the anchor point in the model
		origin		: in boolean; -- when true, an origin is drawn at the anchor point
		rotation	: in type_rotation;
		alignment	: in type_text_alignment);


	
	-- Draw a vectorized text with the
	-- given linewidth:
	procedure draw_vector_text (
		text	: in pac_text.type_vector_text);

	
	
end et_canvas.text;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
