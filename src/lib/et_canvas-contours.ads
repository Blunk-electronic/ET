------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           CANVAS CONTOURS                                --
--                                                                          --
--                               S p e c                                    --
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


generic

	
package et_canvas.contours is

	use pac_contours;


	-- This procedure draws a contour.
	-- If the given linewidth is zero, then
	-- it will be set internally to a minimum
	-- that is independed of the zoom-factor.
	-- It is recommended to set the linewidth to zero when
	-- the contour is to be filled.
	-- This procedure performs a stroke after drawing
	-- the contour.
	-- If a segment of the contour is being moved by the
	-- operator then it gets drawn according to the current
	-- point_of_attack and object_tool (see procedure draw_line in
	-- package et_canvas):
	procedure draw_contour (
		contour	: in type_contour'class;

		-- This is the position of the parent object (incl. rotation)					   
		pos 	: in type_position := origin_zero_rotation;

		-- This is the offset by which the contour is moved
		-- in addition to pos:
		offset	: in type_position := origin_zero_rotation;
		-- CS currently the rotation of the contour about itself is ignored.
		
		style	: in type_line_style := CONTINUOUS;
		
		filled	: in type_filled;
		-- NOTE: If the given contour is not closed, then
		-- it will not be filled, regardless of this argument.
		-- In that case the contour will be drawn with a minimal
		-- linewidth.
		
		width	: in type_distance_positive;
		mirror	: in type_mirror := MIRROR_NO);
		-- CS fill style


	-- This procedure draws a filled contour
	-- with a circular cutout area inside:
	procedure draw_contour_with_circular_cutout (
		outer_border	: in type_contour'class;
		inner_border	: in type_circle;
		pos 			: in type_position := origin_zero_rotation; -- includes x,y, rotation
		offset			: in type_position := origin_zero_rotation;
		mirror			: in type_mirror := MIRROR_NO);

	
	-- This procedure draws a filled contour
	-- with an arbitrary cutout area inside:
	procedure draw_contour_with_arbitrary_cutout (
		outer_border	: in type_contour'class;
		inner_border	: in type_contour'class;
		pos 			: in type_position := origin_zero_rotation; -- includes x,y, rotation
		offset			: in type_position := origin_zero_rotation;
		mirror			: in type_mirror := MIRROR_NO);

	
		
end et_canvas.contours;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
