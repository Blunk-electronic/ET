------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           CANVAS CONTOURS                                --
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

-- with ada.strings.bounded;
-- with ada.strings;
-- with ada.strings.fixed;
-- 


generic

	
package et_canvas.contours is

	use cairo;

	use pac_contours;


	-- This procedure draws a contour.
	-- If filled is YES, then the given linewidth has no meaning
	-- because the linewidth will be set internally to a minimum
	-- that is independed of the zoom-factor.
	-- If filled is NO, then the given linewidth is applied
	-- to the contour and subjected to the current zoom-factor:
	procedure draw_contour (
		contour	: in type_contour'class;
		style	: in type_line_style := CONTINUOUS;
		filled	: in type_filled;
		width	: in type_distance_positive);
		-- CS fill style


	-- This procedure draws a filled contour
	-- with a circular cutout area inside:
	procedure draw_contour_with_circular_cutout (
		outer_border	: in type_contour'class;
		inner_border	: in type_circle);


	-- This procedure draws a filled contour
	-- with an arbitrary cutout area inside:
	procedure draw_contour_with_arbitrary_cutout (
		outer_border	: in type_contour'class;
		inner_border	: in type_contour'class);
	
		
end et_canvas.contours;
