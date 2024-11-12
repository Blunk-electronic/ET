------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         PRIMITIVE OBJECTS                                --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--


package et_primitive_objects is
		

	type type_direction_of_rotation is (
		CW,		-- clockwise
		CCW);	-- counterclockwise

	function to_string (direction : in type_direction_of_rotation) return string;
	function to_direction (direction : in string) return type_direction_of_rotation;

	-- Changes CCW to CW and vice versa.
	function reverse_direction (direction : in type_direction_of_rotation)
		return type_direction_of_rotation;

	


	type type_shape is (LINE, ARC, CIRCLE);

	function to_shape (shape : in string) return type_shape;
	function to_string (shape : in type_shape) return string;
	


	
	type type_filled is (NO, YES);
	function to_string (filled : in type_filled) return string;
	function to_filled (filled : in string) return type_filled;
	filled_default : constant type_filled := NO;


	-- FILL STYLE OF OBJECTS WITH A CLOSED CIRCUMFENCE		
	
	type type_fill_style is (SOLID, HATCHED);
	fill_style_default : constant type_fill_style := SOLID;
	
	function to_string (fill_style : in type_fill_style) return string;
	function to_fill_style (fill_style : in string) return type_fill_style;

	

	
	-- Whether a line, arc, circle or contour is drawn dashed or not:
	type type_line_style is (CONTINUOUS, DASHED);
	-- CS other pattersn like jotted, dash-point, ... ?

	
		
end et_primitive_objects;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
