------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             PATH AND BEND                                --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                -- 
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
-- <http://www.gnu.org/licenses/>.   
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

generic

package et_geometry_2a.path is
	
-- PATH FROM POINT TO POINT

	type type_bend_style is (
		STRAIGTH_THEN_ANGLED,
		DIRECT,
		ANGLED_THEN_STRAIGHT,
		VERTICAL_THEN_HORIZONTAL,
		HORIZONTAL_THEN_VERTICAL
		);

	type type_bended is (NO, YES);

	
	-- When creating a path from one point to another use this type.
	-- NOTE: This is general stuff. This does apply to all kinds of lines
	-- from one point to another (nets, documentation, tracks, ...) !
	-- If no bend, then we have just a start and an end point which 
	--  will result in a direct line between the two points.
	-- If bended, then we get an extra point where the bending takes place
	--  which will result in two lines that connect the two points:
	type type_path (bended : type_bended) is record
		A, B : type_vector_model;
		case bended is
			when NO		=> null; -- no bend
			when YES	=> bend_point : type_vector_model;
		end case;
	end record;



	-- Computes a path between two points according to the given bend style:
	function to_path (
		A, B	: in type_vector_model;
		style	: in type_bend_style)
		return type_path;
	


	-- When a path is being drawn from one point to another
	-- then we speak about a path from start point to end point
	-- and optionally a bending point where the path changes
	-- direction.
	-- This type is required for all kinds of lines (nets, documentation, tracks, ...)
	-- when being drawn via the GUI.
	type type_path_live is record
		A	: type_vector_model;
		B	: type_vector_model;

		bended		: type_bended := NO;
		bend_point	: type_vector_model;
		bend_style	: type_bend_style := HORIZONTAL_THEN_VERTICAL;
	end record;

	

	-- Switches to the next bend style of the given live path:
	procedure next_bend_style (
		path : in out type_path_live);

	
end et_geometry_2a.path;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
